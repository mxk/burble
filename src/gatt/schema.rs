use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::{mem, u128};

use structbuf::{Pack, Packer, StructBuf, Unpack};

use crate::att::{Access, Handle, Perms};
use crate::gap::{Uuid, Uuid16};

use super::*;

/// Schema data offset type. `u16` is enough for 3k 128-bit characteristics.
type Offset = u16;

/// Database schema.
///
/// Defines the structure of all services, assigns attribute handles, and
/// contains all attribute values required to calculate the database hash
/// ([Vol 3] Part G, Section 7.3.1).
#[derive(Clone, Debug, Default)]
pub struct Schema {
    /// Attribute metadata sorted by handle.
    attr: Vec<Attr>,
    /// Concatenated data for GATT profile attributes. For 128-bit attributes,
    /// the data is preceded by a 16-byte UUID. 128-bit characteristic
    /// declaration and subsequent value share the same UUID:
    /// `<props, handle, (uuid>, ...)`.
    data: Vec<u8>,
    /// Database hash.
    hash: u128,
}

impl Schema {
    /// Creates an empty schema.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        // TODO: Tune for minimum GATT server
        Self {
            attr: Vec::with_capacity(16),
            data: Vec::with_capacity(64),
            hash: 0,
        }
    }

    /// Defines a primary service ([Vol 3] Part G, Section 3.1). Services with
    /// 16-bit UUIDs should be defined before those with 128-bit UUIDs.
    #[inline]
    pub fn primary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        includes: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<Characteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Type::PRIMARY_SERVICE, uuid.into(), includes.as_ref());
        (hdl, chars(&mut self.builder(Characteristics)))
    }

    /// Defines a secondary service ([Vol 3] Part G, Section 3.1). Services with
    /// 16-bit UUIDs should be defined before those with 128-bit UUIDs.
    #[inline]
    pub fn secondary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        includes: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<Characteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Type::SECONDARY_SERVICE, uuid.into(), includes.as_ref());
        (hdl, chars(&mut self.builder(Characteristics)))
    }

    /// Defines a primary or secondary service.
    pub fn service(&mut self, typ: Uuid16, uuid: Uuid, includes: &[Handle]) -> Handle {
        let hdl = self.decl(typ, |v| v.uuid(uuid));
        for &hdl in includes {
            self.include(hdl);
        }
        hdl
    }

    /// Returns a new builder.
    #[inline]
    fn builder<T>(&mut self, v: T) -> Builder<T> {
        Builder(self, v)
    }

    /// Defines a service include ([Vol 3] Part G, Section 3.2).
    fn include(&mut self, hdl: Handle) {
        let (start, end) = self.service_group(hdl).expect("invalid service handle");
        let uuid = (start.len == 2).then(|| self.value(start).unpack().u16());
        let end = end.hdl;
        self.decl(Type::INCLUDE, |v| {
            v.u16(hdl).u16(end);
            uuid.map(|u| v.u16(u));
        });
    }

    /// Creates a read-only GATT profile declaration with value set by `val`.
    #[inline]
    fn decl(&mut self, typ: Uuid16, val: impl FnOnce(&mut Packer)) -> Handle {
        fn push(s: &mut Schema, typ: Uuid16, val: &[u8]) -> Handle {
            let (hdl, off) = s.next_attr();
            #[allow(clippy::cast_possible_truncation)]
            s.attr.push(Attr {
                hdl,
                typ: Some(typ),
                perms: Perms::new(Access::read()),
                off,
                len: val.len() as _,
            });
            s.data.extend_from_slice(val);
            hdl
        }
        // Maximum length of a characteristic declaration value, which is the
        // longest value stored in the schema ([Vol 3] Part G, Section 3.3.1).
        let mut b = StructBuf::new(1 + 2 + 16);
        val(&mut b.append());
        push(self, typ, &b)
    }

    /// Creates a generic attribute.
    fn attr(&mut self, typ: Uuid, perms: Perms) -> Handle {
        let (hdl, off) = self.next_attr();
        let typ16 = typ.as_uuid16();
        if typ16.is_none() {
            self.data.extend_from_slice(&u128::from(typ).to_le_bytes());
        }
        self.attr.push(Attr {
            hdl,
            typ: typ16,
            perms,
            off,
            len: 0,
        });
        hdl
    }

    /// Returns the next unused handle.
    #[inline]
    fn next_handle(&self) -> Handle {
        (self.attr.last()).map_or(Handle::MIN, |at| {
            at.hdl.next().expect("maximum handle reached")
        })
    }

    /// Returns the next unused handle and data offset for a new attribute.
    #[inline]
    fn next_attr(&self) -> (Handle, u16) {
        let off = Offset::try_from(self.data.len())
            .expect("schema data overflow (see Offset type in gatt/schema.rs)");
        (self.next_handle(), off)
    }

    /// Returns the index and entry of the specified handle or [`None`] if the
    /// handle is invalid.
    #[inline]
    fn get(&self, hdl: Handle) -> Option<(usize, &Attr)> {
        fn search(hdl: Handle, attrs: &[Attr]) -> Option<(usize, &Attr)> {
            attrs.binary_search_by(|at| at.hdl.cmp(&hdl)).ok().map(|i| {
                // SAFETY: Entry was found at i
                (i, unsafe { attrs.get_unchecked(i) })
            })
        }
        let i = usize::from(hdl) - 1;
        // The entry can exist either at or before index `i` (if there are
        // gaps). Most of the time, the 1-based handle value should also be the
        // 0-based index.
        let attrs = match self.attr.get(i) {
            Some(at) if at.hdl == hdl => return Some((i, at)),
            // SAFETY: `i` is in bounds
            Some(_) => unsafe { self.attr.get_unchecked(..i) },
            None => &self.attr,
        };
        search(hdl, attrs)
    }

    /// Returns the attribute type.
    #[inline]
    fn typ(&self, at: &Attr) -> Uuid {
        at.typ.map_or_else(
            // SAFETY: 128-bit UUID is at self.data[at.off..at.off+16]
            || unsafe {
                #[allow(clippy::cast_ptr_alignment)]
                let p = self.data.as_ptr().add(usize::from(at.off)).cast::<u128>();
                Uuid::new_unchecked(u128::from_le(p.read_unaligned()))
            },
            Uuid16::as_uuid,
        )
    }

    /// Returns the attribute value.
    #[inline]
    fn value(&self, at: &Attr) -> &[u8] {
        let i = usize::from(at.off) + (usize::from(at.typ.is_none()) << 4);
        // SAFETY: i..i + at.len is always valid
        unsafe { self.data.get_unchecked(i..i + usize::from(at.len)) }
    }

    /// Returns the start and end entries of the service group defined by `hdl`
    /// or [`None`] if the handle does not refer to a service.
    fn service_group(&self, hdl: Handle) -> Option<(&Attr, &Attr)> {
        let (i, at) = self.get(hdl)?;
        if !matches!(
            at.typ,
            Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE)
        ) {
            return None;
        }
        let mut it = self.attr.iter();
        it.nth(i);
        let n = it
            .take_while(|at| {
                !matches!(
                    at.typ,
                    Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE)
                )
            })
            .count();
        // SAFETY: i and i + n are in bounds
        Some(unsafe { (self.attr.get_unchecked(i), self.attr.get_unchecked(i + n)) })
    }
}

/// Attribute entry. 128-bit UUIDs are stored in the schema data at `off`.
#[derive(Clone, Debug)]
#[must_use]
struct Attr {
    hdl: Handle,
    typ: Option<Uuid16>,
    perms: Perms,
    off: Offset,
    len: u8,
}

/// Schema builder used to define services and characteristics.
#[allow(missing_debug_implementations)]
pub struct Builder<'a, T>(&'a mut Schema, T);

impl<T> Deref for Builder<'_, T> {
    type Target = Schema;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<T> DerefMut for Builder<'_, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// [`Builder`] type.
#[allow(missing_debug_implementations)]
#[derive(Default)]
pub struct Characteristics;

impl Builder<'_, Characteristics> {
    /// Defines a single-value characteristic ([Vol 3] Part G, Section 3.3).
    /// Mandatory service characteristics must precede optional ones and 16-bit
    /// UUID characteristics should come before 128-bit ones.
    #[inline]
    pub fn characteristic<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        props: Prop,
        perms: impl Into<Perms>,
        descs: impl FnOnce(&mut Builder<Descriptors>) -> T,
    ) -> (Handle, T) {
        let hdl = self.decl_value(uuid.into(), props, perms.into());
        let b = &mut self.builder(Descriptors {
            need_ext_props: props.contains(Prop::EXT_PROPS),
            ..Descriptors::default()
        });
        let v = descs(b);
        b.finalize();
        (hdl, v)
    }

    // TODO: Method for multi-value characteristics

    /// Adds characteristic and characteristic value declarations.
    fn decl_value(&mut self, uuid: Uuid, props: Prop, perms: Perms) -> Handle {
        let hdl = self.next_handle().next().expect("maximum handle reached");
        #[allow(clippy::cast_possible_truncation)]
        let off = (self.data.len() + 3) as u16;
        self.decl(Type::CHARACTERISTIC, |v| {
            v.u8(props.bits()).u16(hdl).uuid(uuid);
        });
        self.next_attr(); // Ensure that `off` is valid
        self.attr.push(Attr {
            hdl,
            typ: uuid.as_uuid16(),
            perms,
            off,
            len: 0,
        });
        hdl
    }
}

/// [`Builder`] type.
#[allow(missing_debug_implementations)]
#[derive(Default)]
pub struct Descriptors {
    need_ext_props: bool,
    have_cccd: bool,
    have_aggregate_fmt: bool,
    fmt: [Option<Handle>; 4], // TODO: Reasonable limit?
}

impl Builder<'_, Descriptors> {
    /// Declares a characteristic descriptor ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn descriptor(&mut self, uuid: impl Into<Uuid>, perms: impl Into<Perms>) -> Handle {
        self.attr(uuid.into(), perms.into())
    }

    /// Declares a Characteristic Extended Properties descriptor
    /// ([Vol 3] Part G, Section 3.3.3.1).
    pub fn ext_props(&mut self, props: ExtProp) {
        assert!(
            self.1.need_ext_props,
            "EXT_PROPS not set or descriptor already exists"
        );
        self.1.need_ext_props = false;
        self.decl(Type::CHARACTERISTIC_EXTENDED_PROPERTIES, |v| {
            v.u16(props.bits());
        });
    }

    /// Declares a Client Characteristic Configuration descriptor
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    pub fn client_cfg(&mut self, perms: impl Into<Perms>) -> Handle {
        assert!(!self.1.have_cccd, "descriptor already exists");
        self.1.have_cccd = true;
        self.attr(
            Type::CLIENT_CHARACTERISTIC_CONFIGURATION.as_uuid(),
            perms.into(),
        )
    }

    /// Declares a Characteristic Presentation Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.5).
    pub fn presentation_fmt(
        &mut self,
        fmt: Format,
        exp: i8,
        unit: Unit,
        desc: Description,
    ) -> Handle {
        let Some(i) = self.1.fmt.iter().position(Option::is_none) else {
            panic!("presentation format limit reached");
        };
        let hdl = self.decl(Type::CHARACTERISTIC_PRESENTATION_FORMAT, |v| {
            v.u8(fmt).i8(exp).u16(unit).u8(desc.ns()).u16(desc.raw());
        });
        self.1.fmt[i] = Some(hdl);
        hdl
    }

    /// Declares a Characteristic Aggregate Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.6).
    pub fn aggregate_fmt(&mut self, hdls: impl Iterator<Item = Handle>) {
        assert!(!self.1.have_aggregate_fmt, "descriptor already exists");
        self.1.have_aggregate_fmt = true;
        self.decl(Type::CHARACTERISTIC_AGGREGATE_FORMAT, |v| {
            for hdl in hdls {
                v.u16(hdl);
            }
        });
    }

    /// Finalizes characteristic definition by adding required descriptors.
    fn finalize(&mut self) {
        if self.1.need_ext_props {
            self.ext_props(ExtProp::empty());
        }
        if !self.1.have_aggregate_fmt && self.1.fmt[1].is_some() {
            let fmt = mem::take(&mut self.1.fmt);
            self.aggregate_fmt(fmt.into_iter().map_while(|h| h));
        }
    }
}

trait UuidPacker {
    fn uuid(&mut self, u: impl Into<Uuid>);
}

impl UuidPacker for Packer<'_> {
    /// Writes either a 16- or a 128-bit UUID at the current index.
    #[inline]
    fn uuid(&mut self, u: impl Into<Uuid>) {
        let u = u.into();
        match u.as_u16() {
            Some(v) => self.u16(v),
            None => self.u128(u),
        };
    }
}

#[cfg(test)]
mod tests {
    use std::ops::RangeInclusive;
    use std::ptr::addr_of;

    use super::*;

    #[test]
    fn service_group() {
        fn eq(s: &Schema, h: Handle, r: RangeInclusive<usize>) {
            let (start, end) = s.service_group(h).unwrap();
            assert_eq!(start as *const _, addr_of!(s.attr[*r.start()]));
            assert_eq!(end as *const _, addr_of!(s.attr[*r.end()]));
        }

        let mut s = Schema::new();
        let (h1, _) = s.primary_service(Uuid16::sig(1), [], |_| {});
        let (h2, _) = s.primary_service(Uuid16::sig(2), [h1], |_| {});
        eq(&s, h1, 0..=0);
        eq(&s, h2, 1..=2);

        let (h3, _) = s.primary_service(Uuid16::sig(3), [], |_| {});
        eq(&s, h2, 1..=2);
        eq(&s, h3, 3..=3);
    }
}
