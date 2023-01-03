use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};

use smallvec::SmallVec;
use structbuf::{Pack, Packer, StructBuf, Unpack};

use crate::att::{Access, Handle, Perms};
use crate::gap::{Uuid, Uuid16};

use super::*;

/// Schema data index type. `u16` is enough for 3k 128-bit characteristics.
type Idx = u16;

/// Read-only database schema.
///
/// Describes the service structure, attribute permissions, and attribute values
/// used in the database hash calculation.
#[derive(Clone, Debug, Default)]
pub struct Schema {
    /// Attribute metadata sorted by handle.
    attr: Box<[Attr]>,
    /// Concatenated GATT profile attribute values and 128-bit UUIDs.
    data: Box<[u8]>,
    /// Database hash.
    hash: u128,
}

impl Schema {
    /// Creates a new schema builder.
    #[inline]
    #[must_use]
    pub fn build() -> SchemaBuilder {
        SchemaBuilder {
            attr: Vec::with_capacity(128),
            data: Vec::with_capacity(1024),
            ..SchemaBuilder::default()
        }
    }

    /// Returns the database hash ([Vol 3] Part G, Section 7.3).
    #[inline(always)]
    #[must_use]
    pub const fn hash(&self) -> u128 {
        self.hash
    }

    /// Returns the attribute type.
    #[inline]
    fn typ(&self, at: &Attr) -> Uuid {
        at.typ.map_or_else(
            // SAFETY: 128-bit UUID is at self.data[at.val.0..]
            || unsafe {
                #[allow(clippy::cast_ptr_alignment)]
                let p = (self.data.as_ptr().add(usize::from(at.val.0))).cast::<u128>();
                Uuid::new_unchecked(u128::from_le(p.read_unaligned()))
            },
            Uuid16::as_uuid,
        )
    }
}

impl CommonOps for Schema {
    #[inline(always)]
    fn attr(&self) -> &[Attr] {
        &self.attr
    }

    #[inline(always)]
    fn data(&self) -> &[u8] {
        &self.data
    }
}

/// Attribute entry.
#[derive(Clone, Copy, Debug)]
#[must_use]
struct Attr {
    hdl: Handle,
    typ: Option<Uuid16>,
    val: (Idx, Idx),
    perms: Perms,
}

impl Attr {
    /// Returns whether the attribute is a service declaration.
    #[inline(always)]
    const fn is_service(&self) -> bool {
        matches!(
            self.typ,
            Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE)
        )
    }

    /// Returns the attribute value length.
    #[inline(always)]
    const fn len(&self) -> usize {
        self.val.1 as usize - self.val.0 as usize
    }
}

/// Operations shared by [`Schema`] and [`SchemaBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns the index and attribute entry of the specified handle or
    /// [`None`] if the handle is invalid.
    #[inline]
    #[must_use]
    fn get(&self, hdl: Handle) -> Option<(usize, &Attr)> {
        fn search(attr: &[Attr], hdl: Handle) -> Option<(usize, &Attr)> {
            attr.binary_search_by(|at| at.hdl.cmp(&hdl)).ok().map(|i| {
                // SAFETY: Attribute was found at `i`
                (i, unsafe { attr.get_unchecked(i) })
            })
        }
        let i = usize::from(hdl) - 1;
        // The attribute can exist either at or before index `i` (if there are
        // gaps). Most of the time, the 1-based handle value should also be the
        // 0-based index.
        let attr = match self.attr().get(i) {
            Some(at) if at.hdl == hdl => return Some((i, at)),
            // SAFETY: `i` is in bounds
            Some(_) => unsafe { self.attr().get_unchecked(..i) },
            None => self.attr(),
        };
        search(attr, hdl)
    }

    /// Returns the attribute value.
    #[inline(always)]
    #[must_use]
    fn value(&self, at: &Attr) -> &[u8] {
        // SAFETY: self.data()[val] is always valid
        unsafe { (self.data()).get_unchecked(at.val.0 as usize..at.val.1 as usize) }
    }

    /// Returns all attributes of the service group defined by `hdl` or an empty
    /// slice if the handle does not refer to a service.
    fn service_group(&self, hdl: Handle) -> &[Attr] {
        let Some((i, at)) = self.get(hdl) else { return &[] };
        if !at.is_service() {
            return &[];
        }
        let mut it = self.attr().iter();
        it.nth(i);
        let j = (it.position(Attr::is_service)).map_or(self.attr().len(), |n| i + 1 + n);
        // SAFETY: `i..j` is in bounds
        unsafe { self.attr().get_unchecked(i..j) }
    }
}

/// Schema builder used to define services, characteristics, and descriptors.
#[derive(Debug, Default)]
pub struct SchemaBuilder {
    attr: Vec<Attr>,
    data: Vec<u8>,
    need_ext_props: bool,
    have_cccd: bool,
    have_aggregate_fmt: bool,
    fmt: SmallVec<[Handle; 4]>,
}

impl SchemaBuilder {
    /// Returns the final read-only schema.
    #[inline]
    #[must_use]
    pub fn freeze(self) -> Schema {
        let hash = self.calc_hash();
        Schema {
            attr: self.attr.into_boxed_slice(),
            data: self.data.into_boxed_slice(),
            hash,
        }
    }

    /// Defines a primary service ([Vol 3] Part G, Section 3.1). 16-bit UUID
    /// services should precede 128-bit ones.
    #[inline]
    pub fn primary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        includes: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Type::PRIMARY_SERVICE, uuid.into(), includes.as_ref());
        (hdl, chars(&mut self.builder()))
    }

    /// Defines a secondary service ([Vol 3] Part G, Section 3.1). 16-bit UUID
    /// services should precede 128-bit ones.
    #[inline]
    pub fn secondary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        includes: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Type::SECONDARY_SERVICE, uuid.into(), includes.as_ref());
        (hdl, chars(&mut self.builder()))
    }

    /// Declares a primary or secondary service and any includes
    /// ([Vol 3] Part G, Section 3.2).
    pub fn service(&mut self, typ: Uuid16, uuid: Uuid, includes: &[Handle]) -> Handle {
        let hdl = self.decl(typ, |v| v.uuid(uuid));
        for &inc in includes {
            let (start, end) = ends_of(self.service_group(inc)).expect("invalid service handle");
            let uuid = (start.len() == 2).then(|| self.value(start).unpack().u16());
            let end = end.hdl;
            self.decl(Type::INCLUDE, |v| {
                v.u16(inc).u16(end);
                uuid.map(|u| v.u16(u));
            });
        }
        hdl
    }

    /// Creates a read-only GATT profile declaration with value set by `val`.
    #[inline]
    fn decl(&mut self, typ: Uuid16, val: impl FnOnce(&mut Packer)) -> Handle {
        fn append_attr(s: &mut SchemaBuilder, typ: Uuid16, val: &[u8]) -> Handle {
            let hdl = s.next_handle();
            let val = s.append_data(val);
            s.attr.push(Attr {
                hdl,
                typ: Some(typ),
                val,
                perms: Perms::new(Access::READ),
            });
            hdl
        }
        // Maximum length of the Characteristic declaration value, which is the
        // longest value stored in the schema ([Vol 3] Part G, Section 3.3.1).
        let mut b = StructBuf::new(1 + 2 + 16);
        val(&mut b.append());
        append_attr(self, typ, &b)
    }

    /// Creates a generic attribute with an externally stored value.
    fn attr(&mut self, typ: Uuid, perms: Perms) -> Handle {
        let typ16 = typ.as_uuid16();
        if typ16.is_none() {
            self.append_data(u128::from(typ).to_le_bytes());
        }
        self.append_attr(self.next_handle(), typ16, perms)
    }

    /// Returns the next unused handle.
    #[inline]
    fn next_handle(&self) -> Handle {
        self.attr.last().map_or(Handle::MIN, |at| {
            at.hdl.next().expect("maximum handle reached")
        })
    }

    /// Appends a new attribute entry. If `typ == None`, then the last 16 data
    /// bytes must contain the 128-bit UUID.
    #[inline]
    fn append_attr(&mut self, hdl: Handle, typ: Option<Uuid16>, perms: Perms) -> Handle {
        #[allow(clippy::cast_possible_truncation)]
        let i = match typ {
            None => (self.data.len() - 16) as Idx,
            Some(_) => 0,
        };
        self.attr.push(Attr {
            hdl,
            typ,
            val: (i, i),
            perms,
        });
        hdl
    }

    /// Appends `v` to schema data and returns the resulting index range.
    #[inline]
    fn append_data(&mut self, v: impl AsRef<[u8]>) -> (Idx, Idx) {
        #[allow(clippy::cast_possible_truncation)]
        let start = self.data.len() as Idx;
        self.data.extend_from_slice(v.as_ref());
        let end = Idx::try_from(self.data.len())
            .expect("schema data overflow (see Idx type in gatt/schema.rs)");
        (start, end)
    }

    /// Calculates the database hash ([Vol 3] Part G, Section 7.3.1).
    #[must_use]
    fn calc_hash(&self) -> u128 {
        use aes::Aes128;
        use cmac::digest::{FixedOutput, Key};
        use cmac::{Cmac, Mac};
        let mut m = Cmac::<Aes128>::new(&Key::<Aes128>::default());
        for at in &self.attr {
            let Some(typ) = at.typ else { continue };
            let val = match typ {
                Type::PRIMARY_SERVICE
                | Type::SECONDARY_SERVICE
                | Type::INCLUDE
                | Type::CHARACTERISTIC
                | Type::CHARACTERISTIC_EXTENDED_PROPERTIES => self.value(at),
                Type::CHARACTERISTIC_USER_DESCRIPTION
                | Type::CLIENT_CHARACTERISTIC_CONFIGURATION
                | Type::SERVER_CHARACTERISTIC_CONFIGURATION
                | Type::CHARACTERISTIC_PRESENTATION_FORMAT
                | Type::CHARACTERISTIC_AGGREGATE_FORMAT => &[],
                _ => continue,
            };
            m.update(&u16::from(at.hdl).to_le_bytes());
            m.update(&u16::from(typ).to_le_bytes());
            m.update(val);
        }
        u128::from_be_bytes(*m.finalize_fixed().as_ref())
    }

    /// Returns a new builder.
    #[inline(always)]
    fn builder<T>(&mut self) -> Builder<T> {
        Builder(self, PhantomData::default())
    }
}

impl CommonOps for SchemaBuilder {
    #[inline(always)]
    fn attr(&self) -> &[Attr] {
        &self.attr
    }

    #[inline(always)]
    fn data(&self) -> &[u8] {
        &self.data
    }
}

/// Service and characteristic schema builder.
#[derive(Debug)]
#[repr(transparent)]
pub struct Builder<'a, T>(&'a mut SchemaBuilder, PhantomData<T>);

impl<T> Deref for Builder<'_, T> {
    type Target = SchemaBuilder;

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

/// [`Builder`] marker type.
#[derive(Debug, Default)]
pub struct ServiceCharacteristics;

impl Builder<'_, ServiceCharacteristics> {
    /// Defines a single-value characteristic ([Vol 3] Part G, Section 3.3).
    /// Mandatory service characteristics must precede optional ones and 16-bit
    /// UUID characteristics should precede 128-bit ones.
    #[inline]
    pub fn characteristic<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        props: Prop,
        perms: impl Into<Perms>,
        descs: impl FnOnce(&mut Builder<CharacteristicDescriptors>) -> T,
    ) -> (Handle, T) {
        let hdl = self.decl_value(uuid.into(), props, perms.into());
        let mut b = self.builder(props.contains(Prop::EXT_PROPS));
        let v = descs(&mut b);
        b.finalize();
        (hdl, v)
    }

    // TODO: Method for multi-value characteristics

    /// Adds characteristic and characteristic value declarations.
    fn decl_value(&mut self, uuid: Uuid, props: Prop, perms: Perms) -> Handle {
        let hdl = self.next_handle().next().expect("maximum handle reached");
        self.decl(Type::CHARACTERISTIC, |v| {
            v.u8(props.bits()).u16(hdl).uuid(uuid);
        });
        self.append_attr(hdl, uuid.as_uuid16(), perms)
    }

    /// Returns a new descriptor builder.
    fn builder(&mut self, need_ext_props: bool) -> Builder<CharacteristicDescriptors> {
        self.need_ext_props = need_ext_props;
        self.have_cccd = false;
        self.have_aggregate_fmt = false;
        // SAFETY: 0 is a valid length and there is nothing to drop
        unsafe { self.fmt.set_len(0) };
        self.0.builder()
    }
}

/// [`Builder`] marker type.
#[derive(Debug, Default)]
pub struct CharacteristicDescriptors;

impl Builder<'_, CharacteristicDescriptors> {
    /// Declares a non-GATT profile characteristic descriptor
    /// ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn descriptor(&mut self, uuid: impl Into<Uuid>, perms: impl Into<Perms>) -> Handle {
        self.0.attr(uuid.into(), perms.into())
    }

    /// Declares a Characteristic Extended Properties descriptor
    /// ([Vol 3] Part G, Section 3.3.3.1).
    pub fn ext_props(&mut self, props: ExtProp) {
        assert!(
            self.need_ext_props,
            "EXT_PROPS not set or descriptor already exists"
        );
        self.need_ext_props = false;
        self.decl(Type::CHARACTERISTIC_EXTENDED_PROPERTIES, |v| {
            v.u16(props.bits());
        });
    }

    /// Declares a Client Characteristic Configuration descriptor
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    pub fn client_cfg(&mut self, perms: impl Into<Perms>) -> Handle {
        assert!(!self.have_cccd, "descriptor already exists");
        self.have_cccd = true;
        self.0.attr(
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
        let hdl = self.decl(Type::CHARACTERISTIC_PRESENTATION_FORMAT, |v| {
            v.u8(fmt).i8(exp).u16(unit).u8(desc.ns()).u16(desc.raw());
        });
        self.fmt.push(hdl);
        hdl
    }

    /// Declares a Characteristic Aggregate Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.6).
    pub fn aggregate_fmt(&mut self, hdls: impl Iterator<Item = Handle>) {
        assert!(!self.have_aggregate_fmt, "descriptor already exists");
        self.have_aggregate_fmt = true;
        self.decl(Type::CHARACTERISTIC_AGGREGATE_FORMAT, |v| {
            for hdl in hdls {
                v.u16(hdl);
            }
        });
    }

    /// Finalizes characteristic definition by adding required descriptors.
    fn finalize(&mut self) {
        if self.need_ext_props {
            self.ext_props(ExtProp::empty());
        }
        if !self.have_aggregate_fmt && self.fmt.len() > 1 {
            let fmt = mem::take(&mut self.fmt);
            self.aggregate_fmt(fmt.into_iter());
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

/// Returns the first and last items in `v`.
#[inline(always)]
const fn ends_of<T>(v: &[T]) -> Option<(&T, &T)> {
    match *v {
        [] => None,
        [ref a] => Some((a, a)),
        [ref a, .., ref b] => Some((a, b)),
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;
    use std::ops::RangeInclusive;
    use std::ptr::addr_of;

    use crate::gap::{CharacteristicId, GattServiceId};

    use super::*;

    #[test]
    fn service_group() {
        fn eq(s: &SchemaBuilder, h: Handle, r: RangeInclusive<usize>) {
            let (start, end) = ends_of(s.service_group(h)).unwrap();
            assert_eq!(start as *const _, addr_of!(s.attr[*r.start()]));
            assert_eq!(end as *const _, addr_of!(s.attr[*r.end()]));
        }

        let mut s = Schema::build();
        let (h1, _) = s.primary_service(Uuid16::sig(1), [], |_| {});
        let (h2, _) = s.primary_service(Uuid16::sig(2), [h1], |_| {});
        eq(&s, h1, 0..=0);
        eq(&s, h2, 1..=2);

        let (h3, _) = s.primary_service(Uuid16::sig(3), [], |_| {});
        eq(&s, h2, 1..=2);
        eq(&s, h3, 3..=3);
    }

    #[test]
    fn hash() {
        let mut s = Schema::build();
        s.primary_service(GattServiceId::GenericAccess, [], |s| {
            s.characteristic(
                CharacteristicId::DeviceName,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                |_| {},
            );
            s.characteristic(
                CharacteristicId::Appearance,
                Prop::READ,
                Access::READ,
                |_| {},
            )
        });
        s.primary_service(GattServiceId::GenericAttribute, [], |s| {
            s.characteristic(
                CharacteristicId::ServiceChanged,
                Prop::INDICATE,
                Access::NONE,
                |s| {
                    s.client_cfg(Access::READ_WRITE);
                },
            );
            s.characteristic(
                CharacteristicId::ClientSupportedFeatures,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                |_| {},
            );
            s.characteristic(
                CharacteristicId::DatabaseHash,
                Prop::READ,
                Access::READ,
                |_| {},
            );
        });
        s.primary_service(GattServiceId::Glucose, [], |s| {
            // Hack to include a service that hasn't been defined yet
            s.decl(Type::INCLUDE, |v| {
                v.u16(0x14_u16).u16(0x16_u16).u16(0x180F_u16);
            });
            s.characteristic(
                CharacteristicId::GlucoseMeasurement,
                Prop::READ | Prop::INDICATE | Prop::EXT_PROPS,
                Access::READ,
                |s| {
                    s.client_cfg(Access::READ_WRITE);
                },
            );
        });
        s.secondary_service(GattServiceId::Battery, [], |s| {
            s.characteristic(
                CharacteristicId::BatteryLevel,
                Prop::READ,
                Access::READ,
                |_| {},
            );
        });
        assert_eq!(
            s.freeze().hash,
            0xF1_CA_2D_48_EC_F5_8B_AC_8A_88_30_BB_B9_FB_A9_90
        );
    }
}
