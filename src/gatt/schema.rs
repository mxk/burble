use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Range};

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
        SchemaBuilder::new()
    }

    /// Returns the database hash ([Vol 3] Part G, Section 7.3).
    #[inline(always)]
    #[must_use]
    pub const fn hash(&self) -> u128 {
        self.hash
    }

    /// Returns an iterator over primary services or [`None`] if there are no
    /// primary services in the specified handle range.
    pub(crate) fn primary_services(&self, hdls: HandleRange) -> Option<PrimaryServices> {
        let r = self.get_range(hdls)?;
        Some(PrimaryServices {
            schema: self,
            rng: r.off + r.attr.iter().position(Attr::is_primary_service)?..r.off + r.attr.len(),
        })
    }

    /// Returns the next attribute group defined by start (inclusive) and end
    /// (exclusive) functions, where the starting index must be in the specified
    /// range.
    ///
    /// # Safety
    ///
    /// `rng` must be a valid subslice of all attributes.
    unsafe fn next_group(
        &self,
        rng: Range<usize>,
        start: impl Fn(&Attr) -> bool,
        end: impl Fn(&Attr) -> bool,
    ) -> Option<AttrRange> {
        let i = rng.start + self.attr.get_unchecked(rng).iter().position(start)?;
        let j = (self.attr.get_unchecked(i + 1..).iter().position(end))
            .map_or(self.attr.len(), |j| i + 1 + j);
        Some(self.range(i..j))
    }

    /// Returns all attributes within the specified handle range or [`None`] if
    /// the handle range is empty.
    fn get_range(&self, hdls: HandleRange) -> Option<AttrRange> {
        let i = self
            .get(hdls.start())
            .map_or_else(|i| (i < self.attr.len()).then_some(i), |(i, _)| Some(i))?;
        let j = self
            .get(hdls.end())
            .map_or_else(|j| (j > 0).then_some(j), |(j, _)| Some(j + 1))?;
        Some(self.range(i..j))
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
    const PRI: Uuid16 = Declaration::PrimaryService.uuid16();
    const SEC: Uuid16 = Declaration::SecondaryService.uuid16();

    /// Returns whether the attribute is a service declaration.
    #[inline(always)]
    const fn is_service(&self) -> bool {
        matches!(self.typ, Some(Self::PRI | Self::SEC))
    }

    /// Returns whether the attribute is a primary service declaration.
    #[inline(always)]
    const fn is_primary_service(&self) -> bool {
        matches!(self.typ, Some(Self::PRI))
    }

    /// Returns the attribute value length.
    #[inline(always)]
    const fn len(&self) -> usize {
        self.val.1 as usize - self.val.0 as usize
    }
}

/// A non-empty subset of attributes.
#[derive(Clone, Copy, Debug)]
struct AttrRange<'a> {
    off: usize,
    attr: &'a [Attr],
}

impl AttrRange<'_> {
    /// Returns the first attribute.
    #[inline(always)]
    fn first(&self) -> &Attr {
        // SAFETY: self.attr is non-empty
        unsafe { self.attr.get_unchecked(0) }
    }

    /// Returns the last attribute.
    #[inline(always)]
    fn last(&self) -> &Attr {
        // SAFETY: self.attr is non-empty
        unsafe { self.attr.get_unchecked(self.attr.len() - 1) }
    }
}

impl Deref for AttrRange<'_> {
    type Target = [Attr];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.attr
    }
}

/// Iterator over primary services within a handle range.
#[derive(Clone, Debug)]
pub struct PrimaryServices<'a> {
    schema: &'a Schema,
    rng: Range<usize>,
}

impl<'a> Iterator for PrimaryServices<'a> {
    type Item = (Handle, Handle, &'a [u8]);

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: self.rng is always a valid subslice
        let g = unsafe {
            self.schema
                .next_group(self.rng.clone(), Attr::is_primary_service, Attr::is_service)?
        };
        self.rng.start = self.rng.end.min(g.off + g.attr.len());
        let first = g.first();
        Some((first.hdl, g.last().hdl, self.schema.value(first)))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.rng.end - self.rng.start))
    }
}

/// Operations shared by [`Schema`] and [`SchemaBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns an attribute range.
    #[inline(always)]
    fn range(&self, r: Range<usize>) -> AttrRange {
        debug_assert!(r.start < r.end && r.end <= self.attr().len());
        AttrRange {
            off: r.start,
            // SAFETY: r is a valid non-empty range
            attr: unsafe { self.attr().get_unchecked(r) },
        }
    }

    /// Returns the index and attribute entry of the specified handle or the
    /// index where the handle can be inserted.
    #[inline]
    fn get(&self, hdl: Handle) -> std::result::Result<(usize, &Attr), usize> {
        fn search(attr: &[Attr], hdl: Handle) -> std::result::Result<(usize, &Attr), usize> {
            // Handle will often be 0xFFFF, so do an explicit comparison with
            // the last entry to avoid doing a binary search.
            match *attr {
                [] => Err(0),
                [.., ref last] if last.hdl < hdl => Err(attr.len()),
                _ => (attr.binary_search_by(|at| at.hdl.cmp(&hdl)))
                    // SAFETY: Attribute was found at `i`
                    .map(|i| unsafe { (i, attr.get_unchecked(i)) }),
            }
        }
        let i = usize::from(hdl) - 1;
        // The attribute can exist either at or before index `i` (if there are
        // gaps). Most of the time, the 1-based handle value should also be the
        // 0-based index.
        let attr = match self.attr().get(i) {
            Some(at) if at.hdl == hdl => return Ok((i, at)),
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

    /// Returns all attributes of the service group defined by `hdl` or [`None`]
    /// if the handle does not refer to a service.
    fn service_group(&self, hdl: Handle) -> Option<AttrRange> {
        let Ok((i, at)) = self.get(hdl) else { return None };
        at.is_service().then(|| {
            // SAFETY: i < self.attr.len()
            let j = unsafe { self.attr().get_unchecked(i + 1..).iter() }
                .position(Attr::is_service)
                .map_or(self.attr().len(), |n| i + 1 + n);
            self.range(i..j)
        })
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
    /// Creates a new schema builder.
    #[inline]
    #[must_use]
    fn new() -> Self {
        Self {
            attr: Vec::with_capacity(128),
            data: Vec::with_capacity(1024),
            ..Self::default()
        }
    }

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

    /// Defines a primary service ([Vol 3] Part G, Section 3.1).
    ///
    /// 16-bit UUID services should precede 128-bit ones.
    #[inline]
    pub fn primary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        include: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Declaration::PrimaryService, uuid.into(), include.as_ref());
        (hdl, chars(self.builder()))
    }

    /// Defines a secondary service ([Vol 3] Part G, Section 3.1).
    ///
    /// 16-bit UUID services should precede 128-bit ones.
    #[inline]
    pub fn secondary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        include: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Declaration::SecondaryService, uuid.into(), include.as_ref());
        (hdl, chars(self.builder()))
    }

    /// Declares a primary or secondary service and any included services
    /// ([Vol 3] Part G, Section 3.2).
    pub fn service(&mut self, typ: Declaration, uuid: Uuid, include: &[Handle]) -> Handle {
        let hdl = self.decl(typ, |v| v.uuid(uuid));
        for &inc in include {
            let g = self.service_group(inc).expect("invalid service handle");
            let uuid = (g.first().len() == 2).then(|| self.value(g.first()).unpack().u16());
            let end = g.last().hdl;
            self.decl(Declaration::Include, |v| {
                v.u16(inc).u16(end);
                uuid.map(|u| v.u16(u));
            });
        }
        hdl
    }

    /// Creates a read-only GATT profile declaration with value set by `val`.
    #[inline]
    fn decl(&mut self, typ: impl Into<Uuid16>, val: impl FnOnce(&mut Packer)) -> Handle {
        fn append_attr(this: &mut SchemaBuilder, typ: Uuid16, val: &[u8]) -> Handle {
            let hdl = this.next_handle();
            let val = this.append_data(val);
            this.attr.push(Attr {
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
        append_attr(self, typ.into(), &b)
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
        use Descriptor::*;
        let mut m = Cmac::<Aes128>::new(&Key::<Aes128>::default());
        for at in &self.attr {
            let Some(typ) = at.typ else { continue };
            let val = match Declaration::try_from(typ) {
                Ok(_) => self.value(at),
                _ => match Descriptor::try_from(typ) {
                    Ok(CharacteristicExtendedProperties) => self.value(at),
                    Ok(
                        CharacteristicUserDescription
                        | ClientCharacteristicConfiguration
                        | ServerCharacteristicConfiguration
                        | CharacteristicPresentationFormat
                        | CharacteristicAggregateFormat,
                    ) => &[],
                    _ => continue,
                },
            };
            m.update(&u16::from(at.hdl).to_le_bytes());
            m.update(&u16::from(typ).to_le_bytes());
            m.update(val);
        }
        u128::from_be_bytes(*m.finalize_fixed().as_ref())
    }

    /// Returns a new builder.
    #[inline(always)]
    fn builder<T>(&mut self) -> &mut Builder<T> {
        // SAFETY: Builder is a `repr(transparent)` onetype
        unsafe { &mut *(self as *mut Self).cast() }
    }
}

/// Service and characteristic schema builder.
#[derive(Debug)]
#[repr(transparent)]
pub struct Builder<T>(SchemaBuilder, PhantomData<T>);

impl<T> Builder<T> {
    /// Creates a generic attribute with an externally stored value.
    fn attr(&mut self, typ: Uuid, perms: Perms) -> Handle {
        let typ16 = typ.as_uuid16();
        if typ16.is_none() {
            self.append_data(u128::from(typ).to_le_bytes());
        }
        let hdl = self.next_handle();
        self.append_attr(hdl, typ16, perms)
    }
}

impl<T> Deref for Builder<T> {
    type Target = SchemaBuilder;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Builder<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// [`Builder`] marker type.
#[derive(Debug, Default)]
pub struct ServiceCharacteristics;

impl Builder<ServiceCharacteristics> {
    /// Defines a single-value characteristic ([Vol 3] Part G, Section 3.3).
    ///
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
        let b = self.builder(props.contains(Prop::EXT_PROPS));
        let v = descs(b);
        b.finalize();
        (hdl, v)
    }

    // TODO: Method for multi-value characteristics

    /// Adds characteristic and characteristic value declarations.
    fn decl_value(&mut self, uuid: Uuid, props: Prop, perms: Perms) -> Handle {
        let hdl = self.next_handle().next().expect("maximum handle reached");
        self.decl(Declaration::Characteristic, |v| {
            v.u8(props.bits()).u16(hdl).uuid(uuid);
        });
        self.append_attr(hdl, uuid.as_uuid16(), perms)
    }

    /// Returns a new descriptor builder.
    fn builder(&mut self, need_ext_props: bool) -> &mut Builder<CharacteristicDescriptors> {
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

impl Builder<CharacteristicDescriptors> {
    /// Declares a non-GATT profile characteristic descriptor
    /// ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn descriptor(&mut self, uuid: impl Into<Uuid>, perms: impl Into<Perms>) -> Handle {
        self.attr(uuid.into(), perms.into())
    }

    /// Declares a Characteristic Extended Properties descriptor
    /// ([Vol 3] Part G, Section 3.3.3.1).
    ///
    /// This descriptor will be added automatically if the characteristic
    /// properties contain `EXT_PROPS` flag.
    pub fn ext_props(&mut self, props: ExtProp) {
        assert!(
            self.need_ext_props,
            "EXT_PROPS not set or descriptor already exists"
        );
        self.need_ext_props = false;
        self.decl(Descriptor::CharacteristicExtendedProperties, |v| {
            v.u16(props.bits());
        });
    }

    /// Declares a Client Characteristic Configuration descriptor
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    pub fn client_cfg(&mut self, perms: impl Into<Perms>) -> Handle {
        assert!(!self.have_cccd, "descriptor already exists");
        self.have_cccd = true;
        self.attr(
            Descriptor::ClientCharacteristicConfiguration.uuid(),
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
        let hdl = self.decl(Descriptor::CharacteristicPresentationFormat, |v| {
            v.u8(fmt).i8(exp).u16(unit).u8(desc.ns()).u16(desc);
        });
        self.fmt.push(hdl);
        hdl
    }

    /// Declares a Characteristic Aggregate Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.6).
    ///
    /// This descriptor will be added automatically when more than one
    /// Presentation Format descriptor is present.
    pub fn aggregate_fmt(&mut self, hdls: impl AsRef<[Handle]>) {
        assert!(!self.have_aggregate_fmt, "descriptor already exists");
        self.have_aggregate_fmt = true;
        self.decl(Descriptor::CharacteristicAggregateFormat, |v| {
            for &hdl in hdls.as_ref() {
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
            let fmt = std::mem::take(&mut self.fmt);
            self.aggregate_fmt(&fmt);
            self.fmt = fmt; // Preserve any allocated capacity
        }
    }
}

/// Packer extension functions.
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

    use super::*;

    #[test]
    fn service_group() {
        fn eq(b: &SchemaBuilder, h: Handle, r: Range<usize>) {
            let g = b.service_group(h).unwrap();
            assert_eq!(g.off..g.off + g.attr.len(), r);
        }

        let mut b = Schema::build();
        let (h1, _) = b.primary_service(Service::GenericAccess, [], |_| {});
        let (h2, _) = b.primary_service(Service::GenericAttribute, [h1], |_| {});
        eq(&b, h1, 0..1);
        eq(&b, h2, 1..3);

        let (h3, _) = b.primary_service(Service::Battery, [], |_| {});
        eq(&b, h2, 1..3);
        eq(&b, h3, 3..4);
    }

    /// Example database hash ([Vol 3] Part G, Appendix B).
    #[test]
    fn hash() {
        let mut b = Schema::build();
        b.primary_service(Service::GenericAccess, [], |b| {
            b.characteristic(
                Characteristic::DeviceName,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                |_| {},
            );
            b.characteristic(Characteristic::Appearance, Prop::READ, Access::READ, |_| {})
        });
        b.primary_service(Service::GenericAttribute, [], |b| {
            b.characteristic(
                Characteristic::ServiceChanged,
                Prop::INDICATE,
                Access::NONE,
                |b| b.client_cfg(Access::READ_WRITE),
            );
            b.characteristic(
                Characteristic::ClientSupportedFeatures,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                |_| {},
            );
            b.characteristic(
                Characteristic::DatabaseHash,
                Prop::READ,
                Access::READ,
                |_| {},
            );
        });
        b.primary_service(Service::Glucose, [], |b| {
            // Hack to include a service that hasn't been defined yet
            b.decl(Declaration::Include, |v| {
                v.u16(0x0014_u16).u16(0x0016_u16).u16(0x180F_u16);
            });
            b.characteristic(
                Characteristic::GlucoseMeasurement,
                Prop::READ | Prop::INDICATE | Prop::EXT_PROPS,
                Access::READ,
                |b| b.client_cfg(Access::READ_WRITE),
            );
        });
        b.secondary_service(Service::Battery, [], |b| {
            b.characteristic(
                Characteristic::BatteryLevel,
                Prop::READ,
                Access::READ,
                |_| {},
            );
        });
        assert_eq!(
            b.freeze().hash,
            0xF1_CA_2D_48_EC_F5_8B_AC_8A_88_30_BB_B9_FB_A9_90
        );
    }
}
