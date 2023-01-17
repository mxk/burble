use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Range};
use std::{iter, slice};

use smallvec::SmallVec;
use structbuf::{Pack, Packer, StructBuf, Unpack};
use tracing::info;

use crate::att::{Access, Handle, Perms};
use crate::gap::{Uuid, Uuid16, UuidPacker, UuidType, UuidVec};

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

    /// Returns an iterator over primary services with optional UUID matching.
    #[inline]
    pub fn primary_services(
        &self,
        start: Handle,
        uuid: Option<Uuid>,
    ) -> impl Iterator<Item = SchemaEntry<ServiceDef>> {
        let i = self.get(start).unwrap_or_else(|i| i);
        let uuid = uuid.map_or_else(UuidVec::default, UuidVec::new);
        // SAFETY: `0 <= i <= self.attr.len()`
        GroupIter::new(self, unsafe { self.attr.get_unchecked(i..) }, move |at| {
            at.is_primary_service() && (uuid.is_empty() || self.value(at) == uuid.as_ref())
        })
    }

    // Returns an iterator over service includes.
    pub fn includes(&self, hdls: HandleRange) -> impl Iterator<Item = SchemaEntry<IncludeDef>> {
        (self.service_attrs(hdls).iter())
            .map_while(|at| at.is_include().then(|| SchemaEntry::new(self, at, at.hdl)))
    }

    // Returns an iterator over service characteristics.
    pub fn characteristics(
        &self,
        hdls: HandleRange,
    ) -> impl Iterator<Item = SchemaEntry<CharacteristicDef>> {
        GroupIter::new(self, self.service_attrs(hdls), Attr::is_char)
    }

    /// Returns an iterator over characteristic descriptors.
    pub fn descriptors(
        &self,
        hdls: HandleRange,
    ) -> impl Iterator<Item = SchemaEntry<DescriptorDef>> {
        let attr = self.subset(hdls).and_then(|s| {
            use private::Group;
            // SAFETY: `0 <= s.off < self.attr.len()`
            let decl = unsafe { self.attr.get_unchecked(..s.off).iter() }
                .rfind(|&at| Attr::is_char(at))?;
            let val_hdl = self.value(decl).unpack().split_at(1).1.u16();
            // Handle range must start after the characteristic value and cannot
            // cross characteristic boundary.
            (Handle::new(val_hdl).map_or(false, |h| h < s.first().hdl)
                && !(s.attr.iter()).any(|at| CharacteristicDef::is_next_group(at.typ)))
            .then_some(s.attr)
        });
        (attr.unwrap_or_default().iter()).map(|at| SchemaEntry::new(self, at, at.hdl))
    }

    /// Logs schema contents.
    pub fn dump(&self) {
        use Declaration::*;
        macro_rules! log {
            ($at:ident, $fmt:expr$(, $($args:tt)*)?) => {
                info!("[{:#06X}] {}", u16::from($at.hdl), format_args!($fmt$(, $($args)*)?))
            };
        }
        let mut val_hdl = Handle::MIN;
        let mut last_char_hdl = Handle::MIN;
        let mut cont = ' ';
        info!("GATT schema:");
        for at in self.attr.iter() {
            let v = self.value(at);
            let mut v = v.unpack();
            if let Some(typ) = at.typ {
                match typ.typ() {
                    UuidType::Declaration(d) => match d {
                        PrimaryService | SecondaryService => {
                            last_char_hdl = (self.service_group(at.hdl).unwrap().attr.iter())
                                .rfind(|at| at.is_char())
                                .map_or(Handle::MIN, |at| at.hdl);
                            let sec = ((!at.is_primary_service()).then_some("(Secondary) "))
                                .unwrap_or_default();
                            let uuid = Uuid::try_from(v.as_ref()).unwrap();
                            if let Some(UuidType::Service(s)) = uuid.as_uuid16().map(Uuid16::typ) {
                                log!(at, "{sec}{s} <{uuid}>");
                            } else {
                                log!(at, "{sec}Service <{uuid}>");
                            }
                        }
                        Include => log!(at, "|__ [Include {:#06X}..={:#06X}]", v.u16(), v.u16()),
                        Characteristic => {
                            cont = if at.hdl < last_char_hdl { '|' } else { ' ' };
                            let _prop = Prop::from_bits(v.u8()).unwrap();
                            val_hdl = Handle::new(v.u16()).unwrap();
                            let uuid = Uuid::try_from(v.as_ref()).unwrap();
                            if let Some(UuidType::Characteristic(c)) =
                                uuid.as_uuid16().map(Uuid16::typ)
                            {
                                log!(at, "|__ {c} <{uuid}>");
                            } else {
                                log!(at, "|__ Characteristic <{uuid}>");
                            }
                        }
                    },
                    UuidType::Characteristic(_) => log!(at, "{cont}   |__ [Value <{typ}>]"),
                    UuidType::Descriptor(d) => log!(at, "{cont}   |__ {d} <{typ}>"),
                    t => log!(at, "Unexpected {t}"),
                }
            } else {
                let typ = self.typ(at);
                if at.hdl <= val_hdl {
                    log!(at, "{cont}   |__ [Value <{typ}>]");
                } else {
                    log!(at, "{cont}   |__ Descriptor <{typ}>");
                }
            }
        }
    }

    /// Returns a subset of attributes for one service. The service declaration
    /// is skipped.
    fn service_attrs(&self, hdls: HandleRange) -> &[Attr] {
        let attr = self.subset(hdls).and_then(|s| {
            let attr = if s.first().is_service() {
                // SAFETY: `s` is not empty
                unsafe { s.attr.get_unchecked(1..) }
            } else {
                s.attr
            };
            // Handle range cannot cross service boundary
            (!attr.iter().any(Attr::is_service)).then_some(attr)
        });
        attr.unwrap_or_default()
    }

    /// Returns all attributes within the specified handle range or [`None`] if
    /// the handle range is empty.
    fn subset(&self, hdls: HandleRange) -> Option<Subset> {
        let i = self
            .get(hdls.start())
            .map_or_else(|i| (i < self.attr.len()).then_some(i), Some)?;
        let j = self
            .get(hdls.end())
            .map_or_else(|j| (j > 0).then_some(j), |j| Some(j + 1))?;
        Some(Subset::new(&self.attr, i..j))
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

/// Schema service definition marker type.
#[derive(Debug)]
pub struct ServiceDef;

/// Schema characteristic definition marker type.
#[derive(Debug)]
pub struct CharacteristicDef;

/// Schema include definition marker type.
#[derive(Debug)]
pub struct IncludeDef;

/// Schema descriptor definition marker type.
#[derive(Debug)]
pub struct DescriptorDef;

/// Trait implemented by [`ServiceDef`] and [`CharacteristicDef`] markers.
pub trait Group: private::Group {}

impl Group for ServiceDef {}
impl Group for CharacteristicDef {}

mod private {
    use super::*;

    pub trait Group {
        /// Offset of the UUID in the declaration value.
        const UUID_OFF: usize = 0;

        #[inline(always)]
        fn is_next_group(typ: Option<Uuid16>) -> bool {
            matches!(typ, Some(Attr::PRI | Attr::SEC))
        }
    }

    impl Group for ServiceDef {}

    impl Group for CharacteristicDef {
        const UUID_OFF: usize = 3;

        #[inline(always)]
        fn is_next_group(typ: Option<Uuid16>) -> bool {
            // INC isn't needed, but including it improves the generated code
            matches!(typ, Some(Attr::PRI | Attr::SEC | Attr::INC | Attr::CHAR))
        }
    }
}

/// Schema attribute information.
#[derive(Clone, Copy, Debug)]
pub struct SchemaEntry<'a, T> {
    hdls: HandleRange,
    typ: Uuid,
    val: &'a [u8],
    _marker: PhantomData<T>,
}

impl<'a, T> SchemaEntry<'a, T> {
    /// Combines information about a schema entry.
    #[inline(always)]
    #[must_use]
    fn new(s: &'a Schema, at: &Attr, end_hdl: Handle) -> Self {
        Self {
            hdls: HandleRange::new(at.hdl, end_hdl),
            typ: s.typ(at),
            val: s.value(at),
            _marker: PhantomData,
        }
    }

    /// Returns the attribute handle.
    #[inline(always)]
    #[must_use]
    pub const fn handle(&self) -> Handle {
        self.hdls.start()
    }

    /// Returns the attribute type.
    #[inline(always)]
    #[must_use]
    pub const fn typ(&self) -> Uuid {
        self.typ
    }

    /// Returns the attribute value.
    #[inline(always)]
    #[must_use]
    pub const fn value(&self) -> &'a [u8] {
        self.val
    }
}

impl<T: Group> SchemaEntry<'_, T> {
    /// Returns the group handle range.
    #[inline(always)]
    pub const fn handle_range(&self) -> HandleRange {
        // We could use 0xFFFF for the end handle of the last service in the
        // schema. This avoids an extra round-trip for primary service
        // discovery, but adds one for characteristic descriptor discovery.
        // Leaving the handle range open allows new services to be added without
        // invalidating existing ones.
        self.hdls
    }

    /// Returns the UUID from the declaration value.
    #[inline]
    #[must_use]
    pub fn uuid(&self) -> Uuid {
        // SAFETY: Attribute value contains the UUID at UUID_OFF.
        Uuid::try_from(unsafe { self.val.get_unchecked(T::UUID_OFF..) }).unwrap()
    }
}

impl<'a, T> AsRef<[u8]> for SchemaEntry<'a, T> {
    #[inline(always)]
    fn as_ref(&self) -> &'a [u8] {
        self.val
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
    const INC: Uuid16 = Declaration::Include.uuid16();
    const CHAR: Uuid16 = Declaration::Characteristic.uuid16();

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

    /// Returns whether the attribute is an include declaration.
    #[inline(always)]
    const fn is_include(&self) -> bool {
        matches!(self.typ, Some(Self::INC))
    }

    /// Returns whether the attribute is a characteristic declaration.
    #[inline(always)]
    const fn is_char(&self) -> bool {
        matches!(self.typ, Some(Self::CHAR))
    }

    /// Returns the attribute value length.
    #[inline(always)]
    const fn len(&self) -> usize {
        self.val.1 as usize - self.val.0 as usize
    }
}

/// A non-empty subset of attributes.
#[derive(Clone, Copy, Debug)]
struct Subset<'a> {
    off: usize,
    attr: &'a [Attr],
}

impl<'a> Subset<'a> {
    /// Creates a new subset of attributes.
    #[inline(always)]
    fn new(attr: &[Attr], r: Range<usize>) -> Subset {
        debug_assert!(!r.is_empty() && r.end <= attr.len());
        Subset {
            off: r.start,
            // SAFETY: r is a valid non-empty range
            attr: unsafe { attr.get_unchecked(r) },
        }
    }

    /// Returns the first attribute.
    #[inline(always)]
    fn first(&self) -> &'a Attr {
        // SAFETY: self.attr is non-empty
        unsafe { self.attr.get_unchecked(0) }
    }

    /// Returns the last attribute.
    #[inline(always)]
    fn last(&self) -> &'a Attr {
        // SAFETY: self.attr is non-empty
        unsafe { self.attr.get_unchecked(self.attr.len() - 1) }
    }
}

struct GroupIter<'a, T, F> {
    schema: &'a Schema,
    it: iter::Peekable<slice::Iter<'a, Attr>>,
    is_start: F,
    _marker: PhantomData<T>,
}

impl<'a, T: Group, F: Fn(&Attr) -> bool> GroupIter<'a, T, F> {
    /// Creates a new attribute group iterator.
    #[inline(always)]
    #[must_use]
    fn new(schema: &'a Schema, it: &'a [Attr], is_start: F) -> Self {
        Self {
            schema,
            it: it.iter().peekable(),
            is_start,
            _marker: PhantomData,
        }
    }
}

impl<'a, T: Group, F: Fn(&Attr) -> bool> Iterator for GroupIter<'a, T, F> {
    type Item = SchemaEntry<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let decl = self.it.find(|at| (self.is_start)(at))?;
        let mut end = decl.hdl;
        while !self.it.peek().map_or(true, |at| T::is_next_group(at.typ)) {
            // SAFETY: `peek()` returned another attribute
            end = unsafe { self.it.next().unwrap_unchecked() }.hdl;
        }
        Some(SchemaEntry::new(self.schema, decl, end))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it.size_hint()
    }
}

impl<T: Group, F: Fn(&Attr) -> bool> iter::FusedIterator for GroupIter<'_, T, F> {}

/// Operations shared by [`Schema`] and [`SchemaBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns the index of the specified handle or the index where that handle
    /// could be inserted.
    #[inline]
    fn get(&self, hdl: Handle) -> std::result::Result<usize, usize> {
        fn search(attr: &[Attr], hdl: Handle) -> std::result::Result<usize, usize> {
            attr.binary_search_by(|at| at.hdl.cmp(&hdl))
        }
        let i = usize::from(hdl) - 1;
        // The attribute can exist at or, if there are gaps, before index `i`.
        // Usually, the 1-based handle value should also be the 0-based index.
        let prior = match self.attr().get(i) {
            Some(at) if at.hdl == hdl => return Ok(i),
            // SAFETY: `i` is in bounds
            Some(_) => unsafe { self.attr().get_unchecked(..i) },
            None => self.attr(),
        };
        search(prior, hdl)
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
    fn service_group(&self, hdl: Handle) -> Option<Subset> {
        let Ok(i) = self.get(hdl) else { return None };
        // SAFETY: `hdl` was found at `i`.
        unsafe { self.attr().get_unchecked(i).is_service() }.then(|| {
            // SAFETY: `i < self.attr.len()`
            let j = unsafe { self.attr().get_unchecked(i + 1..).iter() }
                .position(Attr::is_service)
                .map_or(self.attr().len(), |j| i + 1 + j);
            Subset::new(self.attr(), i..j)
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
        chars: impl FnOnce(&mut Builder<ServiceDef>) -> T,
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
        chars: impl FnOnce(&mut Builder<ServiceDef>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Declaration::SecondaryService, uuid.into(), include.as_ref());
        (hdl, chars(self.builder()))
    }

    /// Declares a primary or secondary service and any included services
    /// ([Vol 3] Part G, Section 3.2).
    pub fn service(&mut self, typ: Declaration, uuid: Uuid, include: &[Handle]) -> Handle {
        let hdl = self.decl(typ, |v| v.uuid(uuid));
        for &inc in include {
            let s = self.service_group(inc).expect("invalid service handle");
            let uuid = (s.first().len() == 2).then(|| self.value(s.first()).unpack().u16());
            let end = s.last().hdl;
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

impl Builder<ServiceDef> {
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
        descs: impl FnOnce(&mut Builder<CharacteristicDef>) -> T,
    ) -> (Handle, T) {
        let uuid = uuid.into();
        let hdl = self.decl_value(uuid, props, perms.into());
        let b = self.builder(props.contains(Prop::EXT_PROPS));
        let v = descs(b);
        b.finalize();
        (hdl, v)
    }

    // TODO: Method for multi-value characteristics

    /// Adds characteristic and characteristic value declarations.
    fn decl_value(&mut self, uuid: Uuid, props: Prop, perms: Perms) -> Handle {
        let val_hdl = self.next_handle().next().expect("maximum handle reached");
        self.decl(Declaration::Characteristic, |v| {
            v.u8(props.bits()).u16(val_hdl).uuid(uuid);
        });
        self.append_attr(val_hdl, uuid.as_uuid16(), perms)
    }

    /// Returns a new descriptor builder.
    fn builder(&mut self, need_ext_props: bool) -> &mut Builder<CharacteristicDef> {
        self.need_ext_props = need_ext_props;
        self.have_cccd = false;
        self.have_aggregate_fmt = false;
        // SAFETY: 0 is a valid length and there is nothing to drop
        unsafe { self.fmt.set_len(0) };
        self.0.builder()
    }
}

impl Builder<CharacteristicDef> {
    /// Declares a non-GATT profile characteristic descriptor
    /// ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn descriptor(&mut self, uuid: impl Into<Uuid>, perms: impl Into<Perms>) -> Handle {
        let uuid = uuid.into();
        self.attr(uuid, perms.into())
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
            let s = b.service_group(h).unwrap();
            assert_eq!(s.off..s.off + s.attr.len(), r);
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
        assert_eq!(
            appendix_b().hash,
            0xF1_CA_2D_48_EC_F5_8B_AC_8A_88_30_BB_B9_FB_A9_90
        );
    }

    #[test]
    fn primary_services() {
        use Service::*;
        let mut s = appendix_b();

        let mut it = s.primary_services(Handle::MIN, None);
        group_eq(it.next(), 0x0001, 0x0005, GenericAccess);
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        group_eq(it.next(), 0x000E, 0x0013, Glucose);
        assert!(it.next().is_none());
        assert!(it.next().is_none());
        drop(it);

        let mut it = s.primary_services(Handle::MIN, Some(GenericAttribute.uuid()));
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        assert!(it.next().is_none());
        drop(it);

        // Remove the battery service
        let mut v = s.attr.to_vec();
        v.truncate(s.attr.len() - 3);
        s.attr = v.into_boxed_slice();

        let mut it = s.primary_services(Handle::new(0x0002).unwrap(), None);
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        group_eq(it.next(), 0x000E, 0x0013, Glucose);
        assert!(it.next().is_none());
    }

    #[test]
    fn characteristics() {
        use Characteristic::*;

        let s = appendix_b();
        let mut pri = s.primary_services(Handle::MIN, None);

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0002, 0x0003, DeviceName);
        group_eq(it.next(), 0x0004, 0x0005, Appearance);
        assert!(it.next().is_none());

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0007, 0x0009, ServiceChanged);
        group_eq(it.next(), 0x000A, 0x000B, ClientSupportedFeatures);
        group_eq(it.next(), 0x000C, 0x000D, DatabaseHash);
        assert!(it.next().is_none());

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0010, 0x0013, GlucoseMeasurement);
        assert!(it.next().is_none());
    }

    fn group_eq<T: Group>(v: Option<SchemaEntry<T>>, decl: u16, end: u16, uuid: impl Into<Uuid16>) {
        let v = v.unwrap();
        assert_eq!(
            v.handle_range(),
            HandleRange::new(Handle::new(decl).unwrap(), Handle::new(end).unwrap())
        );
        assert_eq!(v.uuid(), uuid.into().as_uuid());
    }

    fn appendix_b() -> Schema {
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
        b.freeze()
    }
}
