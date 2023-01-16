use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Range};

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

    /// Returns an iterator over primary services that match the specified
    /// criteria.
    #[inline]
    pub fn primary_services(&self, hdls: HandleRange, uuid: Option<Uuid>) -> PrimaryServiceIter {
        let rng = (self.range(hdls)).map_or(Range::default(), |r| r.bounds());
        PrimaryServiceIter::new(self, rng, uuid)
    }

    /// Returns the schema of the service defined by the specified handle range
    /// or [`None`] if the handle range does not describe a service.
    #[inline]
    #[must_use]
    pub fn service(&self, hdls: HandleRange) -> Option<GroupSchema<ServiceDef>> {
        let s = GroupSchema::new(self, self.service_group(hdls.start())?);
        (hdls.end() == s.end_group_handle() || hdls.end() == s.attr.last().hdl).then_some(s)
    }

    // Returns an iterator over include declarations for one service.
    pub fn includes(&self, hdls: HandleRange) -> impl Iterator<Item = (Handle, &[u8])> + '_ {
        let attr = self.range(hdls).map_or(Default::default(), |g| {
            if g.first().is_service() {
                // SAFETY: g is not empty
                unsafe { g.attr.get_unchecked(1..) }
            } else {
                g.attr
            }
        });
        (attr.iter()).map_while(|at| at.is_include().then_some((at.hdl, self.value(at))))
    }

    /// Returns an iterator over all descriptors for one characteristic.
    pub fn descriptors(&self, hdls: HandleRange) -> impl Iterator<Item = (Handle, Uuid)> + '_ {
        let attr = self.range(hdls).map_or(Default::default(), |g| g.attr);
        // TODO: Verify that the range covers all descriptors for one characteristic
        (attr.iter())
            .map_while(|at| (!at.is_service() && !at.is_char()).then(|| (at.hdl, self.typ(at))))
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

    /// Returns all attributes within the specified handle range or [`None`] if
    /// the handle range is empty.
    fn range(&self, hdls: HandleRange) -> Option<Group> {
        let i = self
            .get(hdls.start())
            .map_or_else(|i| (i < self.attr.len()).then_some(i), |(i, _)| Some(i))?;
        let j = self
            .get(hdls.end())
            .map_or_else(|j| (j > 0).then_some(j), |(j, _)| Some(j + 1))?;
        Some(self.group(i..j))
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

/// Read-only schema for one service or characteristic.
#[derive(Clone, Debug)]
pub struct GroupSchema<'a, T> {
    schema: &'a Schema,
    attr: Group<'a>,
    _marker: PhantomData<T>,
}

impl<'a, T: GroupInfo> GroupSchema<'a, T> {
    /// Creates a new group schema.
    #[inline(always)]
    #[must_use]
    const fn new(schema: &'a Schema, attr: Group<'a>) -> Self {
        assert!(attr.attr.len() >= T::MIN_ATTRS);
        Self {
            schema,
            attr,
            _marker: PhantomData,
        }
    }

    /// Returns the UUID from the declaration value.
    #[inline]
    #[must_use]
    pub fn uuid(&self) -> Uuid {
        // SAFETY: Declaration value contains the UUID at UUID_OFF.
        Uuid::try_from(unsafe { self.decl_value().get_unchecked(T::UUID_OFF..) }).unwrap()
    }

    /// Returns the group handle range.
    #[inline]
    pub fn handle_range(&self) -> HandleRange {
        HandleRange::new(self.decl_handle(), self.end_group_handle())
    }

    /// Returns the declaration handle.
    #[inline]
    #[must_use]
    pub fn decl_handle(&self) -> Handle {
        self.attr.first().hdl
    }

    /// Returns the declaration value.
    #[inline]
    #[must_use]
    pub fn decl_value(&self) -> &'a [u8] {
        self.schema.value(self.attr.first())
    }

    /// Returns whether this is the last group within the entire schema, which
    /// allows it to claim all remaining handles.
    #[inline]
    #[must_use]
    pub fn is_last(&self) -> bool {
        self.attr.off + self.attr.len() == self.schema.attr.len()
    }

    /// Returns the End Group Handle, which will be 0xFFFF for the last group in
    /// the schema. This avoids an extra round trip for the Attribute Not Found
    /// response ([Vol 3] Part G, Section 4.4.1).
    #[inline]
    #[must_use]
    pub fn end_group_handle(&self) -> Handle {
        if self.is_last() {
            Handle::MAX
        } else {
            self.attr.last().hdl
        }
    }
}

impl<'a> GroupSchema<'a, ServiceDef> {
    // Returns an iterator over all service characteristics.
    #[inline]
    pub fn characteristics(&self) -> CharacteristicIter<'a> {
        CharacteristicIter::new(self.schema, self.attr)
    }
}

impl GroupSchema<'_, CharacteristicDef> {}

/// Iterator over primary services within a handle range with optional UUID
/// matching.
#[derive(Clone, Debug)]
#[must_use]
pub struct PrimaryServiceIter<'a> {
    schema: &'a Schema,
    rng: Range<usize>,
    uuid: UuidVec,
}

impl<'a> PrimaryServiceIter<'a> {
    /// Creates a primary service iterator.
    #[inline]
    fn new(schema: &'a Schema, rng: Range<usize>, uuid: Option<Uuid>) -> Self {
        let uuid = uuid.map_or(UuidVec::default(), UuidVec::new);
        Self { schema, rng, uuid }
    }

    /// Returns whether at least one more primary service is available.
    #[inline]
    #[must_use]
    fn more(&mut self) -> bool {
        // SAFETY: self.rng is always a valid subslice
        for at in unsafe { self.schema.attr.get_unchecked(self.rng.clone()) } {
            if at.is_primary_service()
                && (self.uuid.is_empty() || self.schema.value(at) == self.uuid.as_ref())
            {
                return true;
            }
            self.rng.start += 1;
        }
        false
    }
}

impl<'a> Iterator for PrimaryServiceIter<'a> {
    type Item = GroupSchema<'a, ServiceDef>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.more() {
            return None;
        }
        let i = self.rng.start;
        // SAFETY: i < self.schema.attr.len()
        let j = unsafe { self.schema.attr.get_unchecked(i + ServiceDef::MIN_ATTRS..) }
            .iter()
            .position(Attr::is_service)
            .map_or(self.schema.attr.len(), |j| i + ServiceDef::MIN_ATTRS + j);
        self.rng.start = j.min(self.rng.end);
        Some(GroupSchema::new(self.schema, self.schema.group(i..j)))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.rng.end - self.rng.start))
    }
}

/// Iterator over service characteristics.
#[derive(Clone, Debug)]
#[must_use]
pub struct CharacteristicIter<'a> {
    schema: &'a Schema,
    attr: Group<'a>,
}

impl<'a> CharacteristicIter<'a> {
    /// Creates a service characteristic iterator.
    #[inline]
    fn new(schema: &'a Schema, mut attr: Group<'a>) -> Self {
        attr.take(attr.iter().position(Attr::is_char).unwrap_or(attr.len()));
        Self { schema, attr }
    }
}

impl<'a> Iterator for CharacteristicIter<'a> {
    type Item = GroupSchema<'a, CharacteristicDef>;

    fn next(&mut self) -> Option<Self::Item> {
        // `self.attr` is either empty, or starts with a characteristic
        // declaration and value attributes.
        let n = (self.attr.get(CharacteristicDef::MIN_ATTRS..)?.iter())
            .position(Attr::is_char)
            .map_or(self.attr.len(), |i| CharacteristicDef::MIN_ATTRS + i);
        Some(GroupSchema::new(self.schema, self.attr.take(n)))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.attr.len() / 2))
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

    /// Returns whether the attribute is an include declaration.
    #[inline(always)]
    const fn is_include(&self) -> bool {
        const INC: Uuid16 = Declaration::Include.uuid16();
        matches!(self.typ, Some(INC))
    }

    /// Returns whether the attribute is a characteristic declaration.
    #[inline(always)]
    const fn is_char(&self) -> bool {
        const CHAR: Uuid16 = Declaration::Characteristic.uuid16();
        matches!(self.typ, Some(CHAR))
    }

    /// Returns the attribute value length.
    #[inline(always)]
    const fn len(&self) -> usize {
        self.val.1 as usize - self.val.0 as usize
    }
}

/// A non-empty attribute group.
#[derive(Clone, Copy, Debug)]
struct Group<'a> {
    off: usize,
    attr: &'a [Attr],
}

impl<'a> Group<'a> {
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

    /// Returns the bounds of the range within the entire set of attributes.
    #[inline(always)]
    const fn bounds(&self) -> Range<usize> {
        self.off..self.off + self.attr.len()
    }

    /// Returns a new group of the first `n` attributes, advancing `self` by
    /// `n`.
    ///
    /// # Panics
    ///
    /// Panics if `n` is out of bounds.
    #[inline]
    fn take(&mut self, n: usize) -> Self {
        let (attr, tail) = self.attr.split_at(n);
        let head = Self {
            off: self.off,
            attr,
        };
        self.off += n;
        self.attr = tail;
        head
    }
}

impl<'a> Deref for Group<'a> {
    type Target = [Attr];

    #[inline]
    fn deref(&self) -> &'a Self::Target {
        self.attr
    }
}

/// Trait implemented by [`ServiceDef`] and [`CharacteristicDef`] markers.
pub trait GroupInfo {
    /// Minimum number of attributes in the group.
    const MIN_ATTRS: usize = 1;
    /// Offset of the UUID in the declaration value.
    const UUID_OFF: usize = 0;
}

impl GroupInfo for ServiceDef {}

impl GroupInfo for CharacteristicDef {
    const MIN_ATTRS: usize = 2;
    const UUID_OFF: usize = 3;
}

/// Operations shared by [`Schema`] and [`SchemaBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns an attribute group.
    #[inline(always)]
    fn group(&self, r: Range<usize>) -> Group {
        debug_assert!(r.start < r.end && r.end <= self.attr().len());
        Group {
            off: r.start,
            // SAFETY: r is a valid non-empty range
            attr: unsafe { self.attr().get_unchecked(r) },
        }
    }

    /// Returns the index and attribute entry of the specified handle or the
    /// index where that handle could be inserted.
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
        // The attribute can exist at or, if there are gaps, before index `i`.
        // Usually, the 1-based handle value should also be the 0-based index.
        let prior = match self.attr().get(i) {
            Some(at) if at.hdl == hdl => return Ok((i, at)),
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
    fn service_group(&self, hdl: Handle) -> Option<Group> {
        let Ok((i, at)) = self.get(hdl) else { return None };
        at.is_service().then(|| {
            // SAFETY: i < self.attr.len()
            let j = unsafe { self.attr().get_unchecked(i + ServiceDef::MIN_ATTRS..) }
                .iter()
                .position(Attr::is_service)
                .map_or(self.attr().len(), |n| i + ServiceDef::MIN_ATTRS + n);
            self.group(i..j)
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

/// [`GroupSchema`] and [`Builder`] marker type.
#[derive(Debug, Default)]
pub struct ServiceDef;

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

/// [`GroupSchema`] and  [`Builder`] marker type.
#[derive(Debug, Default)]
pub struct CharacteristicDef;

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
        assert_eq!(
            appendix_b().hash,
            0xF1_CA_2D_48_EC_F5_8B_AC_8A_88_30_BB_B9_FB_A9_90
        );
    }

    #[test]
    fn primary_services() {
        let mut s = appendix_b();

        let mut it = s.primary_services(HandleRange::ALL, None);
        group_eq(it.next(), 0x0001, 0x0005, Service::GenericAccess);
        group_eq(it.next(), 0x0006, 0x000D, Service::GenericAttribute);
        group_eq(it.next(), 0x000E, 0x0013, Service::Glucose);
        assert!(it.next().is_none());
        assert!(it.next().is_none());

        let mut it = s.primary_services(HandleRange::ALL, Some(Service::GenericAttribute.uuid()));
        group_eq(it.next(), 0x0006, 0x000D, Service::GenericAttribute);
        assert!(it.next().is_none());

        // Remove the battery service
        let mut v = s.attr.to_vec();
        v.truncate(s.attr.len() - 3);
        s.attr = v.into_boxed_slice();

        let rng = HandleRange::new(Handle::new(0x0002).unwrap(), Handle::new(0x000E).unwrap());
        let mut it = s.primary_services(rng, None);
        group_eq(it.next(), 0x0006, 0x000D, Service::GenericAttribute);
        group_eq(it.next(), 0x000E, 0xFFFF, Service::Glucose);
        assert!(it.next().is_none());
    }

    #[test]
    fn characteristics() {
        let s = appendix_b();
        let mut pri = s.primary_services(HandleRange::ALL, None);

        let mut it = pri.next().unwrap().characteristics();
        group_eq(it.next(), 0x0002, 0x0003, Characteristic::DeviceName);
        group_eq(it.next(), 0x0004, 0x0005, Characteristic::Appearance);
        assert!(it.next().is_none());

        let mut it = pri.next().unwrap().characteristics();
        group_eq(it.next(), 0x0007, 0x0009, Characteristic::ServiceChanged);
        group_eq(
            it.next(),
            0x000A,
            0x000B,
            Characteristic::ClientSupportedFeatures,
        );
        group_eq(it.next(), 0x000C, 0x000D, Characteristic::DatabaseHash);
        assert!(it.next().is_none());

        let mut it = pri.next().unwrap().characteristics();
        group_eq(
            it.next(),
            0x0010,
            0x0013,
            Characteristic::GlucoseMeasurement,
        );
        assert!(it.next().is_none());
    }

    fn group_eq<T: GroupInfo>(
        s: Option<GroupSchema<T>>,
        decl: u16,
        end: u16,
        uuid: impl Into<Uuid16>,
    ) {
        let s = s.unwrap();
        assert_eq!(s.decl_handle(), Handle::new(decl).unwrap());
        assert_eq!(s.end_group_handle(), Handle::new(end).unwrap());
        assert_eq!(s.uuid(), uuid.into().as_uuid());
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
