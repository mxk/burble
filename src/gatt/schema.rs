use std::marker::PhantomData;
use std::ops::Range;
use std::{iter, slice};

use structbuf::Unpack;
use tracing::{info, warn};

pub use builder::*;

use crate::gap::{Uuid, Uuid16, UuidType, UuidVec};

use super::*;

mod builder;

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
    #[inline(always)]
    #[must_use]
    pub fn build() -> Builder<Self> {
        Builder::new()
    }

    /// Returns the database hash ([Vol 3] Part G, Section 7.3).
    #[inline(always)]
    #[must_use]
    pub const fn hash(&self) -> u128 {
        self.hash
    }

    /// Returns an iterator over primary services with optional UUID matching
    /// ([Vol 3] Part G, Section 4.4).
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

    /// Returns an iterator over service includes
    /// ([Vol 3] Part G, Section 4.5.1).
    pub fn includes(&self, hdls: HandleRange) -> impl Iterator<Item = SchemaEntry<IncludeDef>> {
        (self.service_attrs(hdls).iter())
            .map_while(|at| at.is_include().then(|| SchemaEntry::new(self, at, at.hdl)))
    }

    /// Returns an iterator over service characteristics
    /// ([Vol 3] Part G, Section 4.6.1).
    pub fn characteristics(
        &self,
        hdls: HandleRange,
    ) -> impl Iterator<Item = SchemaEntry<CharacteristicDef>> {
        GroupIter::new(self, self.service_attrs(hdls), Attr::is_char)
    }

    /// Returns an iterator over characteristic descriptors
    /// ([Vol 3] Part G, Section 4.7.1).
    pub fn descriptors(
        &self,
        hdls: HandleRange,
    ) -> impl Iterator<Item = SchemaEntry<DescriptorDef>> {
        let attr = self.subset(hdls).and_then(|s| {
            use private::Group;
            // SAFETY: `0 <= s.off < self.attr.len()`
            let decl = unsafe { self.attr.get_unchecked(..s.off).iter() }
                .rfind(|&at| Attr::is_char(at))?;
            // Handle range must start after the characteristic value and cannot
            // cross characteristic boundary.
            (value_handle(self.value(decl)) < s.first().hdl
                && !(s.attr.iter()).any(|at| CharacteristicDef::is_next_group(at.typ)))
            .then_some(s.attr)
        });
        (attr.unwrap_or_default().iter()).map(|at| SchemaEntry::new(self, at, at.hdl))
    }

    /*
    pub fn handle_range_access(
        &self,
        op: Opcode,
        req: Access,
        hdls: HandleRange,
        uuid: Option<Uuid>,
    ) -> RspResult<impl Iterator<Item = Handle>> {
        //self.subset(hdls).map(|s| s.attr).unwrap_or_default()
        unimplemented!();
        Ok(self.attr.iter().map(|at| at.hdl))
    }
    */

    /// Performs access permission check for the specified read/write request.
    pub fn access_check(&self, op: Opcode, req: Access, hdl: Handle) -> RspResult<Handle> {
        use {ErrorCode::*, Opcode::*};
        debug_assert_eq!(op.access_type(), Some(req.typ()));
        debug_assert!(matches!(req.typ(), Access::READ | Access::WRITE));
        let error = |e| Err(super::ErrorRsp::new(op as _, Some(hdl), e));
        let Some((at, ch)) = self.characteristic_for_handle(hdl) else {
            warn!("Denied {op} for invalid or non-characteristic {hdl}");
            return error(InvalidHandle);
        };
        // [Vol 3] Part F, Section 4
        if let Err(e) = at.perms.test(req) {
            warn!("Denied {op} to {hdl} due to {e}");
            return error(e);
        }
        if hdl > ch.val.hdl {
            // [Vol 3] Part G, Section 3.3.3.1 and 3.3.3.2
            return if req.typ() == Access::WRITE
                && at.typ == Some(Descriptor::CharacteristicUserDescription.uuid16())
                && !(ch.ext_props).map_or(false, |p| p.contains(ExtProp::WRITABLE_AUX))
            {
                warn!("Denied {op} to {hdl} because WRITABLE_AUX bit is not set");
                error(WriteNotPermitted)
            } else {
                Ok(hdl)
            };
        }
        // [Vol 3] Part G, Section 3.3.1.1
        let bit = match op {
            ReadReq                                   // [Vol 3] Part G, Section 4.8.1 and 4.8.3
            | ReadByTypeReq                           // [Vol 3] Part G, Section 4.8.2
            | ReadBlobReq                             // [Vol 3] Part G, Section 4.8.3
            | ReadMultipleReq                         // [Vol 3] Part G, Section 4.8.4
            | ReadMultipleVariableReq => Prop::READ,  // [Vol 3] Part G, Section 4.8.5
            WriteCmd => Prop::WRITE_CMD,              // [Vol 3] Part G, Section 4.9.1
            WriteReq                                  // [Vol 3] Part G, Section 4.9.3
            | PrepareWriteReq => Prop::WRITE,         // [Vol 3] Part G, Section 4.9.4
            SignedWriteCmd => Prop::SIGNED_WRITE_CMD, // [Vol 3] Part G, Section 4.9.2
            _ => {
                warn!("Denied non-read/write {op} for {hdl}");
                return error(RequestNotSupported);
            }
        };
        if !ch.props.contains(bit) {
            let e = if req.typ() == Access::READ {
                ReadNotPermitted
            } else {
                WriteNotPermitted
            };
            warn!("Denied {op} for {hdl} due to {e} by properties");
            return error(e);
        }
        Ok(hdl)
    }

    /// Logs schema contents.
    pub fn dump(&self) {
        use Declaration::*;
        macro_rules! log {
            ($at:ident, $fmt:expr$(, $($args:tt)*)?) => {
                info!("[{:#06X}] {}", u16::from($at.hdl), format_args!($fmt$(, $($args)*)?))
            };
        }
        let mut vhdl = Handle::MIN;
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
                            vhdl = Handle::new(v.u16()).unwrap();
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
                if at.hdl <= vhdl {
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

    /// Returns the attribute and characteristic information for the specified
    /// handle.
    fn characteristic_for_handle(&self, hdl: Handle) -> Option<(&Attr, CharInfo)> {
        use private::Group;
        let i = self.get(hdl).ok()?;
        // SAFETY: 0 <= i < self.attr.len()
        let decl = unsafe { self.attr.get_unchecked(..=i).iter() }.rposition(Attr::is_char)?;
        // SAFETY: 0 <= decl <= i < self.attr.len()
        let end = unsafe { self.attr.get_unchecked(decl + 1..).iter() }
            .position(|at| CharacteristicDef::is_next_group(at.typ))
            .map_or(self.attr.len(), |j| decl + 1 + j);
        if end <= i {
            return None; // hdl is not part of a characteristic definition
        }
        // SAFETY: 0 <= decl < self.attr.len()
        let v = self.value(unsafe { self.attr.get_unchecked(decl) });
        let vhdl = value_handle(v);
        // SAFETY: 0 <= decl < end <= self.attr.len()
        let val = unsafe { self.attr.get_unchecked(decl + 1..end).iter() }
            .position(|at| at.hdl == vhdl)
            .map(|j| decl + 1 + j)?;
        // SAFETY: 0 < val < end <= self.attr.len()
        let desc = unsafe { self.attr.get_unchecked(val + 1..end) };
        // SAFETY: All bits are valid and a valid handle is at indices 1-2
        let props = unsafe { Prop::from_bits_unchecked(*v.get_unchecked(0)) };
        let ext_props = props.contains(Prop::EXT_PROPS).then(|| {
            (desc.iter().find(|&at| Attr::is_ext_props(at))).map_or(ExtProp::empty(), |at| {
                ExtProp::from_bits_truncate(self.value(at).unpack().u16())
            })
        });
        Some((
            // SAFETY: 0 <= desc <= i < end <= self.attr.len()
            unsafe { self.attr.get_unchecked(i) },
            CharInfo {
                props,
                ext_props,
                // SAFETY: 0 < desc < val < end <= self.attr.len()
                val: unsafe { self.attr.get_unchecked(val) },
                desc,
            },
        ))
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

/// Operations shared by [`Schema`] and [`SchemaBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns the index of the specified handle or the index where that handle
    /// can be inserted.
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

/// Trait implemented by [`ServiceDef`] and [`CharacteristicDef`] markers.
pub trait Group: private::Group {}

impl Group for ServiceDef {}
impl Group for CharacteristicDef {}

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

    /// Returns the service or characteristic UUID.
    #[inline]
    #[must_use]
    pub fn uuid(&self) -> Uuid {
        // SAFETY: Attribute value contains the UUID at UUID_OFF.
        Uuid::try_from(unsafe { self.val.get_unchecked(T::UUID_OFF..) }).unwrap()
    }
}

impl SchemaEntry<'_, CharacteristicDef> {
    /// Returns the characteristic properties.
    #[inline]
    #[must_use]
    pub fn properties(&self) -> Prop {
        // SAFETY: All bits are valid
        unsafe { Prop::from_bits_unchecked(self.val.unpack().u8()) }
    }

    /// Returns the handle of the value attribute.
    #[inline]
    #[must_use]
    pub fn value_handle(&self) -> Handle {
        value_handle(self.val)
    }
}

impl SchemaEntry<'_, DescriptorDef> {
    /// Returns the descriptor UUID.
    #[inline(always)]
    #[must_use]
    pub const fn uuid(&self) -> Uuid {
        self.typ
    }
}

impl<'a, T> AsRef<[u8]> for SchemaEntry<'a, T> {
    #[inline(always)]
    fn as_ref(&self) -> &'a [u8] {
        self.val
    }
}

/// Information about a single characteristic.
#[derive(Clone, Copy, Debug)]
struct CharInfo<'a> {
    props: Prop,
    ext_props: Option<ExtProp>,
    val: &'a Attr,
    desc: &'a [Attr],
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
    const EXT_PROPS: Uuid16 = Descriptor::CharacteristicExtendedProperties.uuid16();

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

    /// Returns whether the attribute is an extended properties descriptor.
    #[inline(always)]
    const fn is_ext_props(&self) -> bool {
        matches!(self.typ, Some(Self::EXT_PROPS))
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
            end = unsafe { self.it.next().unwrap_unchecked().hdl };
        }
        Some(SchemaEntry::new(self.schema, decl, end))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.it.size_hint()
    }
}

impl<T: Group, F: Fn(&Attr) -> bool> iter::FusedIterator for GroupIter<'_, T, F> {}

/// Returns the characteristic value attribute handle from the value of the
/// characteristic declaration.
#[inline]
fn value_handle(decl: &[u8]) -> Handle {
    Handle::new(decl.unpack().split_at(1).1.u16()).unwrap_or(Handle::MAX)
}

mod private {
    use super::*;

    /// Sealed implementation of an attribute group.
    pub trait Group {
        /// Offset of the UUID in the declaration value.
        const UUID_OFF: usize = 0;

        /// Returns whether the specified attribute type is not part of the
        /// current group.
        #[inline(always)]
        #[must_use]
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
