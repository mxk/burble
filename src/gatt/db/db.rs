use std::marker::PhantomData;
use std::ops::Range;
use std::{iter, mem, slice};

use structbuf::Unpack;
use tracing::{info, warn};

pub use builder::*;

use crate::gap::{Uuid, Uuid16, UuidType, UuidVec};

use super::*;

mod builder;

/// Database data index type. `u16` is enough for 3k 128-bit characteristics.
type Idx = u16;

/// Read-only attribute database.
///
/// Describes the service structure, attribute permissions, and stores read-only
/// values.
#[derive(Clone, Debug, Default)]
pub struct Db {
    /// Attribute metadata sorted by handle.
    attr: Box<[Attr]>,
    /// Concatenated GATT profile attribute values and 128-bit UUIDs, ending
    /// with a 128-bit hash in little-endian byte order.
    data: Box<[u8]>,
}

impl Db {
    /// Creates a new database builder.
    #[inline(always)]
    #[must_use]
    pub fn build() -> Builder<Self> {
        Builder::new()
    }

    /// Returns the database hash ([Vol 3] Part G, Section 7.3).
    #[inline(always)]
    #[must_use]
    pub const fn hash(&self) -> u128 {
        let (p, i) = (self.data.as_ptr(), self.data.len() - mem::size_of::<u128>());
        // SAFETY: `self.data` always ends with the hash and the dereferenced
        // value has an alignment of 1.
        u128::from_le_bytes(unsafe { *p.add(i).cast() })
    }

    /// Returns an iterator over all attributes in handle order.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Handle, Uuid, &[u8])> {
        (self.attr.iter()).map(|at| (at.hdl, self.typ(at), self.value(at)))
    }

    /// Returns the type and value of the specified handle or [`None`] if the
    /// handle is invalid. The value will be empty if it's not part of the
    /// database.
    #[inline]
    #[must_use]
    pub fn get(&self, hdl: Handle) -> Option<(Uuid, &[u8])> {
        (self.try_get(hdl).ok()).map(|at| (self.typ(at), self.value(at)))
    }

    /// Returns characteristic information for any handle within the
    /// characteristic definition. Returns [`None`] if the handle is not part of
    /// any characteristic.
    #[inline]
    #[must_use]
    pub(super) fn get_characteristic(&self, hdl: Handle) -> Option<CharInfo> {
        (self.try_get(hdl).ok()).and_then(|at| self.characteristic_for_attr(at))
    }

    /// Returns an iterator over primary services with optional UUID matching
    /// ([Vol 3] Part G, Section 4.4).
    #[inline]
    pub fn primary_services(
        &self,
        start: Handle,
        uuid: Option<Uuid>,
    ) -> impl Iterator<Item = DbEntry<ServiceDef>> {
        let i = self.try_get(start).map_or_else(|i| i, |at| self.index(at));
        let uuid = uuid.map_or_else(UuidVec::default, UuidVec::new);
        // SAFETY: 0 <= i <= self.attr.len()
        GroupIter::new(self, unsafe { self.attr.get_unchecked(i..) }, move |at| {
            at.is_primary_service() && (uuid.is_empty() || self.value(at) == uuid.as_ref())
        })
    }

    /// Returns an iterator over service includes
    /// ([Vol 3] Part G, Section 4.5.1).
    pub fn includes(&self, hdls: HandleRange) -> impl Iterator<Item = DbEntry<IncludeDef>> {
        (self.service_attrs(hdls).iter())
            .map_while(|at| at.is_include().then(|| DbEntry::new(self, at, at.hdl)))
    }

    /// Returns an iterator over service characteristics
    /// ([Vol 3] Part G, Section 4.6.1).
    pub fn characteristics(
        &self,
        hdls: HandleRange,
    ) -> impl Iterator<Item = DbEntry<CharacteristicDef>> {
        GroupIter::new(self, self.service_attrs(hdls), Attr::is_char)
    }

    /// Returns an iterator over characteristic descriptors
    /// ([Vol 3] Part G, Section 4.7.1).
    pub fn descriptors(&self, hdls: HandleRange) -> impl Iterator<Item = DbEntry<DescriptorDef>> {
        let attr = self.subset(hdls).and_then(|s| {
            use private::Group;
            // SAFETY: 0 <= s.off < self.attr.len()
            let decl = unsafe { self.attr.get_unchecked(..s.off).iter() }
                .rfind(|&at| Attr::is_char(at))?;
            // Handle range must start after the characteristic value and cannot
            // cross characteristic boundary.
            (value_handle(self.value(decl)) < s.first().hdl
                && !(s.attr.iter()).any(|at| CharacteristicDef::is_next_group(at.typ)))
            .then_some(s.attr)
        });
        (attr.unwrap_or_default().iter()).map(|at| DbEntry::new(self, at, at.hdl))
    }

    /// Performs read/write access permission check for a single handle.
    #[inline]
    pub fn try_access(&self, req: Request, hdl: Handle) -> RspResult<Handle> {
        self.try_multi_access(req, [hdl]).map(|_| hdl)
    }

    /// Performs read/write access permission check for multiple handles.
    #[inline]
    pub fn try_multi_access<T: AsRef<[Handle]>>(&self, req: Request, hdls: T) -> RspResult<T> {
        let v = hdls.as_ref();
        if v.is_empty() {
            return req.op.err(ErrorCode::InvalidPdu); // Should never happen
        }
        // [Vol 3] Part F, Section 3.4.4.7
        for &hdl in v {
            let Ok(at) = self.try_get(hdl) else {
                warn!("Denied {} for invalid {hdl}", req.op);
                return req.op.hdl_err(ErrorCode::InvalidHandle, hdl);
            };
            self.access_check(req, at)?;
        }
        Ok(hdls)
    }

    /// Performs read/write access permission check for a range of handles with
    /// UUID type matching.
    #[inline]
    pub fn try_range_access(
        &self,
        req: Request,
        hdls: HandleRange,
        uuid: Uuid,
    ) -> RspResult<Vec<Handle>> {
        let attr = self.subset(hdls).map_or_else(Default::default, |s| s.attr);
        let mut it = (attr.iter())
            .filter_map(|at| (self.typ(at) == uuid).then(|| self.access_check(req, at)))
            .peekable();
        // [Vol 3] Part F, Section 3.4.4.1
        match it.peek() {
            None => return req.op.hdl_err(ErrorCode::AttributeNotFound, hdls.start()),
            Some(&r) => {
                r?;
            }
        }
        Ok(it.map_while(RspResult::ok).collect())
    }

    /// Logs database contents.
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
        info!("GATT database:");
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
                            if let UuidType::Service(s) = uuid.typ() {
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
                            if let UuidType::Characteristic(c) = uuid.typ() {
                                log!(at, "|__ {c} <{uuid}>");
                            } else {
                                log!(at, "|__ Characteristic <{uuid}>");
                            }
                        }
                        t => log!(at, "Unexpected {t}"),
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

    /// Performs read/write access permission check for the specified attribute.
    fn access_check(&self, req: Request, at: &Attr) -> RspResult<Handle> {
        use Opcode::*;
        let (op, hdl) = (req.op, at.hdl);
        // [Vol 3] Part F, Section 4
        if let Err(e) = at.perms.test(req.ac) {
            warn!("Denied {op} to {hdl} due to {e}");
            return op.hdl_err(e, hdl);
        }
        let Some(ch) = self.characteristic_for_attr(at) else {
            return Ok(hdl); // Permission check passed and no properties to test
        };
        if hdl != ch.vhdl {
            // [Vol 3] Part G, Section 3.3.3.1 and 3.3.3.2
            if req.ac.typ() == Access::WRITE
                && matches!(at.typ, Some(Descriptor::CHARACTERISTIC_USER_DESCRIPTION))
                && !(ch.ext_props).map_or(false, |p| p.contains(ExtProp::WRITABLE_AUX))
            {
                warn!("Denied {op} to {hdl} because WRITABLE_AUX bit is not set");
                return op.hdl_err(ErrorCode::WriteNotPermitted, hdl);
            }
            return Ok(hdl); // Descriptor or declaration access
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
                return op.hdl_err(ErrorCode::RequestNotSupported, hdl);
            }
        };
        if !ch.props.contains(bit) {
            let e = if req.ac.typ() == Access::READ {
                ErrorCode::ReadNotPermitted
            } else {
                ErrorCode::WriteNotPermitted
            };
            warn!("Denied {op} for {hdl} due to {e} by properties");
            return op.hdl_err(e, hdl);
        }
        // [Vol 3] Part G, Section 3.3.3.1
        if matches!(op, PrepareWriteReq)
            && !(ch.ext_props).map_or(true, |p| p.contains(ExtProp::RELIABLE_WRITE))
        {
            // It's not clear how to handle this. WRITE allows Section 4.9.4
            // procedure, and RELIABLE_WRITE allows Section 4.9.5 procedure, but
            // they are the exact same procedure from the server's perspective.
            // We deny it only if the Characteristic Extended Properties
            // descriptor exists and the RELIABLE_WRITE bit is not set.
            warn!("Denied {op} for {hdl} because RELIABLE_WRITE bit is not set");
            return op.hdl_err(ErrorCode::WriteNotPermitted, hdl);
        }
        Ok(hdl) // Characteristic value access
    }

    /// Returns characteristic information for the specified attribute.
    fn characteristic_for_attr(&self, at: &Attr) -> Option<CharInfo> {
        use private::Group;
        let i = self.index(at);
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
        let dval = self.value(unsafe { self.attr.get_unchecked(decl) });
        let vhdl = value_handle(dval);
        // SAFETY: 0 <= decl < end <= self.attr.len()
        let val = unsafe { self.attr.get_unchecked(decl + 1..end).iter() }
            .position(|at| at.hdl == vhdl)
            .map(|j| decl + 1 + j)
            .expect("invalid characteristic");
        // SAFETY: 0 < val < end <= self.attr.len()
        let desc = unsafe { self.attr.get_unchecked(val + 1..end) };
        // SAFETY: All bits are valid and a valid handle is at indices 1-2
        let props = unsafe { Prop::from_bits_unchecked(*dval.get_unchecked(0)) };
        let ext_props = props.contains(Prop::EXT_PROPS).then(|| {
            (desc.iter().find(|&at| Attr::is_ext_props(at))).map_or(ExtProp::empty(), |at| {
                ExtProp::from_bits_truncate(self.value(at).unpack().u16())
            })
        });
        // SAFETY: 0 < desc < val < end <= self.attr.len()
        let at = unsafe { self.attr.get_unchecked(val) };
        Some(CharInfo {
            props,
            ext_props,
            vhdl: at.hdl,
            uuid: self.typ(at),
            desc,
        })
    }

    /// Returns all attributes within the specified handle range or [`None`] if
    /// the handle range is empty.
    fn subset(&self, hdls: HandleRange) -> Option<Subset> {
        let i = self.try_get(hdls.start()).map_or_else(
            |i| (i < self.attr.len()).then_some(i),
            |at| Some(self.index(at)),
        )?;
        let j = (self.try_get(hdls.end()))
            .map_or_else(|j| (j > 0).then_some(j), |j| Some(self.index(j) + 1))?;
        Some(Subset::new(&self.attr, i..j))
    }

    /// Returns the attribute type.
    #[inline]
    fn typ(&self, at: &Attr) -> Uuid {
        at.typ.map_or_else(
            // SAFETY: 128-bit UUID is at self.data[at.val.0 - 16..]
            || unsafe {
                let i = usize::from(at.val.0) - 16;
                Uuid::new_unchecked(u128::from_le_bytes(*self.data.as_ptr().add(i).cast()))
            },
            Uuid16::as_uuid,
        )
    }
}

/// Operations shared by [`Db`] and [`DbBuilder`].
trait CommonOps {
    /// Returns the attribute metadata.
    fn attr(&self) -> &[Attr];

    /// Returns the attribute value and 128-bit UUID buffer.
    #[must_use]
    fn data(&self) -> &[u8];

    /// Returns the attribute for the specified handle or the index where that
    /// handle can be inserted.
    #[inline]
    fn try_get(&self, hdl: Handle) -> std::result::Result<&Attr, usize> {
        fn search(attr: &[Attr], hdl: Handle) -> std::result::Result<&Attr, usize> {
            attr.binary_search_by(|at| at.hdl.cmp(&hdl))
                // SAFETY: 0 <= i < attr.len()
                .map(|i| unsafe { attr.get_unchecked(i) })
        }
        let i = usize::from(hdl) - 1;
        // The attribute can exist at or, if there are gaps, before index `i`.
        // Usually, the 1-based handle value should also be the 0-based index.
        let prior = match self.attr().get(i) {
            // SAFETY: 0 <= i < attr.len()
            Some(at) if at.hdl == hdl => return Ok(unsafe { self.attr().get_unchecked(i) }),
            // SAFETY: 0 <= i < attr.len()
            Some(_) => unsafe { self.attr().get_unchecked(..i) },
            None => self.attr(),
        };
        search(prior, hdl)
    }

    /// Returns the index of `at` in `self.attr()`.
    #[inline(always)]
    fn index(&self, at: &Attr) -> usize {
        // TODO: Use `sub_ptr` when stabilized
        // SAFETY: Caller only has access to attributes in self.attr() and
        // `self.attr().as_ptr() <= at`
        unsafe {
            usize::try_from((at as *const Attr).offset_from(self.attr().as_ptr()))
                .unwrap_unchecked()
        }
    }

    /// Returns the attribute value.
    #[inline(always)]
    #[must_use]
    fn value(&self, at: &Attr) -> &[u8] {
        // SAFETY: self.data()[val] is always valid
        unsafe { (self.data()).get_unchecked(usize::from(at.val.0)..usize::from(at.val.1)) }
    }

    /// Returns all attributes of the service group defined by `hdl` or [`None`]
    /// if the handle does not refer to a service.
    fn service_group(&self, hdl: Handle) -> Option<Subset> {
        let Ok(at) = self.try_get(hdl) else { return None };
        at.is_service().then(|| {
            let i = self.index(at);
            // SAFETY: 0 <= i < self.attr.len()
            let j = unsafe { self.attr().get_unchecked(i + 1..).iter() }
                .position(Attr::is_service)
                .map_or(self.attr().len(), |j| i + 1 + j);
            Subset::new(self.attr(), i..j)
        })
    }
}

impl CommonOps for Db {
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

/// Database attribute information.
#[derive(Clone, Copy, Debug)]
pub struct DbEntry<'a, T> {
    hdls: HandleRange,
    typ: Uuid,
    val: &'a [u8],
    _marker: PhantomData<T>,
}

impl<'a, T> DbEntry<'a, T> {
    /// Combines information about a database entry.
    #[inline(always)]
    #[must_use]
    fn new(db: &'a Db, at: &Attr, end_hdl: Handle) -> Self {
        Self {
            hdls: HandleRange::new(at.hdl, end_hdl),
            typ: db.typ(at),
            val: db.value(at),
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

impl<T: Group> DbEntry<'_, T> {
    /// Returns the group handle range.
    #[inline(always)]
    pub const fn handle_range(&self) -> HandleRange {
        // We could use 0xFFFF for the end handle of the last service in the
        // database. This avoids an extra round-trip for primary service
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

impl DbEntry<'_, CharacteristicDef> {
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

impl DbEntry<'_, DescriptorDef> {
    /// Returns the descriptor UUID.
    #[inline(always)]
    #[must_use]
    pub const fn uuid(&self) -> Uuid {
        self.typ
    }
}

impl<'a, T> AsRef<[u8]> for DbEntry<'a, T> {
    #[inline(always)]
    fn as_ref(&self) -> &'a [u8] {
        self.val
    }
}

/// Information about a single characteristic.
#[derive(Clone, Copy, Debug)]
pub(super) struct CharInfo<'a> {
    pub props: Prop,
    pub ext_props: Option<ExtProp>,
    pub vhdl: Handle,
    pub uuid: Uuid,
    desc: &'a [Attr],
}

/// Attribute entry. `val` contains start and end indices of the attribute value
/// in the data array. If `typ` is [`None`], then the 128-bit UUID is stored at
/// `val.0 - 16..val.0` in the data array.
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
            Some(Declaration::PRIMARY_SERVICE | Declaration::SECONDARY_SERVICE)
        )
    }

    /// Returns whether the attribute is a primary service declaration.
    #[inline(always)]
    const fn is_primary_service(&self) -> bool {
        matches!(self.typ, Some(Declaration::PRIMARY_SERVICE))
    }

    /// Returns whether the attribute is an include declaration.
    #[inline(always)]
    const fn is_include(&self) -> bool {
        matches!(self.typ, Some(Declaration::INCLUDE))
    }

    /// Returns whether the attribute is a characteristic declaration.
    #[inline(always)]
    const fn is_char(&self) -> bool {
        matches!(self.typ, Some(Declaration::CHARACTERISTIC))
    }

    /// Returns whether the attribute is an extended properties descriptor.
    #[inline(always)]
    const fn is_ext_props(&self) -> bool {
        matches!(
            self.typ,
            Some(Descriptor::CHARACTERISTIC_EXTENDED_PROPERTIES)
        )
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
    db: &'a Db,
    it: iter::Peekable<slice::Iter<'a, Attr>>,
    is_start: F,
    _marker: PhantomData<T>,
}

impl<'a, T: Group, F: Fn(&Attr) -> bool> GroupIter<'a, T, F> {
    /// Creates a new attribute group iterator.
    #[inline(always)]
    #[must_use]
    fn new(db: &'a Db, it: &'a [Attr], is_start: F) -> Self {
        Self {
            db,
            it: it.iter().peekable(),
            is_start,
            _marker: PhantomData,
        }
    }
}

impl<'a, T: Group, F: Fn(&Attr) -> bool> Iterator for GroupIter<'a, T, F> {
    type Item = DbEntry<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let decl = self.it.find(|at| (self.is_start)(at))?;
        let mut end = decl.hdl;
        while !self.it.peek().map_or(true, |at| T::is_next_group(at.typ)) {
            // SAFETY: `peek()` returned another attribute
            end = unsafe { self.it.next().unwrap_unchecked().hdl };
        }
        Some(DbEntry::new(self.db, decl, end))
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
            matches!(
                typ,
                Some(Declaration::PRIMARY_SERVICE | Declaration::SECONDARY_SERVICE)
            )
        }
    }

    impl Group for ServiceDef {}

    impl Group for CharacteristicDef {
        const UUID_OFF: usize = 3;

        #[inline(always)]
        fn is_next_group(typ: Option<Uuid16>) -> bool {
            // INC isn't needed, but including it improves the generated code
            matches!(
                typ,
                Some(
                    Declaration::PRIMARY_SERVICE
                        | Declaration::SECONDARY_SERVICE
                        | Declaration::INCLUDE
                        | Declaration::CHARACTERISTIC
                )
            )
        }
    }
}
