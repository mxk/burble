use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::u128;

use structbuf::{Pack, Packer, StructBuf, Unpack};

use crate::att::{Access, Handle, Perms};
use crate::gap::{Uuid, Uuid16};

use super::*;

/// Maximum length of a characteristic declaration value, which is the longest
/// value stored in the schema ([Vol 3] Part G, Section 3.3.1).
const MAX_VAL_LEN: usize = 1 + 2 + 16;

/// Database schema.
///
/// Defines the structure of all services, assigns attribute handles, and
/// contains all information required to calculate the database hash
/// ([Vol 3] Part G, Section 7.3.1).
#[derive(Clone, Debug, Default)]
pub struct Schema {
    attrs: Vec<Entry>,
    types: HashMap<Handle, Uuid>,
    hash: u128,
}

impl Schema {
    /// Creates an empty schema.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            attrs: Vec::with_capacity(16), // TODO: Tune for minimum GATT server
            types: HashMap::new(),
            hash: 0,
        }
    }

    /// Defines a primary service ([Vol 3] Part G, Section 3.1). 16-bit UUID
    /// services should be defined before those with 128-bit UUIDs.
    #[inline]
    pub fn primary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        incs: impl FnOnce(Builder<ServiceIncludes>),
        chars: impl FnOnce(Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.gatt_attr(Type::PRIMARY_SERVICE, |p| p.uuid(uuid));
        incs(self.builder());
        (hdl, chars(self.builder()))
    }

    /// Defines a secondary service ([Vol 3] Part G, Section 3.1). 16-bit UUID
    /// services should be defined before those with 128-bit UUIDs.
    #[inline]
    pub fn secondary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        incs: impl FnOnce(Builder<ServiceIncludes>),
        chars: impl FnOnce(Builder<ServiceCharacteristics>) -> T,
    ) -> (Handle, T) {
        let hdl = self.gatt_attr(Type::SECONDARY_SERVICE, |p| p.uuid(uuid));
        incs(self.builder());
        (hdl, chars(self.builder()))
    }

    /// Returns the index and entry of the specified handle or [`None`] if the
    /// handle is invalid.
    fn get(&self, hdl: Handle) -> Option<(usize, &Entry)> {
        let i = usize::from(u16::from(hdl));
        // The entry can exist either at or, if there are gaps, before index i.
        let search = match self.attrs.get(i) {
            // Fast path for the common case where the handle value is the index
            Some(e) if e.hdl == hdl => return Some((i, e)),
            // SAFETY: `i` is in bounds
            Some(_) => unsafe { self.attrs.get_unchecked(..i) },
            None => &self.attrs,
        };
        search.binary_search_by(|e| e.hdl.cmp(&hdl)).ok().map(|i| {
            // SAFETY: Entry was found at i
            (i, unsafe { self.attrs.get_unchecked(i) })
        })
    }

    /// Returns the start and end entries of the service group defined by `hdl`
    /// or [`None`] if the handle does not refer to a service.
    fn service_group(&self, hdl: Handle) -> Option<(&Entry, &Entry)> {
        let (i, e) = self.get(hdl)?;
        if !matches!(e.typ, Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE)) {
            return None;
        }
        let mut it = self.attrs.iter();
        it.nth(i);
        let mut n = 0;
        for next in it {
            match next.typ {
                Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE) => break,
                _ => n += 1,
            }
        }
        // SAFETY: i and i + n are in bounds
        Some(unsafe { (self.attrs.get_unchecked(i), self.attrs.get_unchecked(i + n)) })
    }

    /// Returns the start and end indices of the group defined by `hdl`, which
    /// must be either a service or a characteristic handle.
    fn group(&self, hdl: Handle) -> Option<(usize, usize)> {
        let (i, e) = self.get(hdl)?;
        let is_char = match e.typ {
            Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE) => false,
            Some(Type::CHARACTERISTIC) => true,
            _ => return None,
        };
        let mut it = self.attrs.iter();
        it.nth(i);
        let mut n = 0;
        for next in it {
            match next.typ {
                Some(Type::PRIMARY_SERVICE | Type::SECONDARY_SERVICE) => break,
                Some(Type::CHARACTERISTIC) if is_char => break,
                _ => n += 1,
            }
        }
        Some((i, i + n))
    }

    /// Defines a GATT profile attribute, calling `f` to set its value.
    fn gatt_attr(&mut self, typ: Uuid16, f: impl FnOnce(&mut Packer)) -> Handle {
        // TODO: Avoid copy if Packer can operate on &mut [u8]
        let mut b = StructBuf::new(MAX_VAL_LEN);
        f(&mut b.append());
        let hdl = self.next_handle();
        #[allow(clippy::cast_possible_truncation)]
        let mut e = Entry {
            hdl,
            typ: Some(typ),
            perms: Perms::new(Access::read()),
            len: b.len() as _,
            val: Default::default(),
        };
        e.val[..b.len()].copy_from_slice(&b);
        self.attrs.push(e);
        hdl
    }

    /// Defines a non-GATT profile attribute.
    fn attr(&mut self, typ: impl Into<Uuid>, perms: Perms) -> Handle {
        let hdl = self.next_handle();
        let typ = typ.into();
        let e = Entry {
            hdl,
            typ: typ.as_uuid16(),
            perms,
            len: 0,
            val: Default::default(),
        };
        if e.typ.is_none() {
            self.types.insert(hdl, typ);
        }
        self.attrs.push(e);
        hdl
    }

    /// Returns the last attribute handle.
    #[inline]
    fn last_handle(&self) -> Option<Handle> {
        self.attrs.last().map(|e| e.hdl)
    }

    /// Returns the next unused handle.
    ///
    /// # Panics
    ///
    /// Panics if no more handles are available.
    #[inline]
    fn next_handle(&self) -> Handle {
        (self.attrs.last()).map_or(Handle::MIN, |e| e.hdl.next().expect("no available handles"))
    }

    /// Returns a new builder.
    #[inline]
    fn builder<T: Default>(&mut self) -> Builder<T> {
        Builder(self, PhantomData::default())
    }
}

/// A schema entry. To reduce database size, all 128-bit UUIDs are stored in
/// a separate map.
#[derive(Clone, Debug)]
#[must_use]
struct Entry {
    hdl: Handle,
    typ: Option<Uuid16>,
    perms: Perms,
    len: u8,
    val: [u8; MAX_VAL_LEN],
}

impl AsRef<[u8]> for Entry {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        // SAFETY: len is always <= MAX_VAL_LEN
        unsafe { self.val.get_unchecked(..self.len as usize) }
    }
}

/// Schema builder used to define services and characteristics.
#[derive(Debug)]
#[repr(transparent)]
pub struct Builder<'a, T>(&'a mut Schema, PhantomData<T>);

/// Marker type for [`Builder`].
#[derive(Debug, Default)]
pub struct ServiceIncludes;

/// Marker type for [`Builder`].
#[derive(Debug, Default)]
pub struct ServiceCharacteristics;

/// Marker type for [`Builder`].
#[derive(Debug, Default)]
pub struct CharacteristicDescriptors;

impl Builder<'_, ServiceIncludes> {
    /// Defines a service include attribute ([Vol 3] Part G, Section 3.2).
    pub fn include(&mut self, hdl: Handle) {
        let (start, end) = self.service_group(hdl).expect("invalid service handle");
        let uuid = (start.len == 2).then(|| start.unpack().u16());
        let end = end.hdl;
        self.gatt_attr(Type::INCLUDE, |p| {
            p.u16(hdl).u16(end);
            uuid.map(|u| p.u16(u));
        });
    }
}

impl Builder<'_, ServiceCharacteristics> {
    /// Defines a single-value service characteristic
    /// ([Vol 3] Part G, Section 3.3). Mandatory service characteristics must
    /// precede optional ones and 16-bit UUID characteristics should come before
    /// those with 128-bit UUIDs.
    #[inline]
    pub fn char<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        props: Props,
        perms: Perms,
        descs: impl FnOnce(Builder<CharacteristicDescriptors>) -> T,
    ) -> (Handle, T) {
        let uuid = uuid.into();
        let hdl = self.next_handle().next().expect("no available handles");
        self.gatt_attr(Type::CHARACTERISTIC, |p| {
            p.u8(props.bits()).u16(hdl).uuid(uuid);
        });
        (self.attr(uuid, perms), descs(self.builder()))
    }

    // TODO: Method for multi-value characteristics
}

impl Builder<'_, CharacteristicDescriptors> {
    /// Defines a custom characteristic descriptor
    /// ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn desc<T>(&mut self, uuid: impl Into<Uuid>, perms: Perms) -> Handle {
        self.attr(uuid, perms)
    }

    /// Defines an Extended Properties descriptor
    /// ([Vol 3] Part G, Section 3.3.3.1).
    #[inline]
    pub fn ext_props<T>(&mut self, props: ExtProps) {
        self.gatt_attr(Type::CHARACTERISTIC_EXTENDED_PROPERTIES, |p| {
            p.u16(props.bits());
        });
    }

    /// Defines a Client Characteristic Configuration descriptor
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    pub fn client_config<T>(&mut self, p: Perms) {
        self.attr(Type::CLIENT_CHARACTERISTIC_CONFIGURATION, p);
    }
}

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
