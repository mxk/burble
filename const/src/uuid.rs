use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::{NonZeroU128, NonZeroU16};
use std::ops::Deref;
use std::ptr;

use num_enum::TryFromPrimitive;
use structbuf::{Packer, Unpack};

const SHIFT: u32 = u128::BITS - u32::BITS;
const BASE: u128 = 0x00000000_0000_1000_8000_00805F9B34FB;
const MASK_16: u128 = !((u16::MAX as u128) << SHIFT);
const MASK_32: u128 = !((u32::MAX as u128) << SHIFT);

/// 16-, 32-, or 128-bit UUID ([Vol 3] Part B, Section 2.5.1).
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Uuid(NonZeroU128);

impl Uuid {
    /// UUID size in bytes.
    pub const BYTES: usize = std::mem::size_of::<Self>();
    /// Maximum UUID value.
    pub const MAX: Self = Self(
        // SAFETY: Non-zero
        unsafe { NonZeroU128::new_unchecked(u128::MAX) },
    );

    /// Creates a UUID from a `u128`.
    #[inline]
    #[must_use]
    pub const fn new(v: u128) -> Option<Self> {
        match NonZeroU128::new(v) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Creates a UUID from a `u128` without checking whether the value is
    /// non-zero.
    ///
    /// # Safety
    ///
    /// The value must not be zero.
    #[inline]
    #[must_use]
    pub const unsafe fn new_unchecked(v: u128) -> Self {
        Self(NonZeroU128::new_unchecked(v))
    }

    /// Returns the UUID type. Returns [`UuidType::NonSig`] for non-SIG UUID.
    #[inline]
    #[must_use]
    pub fn typ(self) -> UuidType {
        self.as_uuid16().map_or(UuidType::NonSig, Uuid16::typ)
    }

    /// Returns a [`Uuid16`] representation or [`None`] if the UUID is not an
    /// assigned 16-bit UUID.
    #[inline]
    #[must_use]
    pub fn as_uuid16(self) -> Option<Uuid16> {
        self.as_u16().map(uuid16)
    }

    /// Converts an assigned 16-bit Bluetooth SIG UUID to `u16`. This is
    /// mutually exclusive with `as_u32` and `as_u128`.
    #[inline]
    #[must_use]
    pub fn as_u16(self) -> Option<u16> {
        #[allow(clippy::cast_possible_truncation)]
        let v = (self.0.get() >> SHIFT) as u16;
        (self.0.get() & MASK_16 == BASE && v > 0).then_some(v)
    }

    /// Converts an assigned 32-bit Bluetooth SIG UUID to `u32`. This is
    /// mutually exclusive with `as_u16` and `as_u128`.
    #[inline]
    #[must_use]
    pub fn as_u32(self) -> Option<u32> {
        let v = (self.0.get() >> SHIFT) as u32;
        (self.0.get() & MASK_32 == BASE && v > u32::from(u16::MAX)).then_some(v)
    }

    /// Converts an unassigned UUID to `u128`. This is mutually exclusive with
    /// `as_u16` and `as_u32`.
    #[inline]
    #[must_use]
    pub fn as_u128(self) -> Option<u128> {
        (self.0.get() & MASK_32 != BASE).then_some(self.0.get())
    }

    /// Returns the UUID as a little-endian byte array.
    #[inline]
    #[must_use]
    pub const fn to_bytes(self) -> [u8; Self::BYTES] {
        self.0.get().to_le_bytes()
    }
}

impl From<Uuid16> for Uuid {
    #[inline]
    fn from(u: Uuid16) -> Self {
        u.as_uuid()
    }
}

impl TryFrom<&[u8]> for Uuid {
    type Error = ();

    #[inline]
    fn try_from(v: &[u8]) -> Result<Self, Self::Error> {
        match v.len() {
            Self::BYTES => Self::new(v.unpack().u128()),
            Uuid16::BYTES => Uuid16::new(v.unpack().u16()).map(Uuid16::as_uuid),
            _ => None,
        }
        .ok_or(())
    }
}

impl Debug for Uuid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::cast_possible_truncation)]
        if let Some(v) = self.as_u16() {
            write!(f, "{v:#06X}")
        } else if let Some(v) = self.as_u32() {
            write!(f, "{v:#010X}")
        } else {
            let v = self.0.get();
            write!(
                f,
                "{:08X}-{:04X}-{:04X}-{:04X}-{:012X}",
                (v >> 96) as u32,
                (v >> 80) as u16,
                (v >> 64) as u16,
                (v >> 48) as u16,
                (v & ((1 << 48) - 1)) as u64
            )
        }
    }
}

impl Display for Uuid {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.typ() {
            UuidType::NonSig => Debug::fmt(self, f),
            typ => Debug::fmt(&typ, f),
        }
    }
}

impl From<Uuid> for u128 {
    #[inline]
    fn from(u: Uuid) -> Self {
        u.0.get()
    }
}

/// 16-bit Bluetooth SIG UUID.
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Uuid16(NonZeroU16);

impl Uuid16 {
    /// UUID size in bytes.
    pub const BYTES: usize = std::mem::size_of::<Self>();

    /// Creates a 16-bit SIG UUID from a `u16`.
    #[inline]
    #[must_use]
    pub const fn new(v: u16) -> Option<Self> {
        match NonZeroU16::new(v) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Returns the UUID type.
    #[inline(always)]
    pub fn typ(self) -> UuidType {
        let u = self.0.get();
        // SAFETY: UUID_MAP has 256 entries
        (unsafe { &*UUID_MAP.as_ptr().add((u >> 8) as _) })(u)
    }

    /// Returns 128-bit UUID representation.
    #[inline]
    #[must_use]
    pub const fn as_uuid(self) -> Uuid {
        // TODO: Use NonZeroU128::from() when it is const
        // SAFETY: Always non-zero
        unsafe { Uuid::new_unchecked((self.0.get() as u128) << SHIFT | BASE) }
    }

    /// Returns the raw 16-bit UUID value.
    #[inline(always)]
    #[must_use]
    pub(crate) const fn raw(self) -> u16 {
        self.0.get()
    }

    /// Returns the UUID as a little-endian byte array.
    #[inline]
    #[must_use]
    pub const fn to_bytes(self) -> [u8; Self::BYTES] {
        self.0.get().to_le_bytes()
    }
}

impl Debug for Uuid16 {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#06X}", self.0.get())
    }
}

impl Display for Uuid16 {
    #[inline(always)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.typ(), f)
    }
}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for Uuid16 {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_uuid().hash(state);
    }
}

impl From<Uuid16> for u16 {
    #[inline]
    fn from(u: Uuid16) -> Self {
        u.raw()
    }
}

/// 16-bit UUID type.
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum UuidType {
    Protocol(u16),
    ServiceClass(ServiceClass),
    Service(Service),
    Unit(Unit),
    Declaration(Declaration),
    Descriptor(Descriptor),
    Characteristic(Characteristic),
    // TODO: Are Member Service UUIDs used anywhere?
    // ([Assigned Numbers] Section 3.11)
    Member(u16),
    Unknown(u16),
    NonSig,
}

impl From<Uuid> for UuidType {
    #[inline(always)]
    fn from(u: Uuid) -> Self {
        u.typ()
    }
}

impl From<Uuid16> for UuidType {
    #[inline(always)]
    fn from(u: Uuid16) -> Self {
        u.typ()
    }
}

type UuidMap = [fn(u16) -> UuidType; 256];

static UUID_MAP: UuidMap = {
    use UuidType::*;
    #[inline(always)]
    fn is<T: TryFromPrimitive<Primitive = u16>>(u: u16, f: impl FnOnce(T) -> UuidType) -> UuidType {
        T::try_from_primitive(u).map_or(Unknown(u), f)
    }
    let mut m: UuidMap = [Unknown; 256];
    m[0x00] = Protocol;
    m[0x01] = Protocol;
    m[0x10] = |u| is(u, ServiceClass);
    m[0x11] = |u| is(u, ServiceClass);
    m[0x12] = |u| is(u, ServiceClass);
    m[0x13] = |u| is(u, ServiceClass);
    m[0x14] = |u| is(u, ServiceClass);
    m[0x18] = |u| is(u, Service);
    m[0x27] = |u| is(u, Unit);
    m[0x28] = |u| is(u, Declaration);
    m[0x29] = |u| is(u, Descriptor);
    m[0x2A] = |u| is(u, Characteristic);
    m[0x2B] = |u| is(u, Characteristic);
    m[0xFC] = Member;
    m[0xFD] = Member;
    m[0xFE] = Member;
    m
};

impl Debug for UuidType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use UuidType::*;
        match *self {
            Protocol(u) => (f.debug_tuple("Protocol").field(&format_args!("{u:#06X}"))).finish(),
            ServiceClass(ref u) => f.debug_tuple("ServiceClass").field(u).finish(),
            Service(ref u) => f.debug_tuple("Service").field(u).finish(),
            Unit(ref u) => f.debug_tuple("Unit").field(u).finish(),
            Declaration(ref u) => f.debug_tuple("Declaration").field(u).finish(),
            Descriptor(ref u) => f.debug_tuple("Descriptor").field(u).finish(),
            Characteristic(ref u) => f.debug_tuple("Characteristic").field(u).finish(),
            Member(id) => f.debug_tuple("Company").field(&id).finish(),
            Unknown(u) => (f.debug_tuple("Unknown").field(&format_args!("{u:#06X}"))).finish(),
            NonSig => f.write_str("NonSig"),
        }
    }
}

impl Display for UuidType {
    #[inline(always)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// An owned little-endian vector representation of a UUID.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct UuidVec {
    n: u8,
    v: [u8; Uuid::BYTES],
}

impl UuidVec {
    /// Creates a vector representation of a UUID.
    #[inline]
    #[must_use]
    pub fn new(u: Uuid) -> Self {
        let (n, v) = u.as_uuid16().map_or_else(
            || (Uuid::BYTES, u.to_bytes()),
            |u| {
                let mut v = [0; Uuid::BYTES];
                v[..Uuid16::BYTES].copy_from_slice(&u.to_bytes());
                (Uuid16::BYTES, v)
            },
        );
        #[allow(clippy::cast_possible_truncation)]
        Self { n: n as _, v }
    }
}

impl Deref for UuidVec {
    type Target = [u8];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: `n` is 0, 2, or 16
        unsafe { &*ptr::slice_from_raw_parts(self.v.as_ptr().cast(), self.n as _) }
    }
}

/// Packer extension functions.
pub trait UuidPacker {
    fn uuid(&mut self, u: impl Into<Uuid>);
}

impl UuidPacker for Packer<'_> {
    /// Writes either a 16- or a 128-bit UUID at the current index.
    #[inline]
    fn uuid(&mut self, u: impl Into<Uuid>) {
        let u = u.into();
        match u.as_u16() {
            Some(u) => self.u16(u),
            None => self.u128(u),
        };
    }
}

/// Creates an assigned 16-bit SIG UUID from a `u16`.
#[inline]
#[must_use]
const fn uuid16(v: u16) -> Uuid16 {
    // SAFETY: All crate uses guarantee that v != 0
    Uuid16(unsafe { NonZeroU16::new_unchecked(v) })
}

/// Provides implementations for a 16-bit UUID enum.
macro_rules! uuid16_enum {
    (
        $(#[$outer:meta])*
        $vis:vis enum $typ:ident {
            $($item:ident = $uuid:literal,)+
        }
    ) => {
        $(#[$outer])*
        #[derive(
            Clone,
            Copy,
            Debug,
            Eq,
            Ord,
            PartialEq,
            PartialOrd,
            ::num_enum::IntoPrimitive,
            ::num_enum::TryFromPrimitive,
        )]
        #[cfg_attr(test, derive(enum_iterator::Sequence))]
        #[non_exhaustive]
        #[repr(u16)]
        $vis enum $typ {
            $($item = $uuid,)+
        }

        impl $typ {
            ::paste::paste! {$(
                pub const [<$item:snake:upper>]: $crate::Uuid16 = Self::$item.uuid16();
            )+}

            /// Returns the `Uuid` representation of the variant.
            #[inline]
            #[must_use]
            pub const fn uuid(self) -> $crate::Uuid {
                self.uuid16().as_uuid()
            }

            /// Returns the `Uuid16` representation of the variant.
            #[inline(always)]
            #[must_use]
            pub const fn uuid16(self) -> $crate::Uuid16 {
                uuid16(self as _)
            }
        }

        impl ::core::fmt::Display for $typ {
            #[inline(always)]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::fmt::Debug::fmt(self, f)
            }
        }

        impl ::core::convert::TryFrom<$crate::Uuid16> for $typ {
            type Error = ::num_enum::TryFromPrimitiveError<Self>;

            #[inline]
            fn try_from(u: $crate::Uuid16) -> Result<Self, Self::Error> {
                use ::num_enum::TryFromPrimitive;
                Self::try_from_primitive(u.raw())
            }
        }

        impl ::core::cmp::PartialEq<$crate::Uuid> for $typ {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::Uuid) -> bool {
                // Converting to 128-bit avoids branches
                self.uuid() == *rhs
            }
        }

        impl ::core::cmp::PartialEq<$crate::Uuid16> for $typ {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::Uuid16) -> bool {
                *self as u16 == rhs.raw()
            }
        }

        impl ::core::cmp::PartialEq<$typ> for $crate::Uuid {
            #[inline(always)]
            fn eq(&self, rhs: &$typ) -> bool {
                *self == rhs.uuid()
            }
        }

        impl ::core::cmp::PartialEq<$typ> for $crate::Uuid16 {
            #[inline(always)]
            fn eq(&self, rhs: &$typ) -> bool {
                self.raw() == *rhs as u16
            }
        }

        impl ::core::convert::From<$typ> for $crate::Uuid {
            #[inline]
            fn from(v: $typ) -> Self {
                v.uuid()
            }
        }

        impl ::core::convert::From<$typ> for $crate::Uuid16 {
            #[inline]
            fn from(v: $typ) -> Self {
                v.uuid16()
            }
        }
    }
}

include!("uuid16.rs");

#[cfg(test)]
mod tests {
    use enum_iterator::all;

    use super::*;

    #[test]
    fn uuid_type() {
        assert_eq!(uuid16(0x0001).typ(), UuidType::Protocol(0x0001));
        for v in all::<ServiceClass>() {
            assert_eq!(v.uuid16().typ(), UuidType::ServiceClass(v));
        }
        for v in all::<Service>() {
            assert_eq!(v.uuid16().typ(), UuidType::Service(v));
        }
        for v in all::<Unit>() {
            assert_eq!(v.uuid16().typ(), UuidType::Unit(v));
        }
        for v in all::<Declaration>() {
            assert_eq!(v.uuid16().typ(), UuidType::Declaration(v));
        }
        for v in all::<Descriptor>() {
            assert_eq!(v.uuid16().typ(), UuidType::Descriptor(v));
        }
        for v in all::<Characteristic>() {
            assert_eq!(v.uuid16().typ(), UuidType::Characteristic(v));
        }
        assert_eq!(uuid16(0xFEFF).typ(), UuidType::Member(0xFEFF));
        assert_eq!(uuid16(0xFFFF).typ(), UuidType::Unknown(0xFFFF));
    }
}
