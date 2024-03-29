use core::fmt::{Debug, Display, Formatter};
use core::hash::{Hash, Hasher};
use core::num::{NonZeroU128, NonZeroU16};
use core::ops::Deref;
use core::{fmt, mem, ptr};

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
    const BYTES: usize = mem::size_of::<Self>();

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
    #[inline(always)]
    #[must_use]
    pub const unsafe fn new_unchecked(v: u128) -> Self {
        Self(NonZeroU128::new_unchecked(v))
    }

    /// Creates a UUID from a little-endian byte slice, which must be either 16-
    /// or 2-bytes long.
    #[inline]
    #[must_use]
    pub fn from_le_bytes(v: &[u8]) -> Option<Self> {
        match v.len() {
            Self::BYTES => Self::new(v.unpack().u128()),
            Uuid16::BYTES => Uuid16::new(v.unpack().u16()).map(Uuid16::as_uuid),
            _ => None,
        }
    }

    /// Returns the UUID type. Returns [`UuidType::NonSig`] for non-SIG UUIDs.
    #[inline]
    #[must_use]
    pub fn typ(self) -> UuidType {
        self.as_uuid16().map_or(UuidType::NonSig, Uuid16::typ)
    }

    /// Returns a [`Uuid16`] representation or [`None`] if the UUID is not an
    /// assigned 16-bit SIG UUID.
    #[inline]
    #[must_use]
    pub fn as_uuid16(self) -> Option<Uuid16> {
        // SAFETY: `u` is non-zero
        self.as_u16().map(|u| unsafe { Uuid16::new_unchecked(u) })
    }

    /// Converts an assigned 16-bit SIG UUID to `u16`. This is mutually
    /// exclusive with `as_u32` and `as_u128`.
    #[inline]
    #[must_use]
    pub fn as_u16(self) -> Option<u16> {
        #[allow(clippy::cast_possible_truncation)]
        let v = (self.0.get() >> SHIFT) as u16;
        (self.0.get() & MASK_16 == BASE && v > 0).then_some(v)
    }

    /// Converts an assigned 32-bit SIG UUID to `u32`. This is mutually
    /// exclusive with `as_u16` and `as_u128`.
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

    /// Returns a 2- or 16-byte vector representation of the UUID.
    #[inline]
    #[must_use]
    pub fn to_vec(self) -> UuidVec {
        #[allow(clippy::cast_possible_truncation)]
        let (n, v) = self.as_u16().map_or_else(
            || (Self::BYTES as u8, self.0.get().to_le_bytes()),
            |u| {
                let mut v = [0; Self::BYTES];
                v[..Uuid16::BYTES].copy_from_slice(&u.to_le_bytes());
                (Uuid16::BYTES as u8, v)
            },
        );
        UuidVec { n, v }
    }
}

impl From<Uuid16> for Uuid {
    #[inline(always)]
    fn from(u: Uuid16) -> Self {
        u.as_uuid()
    }
}

impl Debug for Uuid {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.typ() {
            UuidType::NonSig => Debug::fmt(self, f),
            typ => Debug::fmt(&typ, f),
        }
    }
}

impl From<Uuid> for u128 {
    #[inline(always)]
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
    const BYTES: usize = mem::size_of::<Self>();

    /// Creates an assigned 16-bit SIG UUID from a `u16`.
    #[inline]
    #[must_use]
    pub const fn new(v: u16) -> Option<Self> {
        match NonZeroU16::new(v) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Creates an assigned 16-bit SIG UUID from a `u16` without checking
    /// whether the value is non-zero.
    ///
    /// # Safety
    ///
    /// The value must not be zero.
    #[inline(always)]
    #[must_use]
    pub const unsafe fn new_unchecked(v: u16) -> Self {
        Self(NonZeroU16::new_unchecked(v))
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
}

impl Debug for Uuid16 {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#06X}", self.0.get())
    }
}

impl Display for Uuid16 {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    #[inline(always)]
    fn from(u: Uuid16) -> Self {
        u.0.get()
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

/// An owned little-endian vector representation of a UUID.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct UuidVec {
    n: u8,
    v: [u8; Uuid::BYTES],
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
                // SAFETY: All crate uses guarantee non-zero discriminants
                pub const [<$item:snake:upper>]: $crate::Uuid16 = unsafe {
                    $crate::Uuid16::new_unchecked(Self::$item as _)
                };
            )+}
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
                Self::try_from_primitive(u.0.get())
            }
        }

        impl ::core::cmp::PartialEq<$crate::Uuid> for $typ {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::Uuid) -> bool {
                // Converting to 128-bit avoids branches
                Uuid::from(*self) == *rhs
            }
        }

        impl ::core::cmp::PartialEq<$crate::Uuid16> for $typ {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::Uuid16) -> bool {
                *self as u16 == rhs.0.get()
            }
        }

        impl ::core::cmp::PartialEq<$typ> for $crate::Uuid {
            #[inline(always)]
            fn eq(&self, rhs: &$typ) -> bool {
                *self == Uuid::from(*rhs)
            }
        }

        impl ::core::cmp::PartialEq<$typ> for $crate::Uuid16 {
            #[inline(always)]
            fn eq(&self, rhs: &$typ) -> bool {
                self.0.get() == *rhs as u16
            }
        }

        impl ::core::convert::From<$typ> for $crate::Uuid {
            #[inline]
            fn from(v: $typ) -> Self {
                // SAFETY: All crate uses guarantee non-zero discriminants
                unsafe { $crate::Uuid16::new_unchecked(v as _) }.as_uuid()
            }
        }

        impl ::core::convert::From<$typ> for $crate::Uuid16 {
            #[inline(always)]
            fn from(v: $typ) -> Self {
                // SAFETY: All crate uses guarantee non-zero discriminants
                unsafe { $crate::Uuid16::new_unchecked(v as _) }
            }
        }
    }
}

include!("uuid16.rs");

impl Service {
    /// Returns whether the server can host at most one instances of this
    /// service.
    #[must_use]
    pub const fn is_singleton(self) -> bool {
        use Service::*;
        #[allow(clippy::match_same_arms)]
        match self {
            GenericAccess => true,
            GenericAttribute => true,
            DeviceInformation => true,
            Battery => false,
            HumanInterfaceDevice => false,
            ScanParameters => true,
            _ => true, // TODO: Specify for all
        }
    }
}

#[cfg(test)]
mod tests {
    use enum_iterator::all;

    use super::*;

    #[test]
    fn uuid_type() {
        // SAFETY: Uuid16 values are non-zero
        unsafe {
            assert_eq!(
                Uuid16::new_unchecked(0x0001).typ(),
                UuidType::Protocol(0x0001)
            );
        };
        for v in all::<ServiceClass>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::ServiceClass(v));
        }
        for v in all::<Service>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::Service(v));
        }
        for v in all::<Unit>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::Unit(v));
        }
        for v in all::<Declaration>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::Declaration(v));
        }
        for v in all::<Descriptor>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::Descriptor(v));
        }
        for v in all::<Characteristic>() {
            assert_eq!(Uuid16::from(v).typ(), UuidType::Characteristic(v));
        }
        // SAFETY: Uuid16 values are non-zero
        unsafe {
            assert_eq!(
                Uuid16::new_unchecked(0xFEFF).typ(),
                UuidType::Member(0xFEFF)
            );
            assert_eq!(
                Uuid16::new_unchecked(0xFFFF).typ(),
                UuidType::Unknown(0xFFFF)
            );
        }
    }
}
