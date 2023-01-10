#![allow(clippy::use_self)]

use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::{NonZeroU128, NonZeroU16};
use std::u16;

use structbuf::Unpack;

const SHIFT: u32 = u128::BITS - u32::BITS;
const BASE: u128 = 0x00000000_0000_1000_8000_00805F9B34FB;
const MASK_16: u128 = !((u16::MAX as u128) << SHIFT);
const MASK_32: u128 = !((u32::MAX as u128) << SHIFT);

/// 16-, 32-, or 128-bit UUID ([Vol 3] Part B, Section 2.5.1).
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Uuid(NonZeroU128);

impl Uuid {
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
    pub const fn to_bytes(self) -> [u8; 16] {
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
            2 => Uuid16::new(v.unpack().u16()).map(Uuid16::as_uuid),
            16 => Uuid::new(v.unpack().u128()),
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
        // TODO: Translate
        Debug::fmt(self, f)
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
    /// Creates a 16-bit SIG UUID from a `u16`.
    #[inline]
    #[must_use]
    pub const fn new(v: u16) -> Option<Self> {
        match NonZeroU16::new(v) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
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
    pub const fn to_bytes(self) -> [u8; 2] {
        self.0.get().to_le_bytes()
    }
}

impl Debug for Uuid16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#06X}", self.0.get())
    }
}

impl Display for Uuid16 {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[allow(clippy::derive_hash_xor_eq)]
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

/// Creates an assigned 16-bit SIG UUID from a `u16`.
#[inline]
#[must_use]
pub(crate) const fn uuid16(v: u16) -> Uuid16 {
    // SAFETY: All crate uses guarantee that v != 0
    Uuid16(unsafe { NonZeroU16::new_unchecked(v) })
}

/// Provides implementations for converting a `repr(u16)` enum into [`Uuid`] and
/// [`Uuid16`].
macro_rules! uuid16_enum {
    ($($t:ty)*) => {$(
        impl $t {
            /// Returns the `Uuid` representation of the variant.
            #[inline]
            #[must_use]
            pub const fn uuid(self) -> $crate::gap::Uuid {
                self.uuid16().as_uuid()
            }

            /// Returns the `Uuid16` representation of the variant.
            #[inline(always)]
            #[must_use]
            pub const fn uuid16(self) -> $crate::gap::Uuid16 {
                $crate::gap::uuid16(self as _)
            }
        }

        impl ::core::convert::TryFrom<$crate::gap::Uuid16> for $t {
            type Error = ::num_enum::TryFromPrimitiveError<Self>;

            #[inline]
            fn try_from(u: $crate::gap::Uuid16) -> Result<Self, Self::Error> {
                use ::num_enum::TryFromPrimitive;
                Self::try_from_primitive(u.raw())
            }
        }

        impl ::core::cmp::PartialEq<$crate::gap::Uuid> for $t {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::gap::Uuid) -> bool {
                // Converting to 128-bit avoids branches
                self.uuid() == *rhs
            }
        }

        impl ::core::cmp::PartialEq<$crate::gap::Uuid16> for $t {
            #[inline(always)]
            fn eq(&self, rhs: &$crate::gap::Uuid16) -> bool {
                *self as u16 == rhs.raw()
            }
        }

        impl ::core::cmp::PartialEq<$t> for $crate::gap::Uuid {
            #[inline(always)]
            fn eq(&self, rhs: &$t) -> bool {
                *self == rhs.uuid()
            }
        }

        impl ::core::cmp::PartialEq<$t> for $crate::gap::Uuid16 {
            #[inline(always)]
            fn eq(&self, rhs: &$t) -> bool {
                self.raw() == *rhs as u16
            }
        }

        impl ::core::convert::From<$t> for $crate::gap::Uuid {
            #[inline]
            fn from(v: $t) -> Self {
                v.uuid()
            }
        }

        impl ::core::convert::From<$t> for $crate::gap::Uuid16 {
            #[inline]
            fn from(v: $t) -> Self {
                v.uuid16()
            }
        }
    )*}
}
pub(crate) use uuid16_enum;
