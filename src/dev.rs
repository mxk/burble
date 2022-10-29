//! Device addressing.

use std::fmt::{Debug, Display, Formatter};

/// Bluetooth device address ([Vol 6] Part B, Section 1.3).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, strum::Display)]
pub enum Addr {
    Public(RawAddr),
    Random(RawAddr),
}

impl Addr {
    /// Constructs a peer address from type and raw components.
    #[inline]
    #[must_use]
    pub fn peer(typ: u8, raw: RawAddr) -> Self {
        // [Vol 4] Part E, Sections 7.7.65.1 and 7.7.65.10
        match typ {
            // Public Device Address or Public Identity Address
            0x00 | 0x02 => Self::Public(raw),
            // Random Device Address or Random (Static) Identity Address
            0x01 | 0x03 => Self::Random(raw),
            _ => panic!("Unknown peer address type 0x{:02X}", typ),
        }
    }

    /// Returns the raw 48-bit address.
    #[inline]
    #[must_use]
    pub const fn raw(self) -> RawAddr {
        match self {
            Self::Public(addr) | Self::Random(addr) => addr,
        }
    }
}

impl Default for Addr {
    #[inline]
    fn default() -> Self {
        Self::Public(RawAddr::default())
    }
}

// 48-bit untyped device address stored in little-endian byte order.
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct RawAddr([u8; 6]);

impl From<[u8; 6]> for RawAddr {
    #[inline]
    fn from(v: [u8; 6]) -> Self {
        Self(v)
    }
}

impl AsRef<[u8]> for RawAddr {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl Debug for RawAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // [Vol 3] Part C, Section 3.2.1.3
        write!(
            f,
            "{:02X}:{:02X}:{:02X}:{:02X}:{:02X}:{:02X}",
            self.0[5], self.0[4], self.0[3], self.0[2], self.0[1], self.0[0]
        )
    }
}

impl Display for RawAddr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}