//! LE-specific types.

use std::fmt::{Debug, Display, Formatter};

use crate::hci::Event;

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
            _ => panic!("Unknown peer address type {:#04X}", typ),
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

/// Transmission power level in dBm.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct TxPower(i8);

impl TxPower {
    /// Maximum power level ([Vol 6] Part A, Section 3).
    pub(crate) const MAX: i8 = 20;

    /// Unknown or no preference power level
    /// ([Vol 4] Part E, Sections 7.5.4 and 7.8.53).
    pub(crate) const NONE: i8 = 0x7F;

    /// Creates a power level of `v` dBm.
    #[inline]
    #[must_use]
    pub const fn new(v: i8) -> Self {
        Self(v)
    }
}

impl Default for TxPower {
    #[inline]
    fn default() -> Self {
        Self(Self::MAX)
    }
}

impl From<&mut Event<'_>> for TxPower {
    #[inline]
    fn from(e: &mut Event<'_>) -> Self {
        Self(e.i8())
    }
}

impl From<TxPower> for i8 {
    #[inline]
    fn from(p: TxPower) -> Self {
        p.0
    }
}
