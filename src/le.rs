//! LE-specific types.

use std::fmt::{Debug, Formatter};

use structbuf::{Packer, Unpacker};

/// Bluetooth device address ([Vol 6] Part B, Section 1.3).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Addr {
    Public(RawAddr),
    Random(RawAddr),
}

impl Addr {
    /// Constructs a peer address from type and raw components.
    #[inline]
    #[must_use]
    pub(super) fn peer(typ: u8, raw: RawAddr) -> Self {
        // [Vol 4] Part E, Sections 7.7.65.1 and 7.7.65.10
        match typ {
            // Public Device Address or Public Identity Address
            0x00 | 0x02 => Self::Public(raw),
            // Random Device Address or Random (Static) Identity Address
            0x01 | 0x03 => Self::Random(raw),
            _ => unreachable!("unknown peer address type {typ:#04X}"),
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

/// SMP command codec ([Vol 3] Part H, Section 3.6.5).
impl burble_crypto::Codec for Addr {
    #[inline]
    fn pack(&self, p: &mut Packer) {
        match *self {
            Self::Public(ref addr) => p.u8(0).put(addr.0),
            Self::Random(ref addr) => p.u8(1).put(addr.0),
        };
    }

    #[inline]
    fn unpack(p: &mut Unpacker) -> Option<Self> {
        Some(match (p.u8(), RawAddr::from_le_bytes(p.bytes())) {
            (0, addr) => Self::Public(addr),
            (1, addr) => Self::Random(addr),
            _ => return None,
        })
    }
}

impl From<Addr> for burble_crypto::Addr {
    #[inline]
    fn from(a: Addr) -> Self {
        let (is_random, raw) = match a {
            Addr::Public(addr) => (false, addr.0),
            Addr::Random(addr) => (true, addr.0),
        };
        Self::from_le_bytes(is_random, raw)
    }
}

// 48-bit untyped device address stored in little-endian byte order.
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct RawAddr([u8; 6]);

impl RawAddr {
    /// Creates a device address from a little-endian byte array.
    #[inline(always)]
    #[must_use]
    pub const fn from_le_bytes(v: [u8; 6]) -> Self {
        Self(v)
    }

    /// Returns the device address as a little-endian byte array.
    #[inline(always)]
    #[must_use]
    pub const fn as_le_bytes(&self) -> &[u8; 6] {
        &self.0
    }
}

impl AsRef<[u8]> for RawAddr {
    #[inline(always)]
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
    #[inline(always)]
    #[must_use]
    pub const fn new(v: i8) -> Self {
        Self(v)
    }
}

impl Default for TxPower {
    #[inline(always)]
    fn default() -> Self {
        Self(Self::MAX)
    }
}

impl From<TxPower> for i8 {
    #[inline(always)]
    fn from(p: TxPower) -> Self {
        p.0
    }
}

crate::impl_display_via_debug! { Addr, RawAddr }
