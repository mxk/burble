use std::fmt::{Debug, Display, Formatter};

/// Bluetooth device address ([Vol 6] Part B, Section 1.3).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, strum::Display)]
pub enum Addr {
    Public(RawAddr),
    Random(RawAddr),
}

impl Addr {
    /// Returns the raw 48-bit address.
    #[inline]
    #[must_use]
    pub const fn raw(self) -> RawAddr {
        match self {
            Self::Public(addr) | Self::Random(addr) => addr,
        }
    }
}

// 48-bit untyped device address stored in little-endian byte order.
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct RawAddr([u8; 6]);

impl From<[u8; 6]> for RawAddr {
    fn from(v: [u8; 6]) -> Self {
        Self(v)
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
