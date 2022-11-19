use std::fmt::{Debug, Display, Formatter};

use nameof::name_of_type;

/// Connection handle.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct ConnHandle(u16);

impl ConnHandle {
    /// Invalid connection handle.
    pub(super) const INVALID: Self = Self(0xFFF);
    /// Maximum valid connection handle ([Vol 4] Part E, Section 5.4.2).
    const MAX: u16 = 0xEFF;

    /// Wraps a raw connection handle.
    #[inline]
    #[must_use]
    pub const fn from_raw(cn: u16) -> Self {
        Self(cn & 0xFFF)
    }

    /// Returns whether the connection handle is valid.
    #[inline]
    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.0 <= Self::MAX
    }
}

impl Default for ConnHandle {
    #[inline]
    fn default() -> Self {
        Self::INVALID
    }
}

impl From<ConnHandle> for u16 {
    #[inline]
    fn from(h: ConnHandle) -> Self {
        h.0
    }
}

impl Debug for ConnHandle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#05X})", name_of_type!(ConnHandle), self.0)
    }
}

impl Display for ConnHandle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Advertising set handle.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct AdvHandle(u8);

impl AdvHandle {
    /// Invalid advertising handle.
    pub const INVALID: Self = Self(0xFF);
    pub(crate) const MAX: u8 = 0xEF; // [Vol 4] Part E, Section 7.8.53

    /// Wraps a raw advertising handle.
    #[inline]
    #[must_use]
    pub(super) const fn from_raw(h: u8) -> Self {
        Self(h)
    }

    /// Returns whether the connection handle is valid.
    #[inline]
    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.0 <= Self::MAX
    }
}

impl Default for AdvHandle {
    #[inline]
    fn default() -> Self {
        Self::INVALID
    }
}

impl From<AdvHandle> for u8 {
    #[inline]
    fn from(h: AdvHandle) -> Self {
        h.0
    }
}

impl Debug for AdvHandle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#04X})", name_of_type!(AdvHandle), self.0)
    }
}

impl Display for AdvHandle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
