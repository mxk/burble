// TODO: Display

/// Connection handle.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct ConnHandle(u16);

impl ConnHandle {
    /// Invalid connection handle.
    pub const INVALID: Self = Self(0xFFFF);
    pub(crate) const MAX: u16 = 0xEFF; // [Vol 4] Part E, Section 5.4.2

    /// Wraps a raw connection handle.
    #[inline]
    #[must_use]
    pub(crate) const fn from_raw(h: u16) -> Self {
        Self(h)
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

/// Advertising set handle.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
