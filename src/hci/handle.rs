/// Connection handle.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct ConnHandle(u16);

impl ConnHandle {
    /// Wraps a raw connection handle.
    #[inline]
    #[must_use]
    pub(super) const fn from_raw(h: u16) -> Self {
        Self(h)
    }

    /// Returns an invalid connection handle.
    #[inline]
    #[must_use]
    pub const fn invalid() -> Self {
        Self(0xFFFF)
    }

    /// Returns whether the connection handle is valid.
    #[inline]
    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.0 >> 12 == 0 // [Vol 4] Part E, Section 5.4.2
    }
}

impl Default for ConnHandle {
    #[inline]
    fn default() -> Self {
        Self::invalid()
    }
}

impl From<ConnHandle> for u16 {
    #[inline]
    fn from(h: ConnHandle) -> Self {
        h.0
    }
}

/// Advertising set handle.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct AdvHandle(u8);

impl AdvHandle {
    /// Wraps a raw advertising handle.
    #[inline]
    #[must_use]
    pub(super) const fn from_raw(h: u8) -> Self {
        Self(h)
    }

    /// Returns an invalid advertising handle.
    #[inline]
    #[must_use]
    pub const fn invalid() -> Self {
        Self(0xFF)
    }

    /// Returns whether the connection handle is valid.
    #[inline]
    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.0 <= 0xEF // [Vol 4] Part E, Section 7.8.53
    }
}

impl Default for AdvHandle {
    #[inline]
    fn default() -> Self {
        Self::invalid()
    }
}

impl From<AdvHandle> for u8 {
    #[inline]
    fn from(h: AdvHandle) -> Self {
        h.0
    }
}