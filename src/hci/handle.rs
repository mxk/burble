use std::fmt::{Debug, Display, Formatter};
use std::num::{NonZeroU16, NonZeroU8};

use crate::util::name_of;

/// Connection handle ([Vol 4] Part E, Section 5.4.2).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct ConnHandle(NonZeroU16);

impl ConnHandle {
    /// Number of meaningful bits.
    pub(crate) const BITS: u16 = 12;
    /// Maximum valid connection handle.
    const MAX: u16 = 0xEFF;

    /// Wraps a raw connection handle. Returns `None` if the handle is invalid.
    #[inline]
    #[must_use]
    pub(crate) fn new(mut v: u16) -> Option<Self> {
        v &= (1 << Self::BITS) - 1;
        // SAFETY: v can't be 0xFFFF, so !v is never 0
        (v <= Self::MAX).then_some(Self(unsafe { NonZeroU16::new_unchecked(!v) }))
    }
}

impl From<ConnHandle> for u16 {
    #[inline]
    fn from(cn: ConnHandle) -> Self {
        !cn.0.get()
    }
}

impl Debug for ConnHandle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#05X})", name_of!(ConnHandle), u16::from(*self))
    }
}

impl Display for ConnHandle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Advertising set handle ([Vol 4] Part E, Section 7.8.53).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct AdvHandle(NonZeroU8);

impl AdvHandle {
    /// Maximum valid advertising handle.
    pub(super) const MAX: u8 = 0xEF;

    /// Wraps a raw advertising handle. Returns `None` if the handle is invalid.
    #[inline]
    #[must_use]
    pub(super) fn new(v: u8) -> Option<Self> {
        // SAFETY: v can't be 0xFF, so !v is never 0
        (v <= Self::MAX).then_some(Self(unsafe { NonZeroU8::new_unchecked(!v) }))
    }
}

impl From<AdvHandle> for u8 {
    #[inline]
    fn from(h: AdvHandle) -> Self {
        !h.0.get()
    }
}

impl Debug for AdvHandle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#04X})", name_of!(AdvHandle), u8::from(*self))
    }
}

impl Display for AdvHandle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
