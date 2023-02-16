use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU16;
use std::ops::{Bound, RangeBounds};

use crate::name_of;

/// Attribute handle ([Vol 3] Part F, Section 3.2.2).
#[allow(clippy::unsafe_derive_deserialize)]
#[derive(
    Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Deserialize, serde::Serialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct Handle(NonZeroU16);

impl Handle {
    pub(crate) const MIN: Self = Self(
        // TODO: Replace with NonZeroU16::{MIN,MAX} when stable
        // SAFETY: Non-zero
        unsafe { NonZeroU16::new_unchecked(0x0001) },
    );
    pub(crate) const MAX: Self = Self(
        // SAFETY: Non-zero
        unsafe { NonZeroU16::new_unchecked(0xFFFF) },
    );

    /// Wraps a raw handle. Returns `None` if the handle is invalid.
    #[inline]
    #[must_use]
    pub(crate) const fn new(h: u16) -> Option<Self> {
        // TODO: Use map() when it is const stable
        match NonZeroU16::new(h) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Returns the next handle or `None` if the maximum handle was reached.
    #[inline]
    pub(crate) const fn next(self) -> Option<Self> {
        Self::new(self.0.get().wrapping_add(1))
    }
}

impl Debug for Handle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#06X})", name_of!(Handle), self.0.get())
    }
}

impl Display for Handle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl From<Handle> for u16 {
    #[inline]
    fn from(h: Handle) -> Self {
        h.0.get()
    }
}

impl From<Handle> for usize {
    #[inline]
    fn from(h: Handle) -> Self {
        Self::from(h.0.get())
    }
}

/// Inclusive range of attribute handles. This is a `Copy` version of
/// `RangeInclusive<Handle>`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[must_use]
pub struct HandleRange {
    start: Handle,
    end: Handle,
}

impl HandleRange {
    /// Handle range that includes all possible handles.
    pub const ALL: Self = Self {
        start: Handle::MIN,
        end: Handle::MAX,
    };

    /// Creates a new handle range `start..=end`.
    #[inline]
    pub const fn new(start: Handle, end: Handle) -> Self {
        assert!(start.0.get() <= end.0.get());
        Self { start, end }
    }

    /// Returns the starting handle.
    #[inline(always)]
    #[must_use]
    pub const fn start(self) -> Handle {
        self.start
    }

    /// Returns the ending handle.
    #[inline(always)]
    #[must_use]
    pub const fn end(self) -> Handle {
        self.end
    }
}

impl RangeBounds<Handle> for HandleRange {
    #[inline]
    fn start_bound(&self) -> Bound<&Handle> {
        Bound::Included(&self.start)
    }

    #[inline]
    fn end_bound(&self) -> Bound<&Handle> {
        Bound::Included(&self.end)
    }

    #[inline]
    fn contains<U>(&self, item: &U) -> bool
    where
        Handle: PartialOrd<U>,
        U: ?Sized + PartialOrd<Handle>,
    {
        self.start <= *item && *item <= self.end
    }
}

impl Default for HandleRange {
    /// Returns a handle range that includes all possible handles.
    #[inline(always)]
    fn default() -> Self {
        Self::ALL
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handle_size() {
        // Required by att::Bearer::read_by_type_op
        assert_eq!(std::mem::size_of::<Handle>(), 2);
        assert_eq!(std::mem::size_of::<HandleRange>(), 4);
    }
}
