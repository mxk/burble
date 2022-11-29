use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU16;

use nameof::name_of_type;

/// Attribute handle ([Vol 3] Part F, Section 3.2.2).
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Handle(NonZeroU16);

impl Handle {
    /// Wraps a raw handle. Returns `None` if the handle is invalid.
    #[inline]
    #[must_use]
    pub(super) const fn new(h: u16) -> Option<Self> {
        // TODO: Use map() when it is const stable
        match NonZeroU16::new(h) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }
}

impl From<Handle> for u16 {
    #[inline]
    fn from(h: Handle) -> Self {
        h.0.get()
    }
}

impl Debug for Handle {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#06X})", name_of_type!(Handle), self.0.get())
    }
}

impl Display for Handle {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
