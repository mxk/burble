use std::fmt::{Debug, Display, Formatter};
use std::num::NonZeroU16;

use nameof::name_of_type;

use crate::hci;

/// LE-U logical link ([Vol 1] Part A, Section 3.5.5.2.2).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct LeU(hci::ConnHandle);

impl LeU {
    /// Creates an LE-U logical link from an HCI connection handle.
    #[inline]
    #[must_use]
    pub const fn new(cn: hci::ConnHandle) -> Self {
        Self(cn)
    }

    /// Associates a channel identifier with an LE-U logical link.
    #[inline]
    #[must_use]
    pub(super) fn chan(self, chan: Cid) -> LeCid {
        assert!(chan.is_le());
        LeCid { link: self, chan }
    }
}

impl From<LeU> for u16 {
    #[inline]
    fn from(link: LeU) -> Self {
        Self::from(link.0)
    }
}

impl From<LeU> for hci::ConnHandle {
    #[inline]
    fn from(link: LeU) -> Self {
        link.0
    }
}

impl Debug for LeU {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#05X})", name_of_type!(LeU), u16::from(*self))
    }
}

impl Display for LeU {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Channel identifier ([Vol 3] Part A, Section 2.1).
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Cid(NonZeroU16);

impl Cid {
    /// Attribute protocol channel.
    pub(super) const ATT: Self = Self::fixed(0x0004);
    /// LE signaling channel.
    pub(super) const LE_SIGNAL: Self = Self::fixed(0x0005);
    /// Security Manager protocol channel.
    pub(super) const SM: Self = Self::fixed(0x0006);

    /// Wraps a fixed CID.
    #[inline]
    #[must_use]
    const fn fixed(v: u16) -> Self {
        // SAFETY: Only called for valid fixed channels
        Self(unsafe { NonZeroU16::new_unchecked(v) })
    }

    /// Wraps a raw CID. Returns `None` if the CID is invalid.
    #[inline]
    #[must_use]
    pub(super) const fn new(v: u16) -> Option<Self> {
        match NonZeroU16::new(v) {
            Some(nz) => Some(Self(nz)),
            None => None,
        }
    }

    /// Returns whether the CID is valid for an LE-U logical link
    /// ([Vol 3] Part A, Section 2.1, Table 2.3).
    #[inline]
    #[must_use]
    pub(super) const fn is_le(self) -> bool {
        matches!(self.0.get(), 0x0004 | 0x0005 | 0x0006 | 0x0020..=0x003E | 0x0040..=0x007F)
    }
}

impl From<Cid> for u16 {
    #[inline]
    fn from(cid: Cid) -> Self {
        cid.0.get()
    }
}

impl Debug for Cid {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#06X})", name_of_type!(Cid), self.0.get())
    }
}

impl Display for Cid {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Channel identifier for an established LE-U logical link.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct LeCid {
    pub(super) link: LeU,
    pub(super) chan: Cid,
}

impl Debug for LeCid {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({:#05X}, {:#04X})",
            name_of_type!(LeCid),
            u16::from(self.link),
            u16::from(self.chan)
        )
    }
}

impl Display for LeCid {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
