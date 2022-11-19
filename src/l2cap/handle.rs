use std::fmt::{Debug, Display, Formatter};

use matches::assert_matches;
use nameof::name_of_type;

use crate::hci;

/// LE-U logical link ([Vol 1] Part A, Section 3.5.5.2.2).
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub(super) struct LeU(pub(super) hci::ConnHandle);

impl LeU {
    /// Creates an LE-U logical link from a valid HCI connection handle.
    #[inline]
    #[must_use]
    pub(super) const fn new(cn: hci::ConnHandle) -> Self {
        assert!(cn.is_valid());
        Self(cn)
    }

    /// Wraps a raw HCI connection handle.
    #[inline]
    #[must_use]
    pub(crate) const fn from_raw(cn: u16) -> Self {
        Self::new(hci::ConnHandle::from_raw(cn))
    }
}

impl From<LeU> for u16 {
    #[inline]
    fn from(link: LeU) -> Self {
        Self::from(link.0)
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
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Cid(pub(super) u16);

impl Cid {
    /// Invalid CID ([Vol 3] Part A, Section 2.1).
    pub(super) const INVALID: Self = Self(0x0000);
    /// BR/EDR signaling channel.
    pub(super) const SIGNAL: Self = Self(0x0001);
    /// Attribute protocol channel.
    pub(super) const ATT: Self = Self(0x0004);
    /// LE signaling channel.
    pub(super) const LE_SIGNAL: Self = Self(0x0005);
    /// Security Manager protocol channel.
    pub(super) const SM: Self = Self(0x0006);

    /// Wraps a raw CID.
    #[inline]
    #[must_use]
    pub(super) const fn from_raw(h: u16) -> Self {
        Self(h)
    }
}

impl From<Cid> for u16 {
    #[inline]
    fn from(h: Cid) -> Self {
        h.0
    }
}

impl Debug for Cid {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({:#06X})", name_of_type!(Cid), self.0)
    }
}

impl Display for Cid {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

/// Channel identifier for an established LE-U logical link.
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub(super) struct LeUCid {
    pub(super) link: LeU,
    pub(super) cid: Cid,
}

impl LeUCid {
    /// Associates a channel identifier with an LE-U logical link.
    #[inline]
    #[must_use]
    pub(super) fn new(link: LeU, cid: Cid) -> Self {
        // [Vol 3] Part A, Section 2.1, Table 2.3
        assert_matches!(cid.0, 0x0004 | 0x0005 | 0x0006 | 0x0020..=0x003E | 0x0040..=0x007F);
        Self { link, cid }
    }
}

impl Debug for LeUCid {
    #[allow(clippy::use_self)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({:#05X}, {:#04X})",
            name_of_type!(LeUCid),
            u16::from(self.link),
            self.cid.0
        )
    }
}

impl Display for LeUCid {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
