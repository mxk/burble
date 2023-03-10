//! Bluetooth LE assigned numbers.

#![warn(unused_crate_dependencies)]

use std::fmt::{Debug, Display, Formatter};

pub use uuid::*;

mod uuid;

/// Company identifier ([Assigned Numbers] Section 7.1).
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CompanyId(pub u16);

impl CompanyId {
    /// Returns the raw company ID.
    #[inline(always)]
    #[must_use]
    pub const fn raw(self) -> u16 {
        self.0
    }

    /// Returns the associated company name or [`None`] if the identifier is
    /// unknown.
    #[must_use]
    pub const fn name(self) -> Option<&'static str> {
        let i = match self.0.checked_sub(Self::MIN) {
            Some(0) => return Some(Self::FIRST),
            Some(i) if (i as usize) < Self::IDX.len() => i as usize,
            _ => return None,
        };
        let off = Self::IDX[i - 1] as usize;
        // SAFETY: `TAB[IDX[i - 1]..IDX[i]]` contains a valid UTF-8 string
        Some(unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                Self::TAB.as_ptr().add(off),
                Self::IDX[i] as usize - off,
            ))
        })
    }
}

impl Debug for CompanyId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or("<unknown>");
        f.debug_tuple("CompanyId")
            .field(&format_args!("{:#06X} => \"{name}\"", self.0))
            .finish()
    }
}

impl Display for CompanyId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name().unwrap_or("<unknown>"))
    }
}

impl From<CompanyId> for u16 {
    #[inline(always)]
    fn from(id: CompanyId) -> Self {
        id.raw()
    }
}

/// Generates company name look-up table. This representation saves several KB
/// in the release binary over a `match` table.
macro_rules! id_map {
    {$first:literal => $name:literal, $($id:literal => $name_:literal,)+} => {
        impl CompanyId {
            const MIN: u16 = $first;
            #[allow(clippy::no_effect)]
            const MAX: u16 = { $($id);+ };
            const FIRST: &'static str = $name;
            #[allow(clippy::no_effect)]
            #[cfg(test)]
            const LAST: &'static str = { $($name_);+ };
            const TAB: &'static [u8] = concat!($($name_),*).as_bytes();
            const IDX: [u16; Self::MAX as usize - $first + 1] = {
                let mut v = [0_u16; Self::MAX as usize - $first + 1];
                let mut i = 0;
                $(
                    i += 1;
                    assert!(i == $id - $first);
                    let (n, overflow) = v[i - 1].overflowing_add($name_.len() as u16);
                    assert!(!overflow);
                    v[i] = n;
                )+
                v
            };
        }
    };
}

include!("company_id.rs");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn company_ids() {
        assert_eq!(CompanyId(CompanyId::MIN).name(), Some(CompanyId::FIRST));
        assert_eq!(CompanyId(0x01F4).name(), Some("UTC Fire and Security"));
        assert_eq!(CompanyId(CompanyId::MAX).name(), Some(CompanyId::LAST));
        assert_eq!(CompanyId(CompanyId::MAX + 1).name(), None);
        assert_eq!(CompanyId(u16::MAX).name(), None);
    }
}
