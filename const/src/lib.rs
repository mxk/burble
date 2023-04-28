#![doc = include_str!("../README.md")]
#![no_std]

use core::fmt;
use core::fmt::{Debug, Display, Formatter};

pub use uuid::*;

#[allow(clippy::decimal_literal_representation)]
#[allow(clippy::unseparated_literal_suffix)]
mod company_ids;
mod uuid;

/// Company identifier ([Assigned Numbers] Section 7.1).
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CompanyId(pub u16);

impl CompanyId {
    /// Returns the associated company name or [`None`] if the identifier is
    /// unknown.
    #[must_use]
    pub const fn name(self) -> Option<&'static str> {
        if self.0 as usize >= Self::END.len() {
            return None;
        }
        let end = Self::END[self.0 as usize] as usize;
        let off = if let Some(prev) = self.0.checked_sub(1) {
            Self::END[prev as usize] as usize
        } else {
            0
        };
        // SAFETY: `TAB[off..end]` contains a valid UTF-8 string
        Some(unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(
                Self::TAB.as_ptr().add(off),
                end - off,
            ))
        })
    }
}

impl Debug for CompanyId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = self.name().unwrap_or("<unknown>");
        f.debug_tuple("CompanyId")
            .field(&format_args!("{:#06X} => \"{name}\"", self.0))
            .finish()
    }
}

impl Display for CompanyId {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.name() {
            Some(name) => f.write_str(name),
            None => write!(f, "CompanyId({:#06X})", self.0),
        }
    }
}
