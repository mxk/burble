#![doc = include_str!("../README.md")]
#![no_std]
#![warn(unused_crate_dependencies)]

extern crate alloc;

use core::fmt;

pub mod descriptor;
pub mod kbd;
pub mod mouse;
pub mod usage;

/// HID report with payload size limited to 62 bytes.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Report {
    /// Number of valid bytes in v.
    n: u8,
    /// Report ID, which may be 0, followed by payload. This representation is
    /// used to be compatible with USB report format.
    v: [u8; 63],
}

impl Report {
    /// Creates a new report with the specified ID and payload.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline]
    #[must_use]
    pub fn new(id: u8, payload: &[u8]) -> Self {
        let mut this = Self::zero(id, payload.len());
        this.v[1..usize::from(this.n)].copy_from_slice(payload);
        this
    }

    /// Creates a new report with the specified ID and a zero-initialized
    /// payload of length `n`.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline]
    #[must_use]
    pub const fn zero(id: u8, n: usize) -> Self {
        #[allow(clippy::cast_possible_truncation)]
        let mut this = Self {
            n: n.wrapping_add(1) as u8,
            v: [0; 63],
        };
        assert!(n < this.v.len(), "payload overflow");
        this.v[0] = id;
        this
    }

    /// Appends a byte to the report.
    ///
    /// # Panics
    ///
    /// Panics if the payload capacity is exceeded.
    #[inline(always)]
    pub fn push(&mut self, b: u8) -> &mut Self {
        self.v[usize::from(self.n)] = b;
        self.n += 1;
        self
    }

    /// Returns the report ID.
    #[inline(always)]
    #[must_use]
    pub const fn id(&self) -> u8 {
        self.v[0]
    }

    /// Returns the report prefixed by the ID if the ID is non-zero.
    #[inline]
    #[must_use]
    pub fn prefixed(&self) -> &[u8] {
        let i = usize::from(self.v[0] == 0);
        // SAFETY: i..n is always in-bounds
        unsafe { self.v.get_unchecked(i..usize::from(self.n)) }
    }
}

impl fmt::Debug for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Report")
            .field("id", &self.id())
            .field("payload", &self.as_ref())
            .finish()
    }
}

impl AsRef<[u8]> for Report {
    /// Returns the report payload without the ID prefix.
    #[inline]
    #[must_use]
    fn as_ref(&self) -> &[u8] {
        // SAFETY: 1..n is always in-bounds
        unsafe { self.v.get_unchecked(1..usize::from(self.n)) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn report() {
        let r = Report::new(1, &[0; 62]);
        assert_eq!(r.id(), 1);
        assert_eq!(r.as_ref(), &[0; 62]);
        let mut full = [0; 63];
        full[0] = 1;
        assert_eq!(r.prefixed(), &full);

        let mut r = Report::new(0, &[]);
        r.push(2);
        assert_eq!(r.id(), 0);
        assert_eq!(r.as_ref(), &[2]);
        assert_eq!(r.prefixed(), &[2]);
    }

    #[test]
    #[should_panic]
    fn report_too_big() {
        let _ = Report::new(0, &[0; 63]);
    }
}
