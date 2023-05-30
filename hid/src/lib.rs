#![doc = include_str!("../README.md")]
#![no_std]
#![warn(unused_crate_dependencies)]

extern crate alloc;

use core::fmt;

pub mod descriptor;
pub mod kbd;
pub mod mouse;
pub mod usage;

/// Common Human Interface Device API. The [`Iterator`] implementation
/// corresponds to the "Interrupt In" pipe.
pub trait Device: Iterator<Item = Report> {
    /// Returns the device report descriptor.
    #[must_use]
    fn descriptor(&self) -> descriptor::ReportDescriptor;

    /// Resets the device to its initial state.
    fn reset(&mut self);

    /// Returns the current report by type and ID, or [`None`] if the type
    /// and/or ID are invalid. Report values change only after calling
    /// [`Self::next()`] or [`Self::set_report()`].
    #[must_use]
    fn get_report(&self, typ: ReportType, id: u8) -> Option<Report>;

    /// Sets the specified report and returns whether it is valid.
    #[must_use]
    fn set_report(&mut self, r: Report) -> bool;

    /// Returns whether the device is in boot protocol mode. Default is report
    /// mode.
    #[inline(always)]
    #[must_use]
    fn is_boot_mode(&self) -> bool {
        false
    }

    /// Enables or disables boot protocol mode. Default is report mode.
    fn set_boot_mode(&mut self, boot: bool) {
        let _ = boot;
        unimplemented!("device does not support boot protocol");
    }
}

/// HID report with payload size limited to 62 bytes.
#[derive(Clone, Copy, Eq, PartialEq)]
#[repr(align(8))]
pub struct Report {
    /// Report ID, which may be 0, followed by payload. This representation is
    /// used to be compatible with USB report format.
    v: [u8; 0xFF >> 2],
    /// Report length (upper 6 bits) and type (lower 2 bits).
    n: u8,
}

impl Report {
    /// Creates a new input report with the specified ID and payload.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline(always)]
    #[must_use]
    pub fn input(id: u8, payload: &[u8]) -> Self {
        Self::new(ReportType::Input, id, payload)
    }

    /// Creates a new output report with the specified ID and payload.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline(always)]
    #[must_use]
    pub fn output(id: u8, payload: &[u8]) -> Self {
        Self::new(ReportType::Output, id, payload)
    }

    /// Creates a new feature report with the specified ID and payload.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline(always)]
    #[must_use]
    pub fn feature(id: u8, payload: &[u8]) -> Self {
        Self::new(ReportType::Feature, id, payload)
    }

    /// Creates a new report with the specified type, ID, and payload.
    ///
    /// # Panics
    ///
    /// Panics if the payload length exceeds capacity.
    #[inline]
    #[must_use]
    pub fn new(typ: ReportType, id: u8, payload: &[u8]) -> Self {
        let mut this = Self::zero(typ, id, payload.len());
        let n = this.len();
        // SAFETY: 0 < n == 1 + payload.len() <= this.v.len()
        unsafe { this.v.get_unchecked_mut(1..n).copy_from_slice(payload) };
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
    pub const fn zero(typ: ReportType, id: u8, n: usize) -> Self {
        #[allow(clippy::cast_possible_truncation)]
        let mut this = Self {
            n: (n.saturating_add(1) as u8) << 2 | typ as u8,
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
        self.v[self.len()] = b;
        self.n += 1 << 2;
        self
    }

    /// Returns the report type.
    #[inline]
    #[must_use]
    pub const fn typ(&self) -> ReportType {
        match self.n & 3 {
            1 => ReportType::Input,
            2 => ReportType::Output,
            3 => ReportType::Feature,
            _ => unreachable!(),
        }
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
        unsafe { self.v.get_unchecked(i..self.len()) }
    }

    /// Returns the number of valid bytes in `self.v`.
    #[inline(always)]
    #[must_use]
    const fn len(&self) -> usize {
        (self.n >> 2) as usize
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
        // SAFETY: 1..self.len() is always in-bounds
        unsafe { self.v.get_unchecked(1..self.len()) }
    }
}

/// HID report type (\[HID\] Section 7.2.1).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum ReportType {
    /// Device to host report.
    Input = 1,
    /// Host to device report.
    Output = 2,
    /// Host to device or device to host report.
    Feature = 3,
}

impl ReportType {
    /// Returns whether the report type is input.
    #[inline(always)]
    #[must_use]
    pub const fn is_input(self) -> bool {
        matches!(self, Self::Input)
    }

    /// Returns whether the report type is output.
    #[inline(always)]
    #[must_use]
    pub const fn is_output(self) -> bool {
        matches!(self, Self::Output)
    }

    /// Returns whether the report type is feature.
    #[inline(always)]
    #[must_use]
    pub const fn is_feature(self) -> bool {
        matches!(self, Self::Feature)
    }
}

#[cfg(test)]
mod tests {
    use core::mem;

    use super::*;

    #[test]
    fn report_size() {
        // Report should fit into a typical cache line
        assert_eq!(mem::size_of::<Report>(), 64);
        assert_eq!(mem::align_of::<Report>(), 8);
    }

    #[test]
    fn report() {
        let r = Report::input(1, &[0; 62]);
        assert_eq!(r.id(), 1);
        assert_eq!(r.as_ref(), &[0; 62]);
        let mut full = [0; 63];
        full[0] = 1;
        assert_eq!(r.prefixed(), &full);

        let mut r = Report::input(0, &[]);
        r.push(2);
        assert_eq!(r.id(), 0);
        assert_eq!(r.as_ref(), &[2]);
        assert_eq!(r.prefixed(), &[2]);
    }

    #[test]
    fn push_last() {
        let mut r = Report::zero(ReportType::Input, 0, 61);
        r.push(42);
        let mut want = [0; 62];
        want[61] = 42;
        assert_eq!(r.as_ref(), &want);
    }

    #[test]
    #[should_panic]
    fn payload_panic() {
        let _ = Report::input(0, &[0; 63]);
    }

    #[test]
    #[should_panic]
    fn push_panic() {
        let mut r = Report::zero(ReportType::Input, 0, 62);
        r.push(42);
    }
}
