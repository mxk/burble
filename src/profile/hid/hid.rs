//! HID-over-GATT profile ([HOGP]).
//!
//! This profile defines how a device with Bluetooth low energy wireless
//! communications can support HID services over the Bluetooth low energy
//! protocol stack using the Generic Attribute Profile. See [HID] and
//! [Usage Tables] for HID documentation.
//!
//! As of 2023-03-06, the GATT Specification Supplement does not describe HID
//! characteristics and descriptors, and the official XML-based spec has also
//! been taken offline. For now, we rely on a [mirror].
//!
//! [HOGP]: https://www.bluetooth.com/specifications/specs/hid-over-gatt-profile-1-0/
//! [HID]: https://www.usb.org/sites/default/files/hid1_11.pdf
//! [HUT]: https://www.usb.org/sites/default/files/hut1_4.pdf
//! [mirror]: https://github.com/oesmith/gatt-xml

use std::collections::VecDeque;

pub use service::*;

mod descriptor;

/// Collection of HID report descriptor items that can be converted to a byte
/// vector.
pub type ReportDescriptor = descriptor::Items;

#[path = "kbd/kbd.rs"]
pub mod kbd;
pub mod mouse;
mod service;

/// Basic HID.
pub trait Dev {
    /// Resets the device state.
    fn reset(&mut self);

    /// Returns a description of device report types and data formats.
    fn report_descriptor(&self, report_id: u8) -> ReportDescriptor;
}

/// HID that generates input reports.
pub trait InputDev: Dev {
    /// Length of the slice returned by [`Self::input()`].
    const IN_LEN: u8;

    /// Polls for the next input report and returns true if a new report is
    /// available.
    fn poll(&mut self) -> bool;

    /// Returns the current input report.
    fn input(&self) -> &[u8];
}

/// HID that receives output reports.
pub trait OutputDev: Dev {
    /// Length of the slice expected by [`Self::set_output()`].
    const OUT_LEN: u8;

    /// Sets the current output report.
    fn set_output(&mut self, v: &[u8]);

    /// Returns the current output report.
    fn output(&self) -> &[u8];
}

/// Helper type for implementing [`InputDev`].
#[derive(Debug)]
struct InputBuf<const N: usize> {
    v: [u8; N],
    b: VecDeque<[u8; N]>,
}

impl<const N: usize> InputBuf<N> {
    fn reset(&mut self) {
        self.v.fill(0);
        self.b.clear();
    }

    fn poll(&mut self) -> bool {
        if let Some(v) = self.b.pop_front() {
            self.v = v;
            return true;
        }
        if self.v.into_iter().any(|v| v != 0) {
            self.v.fill(0);
            return true;
        }
        false
    }

    const fn input(&self) -> &[u8] {
        &self.v
    }

    fn add_input(&mut self, v: [u8; N]) {
        self.b.push_back(v);
    }

    fn last_input(&self) -> [u8; N] {
        *self.b.back().unwrap_or(&self.v)
    }
}

impl<const N: usize> Default for InputBuf<N> {
    fn default() -> Self {
        Self {
            v: [0; N],
            b: VecDeque::new(),
        }
    }
}
