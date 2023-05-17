//! Mouse HID device.

use super::usage::Page;
use super::{descriptor::*, Dev, InputBuf, InputDev};

/// HID mouse.
#[derive(Debug, Default)]
pub struct Mouse(InputBuf<{ Mouse::IN_LEN as usize }>);

impl Mouse {
    /// Creates a new mouse.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: Use enum or bitflags for buttons?

    /// Performs a mouse click. `btn` is 0-7 where 0 is the primary mouse
    /// button.
    pub fn click(&mut self, btn: u8) {
        self.add_input(1 << btn, 0, 0, 0);
        self.add_input(0, 0, 0, 0);
    }

    /// Moves the mouse cursor to a relative offset.
    pub fn move_rel(&mut self, mut dx: i32, mut dy: i32) {
        while dx != 0 || dy != 0 {
            let (dx8, dy8) = (clamp8(dx), clamp8(dy));
            self.add_input(0, dx8, dy8, 0);
            dx -= i32::from(dx8);
            dy -= i32::from(dy8);
        }
    }

    /// Performs vertical scrolling.
    pub fn vscroll(&mut self, mut dv: i32) {
        while dv != 0 {
            let dv8 = clamp8(dv);
            self.add_input(0, 0, 0, dv8);
            dv -= i32::from(dv8);
        }
    }

    fn add_input(&mut self, btn: u8, dx: i8, dy: i8, dv: i8) {
        #[allow(clippy::cast_sign_loss)]
        self.0.add_input([btn, dx as u8, dy as u8, dv as u8]);
    }
}

impl Dev for Mouse {
    fn reset(&mut self) {
        self.0.reset();
    }

    #[rustfmt::skip]
    fn report_descriptor(&self, report_id: u8) -> ReportDescriptor {
        use Item::*;
        ReportDescriptor::new([
            GUsagePage(Page::GenericDesktop),
            LUsage(0x02),     // Mouse
            Collection::application([
                GReportId(report_id),

                //
                // Input
                //

                // Buttons
                GUsagePage(Page::Button),
                GReportSize(1),
                GReportCount(8),
                GLogicalMin(0),
                GLogicalMax(1),
                LUsageMin(0x01), // Button 1 (primary/trigger)
                LUsageMax(0x08), // Button 8
                MInput(Flag::VAR),

                // Movement and scrolling
                GUsagePage(Page::GenericDesktop),
                GReportSize(8),
                GReportCount(Self::IN_LEN - 1),
                GLogicalMin(i32::from(i8::MIN)), // TODO: -127?
                GLogicalMax(i32::from(i8::MAX)),
                LUsage(0x30), // X
                LUsage(0x31), // Y
                LUsage(0x38), // Wheel
                MInput(Flag::VAR | Flag::REL),
            ]),
        ])
    }
}

impl InputDev for Mouse {
    const IN_LEN: u8 = 4;

    fn poll(&mut self) -> bool {
        self.0.poll()
    }

    fn input(&self) -> &[u8] {
        self.0.input()
    }
}

#[allow(clippy::cast_possible_truncation)]
fn clamp8(v: i32) -> i8 {
    v.clamp(i32::from(i8::MIN), i32::from(i8::MAX)) as i8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dev() {
        let m = Mouse::default();
        assert!(!m.report_descriptor(1).as_ref().is_empty());
    }

    #[test]
    fn click() {
        let mut m = Mouse::new();
        m.click(0);
        assert!(m.poll());
        assert_eq!(m.input(), [1, 0, 0, 0]);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 0, 0, 0]);
        assert!(!m.poll());

        m.click(1);
        m.reset();
        assert!(!m.poll());
        assert_eq!(m.input(), [0, 0, 0, 0]);
    }

    #[allow(clippy::cast_sign_loss)]
    #[test]
    fn move_rel() {
        let mut m = Mouse::new();
        m.move_rel(128, -129);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 127, -128_i8 as u8, 0]);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 1, -1_i8 as u8, 0]);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 0, 0, 0]);
    }

    #[test]
    fn vscroll() {
        let mut m = Mouse::new();
        m.vscroll(255);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 0, 0, 127]);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 0, 0, 127]);
        assert!(m.poll());
        assert_eq!(m.input(), [0, 0, 0, 1]);
    }
}
