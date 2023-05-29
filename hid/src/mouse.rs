//! Software HID mouse.

use alloc::collections::VecDeque;
use core::ops::RangeInclusive;

use crate::descriptor::ReportDescriptor;
use crate::Report;

/// Software HID mouse.
///
/// Converts a sequence of button and movement inputs into binary reports. There
/// is no notion of time, so the user is responsible for polling at the
/// appropriate rate to achieve the desired input.
#[derive(Debug)]
pub struct Mouse {
    report_id: u8,
    dpi: u16,
    boot: bool,
    hold: Button,
    buf: VecDeque<Pointer>,
    inp: Pointer,
}

impl Mouse {
    /// Creates a new mouse. A `dpi` of 0 uses pixels as the unit of motion.
    #[inline]
    #[must_use]
    pub const fn new(report_id: u8, dpi: u16) -> Self {
        Self {
            report_id,
            dpi,
            boot: false,
            hold: Button::empty(),
            buf: VecDeque::new(),
            inp: Pointer::NONE,
        }
    }

    /// Enables or disables boot protocol mode. Default is report mode.
    #[inline(always)]
    pub fn set_boot_mode(&mut self, boot: bool) {
        self.boot = boot;
    }

    /// Returns the report descriptor.
    #[inline]
    #[must_use]
    pub fn descriptor(&self) -> ReportDescriptor {
        Pointer::descriptor(self.report_id, self.dpi)
    }

    /// Clicks the specified button(s). Clicked buttons that are already being
    /// held will be released.
    #[inline]
    pub fn click(&mut self, b: Button) {
        self.hold(b);
        self.release(b);
    }

    /// Holds the specified buttons until they are explicitly released.
    #[inline]
    pub fn hold(&mut self, b: Button) {
        self.hold |= b;
        self.buf.push_back(Pointer::button(self.hold));
    }

    /// Releases the specified buttons.
    #[inline]
    pub fn release(&mut self, b: Button) {
        self.hold -= b;
        self.buf.push_back(Pointer::button(self.hold));
    }

    /// Moves the mouse pointer to a relative offset.
    pub fn move_rel(&mut self, mut dx: i32, mut dy: i32) {
        let (mut cx, mut cy) = (Pointer::clamp(dx), Pointer::clamp(dy));
        // Follow the slope if either value overflows the maximum delta
        if (dx != 0 && dy != 0) && (dx != i32::from(cx) || dy != i32::from(cy)) {
            let (scale_y, n, step) = if dx.unsigned_abs() >= dy.unsigned_abs() {
                let n = f64::from(dx) / f64::from(cx);
                (true, n, f64::from(dy) / n)
            } else {
                let n = f64::from(dy) / f64::from(cy);
                (false, n, f64::from(dx) / n)
            };
            #[allow(clippy::cast_possible_truncation)]
            let (n, mut nds) = (n.trunc() as i32, 0);
            for i in 1..=n {
                #[allow(clippy::cast_possible_truncation)]
                let cs = step.mul_add(f64::from(i), f64::from(nds)).round() as _;
                if scale_y {
                    cy = cs;
                } else {
                    cx = cs;
                }
                nds -= i32::from(cs);
                self.buf.push_back(Pointer {
                    b: self.hold,
                    dx: cx,
                    dy: cy,
                });
            }
            (dx, dy) = if scale_y {
                (dx - i32::from(cx) * n, dy + nds)
            } else {
                (dx + nds, dy - i32::from(cy) * n)
            };
            (cx, cy) = (Pointer::clamp(dx), Pointer::clamp(dy));
        }
        while dx != 0 || dy != 0 {
            self.buf.push_back(Pointer {
                b: self.hold,
                dx: cx,
                dy: cy,
            });
            (dx, dy) = (dx - i32::from(cx), dy - i32::from(cy));
            (cx, cy) = (Pointer::clamp(dx), Pointer::clamp(dy));
        }
    }

    /// Returns whether a new report is available.
    #[must_use]
    pub fn poll(&mut self) -> bool {
        let Some(mut head) = self.buf.pop_front() else { return false };
        while let Some(next) = self.buf.front() {
            // Report head if it contains any new inputs
            if head != Pointer::button(self.inp.b) {
                break;
            }
            head = *next;
            self.buf.pop_front();
        }
        self.inp = head;
        true
    }

    /// Returns the current input report. The report changes when
    /// [`Self::poll()`] returns `true`.
    #[inline]
    #[must_use]
    pub fn report(&self) -> Report {
        self.inp.to_report(self.report_id, self.boot)
    }
}

impl Iterator for Mouse {
    type Item = Report;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.poll().then(|| self.report())
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.buf.len();
        (usize::from(n > 0), Some(n))
    }
}

bitflags::bitflags! {
    /// Mouse button.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Button: u8 {
        /// Primary Button. Used for object selecting, dragging, and double click
        /// activation. On macOS, this is the only button. Microsoft operating
        /// systems call this a logical left button, because it is not necessarily
        /// physically located on the left of the pointing device.
        const PRIMARY = 1 << 0;
        /// Secondary Button. Used by newer graphical user interfaces to browse
        /// object properties. Exposed by systems to applications that typically
        /// assign application-specific functionality.
        const SECONDARY = 1 << 1;
        /// Tertiary Button. Optional control. Exposed to applications, but seldom
        /// assigned functionality due to prevalence of two and one button devices.
        const TERTIARY = 1 << 2;
    }
}

/// Mouse pointer report.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Pointer {
    b: Button,
    dx: i8,
    dy: i8,
}

impl Pointer {
    const NONE: Self = Self {
        b: Button::empty(),
        dx: 0,
        dy: 0,
    };

    /// Maximum movement delta value.
    const DMAX: i32 = i8::MAX as _;

    /// Clamps `v` to the pointer movement delta range.
    #[allow(clippy::cast_possible_truncation)]
    #[inline]
    fn clamp(v: i32) -> i8 {
        v.clamp(-Self::DMAX, Self::DMAX) as _
    }

    /// Creates a pointer button input.
    #[inline]
    #[must_use]
    const fn button(b: Button) -> Self {
        Self { b, ..Self::NONE }
    }

    /// Returns the report descriptor.
    #[must_use]
    fn descriptor(id: u8, dpi: u16) -> ReportDescriptor {
        use super::descriptor::{Collection, Flag, Item, Item::*, Unit};
        use super::usage::{Button, GenericDesktop, Page};
        let logical = -Self::DMAX..=Self::DMAX;
        let (physical, exp) = to_physical(logical.clone(), dpi);
        let unit = if dpi == 0 { Unit::NONE } else { Unit::INCHES };
        ReportDescriptor::new([
            GUsagePage(Page::GenericDesktop),
            LUsage(GenericDesktop::Mouse as _),
            Collection::application([
                GReportId(id),
                LUsage(GenericDesktop::Pointer as _),
                Collection::physical([
                    // Buttons
                    GUsagePage(Page::Button),
                    GReportSize(1),
                    GReportCount(8),
                    GLogicalMin(0),
                    GLogicalMax(1),
                    LUsageMin(u32::from(Button::PRIMARY.id())),
                    LUsageMax(u32::from(Button::nth(8).unwrap().id())),
                    MInput(Flag::VAR),
                    // Movement
                    GUsagePage(Page::GenericDesktop),
                    GReportSize(8),
                    GReportCount(2),
                    GLogicalMin(*logical.start()),
                    GLogicalMax(*logical.end()),
                    GPhysicalMin(*physical.start()),
                    GPhysicalMax(*physical.end()),
                    Item::unit_exp_compat(exp),
                    GUnit(unit),
                    LUsage(GenericDesktop::X as _),
                    LUsage(GenericDesktop::Y as _),
                    MInput(Flag::VAR | Flag::REL),
                ]),
            ]),
        ])
    }

    /// Returns the binary report representation.
    #[must_use]
    pub fn to_report(self, id: u8, boot: bool) -> Report {
        let mut b = self.b.bits();
        if boot {
            b &= 0b111;
        }
        #[allow(clippy::cast_sign_loss)]
        Report::new(id, &[b, self.dx as _, self.dy as _])
    }
}

/// Converts logical min/max at the specified resolution to physical min/max and
/// unit exponent.
#[must_use]
fn to_physical(logi: RangeInclusive<i32>, res: u16) -> (RangeInclusive<i32>, i8) {
    if res == 0 {
        return (0..=0, 0);
    }
    let s = 1e8 / f64::from(res);
    let (mut i, mut j) = (f64::from(*logi.start()) * s, f64::from(*logi.end()) * s);
    let mut exp = -8;
    // Get i and j into i32 range
    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::float_cmp)]
    while exp < 0 && (f64::from(i as i32) != i.trunc() || f64::from(j as i32) != j.trunc()) {
        (i, j) = (i / 10., j / 10.);
        exp += 1;
    }
    // Remove trailing zeros
    #[allow(clippy::cast_possible_truncation)]
    let (mut i, mut j) = (i.round() as i32, j.round() as i32);
    while exp < 7 && (i % 10 == 0 && j % 10 == 0) {
        (i, j) = (i / 10, j / 10);
        exp += 1;
    }
    (i..=j, exp)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn descriptor() {
        assert!(!Mouse::new(0, 0).descriptor().as_ref().is_empty());
    }

    #[test]
    fn click() {
        let mut m = Mouse::new(1, 0);
        m.click(Button::PRIMARY);
        assert_eq!(m.next(), Some(Report::new(1, &[1, 0, 0])));
        assert_eq!(m.next(), Some(Report::zero(1, 3)));
        assert_eq!(m.next(), None);
    }

    #[allow(clippy::cast_sign_loss)]
    #[test]
    fn move_rel() {
        let mut m = Mouse::new(0, 400);
        m.move_rel(0, 0);
        assert_eq!(m.next(), None);

        m.move_rel(Pointer::DMAX, -Pointer::DMAX);
        assert_eq!(m.next(), Some(Report::new(0, &[0, 127, -127_i8 as _])));

        m.move_rel(-128, 1);
        assert_eq!(m.next(), Some(Report::new(0, &[0, -127_i8 as _, 1])));
        assert_eq!(m.next(), Some(Report::new(0, &[0, -1_i8 as _, 0])));

        m.move_rel(128, -200);
        assert_eq!(m.next(), Some(Report::new(0, &[0, 81, -127_i8 as _])));
        assert_eq!(m.next(), Some(Report::new(0, &[0, 47, -73_i8 as _])));

        m.move_rel(450, 8);
        assert_eq!(m.next(), Some(Report::new(0, &[0, 127, 2])));
        assert_eq!(m.next(), Some(Report::new(0, &[0, 127, 3])));
        assert_eq!(m.next(), Some(Report::new(0, &[0, 127, 2])));
        assert_eq!(m.next(), Some(Report::new(0, &[0, 69, 1])));
        assert_eq!(m.next(), None);

        // TODO: Should relative values be reset?
        assert_eq!(m.report(), Report::new(0, &[0, 69, 1]));
    }

    #[test]
    fn physical_conv() {
        // [HID] Section 6.2.2.7
        assert_eq!(to_physical(-127..=127, 400), (-3175..=3175, -4));
        assert_eq!(to_physical(-127..=127, 127), (-1..=1, 0));
        assert_eq!(to_physical(-127..=127, 1), (-127..=127, 0));
        assert_eq!(to_physical(0..=1, 512), (0..=195_313, -8));
    }
}
