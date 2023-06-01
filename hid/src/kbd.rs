//! Software HID keyboard.

use alloc::boxed::Box;
use alloc::collections::VecDeque;
use core::fmt::Debug;
use core::{fmt, ops};

use crate::descriptor::{Locale, ReportDescriptor};
use crate::usage::Key;
use crate::{Device, Report, ReportType};

/// Error indicating that a char cannot be typed in the current keyboard map.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct KeyMapError(char);

impl fmt::Display for KeyMapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid char for keyboard map: {:?}", self.0)
    }
}

/// Software HID keyboard.
///
/// Converts a sequence of key inputs into binary reports. There is no notion of
/// time, so the user is responsible for polling at the appropriate rate to
/// achieve the desired input.
#[derive(Debug)]
pub struct Keyboard {
    report_id: u8,
    boot: bool,
    map: Box<dyn KeyMap + Send + Sync>,
    hold: Keys,
    buf: VecDeque<Keys>,
    inp: Keys,
    led: Led,
}

impl Keyboard {
    /// Creates a new keyboard using US keyboard map.
    #[inline]
    #[must_use]
    pub fn us(report_id: u8) -> Self {
        Self {
            report_id,
            boot: false,
            map: Box::new(US),
            hold: Keys::NONE,
            buf: VecDeque::new(),
            inp: Keys::NONE,
            led: Led::empty(),
        }
    }

    /// Returns the keyboard report ID.
    #[inline(always)]
    #[must_use]
    pub const fn report_id(&self) -> u8 {
        self.report_id
    }

    /// Sets the keyboard map. Any buffered inputs are not affected.
    #[inline(always)]
    pub fn set_map(&mut self, map: Box<dyn KeyMap + Send + Sync>) {
        self.map = map;
    }

    /// Presses and releases the specified key(s). Pressed keys that are already
    /// being held will be released.
    #[inline]
    pub fn press(&mut self, ks: impl Into<Keys>) {
        let ks = ks.into();
        self.hold(ks);
        self.release(ks);
    }

    /// Holds the specified keys until they are explicitly released.
    #[inline]
    pub fn hold(&mut self, ks: impl Into<Keys>) {
        self.hold += ks.into();
        self.buf.push_back(self.hold);
    }

    /// Releases the specified keys.
    #[inline]
    pub fn release(&mut self, ks: impl Into<Keys>) {
        self.hold -= ks.into();
        self.buf.push_back(self.hold);
    }

    /// Converts string `s` into a sequence of key presses. The keyboard state
    /// is unaffected if an error is returned.
    pub fn write(&mut self, s: &str) -> Result<(), KeyMapError> {
        let hold = self.hold;
        let n = self.buf.len();
        for c in s.chars() {
            let Some(ks) = self.map.map(c) else {
                self.hold = hold;
                self.buf.truncate(n);
                return Err(KeyMapError(c));
            };
            self.press(ks);
        }
        Ok(())
    }

    /// Returns keyboard LED flags.
    #[inline(always)]
    #[must_use]
    pub const fn led(&self) -> Led {
        self.led
    }
}

impl Device for Keyboard {
    /// Returns the `bcdHID` and `bCountryCode` from the HID descriptor.
    #[inline(always)]
    #[must_use]
    fn hid_descriptor(&self) -> (u16, Locale) {
        (super::BCD_HID, self.map.locale())
    }

    fn report_descriptor(&self) -> ReportDescriptor {
        use super::descriptor::{Collection, Item::*};
        use super::usage::{GenericDesktop, Page};
        ReportDescriptor::new([
            GUsagePage(Page::GenericDesktop),
            LUsage(GenericDesktop::Keyboard as _),
            Collection::application(
                [
                    &[GReportId(self.report_id)],
                    Keys::DESCRIPTOR,
                    Led::DESCRIPTOR,
                ]
                .concat(),
            ),
        ])
    }

    fn reset(&mut self) {
        self.boot = false;
        self.hold = Keys::NONE;
        self.buf.clear();
        self.inp = Keys::NONE;
        self.led = Led::empty();
    }

    fn get_report(&self, typ: ReportType, id: u8) -> Option<Report> {
        if id != self.report_id {
            return None;
        }
        Some(match typ {
            ReportType::Input => self.inp.to_report(id, self.boot),
            ReportType::Output => self.led.to_report(id),
            ReportType::Feature => return None,
        })
    }

    fn set_report(&mut self, r: Report) -> bool {
        if r.typ().is_output() && r.id() == self.report_id {
            if let &[v] = r.as_ref() {
                self.led = Led::from_bits_retain(v);
                return true;
            }
        }
        false
    }

    #[inline(always)]
    fn boot_mode(&self) -> Option<bool> {
        Some(self.boot)
    }

    #[inline(always)]
    fn set_boot_mode(&mut self, boot: bool) {
        self.boot = boot;
    }
}

impl Iterator for Keyboard {
    type Item = Report;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(mut head) = self.buf.pop_front() else { return None };
        while let Some(next) = self.buf.front() {
            // Report head if it contains any new inputs
            if !(head - self.inp).is_empty() {
                break;
            }
            // Report head if skipping it would change key releases
            if self.inp - *next != (self.inp - head) + (head - *next) {
                break;
            }
            head = *next;
            self.buf.pop_front();
        }
        self.inp = head;
        Some(self.inp.to_report(self.report_id, self.boot))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.buf.len();
        (usize::from(n > 0), Some(n))
    }
}

bitflags::bitflags! {
    /// Keyboard LED flags.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Led: u8 {
        const NUM_LOCK = 1 << 0;
        const CAPS_LOCK = 1 << 1;
        const SCROLL_LOCK = 1 << 2;
        const COMPOSE = 1 << 3;
        const KANA = 1 << 4;
    }
}

impl Led {
    /// Output report descriptor items.
    const DESCRIPTOR: &[super::descriptor::Item] = {
        use super::descriptor::{Flag, Item::*};
        use super::usage::{Led, Page};
        &[
            // LEDs
            GUsagePage(Page::Led),
            GReportSize(1),
            GReportCount(5),
            GLogicalMin(0),
            GLogicalMax(1),
            LUsageMin(Led::NumLock as _),
            LUsageMax(Led::Kana as _),
            MOutput(Flag::VAR),
            // LED padding
            GReportSize(3),
            GReportCount(1),
            MOutput(Flag::CONST),
        ]
    };

    /// Returns the binary report representation.
    #[must_use]
    fn to_report(self, id: u8) -> Report {
        Report::output(id, &[self.bits()])
    }

    /// Returns whether number lock is enabled.
    #[inline(always)]
    #[must_use]
    pub const fn num_lock(self) -> bool {
        self.contains(Self::NUM_LOCK)
    }

    /// Returns whether caps lock is enabled.
    #[inline(always)]
    #[must_use]
    pub const fn caps_lock(self) -> bool {
        self.contains(Self::CAPS_LOCK)
    }

    /// Returns whether scroll lock is enabled.
    #[inline(always)]
    #[must_use]
    pub const fn scroll_lock(self) -> bool {
        self.contains(Self::SCROLL_LOCK)
    }
}

bitflags::bitflags! {
    /// Key modifier flags.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Mod: u8 {
        /// Left Ctrl.
        const LCTRL = 1 << 0;
        /// Left Shift.
        const LSHIFT = 1 << 1;
        /// Left Alt.
        const LALT = 1 << 2;
        /// Left GUI (Windows / Meta key).
        const LGUI = 1 << 3;
        /// Right Ctrl.
        const RCTRL = 1 << 4;
        /// Right Shift.
        const RSHIFT = 1 << 5;
        /// Right Alt.
        const RALT = 1 << 6;
        /// Right GUI (Windows / Meta key).
        const RGUI = 1 << 7;
    }
}

impl ops::Add<Key> for Mod {
    type Output = Keys;

    #[inline]
    fn add(self, rhs: Key) -> Self::Output {
        let mut ks = Keys::from(self);
        ks.k[0] = rhs;
        ks
    }
}

impl ops::Add for Key {
    type Output = Keys;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        Keys::from(self) + rhs
    }
}

/// Keyboard report consisting of an unordered set of keys that can be pressed
/// simultaneously.
#[derive(Clone, Copy, Debug, Eq)]
pub struct Keys {
    m: Mod,
    k: [Key; Self::BOOT_KEYS],
}

impl Keys {
    const NONE: Self = Self {
        m: Mod::empty(),
        k: [Key::None; Self::BOOT_KEYS],
    };

    /// Maximum number of concurrent key presses in boot mode.
    const BOOT_KEYS: usize = 6;

    /// Input report descriptor items.
    const DESCRIPTOR: &[super::descriptor::Item] = {
        use super::descriptor::{Flag, Item::*};
        use super::usage::Page;
        #[allow(clippy::cast_possible_truncation)]
        &[
            // Modifier flags
            GUsagePage(Page::Key),
            GReportSize(1),
            GReportCount(8),
            GLogicalMin(0),
            GLogicalMax(1),
            LUsageMin(Key::LeftCtrl as _),
            LUsageMax(Key::RightGui as _),
            MInput(Flag::VAR),
            // Keys
            GReportSize(8),
            GReportCount(Self::BOOT_KEYS as _),
            GLogicalMin(Key::ErrRollOver as _),
            GLogicalMax(Key::PadHex as _),
            LUsageMin(Key::ErrRollOver as _),
            LUsageMax(Key::PadHex as _),
            MInput(Flag::empty()),
        ]
    };

    /// Returns the binary report representation.
    #[must_use]
    fn to_report(self, id: u8, boot: bool) -> Report {
        let mut r = Report::input(id, &[self.m.bits()]);
        if boot {
            r.push(0); // Reserved byte ([HID] Appendix B.1)
        }
        for k in self.k {
            r.push(k as _);
        }
        r
    }

    /// Returns whether no keys are pressed.
    #[inline]
    #[must_use]
    fn is_empty(self) -> bool {
        self.m == Mod::empty() && self.k[0].is_none()
    }

    /// Returns the number of pressed non-modifier keys.
    #[inline]
    #[must_use]
    fn len(self) -> usize {
        (self.k.iter().position(|&k| k.is_none())).unwrap_or(self.k.len())
    }

    /// Returns whether the specified key is pressed. Returns `false` for
    /// [`Key::None`].
    #[must_use]
    fn contains(self, want: Key) -> bool {
        for k in self.k {
            match k {
                Key::None => break,
                _ if k == want => return true,
                _ => {}
            }
        }
        false
    }
}

impl PartialEq for Keys {
    fn eq(&self, other: &Self) -> bool {
        if self.m != other.m {
            return false;
        }
        for (i, &k) in self.k.iter().enumerate() {
            if !other.contains(k) {
                return other.k[i] == k; // k is None?
            }
        }
        true
    }
}

impl From<Key> for Keys {
    #[inline]
    fn from(k: Key) -> Self {
        let mut this = Self::NONE;
        this.k[0] = k;
        this
    }
}

impl From<Mod> for Keys {
    #[inline]
    fn from(m: Mod) -> Self {
        Self { m, ..Self::NONE }
    }
}

impl ops::Add for Keys {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        let mut i = self.len();
        for k in rhs.k {
            if k.is_none() {
                break;
            }
            if !self.contains(k) {
                // Panic instead of reporting ErrRollOver. We'd need to track
                // the state of every defined key to transition back into a
                // non-error state. A software keyboard shouldn't need so many
                // concurrent key presses.
                self.k[i] = k;
                i += 1;
            }
        }
        self.m |= rhs.m;
        self
    }
}

impl ops::Add<Key> for Keys {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Key) -> Self::Output {
        self + Self::from(rhs)
    }
}

impl ops::Sub for Keys {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        let (mut i, mut keep) = (0, [Key::None; Self::BOOT_KEYS]);
        for k in self.k {
            if k.is_none() {
                break;
            }
            if !rhs.contains(k) {
                keep[i] = k;
                i += 1;
            }
        }
        self.m -= rhs.m;
        self.k = keep;
        self
    }
}

impl ops::Sub<Key> for Keys {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Key) -> Self::Output {
        self - Self::from(rhs)
    }
}

impl ops::AddAssign for Keys {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl ops::AddAssign<Key> for Keys {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Key) {
        *self = *self + Self::from(rhs);
    }
}

impl ops::SubAssign for Keys {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl ops::SubAssign<Key> for Keys {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Key) {
        *self = *self - Self::from(rhs);
    }
}

/// A keyboard map of characters to key inputs.
pub trait KeyMap: Debug {
    /// Returns the keyboard map's locale.
    #[must_use]
    fn locale(&self) -> Locale;

    /// Returns the key input that represents character `c` or [`None`] if `c`
    /// does not have a valid representation in this keyboard map. This is the
    /// inverse of what the host does when it receives a keyboard input report,
    /// so the maps on both sides must match.
    #[must_use]
    fn map(&self, c: char) -> Option<Keys>;
}

/// US keyboard map.
#[derive(Debug)]
struct US;

impl KeyMap for US {
    #[inline(always)]
    fn locale(&self) -> Locale {
        Locale::Us
    }

    #[allow(clippy::too_many_lines)]
    fn map(&self, c: char) -> Option<Keys> {
        use Key::*;
        Some(match c {
            // Row 1
            '`' => Backquote.into(),
            '1' => Num1.into(),
            '2' => Num2.into(),
            '3' => Num3.into(),
            '4' => Num4.into(),
            '5' => Num5.into(),
            '6' => Num6.into(),
            '7' => Num7.into(),
            '8' => Num8.into(),
            '9' => Num9.into(),
            '0' => Num0.into(),
            '-' => Minus.into(),
            '=' => Equals.into(),
            '\u{08}' => Backspace.into(),
            '~' => Mod::LSHIFT + Backquote,
            '!' => Mod::LSHIFT + Num1,
            '@' => Mod::LSHIFT + Num2,
            '#' => Mod::LSHIFT + Num3,
            '$' => Mod::LSHIFT + Num4,
            '%' => Mod::LSHIFT + Num5,
            '^' => Mod::LSHIFT + Num6,
            '&' => Mod::LSHIFT + Num7,
            '*' => Mod::LSHIFT + Num8,
            '(' => Mod::LSHIFT + Num9,
            ')' => Mod::LSHIFT + Num0,
            '_' => Mod::LSHIFT + Minus,
            '+' => Mod::LSHIFT + Equals,

            // Row 2
            '\t' => Tab.into(),
            'q' => Q.into(),
            'w' => W.into(),
            'e' => E.into(),
            'r' => R.into(),
            't' => T.into(),
            'y' => Y.into(),
            'u' => U.into(),
            'i' => I.into(),
            'o' => O.into(),
            'p' => P.into(),
            '[' => LeftBracket.into(),
            ']' => RightBracket.into(),
            '\\' => Backslash.into(),
            'Q' => Mod::LSHIFT + Q,
            'W' => Mod::LSHIFT + W,
            'E' => Mod::LSHIFT + E,
            'R' => Mod::LSHIFT + R,
            'T' => Mod::LSHIFT + T,
            'Y' => Mod::LSHIFT + Y,
            'U' => Mod::LSHIFT + U,
            'I' => Mod::LSHIFT + I,
            'O' => Mod::LSHIFT + O,
            'P' => Mod::LSHIFT + P,
            '{' => Mod::LSHIFT + LeftBracket,
            '}' => Mod::LSHIFT + RightBracket,
            '|' => Mod::LSHIFT + Backslash,

            // Row 3
            'a' => A.into(),
            's' => S.into(),
            'd' => D.into(),
            'f' => F.into(),
            'g' => G.into(),
            'h' => H.into(),
            'j' => J.into(),
            'k' => K.into(),
            'l' => L.into(),
            ';' => Semicolon.into(),
            '\'' => Quote.into(),
            '\n' => Enter.into(),
            'A' => Mod::LSHIFT + A,
            'S' => Mod::LSHIFT + S,
            'D' => Mod::LSHIFT + D,
            'F' => Mod::LSHIFT + F,
            'G' => Mod::LSHIFT + G,
            'H' => Mod::LSHIFT + H,
            'J' => Mod::LSHIFT + J,
            'K' => Mod::LSHIFT + K,
            'L' => Mod::LSHIFT + L,
            ':' => Mod::LSHIFT + Semicolon,
            '"' => Mod::LSHIFT + Quote,

            // Row 4
            'z' => Z.into(),
            'x' => X.into(),
            'c' => C.into(),
            'v' => V.into(),
            'b' => B.into(),
            'n' => N.into(),
            'm' => M.into(),
            ',' => Comma.into(),
            '.' => Period.into(),
            '/' => Slash.into(),
            'Z' => Mod::LSHIFT + Z,
            'X' => Mod::LSHIFT + X,
            'C' => Mod::LSHIFT + C,
            'V' => Mod::LSHIFT + V,
            'B' => Mod::LSHIFT + B,
            'N' => Mod::LSHIFT + N,
            'M' => Mod::LSHIFT + M,
            '<' => Mod::LSHIFT + Comma,
            '>' => Mod::LSHIFT + Period,
            '?' => Mod::LSHIFT + Slash,

            // Row 5
            ' ' => Space.into(),

            // ASCII
            '\u{007F}' => Delete.into(),

            _ => return Option::None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn report_descriptor() {
        assert!(!Keyboard::us(0).report_descriptor().as_ref().is_empty());
    }

    #[test]
    fn input() {
        use Key::*;
        let mut b = Keyboard::us(0);

        b.press(Mod::LSHIFT + X);
        assert_eq!(
            b.next(),
            Some(Report::input(
                0,
                &[Mod::LSHIFT.bits(), X as _, 0, 0, 0, 0, 0]
            ))
        );
        assert_eq!(
            b.next(),
            Some(Report::zero(ReportType::Input, 0, 1 + Keys::BOOT_KEYS))
        );

        b.write("Hello!\n").unwrap();
        let want = [
            Mod::LSHIFT + H,
            E.into(),
            L.into(),
            Keys::NONE,
            L.into(),
            O.into(),
            Mod::LSHIFT + Num1,
            Enter.into(),
            Keys::NONE,
        ];
        for k in want {
            assert_eq!(b.next(), Some(k.to_report(0, false)));
        }
        assert_eq!(b.next(), Option::None);
        assert_eq!(
            b.get_report(ReportType::Input, 0),
            Some(Report::zero(ReportType::Input, 0, 1 + Keys::BOOT_KEYS))
        );
    }

    #[test]
    fn output() {
        let mut b = Keyboard::us(0);
        for _ in 0..2 {
            assert!(!b.led().num_lock());
            assert!(!b.led().caps_lock());
            assert!(!b.led().scroll_lock());
            assert!(!b.set_report(Report::output(0, &[])));
        }

        assert!(b.set_report(Report::output(0, &[Led::NUM_LOCK.bits()])));
        assert!(b.led().num_lock());
        assert!(!b.led().caps_lock());
        assert!(!b.led().scroll_lock());

        assert!(b.set_report(Report::output(
            0,
            &[Led::CAPS_LOCK.union(Led::SCROLL_LOCK).bits()],
        )));
        assert!(!b.led().num_lock());
        assert!(b.led().caps_lock());
        assert!(b.led().scroll_lock());

        assert!(!b.set_report(Report::output(0, &[0, 0])));
        assert!(!b.set_report(Report::output(1, &[0])));
        assert!(!b.set_report(Report::input(0, &[0])));
    }

    #[test]
    fn unmapped_char() {
        assert_eq!(Keyboard::us(0).write("•"), Err(KeyMapError('•')));
    }

    #[test]
    fn keys() {
        use Key::*;
        let a = Mod::LSHIFT + A + B;
        let b = Mod::LSHIFT + B + A;

        assert_eq!(a, b);
        assert_ne!(a.k, b.k);
        assert_ne!(a + C, b);
        assert_ne!(a, b + C);

        assert_eq!(a - b, Keys::NONE);
        assert_eq!(a - (b - A + C), Keys::from(A));
        assert_eq!(a + B + A, b);

        assert_eq!((A + B + C + D + E + F) - (G + A + F), B + C + D + E);
    }

    #[test]
    #[should_panic]
    fn err_roll_over() {
        use Key::*;
        let _ = A + B + C + D + E + F + G;
    }

    #[test]
    fn us_map() {
        let keys = [
            '`', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\u{08}', '~', '!',
            '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', // Row 1
            '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\\', 'Q', 'W', 'E',
            'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '|', // Row 2
            'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '\n', 'A', 'S', 'D', 'F', 'G',
            'H', 'J', 'K', 'L', ':', '"', // Row 3
            'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 'Z', 'X', 'C', 'V', 'B', 'N', 'M',
            '<', '>', '?',        // Row 4
            ' ',        // Row 5
            '\u{007F}', // ASCII
        ];
        let kbd = US {};
        for k in keys {
            assert!(kbd.map(k).is_some());
        }
        assert!(kbd.map('\u{0080}').is_none());
    }
}
