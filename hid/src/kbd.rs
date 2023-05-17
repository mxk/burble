//! Keyboard HID device.

use alloc::boxed::Box;

use super::usage;
use super::usage::{GenericDesktop, Page};
use super::{descriptor::*, Dev, InputBuf, InputDev, OutputDev};

/// An interface for converting characters into keyboard usage codes.
pub(super) trait KbdMap {
    /// Converts a character into a [`Key`]. This is the inverse of what the
    /// host does when it receives an input report, so the maps on both sides
    /// have to match.
    fn key(&self, c: char) -> Option<Key>;
}

impl core::fmt::Debug for dyn KbdMap + Send + Sync {
    fn fmt(&self, _: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Ok(())
    }
}

/// Input report.
type In = [u8; Kbd::IN_LEN as usize];

/// Output report.
type Out = [u8; Kbd::OUT_LEN as usize];

/// HID keyboard.
#[derive(Debug)]
pub struct Kbd {
    map: Box<dyn KbdMap + Send + Sync>,
    inp: InputBuf<{ Kbd::IN_LEN as usize }>,
    out: Out,
}

impl Kbd {
    /// Creates a new keyboard using US keyboard map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            map: Box::new(USKbd {}),
            inp: InputBuf::default(),
            out: Out::default(),
        }
    }

    /// Adds a key press to the keyboard buffer.
    pub fn press(&mut self, k: Key) {
        if k.u == self.last_key().u {
            // Explicit key release when pressing the same key twice
            // TODO: Other conditions when this is required?
            self.inp.add_input(In::default());
        }
        self.inp.add_input(k.into());
    }

    /// Returns the last key in the keyboard buffer or the current key if the
    /// buffer is empty.
    #[must_use]
    pub fn last_key(&self) -> Key {
        Key::from(self.inp.last_input())
    }

    /// Converts string s into a sequence of key presses.
    ///
    /// # Panics
    ///
    /// Panics if `s` contains invalid characters for the current keyboard map.
    pub fn write(&mut self, s: &str) {
        for (i, c) in s.chars().enumerate() {
            self.map.key(c).map_or_else(
                || panic!("invalid key at {i} in {s:?}: {c:?}"), // TODO: Error
                |k| self.press(k),
            );
        }
    }

    /// Returns true if number lock is enabled.
    #[must_use]
    pub const fn num_lock(&self) -> bool {
        self.ind().contains(KbdInd::NUM_LOCK)
    }

    /// Returns true if caps lock is enabled.
    #[must_use]
    pub const fn caps_lock(&self) -> bool {
        self.ind().contains(KbdInd::CAPS_LOCK)
    }

    /// Returns true if scroll lock is enabled.
    #[must_use]
    pub const fn scroll_lock(&self) -> bool {
        self.ind().contains(KbdInd::SCROLL_LOCK)
    }

    const fn ind(&self) -> KbdInd {
        KbdInd::from_bits_truncate(self.out[0])
    }
}

impl Default for Kbd {
    fn default() -> Self {
        Self::new()
    }
}

impl Dev for Kbd {
    fn reset(&mut self) {
        self.inp.reset();
        self.out.fill(0);
    }

    #[rustfmt::skip]
    fn report_descriptor(&self, report_id: u8) -> ReportDescriptor {
        use crate::usage::{Key, Led};
        use Item::*;
        ReportDescriptor::new([
            GUsagePage(Page::GenericDesktop),
            LUsage(GenericDesktop::Keyboard as _),
            Collection::application([
                GReportId(report_id),

                //
                // Input
                //

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
                GReportCount(Self::IN_LEN - 1),
                GLogicalMin(Key::ErrRollOver as _),
                GLogicalMax(Key::PadHex as _),
                LUsageMin(Key::ErrRollOver as _),
                LUsageMax(Key::PadHex as _),
                MInput(Flag::empty()),

                //
                // Output
                //

                // Indicators
                GUsagePage(Page::Led),
                GReportSize(1),
                GReportCount(5),
                GLogicalMin(0),
                GLogicalMax(1),
                LUsageMin(Led::NumLock as _),
                LUsageMax(Led::Kana as _),
                MOutput(Flag::VAR),

                // Padding
                GReportSize(3),
                GReportCount(1),
                MOutput(Flag::CONST),
            ]),
        ])
    }
}

impl InputDev for Kbd {
    const IN_LEN: u8 = 2;

    fn poll(&mut self) -> bool {
        self.inp.poll()
    }

    fn input(&self) -> &[u8] {
        self.inp.input()
    }
}

impl OutputDev for Kbd {
    const OUT_LEN: u8 = 1;

    fn set_output(&mut self, out: &[u8]) {
        if out.is_empty() {
            // TODO: macOS issues zero-length writes?
            return;
        }
        self.out = Out::try_from(out).unwrap();
    }

    fn output(&self) -> &[u8] {
        &self.out
    }
}

bitflags::bitflags! {
    /// Key modifier flags.
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    pub struct KeyMod: u8 {
        /// Left Ctrl
        const LCTRL = 1 << 0;
        /// Left Shift
        const LSHIFT = 1 << 1;
        /// Left Alt
        const LALT = 1 << 2;
        /// Left GUI ("Windows key" / Command)
        const LGUI = 1 << 3;
        /// Right Ctrl
        const RCTRL = 1 << 4;
        /// Right Shift
        const RSHIFT = 1 << 5;
        /// Right Alt
        const RALT = 1 << 6;
        /// Right GUI ("Windows key" / Command)
        const RGUI = 1 << 7;
    }
}

/// A single key press with optional modifier keys.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Key {
    m: KeyMod,
    u: usage::Key,
}

impl From<usage::Key> for Key {
    fn from(u: usage::Key) -> Self {
        Self {
            m: KeyMod::empty(),
            u,
        }
    }
}

impl Key {
    /// Returns a combination of left shift and key.
    #[must_use]
    pub fn shift(u: usage::Key) -> Self {
        Self::from(u).with(KeyMod::LSHIFT)
    }

    /// Adds a modifier key to an existing key press.
    #[must_use]
    pub fn with(self, m: KeyMod) -> Self {
        Self {
            m: m | self.m,
            ..self
        }
    }

    /// Returns true if the key and modifier are empty.
    #[must_use]
    pub fn is_empty(self) -> bool {
        self == Self::default()
    }
}

impl From<In> for Key {
    fn from(v: In) -> Self {
        Self {
            m: KeyMod::from_bits_truncate(v[0]),
            u: v[1].into(),
        }
    }
}

impl From<Key> for In {
    fn from(k: Key) -> Self {
        [k.m.bits(), k.u.into()]
    }
}

bitflags::bitflags! {
    /// Keyboard indicator flags.
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    struct KbdInd: u8 {
        const NUM_LOCK = 1 << 0;
        const CAPS_LOCK = 1 << 1;
        const SCROLL_LOCK = 1 << 2;
        const COMPOSE = 1 << 3;
        const KANA = 1 << 4;
    }
}

impl From<Out> for KbdInd {
    fn from(v: Out) -> Self {
        Self::from_bits_truncate(v[0])
    }
}

impl From<KbdInd> for Out {
    fn from(v: KbdInd) -> Self {
        [v.bits()]
    }
}

/// US keyboard map.
pub(super) struct USKbd;

impl KbdMap for USKbd {
    #[allow(clippy::too_many_lines)]
    fn key(&self, c: char) -> Option<Key> {
        use crate::usage::Key::*;
        Some(match c {
            // Row 1
            '`' => Key::from(Backquote),
            '1' => Key::from(Num1),
            '2' => Key::from(Num2),
            '3' => Key::from(Num3),
            '4' => Key::from(Num4),
            '5' => Key::from(Num5),
            '6' => Key::from(Num6),
            '7' => Key::from(Num7),
            '8' => Key::from(Num8),
            '9' => Key::from(Num9),
            '0' => Key::from(Num0),
            '-' => Key::from(Minus),
            '=' => Key::from(Equals),
            '\u{08}' => Key::from(Backspace),
            '~' => Key::shift(Backquote),
            '!' => Key::shift(Num1),
            '@' => Key::shift(Num2),
            '#' => Key::shift(Num3),
            '$' => Key::shift(Num4),
            '%' => Key::shift(Num5),
            '^' => Key::shift(Num6),
            '&' => Key::shift(Num7),
            '*' => Key::shift(Num8),
            '(' => Key::shift(Num9),
            ')' => Key::shift(Num0),
            '_' => Key::shift(Minus),
            '+' => Key::shift(Equals),

            // Row 2
            '\t' => Key::from(Tab),
            'q' => Key::from(Q),
            'w' => Key::from(W),
            'e' => Key::from(E),
            'r' => Key::from(R),
            't' => Key::from(T),
            'y' => Key::from(Y),
            'u' => Key::from(U),
            'i' => Key::from(I),
            'o' => Key::from(O),
            'p' => Key::from(P),
            '[' => Key::from(LeftBracket),
            ']' => Key::from(RightBracket),
            '\\' => Key::from(Backslash),
            'Q' => Key::shift(Q),
            'W' => Key::shift(W),
            'E' => Key::shift(E),
            'R' => Key::shift(R),
            'T' => Key::shift(T),
            'Y' => Key::shift(Y),
            'U' => Key::shift(U),
            'I' => Key::shift(I),
            'O' => Key::shift(O),
            'P' => Key::shift(P),
            '{' => Key::shift(LeftBracket),
            '}' => Key::shift(RightBracket),
            '|' => Key::shift(Backslash),

            // Row 3
            'a' => Key::from(A),
            's' => Key::from(S),
            'd' => Key::from(D),
            'f' => Key::from(F),
            'g' => Key::from(G),
            'h' => Key::from(H),
            'j' => Key::from(J),
            'k' => Key::from(K),
            'l' => Key::from(L),
            ';' => Key::from(Semicolon),
            '\'' => Key::from(Quote),
            '\n' => Key::from(Enter),
            'A' => Key::shift(A),
            'S' => Key::shift(S),
            'D' => Key::shift(D),
            'F' => Key::shift(F),
            'G' => Key::shift(G),
            'H' => Key::shift(H),
            'J' => Key::shift(J),
            'K' => Key::shift(K),
            'L' => Key::shift(L),
            ':' => Key::shift(Semicolon),
            '"' => Key::shift(Quote),

            // Row 4
            'z' => Key::from(Z),
            'x' => Key::from(X),
            'c' => Key::from(C),
            'v' => Key::from(V),
            'b' => Key::from(B),
            'n' => Key::from(N),
            'm' => Key::from(M),
            ',' => Key::from(Comma),
            '.' => Key::from(Period),
            '/' => Key::from(Slash),
            'Z' => Key::shift(Z),
            'X' => Key::shift(X),
            'C' => Key::shift(C),
            'V' => Key::shift(V),
            'B' => Key::shift(B),
            'N' => Key::shift(N),
            'M' => Key::shift(M),
            '<' => Key::shift(Comma),
            '>' => Key::shift(Period),
            '?' => Key::shift(Slash),

            // Row 5
            ' ' => Key::from(Space),

            // ASCII
            '\u{007F}' => Key::from(Delete),

            _ => return Option::None,
        })
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;

    #[test]
    fn dev() {
        let b = Kbd::default();
        assert!(!b.report_descriptor(1).as_ref().is_empty());
    }

    #[test]
    fn input() {
        use usage::Key::*;
        let mut b = Kbd::new();
        b.write("Hello!\n");

        let last = Key::from(Enter);
        assert_eq!(b.last_key(), last);

        let want = [
            Key::shift(H),
            Key::from(E),
            Key::from(L),
            Key::default(),
            Key::from(L),
            Key::from(O),
            Key::shift(Num1),
            Key::from(Enter),
        ];
        for k in want {
            assert!(b.poll());
            assert_eq!(b.input(), In::from(k));
            assert_eq!(b.last_key(), last);
        }

        assert!(b.poll());
        assert_eq!(b.input(), In::default());
        assert_eq!(b.last_key(), Key::default());
        assert!(!b.poll());

        b.write("X");
        b.reset();
        assert!(!b.poll());
        assert_eq!(b.input(), In::default());

        assert!(Key::default().is_empty());
    }

    #[test]
    fn output() {
        let mut b = Kbd::new();
        assert!(!b.num_lock());
        assert!(!b.caps_lock());
        assert!(!b.scroll_lock());
        assert_eq!(b.output(), &[0]);

        b.set_output(&Out::from(KbdInd::NUM_LOCK));
        assert_eq!(KbdInd::from(b.out), KbdInd::NUM_LOCK);
        assert!(b.num_lock());
        assert!(!b.caps_lock());
        assert!(!b.scroll_lock());

        b.set_output(&Out::from(KbdInd::CAPS_LOCK | KbdInd::SCROLL_LOCK));
        assert!(!b.num_lock());
        assert!(b.caps_lock());
        assert!(b.scroll_lock());
    }

    #[test]
    #[should_panic]
    fn unmapped_char() {
        Kbd::new().write("•");
    }

    #[test]
    #[should_panic]
    fn invalid_output() {
        Kbd::new().set_output(&[0, 0]);
    }

    #[test]
    fn us_kbd() {
        let keys = vec![
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
        let kbd = USKbd {};
        for k in keys {
            assert!(kbd.key(k).is_some());
        }
        assert!(kbd.key('\u{0080}').is_none());
    }
}
