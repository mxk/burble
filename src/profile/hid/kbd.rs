//! Keyboard HID device.

use super::{descriptor::*, usage::*, Dev, InputBuf, InputDev, OutputDev};

/// An interface for converting characters into keyboard usage codes.
pub(super) trait KbdMap {
    /// Converts a character into a [`Key`]. This is the inverse of what the
    /// host does when it receives an input report, so the maps on both sides
    /// have to match.
    fn key(&self, c: char) -> Option<Key>;
}

impl std::fmt::Debug for dyn KbdMap + Send + Sync {
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
    fn report_descriptor(&self, report_id: u8) -> Items {
        use Item::*;
        use super::usage::KeyUsage::*;
        Items::from([
            GUsagePage(0x01), // Generic Desktop Page
            LUsage(0x06),     // Keyboard
            MApplication([
                GReportID(report_id),

                //
                // Input
                //

                // Modifier flags
                GUsagePage(0x07), // Keyboard/Keypad Page
                GReportSize(1),
                GReportCount(8),
                GLogicalMin(0),
                GLogicalMax(1),
                LUsageMin(0xE0), // Keyboard LeftControl
                LUsageMax(0xE7), // Keyboard Right GUI
                MInput(Attr::VAR),

                // Keys
                GReportSize(8),
                GReportCount(Self::IN_LEN - 1),
                GLogicalMin(0x01),
                GLogicalMax(KeypadHex as i32),
                LUsageMin(0x01),
                LUsageMax(KeypadHex as u32),
                MInput(Attr::empty()),

                //
                // Output
                //

                // Indicators
                GUsagePage(0x08), // LED Page
                GReportSize(1),
                GReportCount(5),
                GLogicalMin(0),
                GLogicalMax(1),
                LUsageMin(0x01), // Num Lock
                LUsageMax(0x05), // Kana
                MOutput(Attr::VAR),

                // Padding
                GReportSize(3),
                GReportCount(1),
                MOutput(Attr::CONST),
            ].into()),
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
    #[derive(Default)]
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
    u: KeyUsage,
}

impl From<KeyUsage> for Key {
    fn from(u: KeyUsage) -> Self {
        Self {
            m: KeyMod::empty(),
            u,
        }
    }
}

impl Key {
    /// Returns a combination of left shift and key.
    #[must_use]
    pub fn shift(u: KeyUsage) -> Self {
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
        [k.m.bits, k.u.into()]
    }
}

bitflags::bitflags! {
    /// Keyboard indicator flags.
    #[derive(Default)]
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
        [v.bits]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dev() {
        let b = Kbd::default();
        assert!(!b.report_descriptor(1).to_bytes().is_empty());
    }

    #[test]
    fn input() {
        use KeyUsage::*;
        let mut b = Kbd::new();
        b.write("Hello!\n");

        let last = Key::from(KeyEnter);
        assert_eq!(b.last_key(), last);

        let want = [
            Key::shift(KeyH),
            Key::from(KeyE),
            Key::from(KeyL),
            Key::default(),
            Key::from(KeyL),
            Key::from(KeyO),
            Key::shift(Key1),
            Key::from(KeyEnter),
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
}
