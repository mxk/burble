use super::kbd::{KbdMap, Key};

/// Usage page IDs (\[HUT\] Section 3).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u16)]
pub enum Page {
    GenericDesktop = 0x01,
    SimulationControls = 0x02,
    VrControls = 0x03,
    SportControls = 0x04,
    GameControls = 0x05,
    GenericDeviceControls = 0x06,
    Key = 0x07,
    Led = 0x08,
    Button = 0x09,
    Ordinal = 0x0A,
    TelephonyDevice = 0x0B,
    Consumer = 0x0C,
    Digitizers = 0x0D,
    Haptics = 0x0E,
    PhysicalInputDevice = 0x0F,
    Unicode = 0x10,
    SoC = 0x11,
    EyeAndHeadTrackers = 0x12,
    AuxiliaryDisplay = 0x14,
    Sensors = 0x20,
    MedicalInstrument = 0x40,
    BrailleDisplay = 0x41,
    LightingAndIllumination = 0x59,
    Monitor = 0x80,
    MonitorEnumerated = 0x81,
    VesaVirtualControls = 0x82,
    Power = 0x84,
    BatterySystem = 0x85,
    BarcodeScanner = 0x8C,
    Scales = 0x8D,
    MagneticStripeReader = 0x8E,
    CameraControl = 0x90,
    Arcade = 0x91,
    GamingDevice = 0x92,
    FidoAlliance = 0xF1D0,
}

/// Generic Desktop page (\[HUT\] Section 4).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u16)]
pub enum GenericDesktop {
    Pointer = 0x01,
    Mouse = 0x02,
    Joystick = 0x04,
    Gamepad = 0x05,
    Keyboard = 0x06,
    Keypad = 0x07,
    MultiAxisController = 0x08,
    TabletPcSystemControls = 0x09,
    WaterCoolingDevice = 0x0A,
    ComputerChassisDevice = 0x0B,
    WirelessRadioControls = 0x0C,
    PortableDeviceControl = 0x0D,
    SystemMultiAxisController = 0x0E,
    SpatialController = 0x0F,
    AssistiveControl = 0x10,
    DeviceDock = 0x11,
    DockableDevice = 0x12,
    CallStateManagementControl = 0x13,
    X = 0x30,
    Y = 0x31,
    Z = 0x32,
    Rx = 0x33,
    Ry = 0x34,
    Rz = 0x35,
    Slider = 0x36,
    Dial = 0x37,
    Wheel = 0x38,
    HatSwitch = 0x39,
    CountedBuffer = 0x3A,
    ByteCount = 0x3B,
    MotionWakeup = 0x3C,
    Start = 0x3D,
    Select = 0x3E,
    Vx = 0x40,
    Vy = 0x41,
    Vz = 0x42,
    Vbrx = 0x43,
    Vbry = 0x44,
    Vbrz = 0x45,
    Vno = 0x46,
    FeatureNotification = 0x47,
    ResolutionMultiplier = 0x48,
    Qx = 0x49,
    Qy = 0x4A,
    Qz = 0x4B,
    Qw = 0x4C,
    SystemControl = 0x80,
    SystemPowerDown = 0x81,
    SystemSleep = 0x82,
    SystemWakeUp = 0x83,
    SystemContextMenu = 0x84,
    SystemMainMenu = 0x85,
    SystemAppMenu = 0x86,
    SystemMenuHelp = 0x87,
    SystemMenuExit = 0x88,
    SystemMenuSelect = 0x89,
    SystemMenuRight = 0x8A,
    SystemMenuLeft = 0x8B,
    SystemMenuUp = 0x8C,
    SystemMenuDown = 0x8D,
    SystemColdRestart = 0x8E,
    SystemWarmRestart = 0x8F,
    DPadUp = 0x90,
    DPadDown = 0x91,
    DPadRight = 0x92,
    DPadLeft = 0x93,
    IndexTrigger = 0x94,
    PalmTrigger = 0x95,
    Thumbstick = 0x96,
    SystemFunctionShift = 0x97,
    SystemFunctionShiftLock = 0x98,
    SystemFunctionShiftLockIndicator = 0x99,
    SystemDismissNotification = 0x9A,
    SystemDoNotDisturb = 0x9B,
    SystemDock = 0xA0,
    SystemUndock = 0xA1,
    SystemSetup = 0xA2,
    SystemBreak = 0xA3,
    SystemDebuggerBreak = 0xA4,
    ApplicationBreak = 0xA5,
    ApplicationDebuggerBreak = 0xA6,
    SystemSpeakerMute = 0xA7,
    SystemHibernate = 0xA8,
    SystemMicrophoneMute = 0xA9,
    SystemDisplayInvert = 0xB0,
    SystemDisplayInternal = 0xB1,
    SystemDisplayExternal = 0xB2,
    SystemDisplayBoth = 0xB3,
    SystemDisplayDual = 0xB4,
    SystemDisplayToggleIntExtMode = 0xB5,
    SystemDisplaySwapPrimarySecondary = 0xB6,
    SystemDisplayToggleLcdAutoscale = 0xB7,
    SensorZone = 0xC0,
    Rpm = 0xC1,
    CoolantLevel = 0xC2,
    CoolantCriticalLevel = 0xC3,
    CoolantPump = 0xC4,
    ChassisEnclosure = 0xC5,
    WirelessRadioButton = 0xC6,
    WirelessRadioLed = 0xC7,
    WirelessRadioSliderSwitch = 0xC8,
    SystemDisplayRotationLockButton = 0xC9,
    SystemDisplayRotationLockSliderSwitch = 0xCA,
    ControlEnable = 0xCB,
    DockableDeviceUniqueId = 0xD0,
    DockableDeviceVendorId = 0xD1,
    DockableDevicePrimaryUsagePage = 0xD2,
    DockableDevicePrimaryUsageId = 0xD3,
    DockableDeviceDockingState = 0xD4,
    DockableDeviceDisplayOcclusion = 0xD5,
    DockableDeviceObjectType = 0xD6,
    CallActiveLed = 0xE0,
    CallMuteToggle = 0xE1,
    CallMuteLed = 0xE2,
}

/// Keyboard/keypad usage page (\[HUT\] Section 10).
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::FromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum KeyUsage {
    #[default]
    /// No key
    KeyNone = 0x00,

    /// Keyboard a and A
    KeyA = 0x04,
    /// Keyboard b and B
    KeyB = 0x05,
    /// Keyboard c and C
    KeyC = 0x06,
    /// Keyboard d and D
    KeyD = 0x07,
    /// Keyboard e and E
    KeyE = 0x08,
    /// Keyboard f and F
    KeyF = 0x09,
    /// Keyboard g and G
    KeyG = 0x0A,
    /// Keyboard h and H
    KeyH = 0x0B,
    /// Keyboard i and I
    KeyI = 0x0C,
    /// Keyboard j and J
    KeyJ = 0x0D,
    /// Keyboard k and K
    KeyK = 0x0E,
    /// Keyboard l and L
    KeyL = 0x0F,
    /// Keyboard m and M
    KeyM = 0x10,
    /// Keyboard n and N
    KeyN = 0x11,
    /// Keyboard o and O
    KeyO = 0x12,
    /// Keyboard p and P
    KeyP = 0x13,
    /// Keyboard q and Q
    KeyQ = 0x14,
    /// Keyboard r and R
    KeyR = 0x15,
    /// Keyboard s and S
    KeyS = 0x16,
    /// Keyboard t and T
    KeyT = 0x17,
    /// Keyboard u and U
    KeyU = 0x18,
    /// Keyboard v and V
    KeyV = 0x19,
    /// Keyboard w and W
    KeyW = 0x1A,
    /// Keyboard x and X
    KeyX = 0x1B,
    /// Keyboard y and Y
    KeyY = 0x1C,
    /// Keyboard z and Z
    KeyZ = 0x1D,
    /// Keyboard 1 and !
    Key1 = 0x1E,
    /// Keyboard 2 and @
    Key2 = 0x1F,
    /// Keyboard 3 and #
    Key3 = 0x20,
    /// Keyboard 4 and $
    Key4 = 0x21,
    /// Keyboard 5 and %
    Key5 = 0x22,
    /// Keyboard 6 and ^
    Key6 = 0x23,
    /// Keyboard 7 and &
    Key7 = 0x24,
    /// Keyboard 8 and *
    Key8 = 0x25,
    /// Keyboard 9 and (
    Key9 = 0x26,
    /// Keyboard 0 and )
    Key0 = 0x27,
    /// Keyboard Return (ENTER)
    KeyEnter = 0x28,
    /// Keyboard ESCAPE
    KeyEsc = 0x29,
    /// Keyboard DELETE (Backspace)
    KeyBackspace = 0x2A,
    /// Keyboard Tab
    KeyTab = 0x2B,
    /// Keyboard Spacebar
    KeySpace = 0x2C,
    /// Keyboard - and _
    KeyMinus = 0x2D,
    /// Keyboard = and +
    KeyEqual = 0x2E,
    /// Keyboard [ and {
    KeyLeftBracket = 0x2F,
    /// Keyboard ] and }
    KeyRightBracket = 0x30,
    /// Keyboard \ and |
    KeyBackslash = 0x31,
    /// Keyboard ; and :
    KeySemicolon = 0x33,
    /// Keyboard ' and "
    KeyQuote = 0x34,
    /// Keyboard ` and ~
    KeyBackquote = 0x35,
    /// Keyboard , and <
    KeyComma = 0x36,
    /// Keyboard . and >
    KeyPeriod = 0x37,
    /// Keyboard / and ?
    KeySlash = 0x38,
    /// Keyboard Caps Lock
    KeyCapsLock = 0x39,
    /// Keyboard F1
    KeyF1 = 0x3A,
    /// Keyboard F2
    KeyF2 = 0x3B,
    /// Keyboard F3
    KeyF3 = 0x3C,
    /// Keyboard F4
    KeyF4 = 0x3D,
    /// Keyboard F5
    KeyF5 = 0x3E,
    /// Keyboard F6
    KeyF6 = 0x3F,
    /// Keyboard F7
    KeyF7 = 0x40,
    /// Keyboard F8
    KeyF8 = 0x41,
    /// Keyboard F9
    KeyF9 = 0x42,
    /// Keyboard F10
    KeyF10 = 0x43,
    /// Keyboard F11
    KeyF11 = 0x44,
    /// Keyboard F12
    KeyF12 = 0x45,
    /// Keyboard PrintScreen
    KeyPrintScreen = 0x46,
    /// Keyboard Scroll Lock
    KeyScrollLock = 0x47,
    /// Keyboard Pause
    KeyPause = 0x48,
    /// Keyboard Insert
    KeyInsert = 0x49,
    /// Keyboard Home
    KeyHome = 0x4A,
    /// Keyboard PageUp
    KeyPageUp = 0x4B,
    /// Keyboard Delete Forward
    KeyDelete = 0x4C,
    /// Keyboard End
    KeyEnd = 0x4D,
    /// Keyboard PageDown
    KeyPageDown = 0x4E,
    /// Keyboard RightArrow
    KeyRight = 0x4F,
    /// Keyboard LeftArrow
    KeyLeft = 0x50,
    /// Keyboard DownArrow
    KeyDown = 0x51,
    /// Keyboard UpArrow
    KeyUp = 0x52,
    /// Keypad Num Lock and Clear6
    KeypadNumLock = 0x53,
    /// Keypad /
    KeypadSlash = 0x54,
    /// Keypad *
    KeypadMultiply = 0x55,
    /// Keypad -
    KeypadMinus = 0x56,
    /// Keypad +
    KeypadPlus = 0x57,
    /// Keypad ENTER
    KeypadEnter = 0x58,
    /// Keypad 1 and End
    Keypad1 = 0x59,
    /// Keypad 2 and Down Arrow
    Keypad2 = 0x5A,
    /// Keypad 3 and PageDn
    Keypad3 = 0x5B,
    /// Keypad 4 and Left Arrow
    Keypad4 = 0x5C,
    /// Keypad 5
    Keypad5 = 0x5D,
    /// Keypad 6 and Right Arrow
    Keypad6 = 0x5E,
    /// Keypad 7 and Home
    Keypad7 = 0x5F,
    /// Keypad 8 and Up Arrow
    Keypad8 = 0x60,
    /// Keypad 9 and PageUp
    Keypad9 = 0x61,
    /// Keypad 0 and Insert
    Keypad0 = 0x62,
    /// Keypad . and Delete
    KeypadPeriod = 0x63,
    /// Keyboard Application
    KeyApplication = 0x65,
    /// Keypad =
    KeypadEqual = 0x67,
    /// Keyboard F13
    KeyF13 = 0x68,
    /// Keyboard F14
    KeyF14 = 0x69,
    /// Keyboard F15
    KeyF15 = 0x6A,
    /// Keyboard F16
    KeyF16 = 0x6B,
    /// Keyboard F17
    KeyF17 = 0x6C,
    /// Keyboard F18
    KeyF18 = 0x6D,
    /// Keyboard F19
    KeyF19 = 0x6E,
    /// Keyboard F20
    KeyF20 = 0x6F,
    /// Keyboard F21
    KeyF21 = 0x70,
    /// Keyboard F22
    KeyF22 = 0x71,
    /// Keyboard F23
    KeyF23 = 0x72,
    /// Keyboard F24
    KeyF24 = 0x73,
    /// Keyboard Execute
    KeyExecute = 0x74,
    /// Keyboard Help
    KeyHelp = 0x75,
    /// Keyboard Menu
    KeyMenu = 0x76,
    /// Keyboard Select
    KeySelect = 0x77,
    /// Keyboard Stop
    KeyStop = 0x78,
    /// Keyboard Again
    KeyAgain = 0x79,
    /// Keyboard Undo
    KeyUndo = 0x7A,
    /// Keyboard Cut
    KeyCut = 0x7B,
    /// Keyboard Copy
    KeyCopy = 0x7C,
    /// Keyboard Paste
    KeyPaste = 0x7D,
    /// Keyboard Find
    KeyFind = 0x7E,
    /// Keyboard Mute
    KeyMute = 0x7F,
    /// Keyboard Volume Up
    KeyVolUp = 0x80,
    /// Keyboard Volume Down
    KeyVolDown = 0x81,
    /// Keypad Equal Sign
    KeypadEqual2 = 0x86,
    /// Keyboard SysReq/Attention
    KeySysReq = 0x9A,
    /// Keyboard Cancel
    KeyCancel = 0x9B,
    /// Keyboard Clear
    KeyClear = 0x9C,
    /// Keyboard Prior
    KeyPrior = 0x9D,
    /// Keyboard Return
    KeyReturn = 0x9E,
    /// Keyboard Separator
    KeySeparator = 0x9F,
    /// Keyboard Out
    KeyOut = 0xA0,
    /// Keyboard Oper
    KeyOper = 0xA1,
    /// Keyboard Clear/Again
    KeyClearAgain = 0xA2,
    /// Keyboard CrSel/Props
    KeyCrSel = 0xA3,
    /// Keyboard ExSel
    KeyExSel = 0xA4,
    /// Keypad 00
    Keypad00 = 0xB0,
    /// Keypad 000
    Keypad000 = 0xB1,
    /// Thousands Separator
    KeyThousandsSep = 0xB2,
    /// Decimal Separator
    KeyDecimalSep = 0xB3,
    /// Currency Unit
    KeyCurrencyUnit = 0xB4,
    /// Currency Sub-unit
    KeyCurrencySubUnit = 0xB5,
    /// Keypad (
    KeypadLeftParenthesis = 0xB6,
    /// Keypad )
    KeypadRightParenthesis = 0xB7,
    /// Keypad {
    KeypadLeftBrace = 0xB8,
    /// Keypad }
    KeypadRightBrace = 0xB9,
    /// Keypad Tab
    KeypadTab = 0xBA,
    /// Keypad Backspace
    KeypadBackspace = 0xBB,
    /// Keypad A
    KeypadA = 0xBC,
    /// Keypad B
    KeypadB = 0xBD,
    /// Keypad C
    KeypadC = 0xBE,
    /// Keypad D
    KeypadD = 0xBF,
    /// Keypad E
    KeypadE = 0xC0,
    /// Keypad F
    KeypadF = 0xC1,
    /// Keypad XOR
    KeypadXOR = 0xC2,
    /// Keypad ^
    KeypadCaret = 0xC3,
    /// Keypad %
    KeypadPercent = 0xC4,
    /// Keypad <
    KeypadLessThan = 0xC5,
    /// Keypad >
    KeypadGreaterThan = 0xC6,
    /// Keypad &
    KeypadAmpersand = 0xC7,
    /// Keypad &&
    KeypadDoubleAmpersand = 0xC8,
    /// Keypad |
    KeypadPipe = 0xC9,
    /// Keypad ||
    KeypadDoublePipe = 0xCA,
    /// Keypad :
    KeypadColon = 0xCB,
    /// Keypad #
    KeypadHash = 0xCC,
    /// Keypad Space
    KeypadSpace = 0xCD,
    /// Keypad @
    KeypadAt = 0xCE,
    /// Keypad !
    KeypadExclamation = 0xCF,
    /// Keypad Memory Store
    KeypadMemStore = 0xD0,
    /// Keypad Memory Recall
    KeypadMemRecall = 0xD1,
    /// Keypad Memory Clear
    KeypadMemClear = 0xD2,
    /// Keypad Memory Add
    KeypadMemAdd = 0xD3,
    /// Keypad Memory Subtract
    KeypadMemSub = 0xD4,
    /// Keypad Memory Multiply
    KeypadMemMul = 0xD5,
    /// Keypad Memory Divide
    KeypadMemDiv = 0xD6,
    /// Keypad +/-
    KeypadPlusMinus = 0xD7,
    /// Keypad Clear
    KeypadClear = 0xD8,
    /// Keypad Clear Entry
    KeypadClearEntry = 0xD9,
    /// Keypad Binary
    KeypadBin = 0xDA,
    /// Keypad Octal
    KeypadOct = 0xDB,
    /// Keypad Decimal
    KeypadDec = 0xDC,
    /// Keypad Hexadecimal
    KeypadHex = 0xDD,
}

/// US keyboard map.
pub(super) struct USKbd;

impl KbdMap for USKbd {
    #[allow(clippy::too_many_lines)]
    fn key(&self, c: char) -> Option<Key> {
        use KeyUsage::*;
        Some(match c {
            // Row 1
            '`' => Key::from(KeyBackquote),
            '1' => Key::from(Key1),
            '2' => Key::from(Key2),
            '3' => Key::from(Key3),
            '4' => Key::from(Key4),
            '5' => Key::from(Key5),
            '6' => Key::from(Key6),
            '7' => Key::from(Key7),
            '8' => Key::from(Key8),
            '9' => Key::from(Key9),
            '0' => Key::from(Key0),
            '-' => Key::from(KeyMinus),
            '=' => Key::from(KeyEqual),
            '\u{08}' => Key::from(KeyBackspace),
            '~' => Key::shift(KeyBackquote),
            '!' => Key::shift(Key1),
            '@' => Key::shift(Key2),
            '#' => Key::shift(Key3),
            '$' => Key::shift(Key4),
            '%' => Key::shift(Key5),
            '^' => Key::shift(Key6),
            '&' => Key::shift(Key7),
            '*' => Key::shift(Key8),
            '(' => Key::shift(Key9),
            ')' => Key::shift(Key0),
            '_' => Key::shift(KeyMinus),
            '+' => Key::shift(KeyEqual),

            // Row 2
            '\t' => Key::from(KeyTab),
            'q' => Key::from(KeyQ),
            'w' => Key::from(KeyW),
            'e' => Key::from(KeyE),
            'r' => Key::from(KeyR),
            't' => Key::from(KeyT),
            'y' => Key::from(KeyY),
            'u' => Key::from(KeyU),
            'i' => Key::from(KeyI),
            'o' => Key::from(KeyO),
            'p' => Key::from(KeyP),
            '[' => Key::from(KeyLeftBracket),
            ']' => Key::from(KeyRightBracket),
            '\\' => Key::from(KeyBackslash),
            'Q' => Key::shift(KeyQ),
            'W' => Key::shift(KeyW),
            'E' => Key::shift(KeyE),
            'R' => Key::shift(KeyR),
            'T' => Key::shift(KeyT),
            'Y' => Key::shift(KeyY),
            'U' => Key::shift(KeyU),
            'I' => Key::shift(KeyI),
            'O' => Key::shift(KeyO),
            'P' => Key::shift(KeyP),
            '{' => Key::shift(KeyLeftBracket),
            '}' => Key::shift(KeyRightBracket),
            '|' => Key::shift(KeyBackslash),

            // Row 3
            'a' => Key::from(KeyA),
            's' => Key::from(KeyS),
            'd' => Key::from(KeyD),
            'f' => Key::from(KeyF),
            'g' => Key::from(KeyG),
            'h' => Key::from(KeyH),
            'j' => Key::from(KeyJ),
            'k' => Key::from(KeyK),
            'l' => Key::from(KeyL),
            ';' => Key::from(KeySemicolon),
            '\'' => Key::from(KeyQuote),
            '\n' => Key::from(KeyEnter),
            'A' => Key::shift(KeyA),
            'S' => Key::shift(KeyS),
            'D' => Key::shift(KeyD),
            'F' => Key::shift(KeyF),
            'G' => Key::shift(KeyG),
            'H' => Key::shift(KeyH),
            'J' => Key::shift(KeyJ),
            'K' => Key::shift(KeyK),
            'L' => Key::shift(KeyL),
            ':' => Key::shift(KeySemicolon),
            '"' => Key::shift(KeyQuote),

            // Row 4
            'z' => Key::from(KeyZ),
            'x' => Key::from(KeyX),
            'c' => Key::from(KeyC),
            'v' => Key::from(KeyV),
            'b' => Key::from(KeyB),
            'n' => Key::from(KeyN),
            'm' => Key::from(KeyM),
            ',' => Key::from(KeyComma),
            '.' => Key::from(KeyPeriod),
            '/' => Key::from(KeySlash),
            'Z' => Key::shift(KeyZ),
            'X' => Key::shift(KeyX),
            'C' => Key::shift(KeyC),
            'V' => Key::shift(KeyV),
            'B' => Key::shift(KeyB),
            'N' => Key::shift(KeyN),
            'M' => Key::shift(KeyM),
            '<' => Key::shift(KeyComma),
            '>' => Key::shift(KeyPeriod),
            '?' => Key::shift(KeySlash),

            // Row 5
            ' ' => Key::from(KeySpace),

            // ASCII
            '\u{007F}' => Key::from(KeyDelete),

            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
