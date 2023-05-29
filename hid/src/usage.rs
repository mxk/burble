use core::num::NonZeroU16;

/// Usage page IDs (\[HUT\] Section 3).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u16)]
pub enum Page {
    /// Generic Desktop Page.
    GenericDesktop = 0x01,
    /// Simulation Controls Page.
    Sim = 0x02,
    /// VR Controls Page.
    Vr = 0x03,
    /// Sport Controls Page.
    Sport = 0x04,
    /// Game Controls Page.
    Game = 0x05,
    /// Generic Device Controls Page.
    GenericDevice = 0x06,
    /// Keyboard/Keypad Page.
    Key = 0x07,
    /// LED Page.
    Led = 0x08,
    /// Button Page.
    Button = 0x09,
    /// Ordinal Page.
    Ordinal = 0x0A,
    /// Telephony Device Page.
    Telephony = 0x0B,
    /// Consumer Page.
    Consumer = 0x0C,
    /// Digitizers Page.
    Digitizer = 0x0D,
    /// Haptics Page.
    Haptics = 0x0E,
    /// Physical Input Device Page.
    Physical = 0x0F,
    /// Unicode Page.
    Unicode = 0x10,
    /// SoC Page.
    SoC = 0x11,
    /// Eye and Head Trackers Page.
    EyeAndHead = 0x12,
    /// Auxiliary Display Page.
    AuxDisplay = 0x14,
    /// Sensors Page.
    Sensor = 0x20,
    /// Medical Instrument Page.
    Medical = 0x40,
    /// Braille Display Page.
    Braille = 0x41,
    /// Lighting And Illumination Page.
    Lighting = 0x59,
    /// Monitor Page.
    Monitor = 0x80,
    /// Monitor Enumerated Page.
    MonitorEnum = 0x81,
    /// VESA Virtual Controls Page.
    Vesa = 0x82,
    /// Power Page.
    Power = 0x84,
    /// Battery System Page.
    Battery = 0x85,
    /// Barcode Scanner Page.
    Barcode = 0x8C,
    /// Scales Page.
    Scale = 0x8D,
    /// Magnetic Stripe Reader Page.
    MagStripe = 0x8E,
    /// Camera Control Page.
    Camera = 0x90,
    /// Arcade Page.
    Arcade = 0x91,
    /// Gaming Device Page.
    GamingDevice = 0x92,
    /// FIDO Alliance Page.
    Fido = 0xF1D0,
}

/// Generic Desktop usage IDs (\[HUT\] Section 4).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u8)]
pub enum GenericDesktop {
    Pointer = 0x01,
    Mouse = 0x02,
    Joystick = 0x04,
    Gamepad = 0x05,
    Keyboard = 0x06,
    Keypad = 0x07,
    MultiAxisController = 0x08,
    TabletPcSysControls = 0x09,
    WaterCoolingDevice = 0x0A,
    ComputerChassisDevice = 0x0B,
    WirelessRadioControls = 0x0C,
    PortableDeviceControl = 0x0D,
    SysMultiAxisController = 0x0E,
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
    SysControl = 0x80,
    SysPowerDown = 0x81,
    SysSleep = 0x82,
    SysWakeUp = 0x83,
    SysContextMenu = 0x84,
    SysMainMenu = 0x85,
    SysAppMenu = 0x86,
    SysMenuHelp = 0x87,
    SysMenuExit = 0x88,
    SysMenuSelect = 0x89,
    SysMenuRight = 0x8A,
    SysMenuLeft = 0x8B,
    SysMenuUp = 0x8C,
    SysMenuDown = 0x8D,
    SysColdRestart = 0x8E,
    SysWarmRestart = 0x8F,
    DPadUp = 0x90,
    DPadDown = 0x91,
    DPadRight = 0x92,
    DPadLeft = 0x93,
    IndexTrigger = 0x94,
    PalmTrigger = 0x95,
    Thumbstick = 0x96,
    SysFunctionShift = 0x97,
    SysFunctionShiftLock = 0x98,
    SysFunctionShiftLockIndicator = 0x99,
    SysDismissNotification = 0x9A,
    SysDoNotDisturb = 0x9B,
    SysDock = 0xA0,
    SysUndock = 0xA1,
    SysSetup = 0xA2,
    SysBreak = 0xA3,
    SysDebuggerBreak = 0xA4,
    ApplicationBreak = 0xA5,
    ApplicationDebuggerBreak = 0xA6,
    SysSpeakerMute = 0xA7,
    SysHibernate = 0xA8,
    SysMicrophoneMute = 0xA9,
    SysDisplayInvert = 0xB0,
    SysDisplayInternal = 0xB1,
    SysDisplayExternal = 0xB2,
    SysDisplayBoth = 0xB3,
    SysDisplayDual = 0xB4,
    SysDisplayToggleIntExtMode = 0xB5,
    SysDisplaySwapPrimarySecondary = 0xB6,
    SysDisplayToggleLcdAutoscale = 0xB7,
    SensorZone = 0xC0,
    Rpm = 0xC1,
    CoolantLevel = 0xC2,
    CoolantCriticalLevel = 0xC3,
    CoolantPump = 0xC4,
    ChassisEnclosure = 0xC5,
    WirelessRadioButton = 0xC6,
    WirelessRadioLed = 0xC7,
    WirelessRadioSliderSwitch = 0xC8,
    SysDisplayRotationLockButton = 0xC9,
    SysDisplayRotationLockSliderSwitch = 0xCA,
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

/// Keyboard/Keypad usage IDs (\[HUT\] Section 10).
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::FromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Key {
    #[default]
    /// No key.
    None = 0x00,
    /// Keyboard ErrorRollOver.
    ErrRollOver = 0x01,
    /// Keyboard POSTFail.
    PostFail = 0x02,
    /// Keyboard ErrorUndefined.
    ErrUndefined = 0x03,
    /// Keyboard a and A.
    A = 0x04,
    /// Keyboard b and B.
    B = 0x05,
    /// Keyboard c and C.
    C = 0x06,
    /// Keyboard d and D.
    D = 0x07,
    /// Keyboard e and E.
    E = 0x08,
    /// Keyboard f and F.
    F = 0x09,
    /// Keyboard g and G.
    G = 0x0A,
    /// Keyboard h and H.
    H = 0x0B,
    /// Keyboard i and I.
    I = 0x0C,
    /// Keyboard j and J.
    J = 0x0D,
    /// Keyboard k and K.
    K = 0x0E,
    /// Keyboard l and L.
    L = 0x0F,
    /// Keyboard m and M.
    M = 0x10,
    /// Keyboard n and N.
    N = 0x11,
    /// Keyboard o and O.
    O = 0x12,
    /// Keyboard p and P.
    P = 0x13,
    /// Keyboard q and Q.
    Q = 0x14,
    /// Keyboard r and R.
    R = 0x15,
    /// Keyboard s and S.
    S = 0x16,
    /// Keyboard t and T.
    T = 0x17,
    /// Keyboard u and U.
    U = 0x18,
    /// Keyboard v and V.
    V = 0x19,
    /// Keyboard w and W.
    W = 0x1A,
    /// Keyboard x and X.
    X = 0x1B,
    /// Keyboard y and Y.
    Y = 0x1C,
    /// Keyboard z and Z.
    Z = 0x1D,
    /// Keyboard 1 and !.
    Num1 = 0x1E,
    /// Keyboard 2 and @.
    Num2 = 0x1F,
    /// Keyboard 3 and #.
    Num3 = 0x20,
    /// Keyboard 4 and $.
    Num4 = 0x21,
    /// Keyboard 5 and %.
    Num5 = 0x22,
    /// Keyboard 6 and ^.
    Num6 = 0x23,
    /// Keyboard 7 and &.
    Num7 = 0x24,
    /// Keyboard 8 and *.
    Num8 = 0x25,
    /// Keyboard 9 and (.
    Num9 = 0x26,
    /// Keyboard 0 and ).
    Num0 = 0x27,
    /// Keyboard Return (ENTER).
    Enter = 0x28,
    /// Keyboard ESCAPE.
    Esc = 0x29,
    /// Keyboard DELETE (Backspace).
    Backspace = 0x2A,
    /// Keyboard Tab.
    Tab = 0x2B,
    /// Keyboard Spacebar.
    Space = 0x2C,
    /// Keyboard - and _.
    Minus = 0x2D,
    /// Keyboard = and +.
    Equals = 0x2E,
    /// Keyboard [ and {.
    LeftBracket = 0x2F,
    /// Keyboard ] and }.
    RightBracket = 0x30,
    /// Keyboard \ and |.
    Backslash = 0x31,
    /// Keyboard Non-US # and ~.
    NonUsPound = 0x32,
    /// Keyboard ; and :.
    Semicolon = 0x33,
    /// Keyboard ' and ".
    Quote = 0x34,
    /// Keyboard ` and ~.
    Backquote = 0x35,
    /// Keyboard , and <.
    Comma = 0x36,
    /// Keyboard . and >.
    Period = 0x37,
    /// Keyboard / and ?.
    Slash = 0x38,
    /// Keyboard Caps Lock.
    CapsLock = 0x39,
    /// Keyboard F1.
    F1 = 0x3A,
    /// Keyboard F2.
    F2 = 0x3B,
    /// Keyboard F3.
    F3 = 0x3C,
    /// Keyboard F4.
    F4 = 0x3D,
    /// Keyboard F5.
    F5 = 0x3E,
    /// Keyboard F6.
    F6 = 0x3F,
    /// Keyboard F7.
    F7 = 0x40,
    /// Keyboard F8.
    F8 = 0x41,
    /// Keyboard F9.
    F9 = 0x42,
    /// Keyboard F10.
    F10 = 0x43,
    /// Keyboard F11.
    F11 = 0x44,
    /// Keyboard F12.
    F12 = 0x45,
    /// Keyboard PrintScreen.
    PrintScreen = 0x46,
    /// Keyboard Scroll Lock.
    ScrollLock = 0x47,
    /// Keyboard Pause.
    Pause = 0x48,
    /// Keyboard Insert.
    Insert = 0x49,
    /// Keyboard Home.
    Home = 0x4A,
    /// Keyboard PageUp.
    PageUp = 0x4B,
    /// Keyboard Delete Forward.
    Delete = 0x4C,
    /// Keyboard End.
    End = 0x4D,
    /// Keyboard PageDown.
    PageDown = 0x4E,
    /// Keyboard RightArrow.
    Right = 0x4F,
    /// Keyboard LeftArrow.
    Left = 0x50,
    /// Keyboard DownArrow.
    Down = 0x51,
    /// Keyboard UpArrow.
    Up = 0x52,
    /// Keypad Num Lock and Clear.
    PadNumLock = 0x53,
    /// Keypad /.
    PadSlash = 0x54,
    /// Keypad *.
    PadMultiply = 0x55,
    /// Keypad -.
    PadMinus = 0x56,
    /// Keypad +.
    PadPlus = 0x57,
    /// Keypad ENTER.
    PadEnter = 0x58,
    /// Keypad 1 and End.
    Pad1 = 0x59,
    /// Keypad 2 and Down Arrow.
    Pad2 = 0x5A,
    /// Keypad 3 and PageDn.
    Pad3 = 0x5B,
    /// Keypad 4 and Left Arrow.
    Pad4 = 0x5C,
    /// Keypad 5.
    Pad5 = 0x5D,
    /// Keypad 6 and Right Arrow.
    Pad6 = 0x5E,
    /// Keypad 7 and Home.
    Pad7 = 0x5F,
    /// Keypad 8 and Up Arrow.
    Pad8 = 0x60,
    /// Keypad 9 and PageUp.
    Pad9 = 0x61,
    /// Keypad 0 and Insert.
    Pad0 = 0x62,
    /// Keypad . and Delete.
    PadPeriod = 0x63,
    /// Keyboard Non-US \ and |.
    NonUsBackslash = 0x64,
    /// Keyboard Application.
    Application = 0x65,
    /// Keyboard Power.
    Power = 0x66,
    /// Keypad =.
    PadEquals = 0x67,
    /// Keyboard F13.
    F13 = 0x68,
    /// Keyboard F14.
    F14 = 0x69,
    /// Keyboard F15.
    F15 = 0x6A,
    /// Keyboard F16.
    F16 = 0x6B,
    /// Keyboard F17.
    F17 = 0x6C,
    /// Keyboard F18.
    F18 = 0x6D,
    /// Keyboard F19.
    F19 = 0x6E,
    /// Keyboard F20.
    F20 = 0x6F,
    /// Keyboard F21.
    F21 = 0x70,
    /// Keyboard F22.
    F22 = 0x71,
    /// Keyboard F23.
    F23 = 0x72,
    /// Keyboard F24.
    F24 = 0x73,
    /// Keyboard Execute.
    Exec = 0x74,
    /// Keyboard Help.
    Help = 0x75,
    /// Keyboard Menu.
    Menu = 0x76,
    /// Keyboard Select.
    Select = 0x77,
    /// Keyboard Stop.
    Stop = 0x78,
    /// Keyboard Again.
    Again = 0x79,
    /// Keyboard Undo.
    Undo = 0x7A,
    /// Keyboard Cut.
    Cut = 0x7B,
    /// Keyboard Copy.
    Copy = 0x7C,
    /// Keyboard Paste.
    Paste = 0x7D,
    /// Keyboard Find.
    Find = 0x7E,
    /// Keyboard Mute.
    Mute = 0x7F,
    /// Keyboard Volume Up.
    VolUp = 0x80,
    /// Keyboard Volume Down.
    VolDown = 0x81,
    /// Keyboard Locking Caps Lock.
    LockingCapsLock = 0x82,
    /// Keyboard Locking Num Lock.
    LockingNumLock = 0x83,
    /// Keyboard Locking Scroll Lock.
    LockingScrollLock = 0x84,
    /// Keypad Comma.
    PadComma = 0x85,
    /// Keypad Equal Sign.
    PadEqualsAs400 = 0x86,
    /// Keyboard International1.
    Intl1 = 0x87,
    /// Keyboard International2.
    Intl2 = 0x88,
    /// Keyboard International3.
    Intl3 = 0x89,
    /// Keyboard International4.
    Intl4 = 0x8A,
    /// Keyboard International5.
    Intl5 = 0x8B,
    /// Keyboard International6.
    Intl6 = 0x8C,
    /// Keyboard International7.
    Intl7 = 0x8D,
    /// Keyboard International8.
    Intl8 = 0x8E,
    /// Keyboard International9.
    Intl9 = 0x8F,
    /// Keyboard LANG1.
    Lang1 = 0x90,
    /// Keyboard LANG2.
    Lang2 = 0x91,
    /// Keyboard LANG3.
    Lang3 = 0x92,
    /// Keyboard LANG4.
    Lang4 = 0x93,
    /// Keyboard LANG5.
    Lang5 = 0x94,
    /// Keyboard LANG6.
    Lang6 = 0x95,
    /// Keyboard LANG7.
    Lang7 = 0x96,
    /// Keyboard LANG8.
    Lang8 = 0x97,
    /// Keyboard LANG9.
    Lang9 = 0x98,
    /// Keyboard Alternate Erase.
    EraseEaze = 0x99,
    /// Keyboard SysReq/Attention.
    SysReq = 0x9A,
    /// Keyboard Cancel.
    Cancel = 0x9B,
    /// Keyboard Clear.
    Clear = 0x9C,
    /// Keyboard Prior.
    Prior = 0x9D,
    /// Keyboard Return.
    Return = 0x9E,
    /// Keyboard Separator.
    Separator = 0x9F,
    /// Keyboard Out.
    Out = 0xA0,
    /// Keyboard Oper.
    Oper = 0xA1,
    /// Keyboard Clear/Again.
    ClearAgain = 0xA2,
    /// Keyboard CrSel/Props.
    CrSel = 0xA3,
    /// Keyboard ExSel.
    ExSel = 0xA4,
    /// Keypad 00.
    Pad00 = 0xB0,
    /// Keypad 000.
    Pad000 = 0xB1,
    /// Thousands Separator.
    ThousandsSep = 0xB2,
    /// Decimal Separator.
    DecimalSep = 0xB3,
    /// Currency Unit.
    CurrencyUnit = 0xB4,
    /// Currency Sub-unit.
    CurrencySubUnit = 0xB5,
    /// Keypad (.
    PadLeftParen = 0xB6,
    /// Keypad ).
    PadRightParen = 0xB7,
    /// Keypad {.
    PadLeftBrace = 0xB8,
    /// Keypad }.
    PadRightBrace = 0xB9,
    /// Keypad Tab.
    PadTab = 0xBA,
    /// Keypad Backspace.
    PadBackspace = 0xBB,
    /// Keypad A.
    PadA = 0xBC,
    /// Keypad B.
    PadB = 0xBD,
    /// Keypad C.
    PadC = 0xBE,
    /// Keypad D.
    PadD = 0xBF,
    /// Keypad E.
    PadE = 0xC0,
    /// Keypad F.
    PadF = 0xC1,
    /// Keypad XOR.
    PadXor = 0xC2,
    /// Keypad ^.
    PadCaret = 0xC3,
    /// Keypad %.
    PadPercent = 0xC4,
    /// Keypad <.
    PadLessThan = 0xC5,
    /// Keypad >.
    PadGreaterThan = 0xC6,
    /// Keypad &.
    PadAmpersand = 0xC7,
    /// Keypad &&.
    PadDoubleAmpersand = 0xC8,
    /// Keypad |.
    PadVBar = 0xC9,
    /// Keypad ||.
    PadDoubleVBar = 0xCA,
    /// Keypad :.
    PadColon = 0xCB,
    /// Keypad #.
    PadPound = 0xCC,
    /// Keypad Space.
    PadSpace = 0xCD,
    /// Keypad @.
    PadAt = 0xCE,
    /// Keypad !.
    PadExclamation = 0xCF,
    /// Keypad Memory Store.
    PadMemStore = 0xD0,
    /// Keypad Memory Recall.
    PadMemRecall = 0xD1,
    /// Keypad Memory Clear.
    PadMemClear = 0xD2,
    /// Keypad Memory Add.
    PadMemAdd = 0xD3,
    /// Keypad Memory Subtract.
    PadMemSub = 0xD4,
    /// Keypad Memory Multiply.
    PadMemMul = 0xD5,
    /// Keypad Memory Divide.
    PadMemDiv = 0xD6,
    /// Keypad +/-.
    PadPlusMinus = 0xD7,
    /// Keypad Clear.
    PadClear = 0xD8,
    /// Keypad Clear Entry.
    PadClearEntry = 0xD9,
    /// Keypad Binary.
    PadBin = 0xDA,
    /// Keypad Octal.
    PadOct = 0xDB,
    /// Keypad Decimal.
    PadDec = 0xDC,
    /// Keypad Hexadecimal.
    PadHex = 0xDD,
    /// Keyboard LeftControl.
    LeftCtrl = 0xE0,
    /// Keyboard LeftShift.
    LeftShift = 0xE1,
    /// Keyboard LeftAlt.
    LeftAlt = 0xE2,
    /// Keyboard Left GUI.
    LeftGui = 0xE3,
    /// Keyboard RightControl.
    RightCtrl = 0xE4,
    /// Keyboard RightShift.
    RightShift = 0xE5,
    /// Keyboard RightAlt.
    RightAlt = 0xE6,
    /// Keyboard Right GUI.
    RightGui = 0xE7,
}

impl Key {
    /// Returns whether the key is `None`.
    #[inline(always)]
    #[must_use]
    pub const fn is_none(self) -> bool {
        matches!(self, Self::None)
    }

    /// Returns whether the key is not `None`.
    #[inline(always)]
    #[must_use]
    pub const fn is_some(self) -> bool {
        !self.is_none()
    }
}

/// LED usage IDs (\[HUT\] Section 11).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u8)]
pub enum Led {
    NumLock = 0x01,
    CapsLock = 0x02,
    ScrollLock = 0x03,
    Compose = 0x04,
    Kana = 0x05,
    Power = 0x06,
    Shift = 0x07,
    DoNotDisturb = 0x08,
    Mute = 0x09,
    ToneEnable = 0x0A,
    HighCutFilter = 0x0B,
    LowCutFilter = 0x0C,
    EqualizerEnable = 0x0D,
    SoundFieldOn = 0x0E,
    SurroundOn = 0x0F,
    Repeat = 0x10,
    Stereo = 0x11,
    SamplingRateDetect = 0x12,
    Spinning = 0x13,
    Cav = 0x14,
    Clv = 0x15,
    RecordingFormatDetect = 0x16,
    OffHook = 0x17,
    Ring = 0x18,
    MessageWaiting = 0x19,
    DataMode = 0x1A,
    BatteryOperation = 0x1B,
    BatteryOk = 0x1C,
    BatteryLow = 0x1D,
    Speaker = 0x1E,
    Headset = 0x1F,
    Hold = 0x20,
    Microphone = 0x21,
    Coverage = 0x22,
    NightMode = 0x23,
    SendCalls = 0x24,
    CallPickup = 0x25,
    Conference = 0x26,
    StandBy = 0x27,
    CameraOn = 0x28,
    CameraOff = 0x29,
    OnLine = 0x2A,
    OffLine = 0x2B,
    Busy = 0x2C,
    Ready = 0x2D,
    PaperOut = 0x2E,
    PaperJam = 0x2F,
    Remote = 0x30,
    Forward = 0x31,
    Reverse = 0x32,
    Stop = 0x33,
    Rewind = 0x34,
    FastForward = 0x35,
    Play = 0x36,
    Pause = 0x37,
    Record = 0x38,
    Error = 0x39,
    UsageSelectedIndicator = 0x3A,
    UsageInUseIndicator = 0x3B,
    UsageMultiModeIndicator = 0x3C,
    IndicatorOn = 0x3D,
    IndicatorFlash = 0x3E,
    IndicatorSlowBlink = 0x3F,
    IndicatorFastBlink = 0x40,
    IndicatorOff = 0x41,
    FlashOnTime = 0x42,
    SlowBlinkOnTime = 0x43,
    SlowBlinkOffTime = 0x44,
    FastBlinkOnTime = 0x45,
    FastBlinkOffTime = 0x46,
    UsageIndicatorColor = 0x47,
    IndicatorRed = 0x48,
    IndicatorGreen = 0x49,
    IndicatorAmber = 0x4A,
    GenericIndicator = 0x4B,
    SystemSuspend = 0x4C,
    ExternalPowerConnected = 0x4D,
    IndicatorBlue = 0x4E,
    IndicatorOrange = 0x4F,
    GoodStatus = 0x50,
    WarningStatus = 0x51,
    Rgb = 0x52,
    RedChannel = 0x53,
    BlueChannel = 0x54,
    GreenChannel = 0x55,
    Intensity = 0x56,
    SystemMicrophoneMute = 0x57,
    PlayerIndicator = 0x60,
    Player1 = 0x61,
    Player2 = 0x62,
    Player3 = 0x63,
    Player4 = 0x64,
    Player5 = 0x65,
    Player6 = 0x66,
    Player7 = 0x67,
    Player8 = 0x68,
}

/// Button ID (\[HUT\] Section 12).
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Button(NonZeroU16);

impl Button {
    /// Primary Button. Used for object selecting, dragging, and double click
    /// activation. On macOS, this is the only button. Microsoft operating
    /// systems call this a logical left button, because it is not necessarily
    /// physically located on the left of the pointing device.
    pub const PRIMARY: Self = Self(
        // SAFETY: Non-zero
        unsafe { NonZeroU16::new_unchecked(0x01) },
    );

    /// Secondary Button. Used by newer graphical user interfaces to browse
    /// object properties. Exposed by systems to applications that typically
    /// assign application-specific functionality.
    pub const SECONDARY: Self = Self(
        // SAFETY: Non-zero
        unsafe { NonZeroU16::new_unchecked(0x02) },
    );

    /// Tertiary Button. Optional control. Exposed to applications, but seldom
    /// assigned functionality due to prevalence of two and one button devices.
    pub const TERTIARY: Self = Self(
        // SAFETY: Non-zero
        unsafe { NonZeroU16::new_unchecked(0x03) },
    );

    /// Returns the `n`th button usage ID.
    #[inline]
    #[must_use]
    pub const fn nth(n: u16) -> Option<Self> {
        match NonZeroU16::new(n) {
            Some(n) => Some(Self(n)),
            None => None,
        }
    }

    /// Returns the numeric button ID.
    #[inline(always)]
    #[must_use]
    pub const fn id(self) -> u16 {
        self.0.get()
    }
}
