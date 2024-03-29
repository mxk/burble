// TODO: Remove Eq and PartialEq from types that don't need them

/// User input capabilities ([Vol 3] Part H, Section 2.3.2, Table 2.3).
#[allow(dead_code)] // TODO: Implement Keyboard
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub(super) enum InputCap {
    /// Device does not have the ability to indicate 'yes' or 'no'.
    None,
    /// Device has a mechanism for the user to indicate either 'yes' or 'no'.
    YesNo,
    /// Device has a numeric keyboard that can input the numbers '0' to '9' and
    /// a confirmation. Device also has a mechanism for the user to indicate
    /// either 'yes' or 'no'.
    Keyboard,
}

/// User output capabilities ([Vol 3] Part H, Section 2.3.2, Table 2.4).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub(super) enum OutputCap {
    /// Device does not have the ability to display or communicate a 6 digit
    /// decimal number.
    None,
    /// Device has the ability to display or communicate a 6 digit decimal
    /// number.
    Numeric,
}

/// Key generation method ([Vol 3] Part H, Section 2.3.5.1, Table 2.8).
#[derive(Clone, Copy, Debug)]
pub(super) enum KeyGenMethod {
    // TODO: Oob
    JustWorks,
    PasskeyEntry,
    NumCompare,
}

impl KeyGenMethod {
    /// Map of IO capabilities to key generation method
    /// ([Vol 3] Part H, Section 2.3.5.1, Table 2.8).
    #[rustfmt::skip]
    const MAP: [[Self; 5]; 5] = {
        use KeyGenMethod::*;
        [// DisplayOnly    DisplayYesNo  KeyboardOnly  NoInputNoOutput KeyboardDisplay
            [JustWorks,    JustWorks,    PasskeyEntry, JustWorks,      PasskeyEntry  ], // DisplayOnly
            [JustWorks,    NumCompare,   PasskeyEntry, JustWorks,      NumCompare    ], // DisplayYesNo
            [PasskeyEntry, PasskeyEntry, PasskeyEntry, JustWorks,      PasskeyEntry  ], // KeyboardOnly
            [JustWorks,    JustWorks,    JustWorks,    JustWorks,      JustWorks     ], // NoInputNoOutput
            [PasskeyEntry, NumCompare,   PasskeyEntry, JustWorks,      NumCompare    ], // KeyboardDisplay
        ]
    };

    /// Returns key generation method for the given combination of IO
    /// capabilities. The mapping is symmetric, so it doesn't matter which
    /// parameter is the initiator and which one is the responder.
    #[inline(always)]
    pub const fn resolve(a: IoCap, b: IoCap) -> Self {
        Self::MAP[a as usize][b as usize]
    }
}

/// Command code ([Vol 3] Part H, Section 3.3).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Code {
    PairingRequest = 0x01,
    PairingResponse = 0x02,
    PairingConfirm = 0x03,
    PairingRandom = 0x04,
    PairingFailed = 0x05,
    EncryptionInformation = 0x06,
    CentralIdentification = 0x07,
    IdentityInformation = 0x08,
    IdentityAddressInformation = 0x09,
    SigningInformation = 0x0A,
    SecurityRequest = 0x0B,
    PairingPublicKey = 0x0C,
    PairingDhKeyCheck = 0x0D,
    PairingKeypressNotification = 0x0E,
}

/// IO capability ([Vol 3] Part H, Section 3.5.1).
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub(super) enum IoCap {
    DisplayOnly = 0x00,
    DisplayYesNo = 0x01,
    KeyboardOnly = 0x02,
    #[default]
    NoInputNoOutput = 0x03,
    KeyboardDisplay = 0x04,
}

impl IoCap {
    /// Creates IO capabilities from specified input/output configuration
    /// ([Vol 3] Part H, Section 2.3.2, Table 2.5).
    #[must_use]
    #[inline]
    pub const fn new(inp: InputCap, out: OutputCap) -> Self {
        #[allow(clippy::match_same_arms)]
        match (inp, out) {
            (InputCap::None, OutputCap::None) => Self::NoInputNoOutput,
            (InputCap::None, OutputCap::Numeric) => Self::DisplayOnly,
            (InputCap::YesNo, OutputCap::None) => Self::NoInputNoOutput,
            (InputCap::YesNo, OutputCap::Numeric) => Self::DisplayYesNo,
            (InputCap::Keyboard, OutputCap::None) => Self::KeyboardOnly,
            (InputCap::Keyboard, OutputCap::Numeric) => Self::KeyboardDisplay,
        }
    }
}

bitflags::bitflags! {
    /// Requested security properties ([Vol 3] Part H, Section 3.5.1).
    #[derive(Clone, Copy, Debug, Default)]
    #[repr(transparent)]
    pub struct AuthReq: u8 {
        /// Bonding requested.
        const BONDING = 0b01 << 0;
        /// MITM protection (authentication) requested.
        const MITM = 1 << 2;
        /// LE Secure Connections pairing is supported.
        const SC = 1 << 3;
        /// Enable keypress notifications in the Passkey Entry protocol.
        const KEYPRESS = 1 << 4;
        /// h7 function is supported for cross-transport key derivation.
        const CT2 = 1 << 5;
    }
}

bitflags::bitflags! {
    /// LE Key Distribution parameter ([Vol 3] Part H, Section 3.6.1).
    #[derive(Clone, Copy, Debug, Default)]
    #[repr(transparent)]
    pub struct KeyDist: u8 {
        /// Ignored in LE Secure Connections pairing.
        const ENC = 1 << 0;
        /// Distribute IRK using the Identity Information command.
        const ID = 1 << 1;
        /// Distribute CSRK using the Signing Information command.
        const SIGN = 1 << 2;
        /// Derive the BR/EDR Link Key from the LE LTK.
        const LINK = 1 << 3;
    }
}

/// Pairing Failed reason codes ([Vol 3] Part H, Section 3.5.5, Table 3.7).
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    PartialEq,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
    thiserror::Error,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Reason {
    /// User input of passkey failed. For example, the user cancelled the
    /// operation.
    PasskeyEntryFailed = 0x01,
    /// OOB data is not available.
    OobNotAvailable = 0x02,
    /// Pairing procedure cannot be performed as authentication requirements
    /// cannot be met due to IO capabilities of one or both devices.
    AuthenticationRequirements = 0x03,
    /// Confirm value does not match the calculated compare value.
    ConfirmValueFailed = 0x04,
    /// Pairing is not supported by the device.
    PairingNotSupported = 0x05,
    /// Resultant encryption key size is not long enough for the security
    /// requirements of this device.
    EncryptionKeySize = 0x06,
    /// Received command is not supported on this device.
    CommandNotSupported = 0x07,
    /// Pairing failed due to an unspecified reason.
    UnspecifiedReason = 0x08,
    /// Pairing or authentication procedure is disallowed because too little
    /// time has elapsed since last pairing request or security request.
    RepeatedAttempts = 0x09,
    /// Command length is invalid or a parameter is outside the specified range.
    InvalidParameters = 0x0A,
    /// Received DHKey Check value doesn't match the one calculated by the local
    /// device.
    DhKeyCheckFailed = 0x0B,
    /// Confirm values in the numeric comparison protocol do not match.
    NumericComparisonFailed = 0x0C,
    /// Pairing over the LE transport failed due to an in-progress Pairing
    /// Request sent over the BR/EDR transport.
    BrEdrPairingInProgress = 0x0D,
    /// Link Key generated on the BR/EDR transport cannot be used to derive and
    /// distribute keys for the LE transport, or the LTK generated on the LE
    /// transport cannot be used to derive a key for the BR/EDR transport.
    CrossTransportKeyDerivationNotAllowed = 0x0E,
    /// Device chose not to accept a distributed key.
    KeyRejected = 0x0F,
}

/// Keypress notification type ([Vol 3] Part H, Section 3.5.8).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum PasskeyEntry {
    Started = 0,
    DigitEntered = 1,
    DigitErased = 2,
    Cleared = 3,
    Completed = 4,
}

crate::impl_display_via_debug! { Code, IoCap, Reason, PasskeyEntry }
