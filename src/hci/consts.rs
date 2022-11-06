#![allow(clippy::use_self)]

use bitflags::bitflags;

use OpcodeGroup::*;

/// HCI command opcodes ([Vol 4] Part E, Section 7).
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    Eq,
    Ord,
    PartialEq,
    PartialOrd,
    num_enum::FromPrimitive,
    num_enum::IntoPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u16)]
pub enum Opcode {
    /// Opcode 0x0000 is used to update `Num_HCI_Command_Packets`
    /// ([Vol 4] Part E, Section 7.7.14).
    #[default]
    None = 0x0000,

    // HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3)
    SetEventMask = HciControl.ocf(0x0001),
    Reset = HciControl.ocf(0x0003),
    SetEventMaskPage2 = HciControl.ocf(0x0063),
    WriteLeHostSupport = HciControl.ocf(0x006D),

    // Informational parameters commands ([Vol 4] Part E, Section 7.4)
    ReadLocalVersionInformation = InfoParams.ocf(0x0001),
    ReadLocalSupportedCommands = InfoParams.ocf(0x0002),
    ReadBufferSize = InfoParams.ocf(0x0005),
    ReadBdAddr = InfoParams.ocf(0x0009),

    // LE Controller commands ([Vol 4] Part E, Section 7.8)
    LeSetEventMask = Le.ocf(0x0001),
    LeReadBufferSize = Le.ocf(0x0002),
    LeReadBufferSizeV2 = Le.ocf(0x0060),
    LeSetAdvertisingSetRandomAddress = Le.ocf(0x0035),
    LeSetExtendedAdvertisingParameters = Le.ocf(0x0036),
    LeSetExtendedAdvertisingData = Le.ocf(0x0037),
    LeSetExtendedScanResponseData = Le.ocf(0x0038),
    LeSetExtendedAdvertisingEnable = Le.ocf(0x0039),
    LeReadMaximumAdvertisingDataLength = Le.ocf(0x003A),
    LeReadNumberOfSupportedAdvertisingSets = Le.ocf(0x003B),
    LeRemoveAdvertisingSet = Le.ocf(0x003C),
    LeClearAdvertisingSets = Le.ocf(0x003D),
    LeSetPeriodicAdvertisingParameters = Le.ocf(0x003E),
    LeSetPeriodicAdvertisingData = Le.ocf(0x003F),
    LeSetPeriodicAdvertisingEnable = Le.ocf(0x0040),
}

// Opcode group field definitions.
#[derive(Clone, Copy)]
#[repr(u16)]
enum OpcodeGroup {
    _LinkControl = 0x01,
    _LinkPolicy = 0x02,
    HciControl = 0x03,
    InfoParams = 0x04,
    _StatusParams = 0x05,
    _Testing = 0x06,
    Le = 0x08,
    _Vendor = 0x3F, // [Vol 4] Part E, Section 5.4.1
}

impl OpcodeGroup {
    /// Combines OGF with OCF to create a full opcode.
    #[inline]
    const fn ocf(self, ocf: u16) -> u16 {
        (self as u16) << 10 | ocf
    }
}

/// HCI event codes ([Vol 4] Part E, Section 7.7).
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive)]
#[non_exhaustive]
#[repr(u8)]
pub enum EventCode {
    InquiryComplete = 0x01,
    InquiryResult = 0x02,
    ConnectionComplete = 0x03,
    ConnectionRequest = 0x04,
    DisconnectionComplete = 0x05,
    AuthenticationComplete = 0x06,
    RemoteNameRequestComplete = 0x07,
    EncryptionChangeV1 = 0x08,
    EncryptionChangeV2 = 0x59,
    ChangeConnectionLinkKeyComplete = 0x09,
    LinkKeyTypeChanged = 0x0A,
    ReadRemoteSupportedFeaturesComplete = 0x0B,
    ReadRemoteVersionInformationComplete = 0x0C,
    QosSetupComplete = 0x0D,
    CommandComplete = 0x0E,
    CommandStatus = 0x0F,
    HardwareError = 0x10,
    FlushOccurred = 0x11,
    RoleChange = 0x12,
    NumberOfCompletedPackets = 0x13,
    ModeChange = 0x14,
    ReturnLinkKeys = 0x15,
    PinCodeRequest = 0x16,
    LinkKeyRequest = 0x17,
    LinkKeyNotification = 0x18,
    LoopbackCommand = 0x19,
    DataBufferOverflow = 0x1A,
    MaxSlotsChange = 0x1B,
    ReadClockOffsetComplete = 0x1C,
    ConnectionPacketTypeChanged = 0x1D,
    QosViolation = 0x1E,
    PageScanRepetitionModeChange = 0x20,
    FlowSpecificationComplete = 0x21,
    InquiryResultWithRssi = 0x22,
    ReadRemoteExtendedFeaturesComplete = 0x23,
    SynchronousConnectionComplete = 0x2C,
    SynchronousConnectionChanged = 0x2D,
    SniffSubrating = 0x2E,
    ExtendedInquiryResult = 0x2F,
    EncryptionKeyRefreshComplete = 0x30,
    IoCapabilityRequest = 0x31,
    IoCapabilityResponse = 0x32,
    UserConfirmationRequest = 0x33,
    UserPasskeyRequest = 0x34,
    RemoteOobDataRequest = 0x35,
    SimplePairingComplete = 0x36,
    LinkSupervisionTimeoutChanged = 0x38,
    EnhancedFlushComplete = 0x39,
    UserPasskeyNotification = 0x3B,
    KeypressNotification = 0x3C,
    RemoteHostSupportedFeaturesNotification = 0x3D,
    NumberOfCompletedDataBlocks = 0x48,
    LeMetaEvent = 0x3E,
    TriggeredClockCapture = 0x4E,
    SynchronizationTrainComplete = 0x4F,
    SynchronizationTrainReceived = 0x50,
    ConnectionlessPeripheralBroadcastReceive = 0x51,
    ConnectionlessPeripheralBroadcastTimeout = 0x52,
    TruncatedPageComplete = 0x53,
    PeripheralPageResponseTimeout = 0x54,
    ConnectionlessPeripheralBroadcastChannelMapChange = 0x55,
    InquiryResponseNotification = 0x56,
    AuthenticatedPayloadTimeoutExpired = 0x57,
    SamStatusChange = 0x58,
    Vendor = 0xFF, // [Vol 4] Part E, Section 5.4.4
}

impl EventCode {
    /// Returns the format of the associated event parameters.
    #[inline]
    #[must_use]
    pub const fn param_fmt(self) -> EventFmt {
        use {EventCode::*, EventFmt::*, HandleType::*};
        #[allow(clippy::match_same_arms)]
        match self {
            InquiryComplete => Status,
            InquiryResult => Other,
            ConnectionComplete => StatusAndHandle(Conn),
            ConnectionRequest => Other,
            DisconnectionComplete => StatusAndHandle(Conn),
            AuthenticationComplete => StatusAndHandle(Conn),
            RemoteNameRequestComplete => Status,
            EncryptionChangeV1 => StatusAndHandle(Conn),
            EncryptionChangeV2 => StatusAndHandle(Conn),
            ChangeConnectionLinkKeyComplete => StatusAndHandle(Conn),
            LinkKeyTypeChanged => StatusAndHandle(Conn),
            ReadRemoteSupportedFeaturesComplete => StatusAndHandle(Conn),
            ReadRemoteVersionInformationComplete => StatusAndHandle(Conn),
            QosSetupComplete => StatusAndHandle(Conn),
            CommandComplete => Status, // Other format, but want has_status() == true
            CommandStatus => Status,
            HardwareError => Other,
            FlushOccurred => Handle(Conn),
            RoleChange => Status,
            NumberOfCompletedPackets => Other,
            ModeChange => StatusAndHandle(Conn),
            ReturnLinkKeys => Other,
            PinCodeRequest => Other,
            LinkKeyRequest => Other,
            LinkKeyNotification => Other,
            LoopbackCommand => Other,
            DataBufferOverflow => Other,
            MaxSlotsChange => Handle(Conn),
            ReadClockOffsetComplete => StatusAndHandle(Conn),
            ConnectionPacketTypeChanged => StatusAndHandle(Conn),
            QosViolation => Handle(Conn),
            PageScanRepetitionModeChange => Other,
            FlowSpecificationComplete => StatusAndHandle(Conn),
            InquiryResultWithRssi => Other,
            ReadRemoteExtendedFeaturesComplete => StatusAndHandle(Conn),
            SynchronousConnectionComplete => StatusAndHandle(Conn),
            SynchronousConnectionChanged => StatusAndHandle(Conn),
            SniffSubrating => StatusAndHandle(Conn),
            ExtendedInquiryResult => Other,
            EncryptionKeyRefreshComplete => StatusAndHandle(Conn),
            IoCapabilityRequest => Other,
            IoCapabilityResponse => Other,
            UserConfirmationRequest => Other,
            UserPasskeyRequest => Other,
            RemoteOobDataRequest => Other,
            SimplePairingComplete => Status,
            LinkSupervisionTimeoutChanged => Handle(Conn),
            EnhancedFlushComplete => Handle(Conn),
            UserPasskeyNotification => Other,
            KeypressNotification => Other,
            RemoteHostSupportedFeaturesNotification => Other,
            NumberOfCompletedDataBlocks => Other,
            LeMetaEvent => Other,
            TriggeredClockCapture => Handle(Conn),
            SynchronizationTrainComplete => Status,
            SynchronizationTrainReceived => Status,
            ConnectionlessPeripheralBroadcastReceive => Other,
            ConnectionlessPeripheralBroadcastTimeout => Other,
            TruncatedPageComplete => Status,
            PeripheralPageResponseTimeout => Other,
            ConnectionlessPeripheralBroadcastChannelMapChange => Other,
            InquiryResponseNotification => Other,
            AuthenticatedPayloadTimeoutExpired => Handle(Conn),
            SamStatusChange => Handle(Conn),
            Vendor => Other,
        }
    }
}

/// HCI LE subevent codes ([Vol 4] Part E, Section 7.7.65).
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive)]
#[non_exhaustive]
#[repr(u8)]
pub enum SubeventCode {
    ConnectionComplete = 0x01,
    AdvertisingReport = 0x02,
    ConnectionUpdateComplete = 0x03,
    ReadRemoteFeaturesComplete = 0x04,
    LongTermKeyRequest = 0x05,
    RemoteConnectionParameterRequest = 0x06,
    DataLengthChange = 0x07,
    ReadLocalP256PublicKeyComplete = 0x08,
    GenerateDhKeyComplete = 0x09,
    EnhancedConnectionComplete = 0x0A,
    DirectedAdvertisingReport = 0x0B,
    PhyUpdateComplete = 0x0C,
    ExtendedAdvertisingReport = 0x0D,
    PeriodicAdvertisingSyncEstablished = 0x0E,
    PeriodicAdvertisingReport = 0x0F,
    PeriodicAdvertisingSyncLost = 0x10,
    ScanTimeout = 0x11,
    AdvertisingSetTerminated = 0x12,
    ScanRequestReceived = 0x13,
    ChannelSelectionAlgorithm = 0x14,
    ConnectionlessIqReport = 0x15,
    ConnectionIqReport = 0x16,
    CteRequestFailed = 0x17,
    PeriodicAdvertisingSyncTransferReceived = 0x18,
    CisEstablished = 0x19,
    CisRequest = 0x1A,
    CreateBigComplete = 0x1B,
    TerminateBigComplete = 0x1C,
    BigSyncEstablished = 0x1D,
    BigSyncLost = 0x1E,
    RequestPeerScaComplete = 0x1F,
    PathLossThreshold = 0x20,
    TransmitPowerReporting = 0x21,
    BigInfoAdvertisingReport = 0x22,
    SubrateChange = 0x23,
}

impl SubeventCode {
    /// Returns the format of the associated event parameters.
    #[inline]
    #[must_use]
    pub const fn param_fmt(self) -> EventFmt {
        use {EventFmt::*, HandleType::*, SubeventCode::*};
        #[allow(clippy::match_same_arms)]
        match self {
            ConnectionComplete => StatusAndHandle(Conn),
            AdvertisingReport => Other,
            ConnectionUpdateComplete => StatusAndHandle(Conn),
            ReadRemoteFeaturesComplete => StatusAndHandle(Conn),
            LongTermKeyRequest => Handle(Conn),
            RemoteConnectionParameterRequest => Handle(Conn),
            DataLengthChange => Handle(Conn),
            ReadLocalP256PublicKeyComplete => Status,
            GenerateDhKeyComplete => Status,
            EnhancedConnectionComplete => StatusAndHandle(Conn),
            DirectedAdvertisingReport => Other,
            PhyUpdateComplete => StatusAndHandle(Conn),
            ExtendedAdvertisingReport => Other,
            PeriodicAdvertisingSyncEstablished => StatusAndHandle(Sync),
            PeriodicAdvertisingReport => Handle(Sync),
            PeriodicAdvertisingSyncLost => Handle(Sync),
            ScanTimeout => Other,
            AdvertisingSetTerminated => StatusAndHandle(Adv),
            ScanRequestReceived => Handle(Adv),
            ChannelSelectionAlgorithm => Handle(Conn),
            ConnectionlessIqReport => Handle(Sync),
            ConnectionIqReport => Handle(Conn),
            CteRequestFailed => StatusAndHandle(Conn),
            PeriodicAdvertisingSyncTransferReceived => StatusAndHandle(Conn),
            CisEstablished => StatusAndHandle(Conn),
            CisRequest => Handle(Conn),
            CreateBigComplete => StatusAndHandle(Big),
            TerminateBigComplete => Handle(Big),
            BigSyncEstablished => StatusAndHandle(Big),
            BigSyncLost => Handle(Big),
            RequestPeerScaComplete => StatusAndHandle(Conn),
            PathLossThreshold => Handle(Conn),
            TransmitPowerReporting => StatusAndHandle(Conn),
            BigInfoAdvertisingReport => Handle(Sync),
            SubrateChange => StatusAndHandle(Conn),
        }
    }
}

/// Event parameter format.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EventFmt {
    /// Event has neither status nor handle parameters.
    Other,
    /// Event has only a status parameter.
    Status,
    /// Event has only a handle parameter (Connection, Sync, etc.).
    Handle(HandleType),
    /// Event has both status and handle parameters.
    StatusAndHandle(HandleType),
}

impl EventFmt {
    /// Returns whether the associated event has a status parameter.
    #[inline]
    #[must_use]
    pub const fn has_status(self) -> bool {
        matches!(self, Self::Status | Self::StatusAndHandle(_))
    }

    /// Returns the type of handle contained in the associated event.
    #[inline]
    #[must_use]
    pub const fn handle_type(self) -> Option<HandleType> {
        match self {
            Self::Other | Self::Status => None,
            Self::Handle(t) | Self::StatusAndHandle(t) => Some(t),
        }
    }
}

/// Type of handle ([Vol 4] Part E, Section 5.3.1)
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum HandleType {
    /// Connection handle.
    Conn,
    /// Advertising train handle.
    Sync,
    /// Advertising handle.
    Adv,
    /// Broadcast Isochronous Group handle.
    Big,
}

impl HandleType {
    /// Returns whether the handle type is `u8` (`u16` otherwise).
    #[inline]
    #[must_use]
    pub const fn is_u8(self) -> bool {
        matches!(self, HandleType::Adv | HandleType::Big)
    }
}

/// HCI status codes ([Vol 1] Part F, Section 1.3).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::FromPrimitive, strum::Display, thiserror::Error,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Status {
    Success = 0x00,
    UnknownCommand = 0x01,
    UnknownConnectionIdentifier = 0x02,
    HardwareFailure = 0x03,
    PageTimeout = 0x04,
    AuthenticationFailure = 0x05,
    PinOrKeyMissing = 0x06,
    MemoryCapacityExceeded = 0x07,
    ConnectionTimeout = 0x08,
    ConnectionLimitExceeded = 0x09,
    SynchronousConnectionLimitToADeviceExceeded = 0x0A,
    ConnectionAlreadyExists = 0x0B,
    CommandDisallowed = 0x0C,
    ConnectionRejectedDueToLimitedResources = 0x0D,
    ConnectionRejectedDueToSecurityReasons = 0x0E,
    ConnectionRejectedDueToUnacceptableBdAddr = 0x0F,
    ConnectionAcceptTimeoutExceeded = 0x10,
    UnsupportedFeatureOrParameterValue = 0x11,
    InvalidCommandParameters = 0x12,
    RemoteUserTerminatedConnection = 0x13,
    RemoteDeviceTerminatedConnectionDueToLowResources = 0x14,
    RemoteDeviceTerminatedConnectionDueToPowerOff = 0x15,
    ConnectionTerminatedByLocalHost = 0x16,
    RepeatedAttempts = 0x17,
    PairingNotAllowed = 0x18,
    UnknownLmpPdu = 0x19,
    UnsupportedRemoteFeature = 0x1A,
    ScoOffsetRejected = 0x1B,
    ScoIntervalRejected = 0x1C,
    ScoAirModeRejected = 0x1D,
    InvalidLmpLlParameters = 0x1E,
    #[num_enum(default)] // [Vol 4] Part E, Section 1.2
    UnspecifiedError = 0x1F,
    UnsupportedLmpLlParameterValue = 0x20,
    RoleChangeNotAllowed = 0x21,
    LmpLlResponseTimeout = 0x22,
    LmpLlErrorTransactionCollision = 0x23,
    LmpPduNotAllowed = 0x24,
    EncryptionModeNotAcceptable = 0x25,
    LinkKeyCannotBeChanged = 0x26,
    RequestedQosNotSupported = 0x27,
    InstantPassed = 0x28,
    PairingWithUnitKeyNotSupported = 0x29,
    DifferentTransactionCollision = 0x2A,
    QosUnacceptableParameter = 0x2C,
    QosRejected = 0x2D,
    ChannelClassificationNotSupported = 0x2E,
    InsufficientSecurity = 0x2F,
    ParameterOutOfMandatoryRange = 0x30,
    RoleSwitchPending = 0x32,
    ReservedSlotViolation = 0x34,
    RoleSwitchFailed = 0x35,
    ExtendedInquiryResponseTooLarge = 0x36,
    SecureSimplePairingNotSupportedByHost = 0x37,
    HostBusyPairing = 0x38,
    ConnectionRejectedDueToNoSuitableChannelFound = 0x39,
    ControllerBusy = 0x3A,
    UnacceptableConnectionParameters = 0x3B,
    AdvertisingTimeout = 0x3C,
    ConnectionTerminatedDueToMicFailure = 0x3D,
    ConnectionFailedToBeEstablished = 0x3E,
    CoarseClockAdjustmentRejected = 0x40,
    Type0SubmapNotDefined = 0x41,
    UnknownAdvertisingIdentifier = 0x42,
    LimitReached = 0x43,
    OperationCancelledByHost = 0x44,
    PacketTooLong = 0x45,
}

impl Status {
    /// Returns whether status is `Success`.
    #[inline]
    #[must_use]
    pub const fn is_ok(self) -> bool {
        matches!(self, Status::Success)
    }
}

impl Default for Status {
    #[inline]
    fn default() -> Self {
        Self::Success
    }
}

bitflags! {
    /// Maskable events ([Vol 4] Part E, Section 7.3.1).
    #[repr(transparent)]
    pub struct EventMask: u64 {
        const INQUIRY_COMPLETE = 1 << 0;
        const INQUIRY_RESULT = 1 << 1;
        const CONNECTION_COMPLETE = 1 << 2;
        const CONNECTION_REQUEST = 1 << 3;
        const DISCONNECTION_COMPLETE = 1 << 4;
        const AUTHENTICATION_COMPLETE = 1 << 5;
        const REMOTE_NAME_REQUEST_COMPLETE = 1 << 6;
        const ENCRYPTION_CHANGE_V1 = 1 << 7;
        const CHANGE_CONNECTION_LINK_KEY_COMPLETE = 1 << 8;
        const LINK_KEY_TYPE_CHANGED = 1 << 9;
        const READ_REMOTE_SUPPORTED_FEATURES_COMPLETE = 1 << 10;
        const READ_REMOTE_VERSION_INFORMATION_COMPLETE = 1 << 11;
        const QO_S_SETUP_COMPLETE = 1 << 12;
        const HARDWARE_ERROR = 1 << 15;
        const FLUSH_OCCURRED = 1 << 16;
        const ROLE_CHANGE = 1 << 17;
        const MODE_CHANGE = 1 << 19;
        const RETURN_LINK_KEYS = 1 << 20;
        const PIN_CODE_REQUEST = 1 << 21;
        const LINK_KEY_REQUEST = 1 << 22;
        const LINK_KEY_NOTIFICATION = 1 << 23;
        const LOOPBACK_COMMAND = 1 << 24;
        const DATA_BUFFER_OVERFLOW = 1 << 25;
        const MAX_SLOTS_CHANGE = 1 << 26;
        const READ_CLOCK_OFFSET_COMPLETE = 1 << 27;
        const CONNECTION_PACKET_TYPE_CHANGED = 1 << 28;
        const QO_S_VIOLATION = 1 << 29;
        const PAGE_SCAN_MODE_CHANGE = 1 << 30;
        const PAGE_SCAN_REPETITION_MODE_CHANGE = 1 << 31;
        const FLOW_SPECIFICATION_COMPLETE = 1 << 32;
        const INQUIRY_RESULT_WITH_RSSI = 1 << 33;
        const READ_REMOTE_EXTENDED_FEATURES_COMPLETE = 1 << 34;
        const SYNCHRONOUS_CONNECTION_COMPLETE = 1 << 43;
        const SYNCHRONOUS_CONNECTION_CHANGED = 1 << 44;
        const SNIFF_SUBRATING = 1 << 45;
        const EXTENDED_INQUIRY_RESULT = 1 << 46;
        const ENCRYPTION_KEY_REFRESH_COMPLETE = 1 << 47;
        const IO_CAPABILITY_REQUEST = 1 << 48;
        const IO_CAPABILITY_RESPONSE = 1 << 49;
        const USER_CONFIRMATION_REQUEST = 1 << 50;
        const USER_PASSKEY_REQUEST = 1 << 51;
        const REMOTE_OOB_DATA_REQUEST = 1 << 52;
        const SIMPLE_PAIRING_COMPLETE = 1 << 53;
        const LINK_SUPERVISION_TIMEOUT_CHANGED = 1 << 55;
        const ENHANCED_FLUSH_COMPLETE = 1 << 56;
        const USER_PASSKEY_NOTIFICATION = 1 << 58;
        const KEYPRESS_NOTIFICATION = 1 << 59;
        const REMOTE_HOST_SUPPORTED_FEATURES_NOTIFICATION = 1 << 60;
        const LE_META = 1 << 61;
    }
}

impl Default for EventMask {
    /// Returns an event mask with all bits, including reserved ones, set to 1.
    #[inline]
    fn default() -> Self {
        // SAFETY: Controller interprets reserved or unsupported bits as 0.
        unsafe { Self::from_bits_unchecked(u64::MAX) }
    }
}

bitflags! {
    /// Maskable events ([Vol 4] Part E, Section 7.3.69).
    #[repr(transparent)]
    pub struct EventMaskPage2: u64 {
        const PHYSICAL_LINK_COMPLETE = 1 << 0;
        const CHANNEL_SELECTED = 1 << 1;
        const DISCONNECTION_PHYSICAL_LINK_COMPLETE = 1 << 2;
        const PHYSICAL_LINK_LOSS_EARLY_WARNING = 1 << 3;
        const PHYSICAL_LINK_RECOVERY = 1 << 4;
        const LOGICAL_LINK_COMPLETE = 1 << 5;
        const DISCONNECTION_LOGICAL_LINK_COMPLETE = 1 << 6;
        const FLOW_SPEC_MODIFY_COMPLETE = 1 << 7;
        const NUMBER_OF_COMPLETED_DATA_BLOCKS = 1 << 8;
        const AMP_START_TEST = 1 << 9;
        const AMP_TEST_END = 1 << 10;
        const AMP_RECEIVER_REPORT = 1 << 11;
        const SHORT_RANGE_MODE_CHANGE_COMPLETE = 1 << 12;
        const AMP_STATUS_CHANGE = 1 << 13;
        const TRIGGERED_CLOCK_CAPTURE = 1 << 14;
        const SYNCHRONIZATION_TRAIN_COMPLETE = 1 << 15;
        const SYNCHRONIZATION_TRAIN_RECEIVED = 1 << 16;
        const CONNECTIONLESS_PERIPHERAL_BROADCAST_RECEIVE = 1 << 17;
        const CONNECTIONLESS_PERIPHERAL_BROADCAST_TIMEOUT = 1 << 18;
        const TRUNCATED_PAGE_COMPLETE = 1 << 19;
        const PERIPHERAL_PAGE_RESPONSE_TIMEOUT = 1 << 20;
        const CONNECTIONLESS_PERIPHERAL_BROADCAST_CHANNEL_MAP_CHANGE = 1 << 21;
        const INQUIRY_RESPONSE_NOTIFICATION = 1 << 22;
        const AUTHENTICATED_PAYLOAD_TIMEOUT_EXPIRED = 1 << 23;
        const SAM_STATUS_CHANGE = 1 << 24;
        const ENCRYPTION_CHANGE_V2 = 1 << 25;
    }
}

impl Default for EventMaskPage2 {
    /// Returns an event mask with all bits, including reserved ones, set to 1.
    #[inline]
    fn default() -> Self {
        // SAFETY: Controller interprets reserved or unsupported bits as 0.
        unsafe { Self::from_bits_unchecked(u64::MAX) }
    }
}

/// Device connection role ([Vol 4] Part E, Sections 7.7.65.1 and 7.7.65.10).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive)]
#[repr(u8)]
pub enum Role {
    /// Device is acting as Central.
    Central = 0x00,
    /// Device is acting as Peripheral.
    Peripheral = 0x01,
}

bitflags! {
    /// Maskable LE events ([Vol 4] Part E, Section 7.8.1).
    #[repr(transparent)]
    pub struct LeEventMask: u64 {
        const CONNECTION_COMPLETE = 1 << 0;
        const ADVERTISING_REPORT = 1 << 1;
        const CONNECTION_UPDATE_COMPLETE = 1 << 2;
        const READ_REMOTE_FEATURES_COMPLETE = 1 << 3;
        const LONG_TERM_KEY_REQUEST = 1 << 4;
        const REMOTE_CONNECTION_PARAMETER_REQUEST = 1 << 5;
        const DATA_LENGTH_CHANGE = 1 << 6;
        const READ_LOCAL_P256_PUBLIC_KEY_COMPLETE = 1 << 7;
        const GENERATE_DHKEY_COMPLETE = 1 << 8;
        const ENHANCED_CONNECTION_COMPLETE = 1 << 9;
        const DIRECTED_ADVERTISING_REPORT = 1 << 10;
        const PHY_UPDATE_COMPLETE = 1 << 11;
        const EXTENDED_ADVERTISING_REPORT = 1 << 12;
        const PERIODIC_ADVERTISING_SYNC_ESTABLISHED = 1 << 13;
        const PERIODIC_ADVERTISING_REPORT = 1 << 14;
        const PERIODIC_ADVERTISING_SYNC_LOST = 1 << 15;
        const SCAN_TIMEOUT = 1 << 16;
        const ADVERTISING_SET_TERMINATED = 1 << 17;
        const SCAN_REQUEST_RECEIVED = 1 << 18;
        const CHANNEL_SELECTION_ALGORITHM = 1 << 19;
        const CONNECTIONLESS_IQ_REPORT = 1 << 20;
        const CONNECTION_IQ_REPORT = 1 << 21;
        const CTE_REQUEST_FAILED = 1 << 22;
        const PERIODIC_ADVERTISING_SYNC_TRANSFER_RECEIVED = 1 << 23;
        const CIS_ESTABLISHED = 1 << 24;
        const CIS_REQUEST = 1 << 25;
        const CREATE_BIG_COMPLETE = 1 << 26;
        const TERMINATE_BIG_COMPLETE = 1 << 27;
        const BIG_SYNC_ESTABLISHED = 1 << 28;
        const BIG_SYNC_LOST = 1 << 29;
        const REQUEST_PEER_SCA_COMPLETE = 1 << 30;
        const PATH_LOSS_THRESHOLD = 1 << 31;
        const TRANSMIT_POWER_REPORTING = 1 << 32;
        const BIGINFO_ADVERTISING_REPORT = 1 << 33;
        const SUBRATE_CHANGE = 1 << 34;
    }
}

impl Default for LeEventMask {
    /// Returns an event mask with all bits, including reserved ones, set to 1.
    #[inline]
    fn default() -> Self {
        // SAFETY: Controller interprets reserved or unsupported bits as 0.
        unsafe { Self::from_bits_unchecked(u64::MAX) }
    }
}

bitflags! {
    /// Basic properties of an advertising event
    /// ([Vol 4] Part E, Section 7.8.53).
    #[derive(Default)]
    #[repr(transparent)]
    pub struct AdvProp: u16 {
        const CONNECTABLE = 1 << 0;
        const SCANNABLE = 1 << 1;
        const DIRECTED = 1 << 2;
        const HIGH_DUTY_CYCLE = 1 << 3;
        const LEGACY = 1 << 4;
        const ANONYMOUS = 1 << 5;
        const INCLUDE_TX_POWER = 1 << 6;
    }
}

bitflags! {
    /// Channels used for transmitting advertising packets
    /// ([Vol 4] Part E, Section 7.8.53).
    #[repr(transparent)]
    pub struct AdvChanMap: u8 {
        const CH37 = 1 << 0;
        const CH38 = 1 << 1;
        const CH39 = 1 << 2;
    }
}

impl Default for AdvChanMap {
    #[inline]
    fn default() -> Self {
        Self::all()
    }
}

/// Type of address being used in an advertising packet
/// ([Vol 4] Part E, Section 7.8.53).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum AdvAddrType {
    /// Public Device Address.
    #[default]
    Public = 0x00,
    /// Random Device Address
    Random = 0x01,
    /// Controller generates the Resolvable Private Address based on the local
    /// IRK from the resolving list. If the resolving list contains no matching
    /// entry, use the public address.
    PrivateOrPublic = 0x02,
    /// Controller generates the Resolvable Private Address based on the local
    /// IRK from the resolving list. If the resolving list contains no matching
    /// entry, use the random address from
    /// `le_set_advertising_set_random_address()`.
    PrivateOrRandom = 0x03,
}

/// Type of filtering to perform for scan and connection requests
/// ([Vol 4] Part E, Section 7.8.53).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum AdvFilterPolicy {
    /// Process scan and connection requests from all devices (i.e., the Filter
    /// Accept List is not in use).
    #[default]
    None = 0x00,
    /// Process connection requests from all devices and scan requests only from
    /// devices that are in the Filter Accept List.
    FilterScan = 0x01,
    /// Process scan requests from all devices and connection requests only from
    /// devices that are in the Filter Accept List.
    FilterConnect = 0x02,
    /// Process scan and connection requests only from devices in the Filter
    /// Accept List.
    FilterAll = 0x03,
}

/// Physical layer for advertising. LE Coded assumes S=8
/// ([Vol 4] Part E, Section 7.8.53).
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, num_enum::IntoPrimitive)]
#[non_exhaustive]
#[repr(u8)]
pub enum AdvPhy {
    #[default]
    Le1M = 0x01,
    Le2M = 0x02,
    LeCoded = 0x03,
}

/// Defines the interpretation of advertising data
/// ([Vol 4] Part E, Section 7.8.54).
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum AdvDataOp {
    /// Intermediate fragment of fragmented extended advertising data.
    Cont = 0x00,
    /// First fragment of fragmented extended advertising data.
    First = 0x01,
    /// Last fragment of fragmented extended advertising data.
    Last = 0x02,
    /// Complete extended advertising data.
    Complete = 0x03,
    /// Unchanged data (just update the Advertising DID).
    Unchanged = 0x04,
}

/// Bluetooth Core Specification versions ([Assigned Numbers] Section 2.1).
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    Eq,
    Ord,
    PartialEq,
    PartialOrd,
    num_enum::FromPrimitive,
    num_enum::IntoPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum CoreVersion {
    V1_0b = 0x00,
    V1_1 = 0x01,
    V1_2 = 0x02,
    V2_0 = 0x03,
    V2_1 = 0x04,
    V3_0 = 0x05,
    V4_0 = 0x06,
    V4_1 = 0x07,
    V4_2 = 0x08,
    V5_0 = 0x09,
    V5_1 = 0x0A,
    V5_2 = 0x0B,
    V5_3 = 0x0C,
    #[default]
    Unknown = 0xFF,
}
