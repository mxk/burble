#![allow(clippy::use_self)]

use bitflags::bitflags;

use OpcodeGroup::*;

/// HCI command header and buffer sizes ([Vol 4] Part E, Section 5.4.1).
pub(super) const CMD_HDR: usize = 3;
pub(crate) const CMD_BUF: usize = CMD_HDR + u8::MAX as usize;

/// HCI ACL data header and buffer sizes ([Vol 4] Part E, Section 5.4.2).
pub(crate) const ACL_HDR: usize = 4;
pub(crate) const ACL_LE_MIN_DATA_LEN: u16 = 27;

/// HCI event header and buffer sizes ([Vol 4] Part E, Section 5.4.4).
pub(super) const EVT_HDR: usize = 2;
pub(crate) const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

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
    SetControllerToHostFlowControl = HciControl.ocf(0x0031),
    HostBufferSize = HciControl.ocf(0x0033),
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
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive, strum::EnumIter)]
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
    #[must_use]
    pub const fn param_fmt(self) -> EventFmt {
        use EventCode::*;
        const OTHER: EventFmt = EventFmt::empty();
        const STATUS: EventFmt = EventFmt::STATUS;
        const CONN_HANDLE: EventFmt = EventFmt::CONN_HANDLE;
        #[allow(clippy::match_same_arms)]
        match self {
            InquiryComplete => STATUS,
            InquiryResult => OTHER,
            ConnectionComplete => STATUS.union(CONN_HANDLE),
            ConnectionRequest => OTHER,
            DisconnectionComplete => STATUS.union(CONN_HANDLE),
            AuthenticationComplete => STATUS.union(CONN_HANDLE),
            RemoteNameRequestComplete => STATUS,
            EncryptionChangeV1 => STATUS.union(CONN_HANDLE),
            EncryptionChangeV2 => STATUS.union(CONN_HANDLE),
            ChangeConnectionLinkKeyComplete => STATUS.union(CONN_HANDLE),
            LinkKeyTypeChanged => STATUS.union(CONN_HANDLE),
            ReadRemoteSupportedFeaturesComplete => STATUS.union(CONN_HANDLE),
            ReadRemoteVersionInformationComplete => STATUS.union(CONN_HANDLE),
            QosSetupComplete => STATUS.union(CONN_HANDLE),
            CommandComplete => STATUS, // Other format, but want has_status() == true
            CommandStatus => STATUS,
            HardwareError => OTHER,
            FlushOccurred => CONN_HANDLE,
            RoleChange => STATUS,
            NumberOfCompletedPackets => OTHER,
            ModeChange => STATUS.union(CONN_HANDLE),
            ReturnLinkKeys => OTHER,
            PinCodeRequest => OTHER,
            LinkKeyRequest => OTHER,
            LinkKeyNotification => OTHER,
            LoopbackCommand => OTHER,
            DataBufferOverflow => OTHER,
            MaxSlotsChange => CONN_HANDLE,
            ReadClockOffsetComplete => STATUS.union(CONN_HANDLE),
            ConnectionPacketTypeChanged => STATUS.union(CONN_HANDLE),
            QosViolation => CONN_HANDLE,
            PageScanRepetitionModeChange => OTHER,
            FlowSpecificationComplete => STATUS.union(CONN_HANDLE),
            InquiryResultWithRssi => OTHER,
            ReadRemoteExtendedFeaturesComplete => STATUS.union(CONN_HANDLE),
            SynchronousConnectionComplete => STATUS.union(CONN_HANDLE),
            SynchronousConnectionChanged => STATUS.union(CONN_HANDLE),
            SniffSubrating => STATUS.union(CONN_HANDLE),
            ExtendedInquiryResult => OTHER,
            EncryptionKeyRefreshComplete => STATUS.union(CONN_HANDLE),
            IoCapabilityRequest => OTHER,
            IoCapabilityResponse => OTHER,
            UserConfirmationRequest => OTHER,
            UserPasskeyRequest => OTHER,
            RemoteOobDataRequest => OTHER,
            SimplePairingComplete => STATUS,
            LinkSupervisionTimeoutChanged => CONN_HANDLE,
            EnhancedFlushComplete => CONN_HANDLE,
            UserPasskeyNotification => OTHER,
            KeypressNotification => OTHER,
            RemoteHostSupportedFeaturesNotification => OTHER,
            NumberOfCompletedDataBlocks => OTHER,
            LeMetaEvent => OTHER,
            TriggeredClockCapture => CONN_HANDLE,
            SynchronizationTrainComplete => STATUS,
            SynchronizationTrainReceived => STATUS,
            ConnectionlessPeripheralBroadcastReceive => OTHER,
            ConnectionlessPeripheralBroadcastTimeout => OTHER,
            TruncatedPageComplete => STATUS,
            PeripheralPageResponseTimeout => OTHER,
            ConnectionlessPeripheralBroadcastChannelMapChange => OTHER,
            InquiryResponseNotification => OTHER,
            AuthenticatedPayloadTimeoutExpired => CONN_HANDLE,
            SamStatusChange => CONN_HANDLE,
            Vendor => OTHER,
        }
    }

    /// Returns the event mask for `HCI_Set_Event_Mask` and
    /// `HCI_Set_Event_Mask_Page_2` commands, or 0 if the event is not valid for
    /// the specified 1-based `page`.
    #[must_use]
    pub(super) const fn mask(self, page: u8) -> u64 {
        use EventCode::*;
        let (pg, bit) = match self {
            // Page 1 ([Vol 4] Part E, Section 7.3.1)
            InquiryComplete => (1, 0),
            InquiryResult => (1, 1),
            ConnectionComplete => (1, 2),
            ConnectionRequest => (1, 3),
            DisconnectionComplete => (1, 4),
            AuthenticationComplete => (1, 5),
            RemoteNameRequestComplete => (1, 6),
            EncryptionChangeV1 => (1, 7),
            ChangeConnectionLinkKeyComplete => (1, 8),
            LinkKeyTypeChanged => (1, 9),
            ReadRemoteSupportedFeaturesComplete => (1, 10),
            ReadRemoteVersionInformationComplete => (1, 11),
            QosSetupComplete => (1, 12),
            HardwareError => (1, 15),
            FlushOccurred => (1, 16),
            RoleChange => (1, 17),
            ModeChange => (1, 19),
            ReturnLinkKeys => (1, 20),
            PinCodeRequest => (1, 21),
            LinkKeyRequest => (1, 22),
            LinkKeyNotification => (1, 23),
            LoopbackCommand => (1, 24),
            DataBufferOverflow => (1, 25),
            MaxSlotsChange => (1, 26),
            ReadClockOffsetComplete => (1, 27),
            ConnectionPacketTypeChanged => (1, 28),
            QosViolation => (1, 29),
            PageScanRepetitionModeChange => (1, 31),
            FlowSpecificationComplete => (1, 32),
            InquiryResultWithRssi => (1, 33),
            ReadRemoteExtendedFeaturesComplete => (1, 34),
            SynchronousConnectionComplete => (1, 43),
            SynchronousConnectionChanged => (1, 44),
            SniffSubrating => (1, 45),
            ExtendedInquiryResult => (1, 46),
            EncryptionKeyRefreshComplete => (1, 47),
            IoCapabilityRequest => (1, 48),
            IoCapabilityResponse => (1, 49),
            UserConfirmationRequest => (1, 50),
            UserPasskeyRequest => (1, 51),
            RemoteOobDataRequest => (1, 52),
            SimplePairingComplete => (1, 53),
            LinkSupervisionTimeoutChanged => (1, 55),
            EnhancedFlushComplete => (1, 56),
            UserPasskeyNotification => (1, 58),
            KeypressNotification => (1, 59),
            RemoteHostSupportedFeaturesNotification => (1, 60),
            LeMetaEvent => (1, 61),

            // Page 2 ([Vol 4] Part E, Section 7.3.69)
            NumberOfCompletedDataBlocks => (2, 8),
            TriggeredClockCapture => (2, 14),
            SynchronizationTrainComplete => (2, 15),
            SynchronizationTrainReceived => (2, 16),
            ConnectionlessPeripheralBroadcastReceive => (2, 17),
            ConnectionlessPeripheralBroadcastTimeout => (2, 18),
            TruncatedPageComplete => (2, 19),
            PeripheralPageResponseTimeout => (2, 20),
            ConnectionlessPeripheralBroadcastChannelMapChange => (2, 21),
            InquiryResponseNotification => (2, 22),
            AuthenticatedPayloadTimeoutExpired => (2, 23),
            SamStatusChange => (2, 24),
            EncryptionChangeV2 => (2, 25),

            // Unmaskable events
            CommandComplete | CommandStatus | NumberOfCompletedPackets | Vendor => (0, 0),
        };
        ((page == pg) as u64) << bit
    }
}

/// HCI LE subevent codes ([Vol 4] Part E, Section 7.7.65).
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive, strum::EnumIter)]
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
    #[must_use]
    pub const fn param_fmt(self) -> EventFmt {
        use SubeventCode::*;
        const OTHER: EventFmt = EventFmt::empty();
        const STATUS: EventFmt = EventFmt::STATUS;
        const CONN_HANDLE: EventFmt = EventFmt::CONN_HANDLE;
        const SYNC_HANDLE: EventFmt = EventFmt::SYNC_HANDLE;
        const ADV_HANDLE: EventFmt = EventFmt::ADV_HANDLE;
        const BIG_HANDLE: EventFmt = EventFmt::BIG_HANDLE;
        #[allow(clippy::match_same_arms)]
        match self {
            ConnectionComplete => STATUS.union(CONN_HANDLE),
            AdvertisingReport => OTHER,
            ConnectionUpdateComplete => STATUS.union(CONN_HANDLE),
            ReadRemoteFeaturesComplete => STATUS.union(CONN_HANDLE),
            LongTermKeyRequest => CONN_HANDLE,
            RemoteConnectionParameterRequest => CONN_HANDLE,
            DataLengthChange => CONN_HANDLE,
            ReadLocalP256PublicKeyComplete => STATUS,
            GenerateDhKeyComplete => STATUS,
            EnhancedConnectionComplete => STATUS.union(CONN_HANDLE),
            DirectedAdvertisingReport => OTHER,
            PhyUpdateComplete => STATUS.union(CONN_HANDLE),
            ExtendedAdvertisingReport => OTHER,
            PeriodicAdvertisingSyncEstablished => STATUS.union(SYNC_HANDLE),
            PeriodicAdvertisingReport => SYNC_HANDLE,
            PeriodicAdvertisingSyncLost => SYNC_HANDLE,
            ScanTimeout => OTHER,
            AdvertisingSetTerminated => STATUS.union(ADV_HANDLE),
            ScanRequestReceived => ADV_HANDLE,
            ChannelSelectionAlgorithm => CONN_HANDLE,
            ConnectionlessIqReport => SYNC_HANDLE,
            ConnectionIqReport => CONN_HANDLE,
            CteRequestFailed => STATUS.union(CONN_HANDLE),
            PeriodicAdvertisingSyncTransferReceived => STATUS.union(CONN_HANDLE),
            CisEstablished => STATUS.union(CONN_HANDLE),
            CisRequest => CONN_HANDLE,
            CreateBigComplete => STATUS.union(BIG_HANDLE),
            TerminateBigComplete => BIG_HANDLE,
            BigSyncEstablished => STATUS.union(BIG_HANDLE),
            BigSyncLost => BIG_HANDLE,
            RequestPeerScaComplete => STATUS.union(CONN_HANDLE),
            PathLossThreshold => CONN_HANDLE,
            TransmitPowerReporting => STATUS.union(CONN_HANDLE),
            BigInfoAdvertisingReport => SYNC_HANDLE,
            SubrateChange => STATUS.union(CONN_HANDLE),
        }
    }

    /// Returns the event mask for the `HCI_LE_Set_Event_Mask` command.
    #[must_use]
    pub(super) const fn mask(self) -> u64 {
        use SubeventCode::*;
        1 << match self {
            // Page 1 ([Vol 4] Part E, Section 7.8.1)
            ConnectionComplete => 0,
            AdvertisingReport => 1,
            ConnectionUpdateComplete => 2,
            ReadRemoteFeaturesComplete => 3,
            LongTermKeyRequest => 4,
            RemoteConnectionParameterRequest => 5,
            DataLengthChange => 6,
            ReadLocalP256PublicKeyComplete => 7,
            GenerateDhKeyComplete => 8,
            EnhancedConnectionComplete => 9,
            DirectedAdvertisingReport => 10,
            PhyUpdateComplete => 11,
            ExtendedAdvertisingReport => 12,
            PeriodicAdvertisingSyncEstablished => 13,
            PeriodicAdvertisingReport => 14,
            PeriodicAdvertisingSyncLost => 15,
            ScanTimeout => 16,
            AdvertisingSetTerminated => 17,
            ScanRequestReceived => 18,
            ChannelSelectionAlgorithm => 19,
            ConnectionlessIqReport => 20,
            ConnectionIqReport => 21,
            CteRequestFailed => 22,
            PeriodicAdvertisingSyncTransferReceived => 23,
            CisEstablished => 24,
            CisRequest => 25,
            CreateBigComplete => 26,
            TerminateBigComplete => 27,
            BigSyncEstablished => 28,
            BigSyncLost => 29,
            RequestPeerScaComplete => 30,
            PathLossThreshold => 31,
            TransmitPowerReporting => 32,
            BigInfoAdvertisingReport => 33,
            SubrateChange => 34,
        }
    }
}

bitflags! {
    /// Event parameter format.
    #[repr(transparent)]
    pub struct EventFmt: u8 {
        /// Event contains a status parameter.
        const STATUS = 1 << 0;
        /// Event contains a connection handle.
        const CONN_HANDLE = 1 << 1;
        /// Event contains a periodic advertising handle.
        const SYNC_HANDLE = 1 << 2;
        /// Event contains an advertising handle.
        const ADV_HANDLE = 1 << 3;
        /// Event contains a BIG handle.
        const BIG_HANDLE = 1 << 4;
        /// Handle type mask ([Vol 4] Part E, Section 5.3.1)
        const HANDLE = Self::CONN_HANDLE.bits | Self::SYNC_HANDLE.bits |
                       Self::ADV_HANDLE.bits | Self::BIG_HANDLE.bits;
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

/// Returns the company name associated with `id`
/// ([Assigned Numbers] Section 3.11).
#[allow(clippy::match_same_arms, clippy::too_many_lines)]
#[must_use]
pub const fn company_name(id: u16) -> Option<&'static str> {
    // TODO: Concatenate all company names and use more compact O(1) indexing
    Some(match id {
        0xFCC7 => "PB INC.",
        0xFCC8 => "Allthenticate, Inc.",
        0xFCC9 => "SkyHawke Technologies",
        0xFCCA => "Cosmed s.r.l.",
        0xFCCB => "TOTO LTD.",
        0xFCCC => "WiFi Alliance",
        0xFCCD => "Zound Industries International AB",
        0xFCCE => "Luna Health, Inc.",
        0xFCCF => "Google LLC",
        0xFCD0 => "Laerdal Medical AS",
        0xFCD1 => "Shenzhen Benwei Media Co.,Ltd.",
        0xFCD2 => "Allterco Robotics ltd",
        0xFCD3 => "Fisher & Paykel Healthcare",
        0xFCD4 => "OMRON HEALTHCARE",
        0xFCD5 => "Nortek Security & Control",
        0xFCD6 => "SWISSINNO SOLUTIONS AG",
        0xFCD7 => "PowerPal Pty Ltd",
        0xFCD8 => "Appex Factory S.L.",
        0xFCD9 => "Huso, INC",
        0xFCDA => "Draeger",
        0xFCDB => "aconno GmbH",
        0xFCDC => "Amazon.com Services, LLC",
        0xFCDD => "Mobilaris AB",
        0xFCDE => "ARCTOP, INC.",
        0xFCDF => "NIO USA, Inc.",
        0xFCE0 => "Akciju sabiedriba \"SAF TEHNIKA\"",
        0xFCE1 => "Sony Group Corporation",
        0xFCE2 => "Baracoda Daily Healthtech",
        0xFCE3 => "Smith & Nephew Medical Limited",
        0xFCE4 => "Samsara Networks, Inc",
        0xFCE5 => "Samsara Networks, Inc",
        0xFCE6 => "Guard RFID Solutions Inc.",
        0xFCE7 => "TKH Security B.V.",
        0xFCE8 => "ITT Industries",
        0xFCE9 => "MindRhythm, Inc.",
        0xFCEA => "Chess Wise B.V.",
        0xFCEB => "AviOn",
        0xFCEC => "Griffwerk GmbH",
        0xFCED => "Workaround Gmbh",
        0xFCEE => "Velentium, LLC",
        0xFCEF => "Divesoft s.r.o.",
        0xFCF0 => "Security Enhancement Systems, LLC",
        0xFCF1 => "Google LLC",
        0xFCF2 => "Bitwards Oy",
        0xFCF3 => "Armatura LLC",
        0xFCF4 => "Allegion",
        0xFCF5 => "Trident Communication Technology, LLC",
        0xFCF6 => "The Linux Foundation",
        0xFCF7 => "Honor Device Co., Ltd.",
        0xFCF8 => "Honor Device Co., Ltd.",
        0xFCF9 => "Leupold & Stevens, Inc.",
        0xFCFA => "Leupold & Stevens, Inc.",
        0xFCFB => "Shenzhen Benwei Media Co., Ltd.",
        0xFCFC => "Barrot Technology Limited",
        0xFCFD => "Barrot Technology Limited",
        0xFCFE => "Sennheiser Consumer Audio GmbH",
        0xFCFF => "701x",
        0xFD00 => "FUTEK Advanced Sensor Technology, Inc.",
        0xFD01 => "Sanvita Medical Corporation",
        0xFD02 => "LEGO System A/S",
        0xFD03 => "Quuppa Oy",
        0xFD04 => "Shure Inc.",
        0xFD05 => "Qualcomm Technologies, Inc.",
        0xFD06 => "RACEAI LLC",
        0xFD07 => "Swedlock AB",
        0xFD08 => "Bull Group Incorporated Company",
        0xFD09 => "Cousins and Sears LLC",
        0xFD0A => "Luminostics, Inc.",
        0xFD0B => "Luminostics, Inc.",
        0xFD0C => "OSM HK Limited",
        0xFD0D => "Blecon Ltd",
        0xFD0E => "HerdDogg, Inc",
        0xFD0F => "AEON MOTOR CO.,LTD.",
        0xFD10 => "AEON MOTOR CO.,LTD.",
        0xFD11 => "AEON MOTOR CO.,LTD.",
        0xFD12 => "AEON MOTOR CO.,LTD.",
        0xFD13 => "BRG Sports, Inc.",
        0xFD14 => "BRG Sports, Inc.",
        0xFD15 => "Panasonic Corporation",
        0xFD16 => "Sensitech, Inc.",
        0xFD17 => "LEGIC Identsystems AG",
        0xFD18 => "LEGIC Identsystems AG",
        0xFD19 => "Smith & Nephew Medical Limited",
        0xFD1A => "CSIRO",
        0xFD1B => "Helios Sports, Inc.",
        0xFD1C => "Brady Worldwide Inc.",
        0xFD1D => "Samsung Electronics Co., Ltd",
        0xFD1E => "Plume Design Inc.",
        0xFD1F => "3M",
        0xFD20 => "GN Hearing A/S",
        0xFD21 => "Huawei Technologies Co., Ltd.",
        0xFD22 => "Huawei Technologies Co., Ltd.",
        0xFD23 => "DOM Sicherheitstechnik GmbH & Co. KG",
        0xFD24 => "GD Midea AirConditioning Equipment Co., Ltd.",
        0xFD25 => "GD Midea AirConditioning Equipment Co., Ltd.",
        0xFD26 => "Novo Nordisk A/S",
        0xFD27 => "i2Systems",
        0xFD28 => "Julius Blum GmbH",
        0xFD29 => "Asahi Kasei Corporation",
        0xFD2A => "Sony Corporation",
        0xFD2B => "The Access Technologies",
        0xFD2C => "The Access Technologies",
        0xFD2D => "Xiaomi Inc.",
        0xFD2E => "Bitstrata Systems Inc.",
        0xFD2F => "Bitstrata Systems Inc.",
        0xFD30 => "Sesam Solutions BV",
        0xFD31 => "LG Electronics Inc.",
        0xFD32 => "Gemalto Holding BV",
        0xFD33 => "DashLogic, Inc.",
        0xFD34 => "Aerosens LLC.",
        0xFD35 => "Transsion Holdings Limited",
        0xFD36 => "Google LLC",
        0xFD37 => "TireCheck GmbH",
        0xFD38 => "Danfoss A/S",
        0xFD39 => "PREDIKTAS",
        0xFD3A => "Verkada Inc.",
        0xFD3B => "Verkada Inc.",
        0xFD3C => "Redline Communications Inc.",
        0xFD3D => "Woan Technology (Shenzhen) Co., Ltd.",
        0xFD3E => "Pure Watercraft, inc.",
        0xFD3F => "Cognosos, Inc",
        0xFD40 => "Beflex Inc.",
        0xFD41 => "Amazon Lab126",
        0xFD42 => "Globe (Jiangsu) Co.,Ltd",
        0xFD43 => "Apple Inc.",
        0xFD44 => "Apple Inc.",
        0xFD45 => "GB Solution co.,Ltd",
        0xFD46 => "Lemco IKE",
        0xFD47 => "Liberty Global Inc.",
        0xFD48 => "Geberit International AG",
        0xFD49 => "Panasonic Corporation",
        0xFD4A => "Sigma Elektro GmbH",
        0xFD4B => "Samsung Electronics Co., Ltd.",
        0xFD4C => "Adolf Wuerth GmbH & Co KG",
        0xFD4D => "70mai Co.,Ltd.",
        0xFD4E => "70mai Co.,Ltd.",
        0xFD4F => "Forkbeard Technologies AS",
        0xFD50 => "Hangzhou Tuya Information Technology Co., Ltd",
        0xFD51 => "UTC Fire and Security",
        0xFD52 => "UTC Fire and Security",
        0xFD53 => "PCI Private Limited",
        0xFD54 => "Qingdao Haier Technology Co., Ltd.",
        0xFD55 => "Braveheart Wireless, Inc.",
        0xFD56 => "Resmed Ltd",
        0xFD57 => "Volvo Car Corporation",
        0xFD58 => "Volvo Car Corporation",
        0xFD59 => "Samsung Electronics Co., Ltd.",
        0xFD5A => "Samsung Electronics Co., Ltd.",
        0xFD5B => "V2SOFT INC.",
        0xFD5C => "React Mobile",
        0xFD5D => "maxon motor ltd.",
        0xFD5E => "Tapkey GmbH",
        0xFD5F => "Oculus VR, LLC",
        0xFD60 => "Sercomm Corporation",
        0xFD61 => "Arendi AG",
        0xFD62 => "Fitbit, Inc.",
        0xFD63 => "Fitbit, Inc.",
        0xFD64 => "INRIA",
        0xFD65 => "Razer Inc.",
        0xFD66 => "Zebra Technologies Corporation",
        0xFD67 => "Montblanc Simplo GmbH",
        0xFD68 => "Ubique Innovation AG",
        0xFD69 => "Samsung Electronics Co., Ltd",
        0xFD6A => "Emerson",
        0xFD6B => "rapitag GmbH",
        0xFD6C => "Samsung Electronics Co., Ltd.",
        0xFD6D => "Sigma Elektro GmbH",
        0xFD6E => "Polidea sp. z o.o.",
        0xFD6F => "Apple, Inc.",
        0xFD70 => "GuangDong Oppo Mobile Telecommunications Corp., Ltd",
        0xFD71 => "GN Hearing A/S",
        0xFD72 => "Logitech International SA",
        0xFD73 => "BRControls Products BV",
        0xFD74 => "BRControls Products BV",
        0xFD75 => "Insulet Corporation",
        0xFD76 => "Insulet Corporation",
        0xFD77 => "Withings",
        0xFD78 => "Withings",
        0xFD79 => "Withings",
        0xFD7A => "Withings",
        0xFD7B => "WYZE LABS, INC.",
        0xFD7C => "Toshiba Information Systems(Japan) Corporation",
        0xFD7D => "Center for Advanced Research Wernher Von Braun",
        0xFD7E => "Samsung Electronics Co., Ltd.",
        0xFD7F => "Husqvarna AB",
        0xFD80 => "Phindex Technologies, Inc",
        0xFD81 => "CANDY HOUSE, Inc.",
        0xFD82 => "Sony Corporation",
        0xFD83 => "iNFORM Technology GmbH",
        0xFD84 => "Tile, Inc.",
        0xFD85 => "Husqvarna AB",
        0xFD86 => "Abbott",
        0xFD87 => "Google LLC",
        0xFD88 => "Urbanminded LTD",
        0xFD89 => "Urbanminded LTD",
        0xFD8A => "Signify Netherlands B.V.",
        0xFD8B => "Jigowatts Inc.",
        0xFD8C => "Google LLC",
        0xFD8D => "quip NYC Inc.",
        0xFD8E => "Motorola Solutions",
        0xFD8F => "Matrix ComSec Pvt. Ltd.",
        0xFD90 => "Guangzhou SuperSound Information Technology Co.,Ltd",
        0xFD91 => "Groove X, Inc.",
        0xFD92 => "Qualcomm Technologies International, Ltd. (QTIL)",
        0xFD93 => "Bayerische Motoren Werke AG",
        0xFD94 => "Hewlett Packard Enterprise",
        0xFD95 => "Rigado",
        0xFD96 => "Google LLC",
        0xFD97 => "June Life, Inc.",
        0xFD98 => "Disney Worldwide Services, Inc.",
        0xFD99 => "ABB Oy",
        0xFD9A => "Huawei Technologies Co., Ltd.",
        0xFD9B => "Huawei Technologies Co., Ltd.",
        0xFD9C => "Huawei Technologies Co., Ltd.",
        0xFD9D => "Gastec Corporation",
        0xFD9E => "The CocaCola Company",
        0xFD9F => "VitalTech Affiliates LLC",
        0xFDA0 => "Secugen Corporation",
        0xFDA1 => "Groove X, Inc",
        0xFDA2 => "Groove X, Inc",
        0xFDA3 => "Inseego Corp.",
        0xFDA4 => "Inseego Corp.",
        0xFDA5 => "Neurostim OAB, Inc.",
        0xFDA6 => "WWZN Information Technology Company Limited",
        0xFDA7 => "WWZN Information Technology Company Limited",
        0xFDA8 => "PSA Peugeot Citroën",
        0xFDA9 => "Rhombus Systems, Inc.",
        0xFDAA => "Xiaomi Inc.",
        0xFDAB => "Xiaomi Inc.",
        0xFDAC => "Tentacle Sync GmbH",
        0xFDAD => "Houwa System Design, k.k.",
        0xFDAE => "Houwa System Design, k.k.",
        0xFDAF => "Wiliot LTD",
        0xFDB0 => "Proxy Technologies, Inc.",
        0xFDB1 => "Proxy Technologies, Inc.",
        0xFDB2 => "Portable Multimedia Ltd",
        0xFDB3 => "Audiodo AB",
        0xFDB4 => "HP Inc",
        0xFDB5 => "ECSG",
        0xFDB6 => "GWA Hygiene GmbH",
        0xFDB7 => "LivaNova USA Inc.",
        0xFDB8 => "LivaNova USA Inc.",
        0xFDB9 => "Comcast Cable Corporation",
        0xFDBA => "Comcast Cable Corporation",
        0xFDBB => "Profoto",
        0xFDBC => "Emerson",
        0xFDBD => "Clover Network, Inc.",
        0xFDBE => "California Things Inc.",
        0xFDBF => "California Things Inc.",
        0xFDC0 => "Hunter Douglas",
        0xFDC1 => "Hunter Douglas",
        0xFDC2 => "Baidu Online Network Technology (Beijing) Co., Ltd",
        0xFDC3 => "Baidu Online Network Technology (Beijing) Co., Ltd",
        0xFDC4 => "Simavita (Aust) Pty Ltd",
        0xFDC5 => "Automatic Labs",
        0xFDC6 => "Eli Lilly and Company",
        0xFDC7 => "Eli Lilly and Company",
        0xFDC8 => "Hach – Danaher",
        0xFDC9 => "BuschJaeger Elektro GmbH",
        0xFDCA => "Fortin Electronic Systems",
        0xFDCB => "Meggitt SA",
        0xFDCC => "Shoof Technologies",
        0xFDCD => "Qingping Technology (Beijing) Co., Ltd.",
        0xFDCE => "SENNHEISER electronic GmbH & Co. KG",
        0xFDCF => "Nalu Medical, Inc",
        0xFDD0 => "Huawei Technologies Co., Ltd",
        0xFDD1 => "Huawei Technologies Co., Ltd",
        0xFDD2 => "Bose Corporation",
        0xFDD3 => "FUBA Automotive Electronics GmbH",
        0xFDD4 => "LX Solutions Pty Limited",
        0xFDD5 => "Brompton Bicycle Ltd",
        0xFDD6 => "Ministry of Supply",
        0xFDD7 => "Emerson",
        0xFDD8 => "Jiangsu Teranovo Tech Co., Ltd.",
        0xFDD9 => "Jiangsu Teranovo Tech Co., Ltd.",
        0xFDDA => "MHCS",
        0xFDDB => "Samsung Electronics Co., Ltd.",
        0xFDDC => "4iiii Innovations Inc.",
        0xFDDD => "Arch Systems Inc",
        0xFDDE => "Noodle Technology Inc.",
        0xFDDF => "Harman International",
        0xFDE0 => "John Deere",
        0xFDE1 => "Fortin Electronic Systems",
        0xFDE2 => "Google LLC",
        0xFDE3 => "Abbott Diabetes Care",
        0xFDE4 => "JUUL Labs, Inc.",
        0xFDE5 => "SMK Corporation",
        0xFDE6 => "Intelletto Technologies Inc",
        0xFDE7 => "SECOM Co., LTD",
        0xFDE8 => "Robert Bosch GmbH",
        0xFDE9 => "Spacesaver Corporation",
        0xFDEA => "SeeScan, Inc",
        0xFDEB => "Syntronix Corporation",
        0xFDEC => "Mannkind Corporation",
        0xFDED => "Pole Star",
        0xFDEE => "Huawei Technologies Co., Ltd.",
        0xFDEF => "ART AND PROGRAM, INC.",
        0xFDF0 => "Google LLC",
        0xFDF1 => "LAMPLIGHT Co.,Ltd",
        0xFDF2 => "AMICCOM Electronics Corporation",
        0xFDF3 => "Amersports",
        0xFDF4 => "O. E. M. Controls, Inc.",
        0xFDF5 => "Milwaukee Electric Tools",
        0xFDF6 => "AIAIAI ApS",
        0xFDF7 => "HP Inc.",
        0xFDF8 => "Onvocal",
        0xFDF9 => "INIA",
        0xFDFA => "Tandem Diabetes Care",
        0xFDFB => "Tandem Diabetes Care",
        0xFDFC => "Optrel AG",
        0xFDFD => "RecursiveSoft Inc.",
        0xFDFE => "ADHERIUM(NZ) LIMITED",
        0xFDFF => "OSRAM GmbH",
        0xFE00 => "Amazon.com Services, Inc.",
        0xFE01 => "Duracell U.S. Operations Inc.",
        0xFE02 => "Robert Bosch GmbH",
        0xFE03 => "Amazon.com Services, Inc.",
        0xFE04 => "OpenPath Security Inc",
        0xFE05 => "CORE Transport Technologies NZ Limited",
        0xFE06 => "Qualcomm Technologies, Inc.",
        0xFE07 => "Sonos, Inc.",
        0xFE08 => "Microsoft",
        0xFE09 => "Pillsy, Inc.",
        0xFE0A => "ruwido austria gmbh",
        0xFE0B => "ruwido austria gmbh",
        0xFE0C => "Procter & Gamble",
        0xFE0D => "Procter & Gamble",
        0xFE0E => "Setec Pty Ltd",
        0xFE0F => "Signify Netherlands B.V. (formerly Philips Lighting B.V.)",
        0xFE10 => "LAPIS Technology Co., Ltd.",
        0xFE11 => "GMCI Messtechnik GmbH",
        0xFE12 => "MWay Solutions GmbH",
        0xFE13 => "Apple Inc.",
        0xFE14 => "Flextronics International USA Inc.",
        0xFE15 => "Amazon.com Services, Inc..",
        0xFE16 => "Footmarks, Inc.",
        0xFE17 => "Telit Wireless Solutions GmbH",
        0xFE18 => "Runtime, Inc.",
        0xFE19 => "Google LLC",
        0xFE1A => "Tyto Life LLC",
        0xFE1B => "Tyto Life LLC",
        0xFE1C => "NetMedia, Inc.",
        0xFE1D => "Illuminati Instrument Corporation",
        0xFE1E => "Smart Innovations Co., Ltd",
        0xFE1F => "Garmin International, Inc.",
        0xFE20 => "Emerson",
        0xFE21 => "Bose Corporation",
        0xFE22 => "Zoll Medical Corporation",
        0xFE23 => "Zoll Medical Corporation",
        0xFE24 => "August Home Inc",
        0xFE25 => "Apple, Inc.",
        0xFE26 => "Google LLC",
        0xFE27 => "Google LLC",
        0xFE28 => "Ayla Networks",
        0xFE29 => "Gibson Innovations",
        0xFE2A => "DaisyWorks, Inc.",
        0xFE2B => "ITT Industries",
        0xFE2C => "Google LLC",
        0xFE2D => "SMART INNOVATION Co.,Ltd",
        0xFE2E => "ERi,Inc.",
        0xFE2F => "CRESCO Wireless, Inc",
        0xFE30 => "Volkswagen AG",
        0xFE31 => "Volkswagen AG",
        0xFE32 => "ProMark, Inc.",
        0xFE33 => "CHIPOLO d.o.o.",
        0xFE34 => "SmallLoop LLC",
        0xFE35 => "HUAWEI Technologies Co., Ltd",
        0xFE36 => "HUAWEI Technologies Co., Ltd",
        0xFE37 => "Spaceek LTD",
        0xFE38 => "Spaceek LTD",
        0xFE39 => "TTS Tooltechnic Systems AG & Co. KG",
        0xFE3A => "TTS Tooltechnic Systems AG & Co. KG",
        0xFE3B => "Dolby Laboratories",
        0xFE3C => "alibaba",
        0xFE3D => "BD Medical",
        0xFE3E => "BD Medical",
        0xFE3F => "Friday Labs Limited",
        0xFE40 => "Inugo Systems Limited",
        0xFE41 => "Inugo Systems Limited",
        0xFE42 => "Nets A/S",
        0xFE43 => "Andreas Stihl AG & Co. KG",
        0xFE44 => "SK Telecom",
        0xFE45 => "Snapchat Inc",
        0xFE46 => "B&O Play A/S",
        0xFE47 => "General Motors",
        0xFE48 => "General Motors",
        0xFE49 => "SenionLab AB",
        0xFE4A => "OMRON HEALTHCARE Co., Ltd.",
        0xFE4B => "Signify Netherlands B.V. (formerly Philips Lighting B.V.)",
        0xFE4C => "Volkswagen AG",
        0xFE4D => "Casambi Technologies Oy",
        0xFE4E => "NTT docomo",
        0xFE4F => "Molekule, Inc.",
        0xFE50 => "Google LLC",
        0xFE51 => "SRAM",
        0xFE52 => "SetPoint Medical",
        0xFE53 => "3M",
        0xFE54 => "Motiv, Inc.",
        0xFE55 => "Google LLC",
        0xFE56 => "Google LLC",
        0xFE57 => "Dotted Labs",
        0xFE58 => "Nordic Semiconductor ASA",
        0xFE59 => "Nordic Semiconductor ASA",
        0xFE5A => "Cronologics Corporation",
        0xFE5B => "GTtronics HK Ltd",
        0xFE5C => "million hunters GmbH",
        0xFE5D => "Grundfos A/S",
        0xFE5E => "Plastc Corporation",
        0xFE5F => "Eyefi, Inc.",
        0xFE60 => "Lierda Science & Technology Group Co., Ltd.",
        0xFE61 => "Logitech International SA",
        0xFE62 => "Indagem Tech LLC",
        0xFE63 => "Connected Yard, Inc.",
        0xFE64 => "Siemens AG",
        0xFE65 => "CHIPOLO d.o.o.",
        0xFE66 => "Intel Corporation",
        0xFE67 => "Lab Sensor Solutions",
        0xFE68 => "Qualcomm Life Inc",
        0xFE69 => "Qualcomm Life Inc",
        0xFE6A => "Kontakt MicroLocation Sp. z o.o.",
        0xFE6B => "TASER International, Inc.",
        0xFE6C => "TASER International, Inc.",
        0xFE6D => "The University of Tokyo",
        0xFE6E => "The University of Tokyo",
        0xFE6F => "LINE Corporation",
        0xFE70 => "Beijing Jingdong Century Trading Co., Ltd.",
        0xFE71 => "Plume Design Inc",
        0xFE72 => "Abbott (formerly St. Jude Medical, Inc.)",
        0xFE73 => "Abbott (formerly St. Jude Medical, Inc.)",
        0xFE74 => "unwire",
        0xFE75 => "TangoMe",
        0xFE76 => "TangoMe",
        0xFE77 => "HewlettPackard Company",
        0xFE78 => "HewlettPackard Company",
        0xFE79 => "Zebra Technologies",
        0xFE7A => "Bragi GmbH",
        0xFE7B => "Orion Labs, Inc.",
        0xFE7C => "Telit Wireless Solutions (Formerly Stollmann E+V GmbH)",
        0xFE7D => "Aterica Health Inc.",
        0xFE7E => "Awear Solutions Ltd",
        0xFE7F => "Doppler Lab",
        0xFE80 => "Doppler Lab",
        0xFE81 => "Medtronic Inc.",
        0xFE82 => "Medtronic Inc.",
        0xFE83 => "Blue Bite",
        0xFE84 => "RF Digital Corp",
        0xFE85 => "RF Digital Corp",
        0xFE86 => "HUAWEI Technologies Co., Ltd",
        0xFE87 => "Qingdao Yeelink Information Technology Co., Ltd.",
        0xFE88 => "SALTO SYSTEMS S.L.",
        0xFE89 => "B&O Play A/S",
        0xFE8A => "Apple, Inc.",
        0xFE8B => "Apple, Inc.",
        0xFE8C => "TRON Forum",
        0xFE8D => "Interaxon Inc.",
        0xFE8E => "ARM Ltd",
        0xFE8F => "CSR",
        0xFE90 => "JUMA",
        0xFE91 => "Shanghai Imilab Technology Co.,Ltd",
        0xFE92 => "Jarden Safety & Security",
        0xFE93 => "OttoQ In",
        0xFE94 => "OttoQ In",
        0xFE95 => "Xiaomi Inc.",
        0xFE96 => "Tesla Motors Inc.",
        0xFE97 => "Tesla Motors Inc.",
        0xFE98 => "Currant Inc",
        0xFE99 => "Currant Inc",
        0xFE9A => "Estimote",
        0xFE9B => "Samsara Networks, Inc",
        0xFE9C => "GSI Laboratories, Inc.",
        0xFE9D => "Mobiquity Networks Inc",
        0xFE9E => "Dialog Semiconductor B.V.",
        0xFE9F => "Google LLC",
        0xFEA0 => "Google LLC",
        0xFEA1 => "Intrepid Control Systems, Inc.",
        0xFEA2 => "Intrepid Control Systems, Inc.",
        0xFEA3 => "ITT Industries",
        0xFEA4 => "Paxton Access Ltd",
        0xFEA5 => "GoPro, Inc.",
        0xFEA6 => "GoPro, Inc.",
        0xFEA7 => "UTC Fire and Security",
        0xFEA8 => "Savant Systems LLC",
        0xFEA9 => "Savant Systems LLC",
        0xFEAA => "Google LLC",
        0xFEAB => "Nokia",
        0xFEAC => "Nokia",
        0xFEAD => "Nokia",
        0xFEAE => "Nokia",
        0xFEAF => "Nest Labs Inc",
        0xFEB0 => "Nest Labs Inc",
        0xFEB1 => "Electronics Tomorrow Limited",
        0xFEB2 => "Microsoft Corporation",
        0xFEB3 => "Taobao",
        0xFEB4 => "WiSilica Inc.",
        0xFEB5 => "WiSilica Inc.",
        0xFEB6 => "Vencer Co., Ltd",
        0xFEB7 => "Meta Platforms, Inc.",
        0xFEB8 => "Meta Platforms, Inc.",
        0xFEB9 => "LG Electronics",
        0xFEBA => "Tencent Holdings Limited",
        0xFEBB => "adafruit industries",
        0xFEBC => "Dexcom Inc",
        0xFEBD => "Clover Network, Inc",
        0xFEBE => "Bose Corporation",
        0xFEBF => "Nod, Inc.",
        0xFEC0 => "KDDI Corporation",
        0xFEC1 => "KDDI Corporation",
        0xFEC2 => "Blue Spark Technologies, Inc.",
        0xFEC3 => "360fly, Inc.",
        0xFEC4 => "PLUS Location Systems",
        0xFEC5 => "Realtek Semiconductor Corp.",
        0xFEC6 => "Kocomojo, LLC",
        0xFEC7 => "Apple, Inc.",
        0xFEC8 => "Apple, Inc.",
        0xFEC9 => "Apple, Inc.",
        0xFECA => "Apple, Inc.",
        0xFECB => "Apple, Inc.",
        0xFECC => "Apple, Inc.",
        0xFECD => "Apple, Inc.",
        0xFECE => "Apple, Inc.",
        0xFECF => "Apple, Inc.",
        0xFED0 => "Apple, Inc.",
        0xFED1 => "Apple, Inc.",
        0xFED2 => "Apple, Inc.",
        0xFED3 => "Apple, Inc.",
        0xFED4 => "Apple, Inc.",
        0xFED5 => "Plantronics Inc.",
        0xFED6 => "Broadcom",
        0xFED7 => "Broadcom",
        0xFED8 => "Google LLC",
        0xFED9 => "Pebble Technology Corporation",
        0xFEDA => "ISSC Technologies Corp.",
        0xFEDB => "Perka, Inc.",
        0xFEDC => "Jawbone",
        0xFEDD => "Jawbone",
        0xFEDE => "Coin, Inc.",
        0xFEDF => "Design SHIFT",
        0xFEE0 => "Anhui Huami Information Technology Co., Ltd.",
        0xFEE1 => "Anhui Huami Information Technology Co., Ltd.",
        0xFEE2 => "Anki, Inc.",
        0xFEE3 => "Anki, Inc.",
        0xFEE4 => "Nordic Semiconductor ASA",
        0xFEE5 => "Nordic Semiconductor ASA",
        0xFEE6 => "Silvair, Inc.",
        0xFEE7 => "Tencent Holdings Limited.",
        0xFEE8 => "Quintic Corp.",
        0xFEE9 => "Quintic Corp.",
        0xFEEA => "Swirl Networks, Inc.",
        0xFEEB => "Swirl Networks, Inc.",
        0xFEEC => "Tile, Inc.",
        0xFEED => "Tile, Inc.",
        0xFEEE => "Polar Electro Oy",
        0xFEEF => "Polar Electro Oy",
        0xFEF0 => "Intel",
        0xFEF1 => "CSR",
        0xFEF2 => "CSR",
        0xFEF3 => "Google LLC",
        0xFEF4 => "Google LLC",
        0xFEF5 => "Dialog Semiconductor GmbH",
        0xFEF6 => "Wicentric, Inc.",
        0xFEF7 => "Aplix Corporation",
        0xFEF8 => "Aplix Corporation",
        0xFEF9 => "PayPal, Inc.",
        0xFEFA => "PayPal, Inc.",
        0xFEFB => "Telit Wireless Solutions (Formerly Stollmann E+V GmbH)",
        0xFEFC => "Gimbal, Inc.",
        0xFEFD => "Gimbal, Inc.",
        0xFEFE => "GN ReSound A/S",
        0xFEFF => "GN Netcom",
        _ => return None,
    })
}
