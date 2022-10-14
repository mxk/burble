use OpcodeGroup::*;

/// HCI command opcodes ([Vol 4] Part E, Section 7).
#[derive(
    Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd, strum::Display, strum::FromRepr,
)]
#[non_exhaustive]
#[repr(u16)]
pub enum Opcode {
    // Opcode 0x0000 is used to update Num_HCI_Command_Packets ([Vol 4] Part E,
    // Section 7.7.14)
    #[default]
    None = 0x0000,

    // HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3)
    Reset = HciControl.ocf(0x0003),
    WriteLeHostSupport = HciControl.ocf(0x006D),

    // Informational parameters commands ([Vol 4] Part E, Section 7.4)
    ReadLocalVersionInformation = InfoParams.ocf(0x0001),
    ReadLocalSupportedCommands = InfoParams.ocf(0x0002),
}

impl From<u16> for Opcode {
    fn from(v: u16) -> Self {
        Opcode::from_repr(v).unwrap_or(Opcode::None)
    }
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
    _Le = 0x08,
}

impl OpcodeGroup {
    /// Combines OGF with OCF to create a full opcode.
    const fn ocf(self, ocf: u16) -> u16 {
        (self as u16) << 10 | ocf
    }
}

/// HCI event codes ([Vol 4] Part E, Section 7.7).
#[derive(Clone, Copy, Debug, Eq, PartialEq, strum::FromRepr)]
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
    Vendor = 0xFF,
}

/// HCI LE subevent codes ([Vol 4] Part E, Section 7.7.65).
#[derive(Clone, Copy, Debug, Eq, PartialEq, strum::FromRepr)]
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

/// HCI status codes ([Vol 1] Part F, Section 1.3).
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, strum::Display, strum::FromRepr, thiserror::Error,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Status {
    #[default]
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
    /// Returns whether the operation was successful.
    pub fn is_ok(self) -> bool {
        self == Status::Success
    }
}

impl From<u8> for Status {
    fn from(v: u8) -> Self {
        // [Vol 4] Part E, Section 1.2
        Self::from_repr(v).unwrap_or(Status::UnspecifiedError)
    }
}
