//! Service Discovery Protocol constants ([Vol 3] Part B).

use crate::gap::uuid16_enum;

/// SDP service class identifiers ([Assigned Numbers] Section 3.3).
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    Ord,
    PartialEq,
    PartialOrd,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u16)]
pub enum ServiceClass {
    ServiceDiscoveryServer = 0x1000,
    BrowseGroupDescriptor = 0x1001,
    SerialPort = 0x1101,
    LanAccessUsingPpp = 0x1102,
    DialupNetworking = 0x1103,
    IrMcSync = 0x1104,
    ObexObjectPush = 0x1105,
    ObexFileTransfer = 0x1106,
    IrMcSyncCommand = 0x1107,
    Headset = 0x1108,
    CordlessTelephony = 0x1109,
    AudioSource = 0x110A,
    AudioSink = 0x110B,
    AvRemoteControlTarget = 0x110C,
    AvRemoteControl = 0x110E,
    AvRemoteControlController = 0x110F,
    Intercom = 0x1110,
    Fax = 0x1111,
    HeadsetAudioGateway = 0x1112,
    Wap = 0x1113,
    WapClient = 0x1114,
    Panu = 0x1115,
    Nap = 0x1116,
    Gn = 0x1117,
    DirectPrinting = 0x1118,
    ReferencePrinting = 0x1119,
    ImagingResponder = 0x111B,
    ImagingAutomaticArchive = 0x111C,
    ImagingReferencedObjects = 0x111D,
    Handsfree = 0x111E,
    HandsfreeAudioGateway = 0x111F,
    DirectPrintingReferenceObjectsService = 0x1120,
    ReflectedUi = 0x1121,
    PrintingStatus = 0x1123,
    HumanInterfaceDeviceService = 0x1124,
    HcrPrint = 0x1126,
    HcrScan = 0x1127,
    CommonIsdnAccess = 0x1128,
    SimAccess = 0x112D,
    PhonebookAccessPce = 0x112E,
    PhonebookAccessPse = 0x112F,
    HeadsetHs = 0x1131,
    MessageAccessServer = 0x1132,
    MessageNotificationServer = 0x1133,
    GnssServer = 0x1136,
    ThreeDDisplay = 0x1137,
    ThreeDGlasses = 0x1138,
    MpsScUuid = 0x113B,
    CtnAccessService = 0x113C,
    CtnNotificationService = 0x113D,
    PnPInformation = 0x1200,
    GenericNetworking = 0x1201,
    GenericFileTransfer = 0x1202,
    GenericAudio = 0x1203,
    GenericTelephony = 0x1204,
    UpnpService = 0x1205,
    UpnpIpService = 0x1206,
    EsdpUpnpIpPan = 0x1300,
    EsdpUpnpIpLap = 0x1301,
    EsdpUpnpL2Cap = 0x1302,
    VideoSource = 0x1303,
    VideoSink = 0x1304,
    HdpSource = 0x1401,
    HdpSink = 0x1402,
}

uuid16_enum! { ServiceClass }