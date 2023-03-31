/// Basic L2CAP header size ([Vol 3] Part A, Section 3).
pub(super) const L2CAP_HDR: usize = 4;

/// Minimum MTU ([Vol 3] Part A, Section 5.1).
pub(super) const L2CAP_LE_MIN_MTU: u16 = 23;

/// C-frame command type ([Vol 3] Part A, Section 4).
#[derive(Clone, Copy, Debug, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[non_exhaustive]
#[repr(u8)]
pub(super) enum SigCode {
    CommandRejectRsp = 0x01,
    ConnectionReq = 0x02,
    ConnectionRsp = 0x03,
    ConfigurationReq = 0x04,
    ConfigurationRsp = 0x05,
    DisconnectionReq = 0x06,
    DisconnectionRsp = 0x07,
    EchoReq = 0x08,
    EchoRsp = 0x09,
    InformationReq = 0x0A,
    InformationRsp = 0x0B,
    ConnectionParameterUpdateReq = 0x12,
    ConnectionParameterUpdateRsp = 0x13,
    LeCreditBasedConnectionReq = 0x14,
    LeCreditBasedConnectionRsp = 0x15,
    FlowControlCreditInd = 0x16,
    CreditBasedConnectionReq = 0x17,
    CreditBasedConnectionRsp = 0x18,
    CreditBasedReconfigureReq = 0x19,
    CreditBasedReconfigureRsp = 0x1A,
}

impl SigCode {
    /// Returns whether the code represents a request.
    #[must_use]
    pub const fn is_req(self) -> bool {
        use SigCode::*;
        #[allow(clippy::match_same_arms)]
        match self {
            CommandRejectRsp => false,
            ConnectionReq => true,
            ConnectionRsp => false,
            ConfigurationReq => true,
            ConfigurationRsp => false,
            DisconnectionReq => true,
            DisconnectionRsp => false,
            EchoReq => true,
            EchoRsp => false,
            InformationReq => true,
            InformationRsp => false,
            ConnectionParameterUpdateReq => true,
            ConnectionParameterUpdateRsp => false,
            LeCreditBasedConnectionReq => true,
            LeCreditBasedConnectionRsp => false,
            FlowControlCreditInd => false,
            CreditBasedConnectionReq => true,
            CreditBasedConnectionRsp => false,
            CreditBasedReconfigureReq => true,
            CreditBasedReconfigureRsp => false,
        }
    }
}

/// `L2CAP_COMMAND_REJECT_RSP` reason ([Vol 3] Part A, Section 4.1).
#[derive(Clone, Copy, Debug, num_enum::IntoPrimitive)]
#[non_exhaustive]
#[repr(u16)]
pub(super) enum Reason {
    CommandNotUnderstood = 0x0000,
    _SignalingMtuExceeded = 0x0001,
    _InvalidCidInRequest = 0x0002,
}

crate::impl_display_via_debug! { SigCode, Reason }
