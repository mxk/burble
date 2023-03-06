#![allow(clippy::use_self)]

/// Basic L2CAP header size ([Vol 3] Part A, Section 3).
pub(super) const L2CAP_HDR: usize = 4;

/// Minimum MTU ([Vol 3] Part A, Section 5.1).
pub(super) const L2CAP_LE_MIN_MTU: u16 = 23;

/// C-frame command type ([Vol 3] Part A, Section 4).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u16)]
pub(crate) enum SigCode {
    CommandRejectRsp = 0x01,
    DisconnectionReq = 0x06,
    DisconnectionRsp = 0x07,
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

crate::impl_display_via_debug! { SigCode }
