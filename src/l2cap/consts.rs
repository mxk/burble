#![allow(clippy::use_self)]

use crate::l2cap::Cid;

/// C-frame command type ([Vol 3] Part A, Section 4).
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    PartialEq,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u16)]
pub enum Code {
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

impl Code {
    /// Returns whether the command can be used on the specified CID.
    #[must_use]
    pub const fn is_allowed_on(self, cid: Cid) -> bool {
        use Code::*;
        let (signal, le_signal) = (cid.0 == Cid::SIGNAL.0, cid.0 == Cid::LE_SIGNAL.0);
        #[allow(clippy::match_same_arms)]
        match self {
            CommandRejectRsp => signal || le_signal,
            ConnectionReq => signal,
            ConnectionRsp => signal,
            ConfigurationReq => signal,
            ConfigurationRsp => signal,
            DisconnectionReq => signal || le_signal,
            DisconnectionRsp => signal || le_signal,
            EchoReq => signal,
            EchoRsp => signal,
            InformationReq => signal,
            InformationRsp => signal,
            ConnectionParameterUpdateReq => le_signal,
            ConnectionParameterUpdateRsp => le_signal,
            LeCreditBasedConnectionReq => le_signal,
            LeCreditBasedConnectionRsp => le_signal,
            FlowControlCreditInd => signal || le_signal,
            CreditBasedConnectionReq => signal || le_signal,
            CreditBasedConnectionRsp => signal || le_signal,
            CreditBasedReconfigureReq => signal || le_signal,
            CreditBasedReconfigureRsp => signal || le_signal,
        }
    }
}
