use std::fmt::Debug;

use crate::hci;

use super::*;

/// Maximum attribute value length ([Vol 3] Part F, Section 3.2.9).
pub(crate) const MAX_VAL_LEN: usize = 512;

/// Attribute opcode ([Vol 3] Part F, Section 3.3.1 and
/// [Vol 3] Part F, Section 3.4.8).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Opcode {
    ErrorRsp = 0x01,
    ExchangeMtuReq = 0x02,
    ExchangeMtuRsp = 0x03,
    FindInformationReq = 0x04,
    FindInformationRsp = 0x05,
    FindByTypeValueReq = 0x06,
    FindByTypeValueRsp = 0x07,
    ReadByTypeReq = 0x08,
    ReadByTypeRsp = 0x09,
    ReadReq = 0x0A,
    ReadRsp = 0x0B,
    ReadBlobReq = 0x0C,
    ReadBlobRsp = 0x0D,
    ReadMultipleReq = 0x0E,
    ReadMultipleRsp = 0x0F,
    ReadByGroupTypeReq = 0x10,
    ReadByGroupTypeRsp = 0x11,
    WriteReq = 0x12,
    WriteRsp = 0x13,
    WriteCmd = 0x52,
    PrepareWriteReq = 0x16,
    PrepareWriteRsp = 0x17,
    ExecuteWriteReq = 0x18,
    ExecuteWriteRsp = 0x19,
    ReadMultipleVariableReq = 0x20,
    ReadMultipleVariableRsp = 0x21,
    MultipleHandleValueNtf = 0x23,
    HandleValueNtf = 0x1B,
    HandleValueInd = 0x1D,
    HandleValueCfm = 0x1E,
    SignedWriteCmd = 0xD2,
}

impl Opcode {
    /// Returns whether the raw opcode has the Command Flag set.
    #[inline]
    #[must_use]
    pub const fn is_cmd(op: u8) -> bool {
        op & (1 << 6) != 0
    }

    /// Returns whether the Authentication Signature Flag is set.
    #[inline]
    #[must_use]
    pub const fn is_signed(self) -> bool {
        self as u8 & (1 << 7) != 0
    }

    /// Creates a new access request.
    ///
    /// # Panics
    ///
    /// Panics if the opcode is not a read/write request.
    #[inline]
    pub(crate) fn request(self, sec: hci::ConnSec) -> Request {
        let ac = (self.access_type().expect("not a read/write request")).copy(sec);
        Request { op: self, ac }
    }

    /// Returns a non-handle error response.
    #[inline]
    pub const fn err<R>(self, err: ErrorCode) -> RspResult<R> {
        Err(ErrorRsp::new(self as _, None, err))
    }

    /// Returns a handle-specific error response.
    #[inline]
    pub const fn hdl_err<R>(self, err: ErrorCode, hdl: Handle) -> RspResult<R> {
        Err(ErrorRsp::new(self as _, Some(hdl), err))
    }

    /// Returns the PDU type.
    pub(crate) const fn typ(self) -> PduType {
        use {Opcode::*, PduType::*};
        #[allow(clippy::match_same_arms)]
        match self {
            ErrorRsp => Rsp,
            ExchangeMtuReq => Req,
            ExchangeMtuRsp => Rsp,
            FindInformationReq => Req,
            FindInformationRsp => Rsp,
            FindByTypeValueReq => Req,
            FindByTypeValueRsp => Rsp,
            ReadByTypeReq => Req,
            ReadByTypeRsp => Rsp,
            ReadReq => Req,
            ReadRsp => Rsp,
            ReadBlobReq => Req,
            ReadBlobRsp => Rsp,
            ReadMultipleReq => Req,
            ReadMultipleRsp => Rsp,
            ReadByGroupTypeReq => Req,
            ReadByGroupTypeRsp => Rsp,
            WriteReq => Req,
            WriteRsp => Rsp,
            WriteCmd => Cmd,
            PrepareWriteReq => Req,
            PrepareWriteRsp => Rsp,
            ExecuteWriteReq => Req,
            ExecuteWriteRsp => Rsp,
            ReadMultipleVariableReq => Req,
            ReadMultipleVariableRsp => Rsp,
            MultipleHandleValueNtf => Ntf,
            HandleValueNtf => Ntf,
            HandleValueInd => Ind,
            HandleValueCfm => Cfm,
            SignedWriteCmd => Cmd,
        }
    }

    /// Returns read/write access type being performed
    /// ([Vol 3] Part F, Section 3.4.9).
    pub(crate) const fn access_type(self) -> Option<Access> {
        use Opcode::*;
        const READ: Option<Access> = Some(Access::READ);
        const WRITE: Option<Access> = Some(Access::WRITE);
        #[allow(clippy::match_same_arms)]
        match self {
            ErrorRsp => None,
            ExchangeMtuReq => None,
            ExchangeMtuRsp => None,
            FindInformationReq => None,
            FindInformationRsp => None,
            FindByTypeValueReq => None,
            FindByTypeValueRsp => None,
            ReadByTypeReq => READ,
            ReadByTypeRsp => None,
            ReadReq => READ,
            ReadRsp => None,
            ReadBlobReq => READ,
            ReadBlobRsp => None,
            ReadMultipleReq => READ,
            ReadMultipleRsp => None,
            ReadByGroupTypeReq => READ,
            ReadByGroupTypeRsp => None,
            WriteReq => WRITE,
            WriteRsp => None,
            WriteCmd => WRITE,
            PrepareWriteReq => WRITE,
            PrepareWriteRsp => None,
            ExecuteWriteReq => None,
            ExecuteWriteRsp => None,
            ReadMultipleVariableReq => READ,
            ReadMultipleVariableRsp => None,
            MultipleHandleValueNtf => None,
            HandleValueNtf => None,
            HandleValueInd =>q None,
            HandleValueCfm => None,
            SignedWriteCmd => WRITE,
        }
    }

    /// Returns the expected response/confirmation opcode or [`None`] if `self`
    /// is not a request or indication.
    #[inline]
    pub(crate) fn rsp(self) -> Option<Self> {
        use {num_enum::TryFromPrimitive, PduType::*};
        match self.typ() {
            Cmd | Rsp | Ntf | Cfm => None,
            Req | Ind => Self::try_from_primitive(self as u8 + 1).ok(),
        }
    }
}

/// Attribute PDU type ([Vol 3] Part F, Section 3.3).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PduType {
    /// Command sent to a server by a client that does not invoke a response.
    Cmd,
    /// Request sent to a server by a client that invokes a response.
    Req,
    /// Response sent to a client by a server in response to a request.
    Rsp,
    /// Notification sent to a client by a server that does not invoke a
    /// confirmation.
    Ntf,
    /// Indication sent to a client by a server that invokes a confirmation.
    Ind,
    /// Confirmation sent to a server by a client to confirm receipt of an
    /// indication.
    Cfm,
}

/// ATT and Common Profile and Service error codes
/// ([Vol 3] Part F, Section 3.4.1.1 and \[CSS\] Part B, Section 1.2).
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
pub enum ErrorCode {
    /// The attribute handle given was not valid on this server.
    InvalidHandle = 0x01,
    /// The attribute cannot be read.
    ReadNotPermitted = 0x02,
    /// The attribute cannot be written.
    WriteNotPermitted = 0x03,
    /// The attribute PDU was invalid.
    InvalidPdu = 0x04,
    /// The attribute requires authentication before it can be read or written.
    InsufficientAuthentication = 0x05,
    /// ATT Server does not support the request received from the client.
    RequestNotSupported = 0x06,
    /// Offset specified was past the end of the attribute.
    InvalidOffset = 0x07,
    /// The attribute requires authorization before it can be read or written.
    InsufficientAuthorization = 0x08,
    /// Too many prepare writes have been queued.
    PrepareQueueFull = 0x09,
    /// No attribute found within the given attribute handle range.
    AttributeNotFound = 0x0A,
    /// The attribute cannot be read using the ATT_READ_BLOB_REQ PDU.
    AttributeNotLong = 0x0B,
    /// The Encryption Key Size used for encrypting this link is too short.
    EncryptionKeySizeTooShort = 0x0C,
    /// The attribute value length is invalid for the operation.
    InvalidAttributeValueLength = 0x0D,
    /// The attribute request that was requested has encountered an error that
    /// was unlikely, and therefore could not be completed as requested.
    UnlikelyError = 0x0E,
    /// The attribute requires encryption before it can be read or written.
    InsufficientEncryption = 0x0F,
    /// The attribute type is not a supported grouping attribute as defined by a
    /// higher layer specification.
    UnsupportedGroupType = 0x10,
    /// Insufficient Resources to complete the request.
    InsufficientResources = 0x11,
    /// The server requests the client to rediscover the database.
    DatabaseOutOfSync = 0x12,
    /// The attribute parameter value was not allowed.
    ValueNotAllowed = 0x13,
    /// Write operation cannot be fulfilled for reasons other than permissions.
    WriteRequestRejected = 0xFC,
    /// Client Characteristic Configuration descriptor is not configured
    /// according to the requirements of the profile or service.
    CccdImproperlyConfigured = 0xFD,
    /// Request cannot be serviced because an operation that has been previously
    /// triggered is still in progress.
    ProcedureAlreadyInProgress = 0xFE,
    /// Attribute value is out of range.
    OutOfRange = 0xFF,
}

crate::impl_display_via_debug! { Opcode, ErrorCode }
