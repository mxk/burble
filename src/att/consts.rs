#![allow(clippy::use_self)]

/// ATT error codes ([Vol 3] Part F, Section 3.4.1.1).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive, strum::Display, thiserror::Error,
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
}
