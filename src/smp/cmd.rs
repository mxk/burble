use structbuf::{Pack, Packer, Unpack, Unpacker};
use tracing::{error, trace};

use burble_crypto::{Check, Codec, Confirm, Nonce, PublicKey};

use crate::l2cap::Sdu;
use crate::{host, le};

use super::*;

/// SMP command ([Vol 3] Part H, Section 3.3).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Command {
    PairingRequest(PairingParams),
    PairingResponse(PairingParams),
    PairingConfirm(Confirm),
    PairingRandom(Nonce),
    PairingFailed(Reason),
    EncryptionInformation(), // LE legacy pairing only
    CentralIdentification(), // LE legacy pairing only
    IdentityInformation(),   // TODO
    IdentityAddressInformation(le::Addr),
    SigningInformation(), // TODO
    SecurityRequest(AuthReq),
    PairingPublicKey(PublicKey),
    PairingDhKeyCheck(Check),
    PairingKeypressNotification(PasskeyEntry),
}

impl Command {
    pub fn pack<T: host::Transport>(&self, pdu: &mut Sdu<T>) {
        use Command::*;
        let mut p = pdu.append();
        match *self {
            PairingRequest(ref v) => v.pack(p.u8(Code::PairingRequest)),
            PairingResponse(ref v) => v.pack(p.u8(Code::PairingResponse)),
            PairingConfirm(ref v) => v.pack(p.u8(Code::PairingConfirm)),
            PairingRandom(ref v) => v.pack(p.u8(Code::PairingRandom)),
            PairingFailed(v) => p.u8(Code::PairingFailed).u8(v).into(),
            EncryptionInformation() | CentralIdentification() => {
                unimplemented!("legacy pairing command")
            }
            IdentityInformation() => unimplemented!(),
            IdentityAddressInformation(ref v) => v.pack(p.u8(Code::IdentityAddressInformation)),
            SigningInformation() => unimplemented!(),
            SecurityRequest(v) => p.u8(Code::SecurityRequest).u8(v.bits()).into(),
            PairingPublicKey(ref v) => v.pack(p.u8(Code::PairingPublicKey)),
            PairingDhKeyCheck(ref v) => v.pack(p.u8(Code::PairingDhKeyCheck)),
            PairingKeypressNotification(v) => p.u8(Code::PairingKeypressNotification).u8(v).into(),
        }
    }
}

impl<T: host::Transport> TryFrom<Sdu<T>> for Command {
    type Error = Reason;

    fn try_from(pdu: Sdu<T>) -> std::result::Result<Self, Self::Error> {
        // [Vol 3] Part H, Section 3.3
        let mut p = pdu.unpack();
        if p.is_empty() {
            error!("Empty PDU");
            return Err(Reason::InvalidParameters);
        };
        let code = p.u8();
        let Ok(code) = Code::try_from(code) else {
            error!("Unknown command code: {code:#04X}");
            return Err(Reason::CommandNotSupported);
        };
        p.map(|p| match code {
            Code::PairingRequest => PairingParams::unpack(p).map(Self::PairingRequest),
            Code::PairingResponse => PairingParams::unpack(p).map(Self::PairingResponse),
            Code::PairingConfirm => Confirm::unpack(p).map(Self::PairingConfirm),
            Code::PairingRandom => Nonce::unpack(p).map(Self::PairingRandom),
            Code::PairingFailed => Reason::try_from(p.u8()).ok().map(Self::PairingFailed),
            Code::EncryptionInformation => Some(Self::EncryptionInformation()),
            Code::CentralIdentification => Some(Self::CentralIdentification()),
            Code::IdentityInformation => Some(Self::IdentityInformation()),
            Code::IdentityAddressInformation => {
                le::Addr::unpack(p).map(Self::IdentityAddressInformation)
            }
            Code::SigningInformation => Some(Self::SigningInformation()),
            Code::SecurityRequest => {
                Some(Self::SecurityRequest(AuthReq::from_bits_truncate(p.u8())))
            }
            Code::PairingPublicKey => PublicKey::unpack(p).map(Self::PairingPublicKey),
            Code::PairingDhKeyCheck => Check::unpack(p).map(Self::PairingDhKeyCheck),
            Code::PairingKeypressNotification => PasskeyEntry::try_from(p.u8())
                .ok()
                .map(Self::PairingKeypressNotification),
        })
        .flatten()
        .map_or_else(
            || {
                error!("Invalid {code} PDU");
                Err(Reason::InvalidParameters)
            },
            |cmd| {
                trace!("{cmd:?}");
                Ok(cmd)
            },
        )
    }
}

/// Pairing request/response command ([Vol 3] Part H, Section 3.5.1).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct PairingParams {
    /// IO capabilities.
    pub io_cap: IoCap,
    /// OOB authentication data is available flag.
    pub oob_data: bool,
    /// Requested security properties.
    pub auth_req: AuthReq,
    /// Maximum encryption key size that the device can support (7-16 octets).
    pub max_key_len: u8,
    /// Keys that the initiator is requesting to distribute / generate or use
    /// during the Transport Specific Key Distribution phase.
    pub initiator_keys: KeyDist,
    /// Keys that the initiator is requesting the responder to distribute /
    /// generate or use during the Transport Specific Key Distribution phase.
    pub responder_keys: KeyDist,
}

impl PairingParams {
    /// Minimum allowed key length (128-bit).
    pub const MIN_KEY_LEN: u8 = 16;
}

impl Default for PairingParams {
    #[inline]
    fn default() -> Self {
        Self {
            io_cap: IoCap::NoInputNoOutput,
            oob_data: false,
            auth_req: AuthReq::SC,
            max_key_len: Self::MIN_KEY_LEN,
            initiator_keys: KeyDist::empty(),
            responder_keys: KeyDist::empty(),
        }
    }
}

impl Codec for PairingParams {
    #[inline]
    fn pack(&self, p: &mut Packer) {
        p.u8(self.io_cap)
            .bool(self.oob_data)
            .u8(self.auth_req.bits())
            .u8(self.max_key_len)
            .u8(self.initiator_keys.bits())
            .u8(self.responder_keys.bits());
    }

    #[inline]
    fn unpack(p: &mut Unpacker) -> Option<Self> {
        Some(Self {
            io_cap: IoCap::try_from(p.u8()).ok()?,
            oob_data: p.bool(),
            auth_req: AuthReq::from_bits_truncate(p.u8()),
            max_key_len: {
                let v = p.u8();
                #[allow(clippy::manual_range_contains)]
                (v < 7 || 16 < v).then_some(v)?
            },
            initiator_keys: KeyDist::from_bits_truncate(p.u8()),
            responder_keys: KeyDist::from_bits_truncate(p.u8()),
        })
    }
}

impl From<PairingParams> for burble_crypto::IoCap {
    #[inline]
    fn from(p: PairingParams) -> Self {
        Self::new(p.auth_req.bits(), p.oob_data, u8::from(p.io_cap))
    }
}
