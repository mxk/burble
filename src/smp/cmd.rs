use structbuf::{Pack, Packer, Unpack, Unpacker};
use tracing::warn;

use burble_crypto::{Mac, Nonce};

use crate::host;
use crate::l2cap::Sdu;

use super::*;

/// SMP command ([Vol 3] Part H, Section 3.3).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum Command {
    PairingRequest(PairingParams),
    PairingResponse(PairingParams),
    PairingConfirm(Mac),
    PairingRandom(Nonce),
    PairingFailed(Reason),
    //EncryptionInformation(),
    //CentralIdentification(),
    //IdentityInformation(),
    //IdentityAddressInformation(),
    //SigningInformation(),
    //SecurityRequest(),
    //PairingPublicKey(),
    //PairingDhKeyCheck(),
    //PairingKeypressNotification(),
}

impl Command {
    fn pack<T: host::Transport>(&self, pdu: &mut Sdu<T>) {
        use Command::*;
        let mut p = pdu.append();
        #[allow(clippy::drop_ref)]
        match *self {
            PairingRequest(ref v) => v.pack(p.u8(Code::PairingRequest)),
            PairingResponse(ref v) => v.pack(p.u8(Code::PairingResponse)),
            PairingConfirm(v) => drop(p.u8(Code::PairingConfirm).u128(v)),
            PairingRandom(v) => drop(p.u8(Code::PairingRandom).u128(v)),
            PairingFailed(v) => drop(p.u8(Code::PairingRandom).u8(v)),
        }
    }
}

impl<T: host::Transport> TryFrom<Sdu<T>> for Command {
    type Error = Reason;

    fn try_from(pdu: Sdu<T>) -> std::result::Result<Self, Self::Error> {
        // [Vol 3] Part H, Section 3.3
        let mut p = pdu.unpack();
        if p.is_empty() {
            warn!("Empty PDU");
            return Err(Reason::InvalidParameters);
        };
        let code = p.u8();
        let Ok(code) = Code::try_from(code) else {
            warn!("Unknown command code: {code:#04X}");
            return Err(Reason::CommandNotSupported);
        };
        p.map(|p| match code {
            Code::PairingRequest => PairingParams::unpack(p).map(Self::PairingRequest),
            Code::PairingResponse => PairingParams::unpack(p).map(Self::PairingResponse),
            Code::PairingConfirm => Some(Self::PairingConfirm(p.u128().into())),
            Code::PairingRandom => Some(Self::PairingRandom(p.u128().into())),
            Code::PairingFailed => Reason::try_from(p.u8()).ok().map(Self::PairingFailed),
            Code::EncryptionInformation => None,
            Code::CentralIdentification => None,
            Code::IdentityInformation => None,
            Code::IdentityAddressInformation => None,
            Code::SigningInformation => None,
            Code::SecurityRequest => None,
            Code::PairingPublicKey => None,
            Code::PairingDhKeyCheck => None,
            Code::PairingKeypressNotification => None,
        })
        .flatten()
        .ok_or_else(|| {
            warn!("Invalid {code} PDU");
            Reason::InvalidParameters
        })
    }
}

pub(super) trait Codec {
    /// Packs command parameters into a PDU.
    fn pack(&self, p: &mut Packer);

    /// Unpacks command parameters from a PDU.
    fn unpack(p: &mut Unpacker) -> Option<Self>
    where
        Self: Sized;
}

/// Pairing request/response command ([Vol 3] Part H, Section 3.5.1).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct PairingParams {
    /// IO capabilities.
    pub io_cap: IoCap,
    /// OOB authentication data is available flag.
    pub oob_authn: bool,
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

impl Codec for PairingParams {
    #[inline]
    fn pack(&self, p: &mut Packer) {
        p.u8(self.io_cap)
            .bool(self.oob_authn)
            .u8(self.auth_req.bits())
            .u8(self.max_key_len)
            .u8(self.initiator_keys.bits())
            .u8(self.responder_keys.bits());
    }

    #[inline]
    fn unpack(p: &mut Unpacker) -> Option<Self> {
        Some(Self {
            io_cap: IoCap::try_from(p.u8()).ok()?,
            oob_authn: p.bool(),
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
