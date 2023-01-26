//! Attribute Protocol ([Vol 3] Part F).

use std::fmt::Debug;
use std::time::Duration;

use structbuf::{Pack, Packer, Unpacker};
use tracing::{debug, error, warn};

pub(crate) use consts::*;
pub use {handle::*, perm::*};

use crate::gap::Uuid;
use crate::l2cap::{BasicChan, Sdu};
use crate::{host, l2cap};

mod consts;
mod handle;
mod pdu;
mod perm;

/// Error type returned by the ATT layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    L2cap(#[from] l2cap::Error),
    #[error(transparent)]
    Att(#[from] ErrorRsp),
    #[error("timeout while waiting for {0}")]
    Timeout(Opcode),
}

/// Common ATT result type.
pub type Result<T> = std::result::Result<T, Error>;

/// PDU response result.
pub type RspResult<T> = std::result::Result<T, ErrorRsp>;

/// Received ATT protocol data unit ([Vol 3] Part F, Section 3.3).
#[derive(Debug)]
#[repr(transparent)]
pub struct Pdu<T: host::Transport>(Sdu<T>);

/// Response PDU.
#[derive(Debug)]
#[repr(transparent)]
pub struct Rsp<T: host::Transport>(Sdu<T>);

/// `ATT_ERROR_RSP` PDU ([Vol 3] Part F, Section 3.4.1.1).
#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("ATT {req}{} failed with {err}", .hdl.map_or(String::new(), |h| format!(" for handle {:#06X}", u16::from(h))))]
pub struct ErrorRsp {
    req: u8,
    hdl: Option<Handle>,
    err: ErrorCode,
}

impl ErrorRsp {
    /// Creates a new error response.
    #[inline(always)]
    pub(crate) const fn new(req: u8, hdl: Option<Handle>, err: ErrorCode) -> Self {
        Self { req, hdl, err }
    }
}

/// ATT bearer ([Vol 3] Part F, Section 3.2.11).
#[derive(Debug)]
#[repr(transparent)]
pub struct Bearer<T: host::Transport>(BasicChan<T>);

impl<T: host::Transport> Bearer<T> {
    /// Creates an ATT bearer by associating an L2CAP channel with an ATT
    /// server.
    #[inline]
    #[must_use]
    pub(crate) const fn new(ch: BasicChan<T>) -> Self {
        Self(ch)
    }

    /// Returns the next command, request, notification, or indication PDU. This
    /// method is cancel safe.
    pub async fn recv(&self) -> Result<Pdu<T>> {
        let sdu = self.0.recv().await?;
        // [Vol 3] Part F, Section 3.3
        let Some(&op) = sdu.as_ref().first() else {
            warn!("Empty PDU");
            return Err(ErrorRsp::new(0, None, ErrorCode::InvalidPdu).into());
        };
        let Some(op) = Opcode::try_from(op).ok() else {
            warn!("Unknown opcode: {op}");
            return Err(ErrorRsp::new(op, None, ErrorCode::RequestNotSupported).into());
        };
        if matches!(op.typ(), PduType::Rsp | PduType::Cfm) {
            warn!("Unexpected PDU: {op}");
            return Err(ErrorRsp::new(op as _, None, ErrorCode::UnlikelyError).into());
        }
        // TODO: Validate signature?
        Ok(Pdu(sdu))
    }

    /// Returns the access request being made by the specified `pdu`. The
    /// request contains information about the access type (read/write) and the
    /// client's current authentication/authorization/encryption status.
    ///
    /// # Panics
    ///
    /// Panics if the `pdu` is not a read/write request.
    #[inline]
    pub fn access_req(&self, pdu: &Pdu<T>) -> Request {
        // TODO: Set authz/authn/key_len
        pdu.opcode().request()
    }

    /// Sends a response PDU or an `ATT_ERROR_RSP` if the request could not be
    /// completed ([Vol 3] Part F, Section 3.4.1.1). Command-related errors are
    /// ignored.
    pub async fn send_rsp(&self, r: RspResult<Rsp<T>>) -> Result<()> {
        let rsp = match r {
            Ok(r) => r.0,
            Err(e) => {
                warn!(
                    "ATT response: opcode {:#06X} for {:?} failed with {}",
                    e.req, e.hdl, e.err
                );
                if Opcode::is_cmd(e.req) {
                    return Ok(());
                }
                self.pack(Opcode::ErrorRsp, |p| {
                    p.u8(e.req).u16(e.hdl.map_or(0, u16::from)).u8(e.err);
                })
            }
        };
        self.send(rsp).await
    }

    /// Handles `ATT_EXCHANGE_MTU_REQ` ([Vol 3] Part F, Section 3.4.2.1).
    pub(crate) async fn handle_exchange_mtu_req(&mut self, pdu: Pdu<T>) -> Result<()> {
        let r = pdu.exchange_mtu_req().and_then(|mtu| {
            // This procedure can only be performed once, so the current MTU
            // is the minimum one allowed for the channel.
            if mtu < self.0.mtu() {
                return pdu.err(ErrorCode::RequestNotSupported);
            }
            // TODO: Increase server MTU if the controller's ACL data packet
            // limits are too small.
            let srv_mtu = self.0.preferred_mtu();
            let rsp = self.exchange_mtu_rsp(srv_mtu);
            let min = mtu.min(srv_mtu);
            debug!("{} ATT_MTU={min}", self.0.cid());
            self.0.set_mtu(min);
            rsp
        });
        self.send_rsp(r).await
    }

    /// Receives a response or confirmation PDU ([Vol 3] Part F, Section 3.4.9).
    async fn recv_rsp(&self, rsp: Opcode) -> Result<Sdu<T>> {
        let want = u8::from(rsp);
        let err = if rsp.typ() == PduType::Rsp {
            Opcode::ErrorRsp as u8
        } else {
            0
        };
        // Transaction timeout ([Vol 3] Part F, Section 3.3.3)
        let r = tokio::time::timeout(
            Duration::from_secs(30),
            self.0.recv_filter(|mut pdu| {
                let op = pdu.u8();
                op == want || (op == err && err != 0 && pdu.u8() == want)
            }),
        )
        .await;
        let sdu = match r {
            Ok(r) => r?,
            Err(_) => {
                // TODO: Close channel
                return Err(Error::Timeout(rsp));
            }
        };
        let mut p = Unpacker::new(sdu.as_ref());
        if p.u8() == want {
            Ok(sdu)
        } else {
            Err(Error::Att(ErrorRsp {
                req: p.u8(),
                hdl: Handle::new(p.u16()),
                err: ErrorCode::try_from(p.u8()).unwrap_or(ErrorCode::UnlikelyError),
            }))
        }
    }

    /// Returns a response PDU.
    #[allow(clippy::unnecessary_wraps)]
    #[inline(always)]
    fn rsp(&self, op: Opcode, f: impl FnOnce(&mut Packer)) -> RspResult<Rsp<T>> {
        Ok(Rsp(self.pack(op, f)))
    }

    /// Returns an outbound PDU, calling `f` to encode it after writing the
    /// opcode.
    #[inline]
    fn pack(&self, op: Opcode, f: impl FnOnce(&mut Packer)) -> Sdu<T> {
        let mut sdu = self.0.new_sdu();
        f(sdu.append().u8(op));
        sdu
    }

    /// Sends an SDU over the channel.
    #[inline(always)]
    async fn send(&self, sdu: Sdu<T>) -> Result<()> {
        Ok(self.0.send(sdu).await?)
    }
}
