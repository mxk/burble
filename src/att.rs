//! Attribute Protocol ([Vol 3] Part F).

#![allow(dead_code)] // TODO: Remove

use std::fmt::Debug;
use std::time::Duration;

use structbuf::{Pack, Packer, Unpacker};
use tracing::{debug, error, warn};

pub(crate) use {consts::*, handle::*, pdu::*, perm::*};

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
    const fn new(req: u8, hdl: Option<Handle>, err: ErrorCode) -> Self {
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
            warn!("Unknown ATT opcode: {op}");
            return Err(ErrorRsp::new(op, None, ErrorCode::RequestNotSupported).into());
        };
        if matches!(op.typ(), PduType::Rsp | PduType::Cfm) {
            warn!("Unexpected ATT PDU: {op}");
            return Err(ErrorRsp::new(op as _, None, ErrorCode::UnlikelyError).into());
        }
        // TODO: Validate signature?
        Ok(Pdu(sdu))
    }

    /// Calls `f` to perform additional validation of the received request.
    #[inline(always)]
    pub async fn check<P: Send, R>(
        &self,
        params: RspResult<P>,
        f: impl FnOnce(P) -> RspResult<R> + Send,
    ) -> Result<R> {
        let e = match params {
            Ok(p) => match f(p) {
                Ok(r) => return Ok(r),
                Err(e) => e,
            },
            Err(e) => e,
        };
        self.raw_error_rsp(e.req, e.hdl, e.err).await?;
        Err(e.into())
    }

    /// Handles `ATT_EXCHANGE_MTU_REQ` ([Vol 3] Part F, Section 3.4.2.1).
    pub(crate) async fn handle_exchange_mtu_req(&mut self, pdu: Pdu<T>) -> Result<()> {
        let v = self.check(pdu.exchange_mtu_req(), |mtu| {
            // This procedure can only be performed once, so the current MTU
            // is the minimum one allowed for the channel.
            if self.0.mtu() <= mtu {
                Ok((mtu, self.0.preferred_mtu()))
            } else {
                Err(pdu.err(ErrorCode::RequestNotSupported))
            }
        });
        let (cli_mtu, srv_mtu) = v.await?;
        // TODO: Increase server MTU if the controller's ACL data packet limits
        // are too small.
        self.exchange_mtu_rsp(srv_mtu).await?;
        let min = cli_mtu.min(srv_mtu);
        debug!("{} ATT_MTU={min}", self.0.cid());
        self.0.set_mtu(min);
        Ok(())
    }

    /// Receives a response or confirmation PDU ([Vol 3] Part F, Section 3.4.9).
    async fn recv_rsp(&mut self, rsp: Opcode) -> Result<Sdu<T>> {
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

    /// Sends an `ATT_ERROR_RSP` PDU in response to a request that cannot be
    /// performed ([Vol 3] Part F, Section 3.4.1.1). Command-related errors are
    /// ignored.
    async fn raw_error_rsp(&self, req: u8, hdl: Option<Handle>, err: ErrorCode) -> Result<()> {
        warn!("ATT response: opcode {req:#06X} for {hdl:?} failed with {err}");
        if Opcode::is_cmd(req) {
            return Ok(());
        }
        let pdu = self.pack(Opcode::ErrorRsp, |p| {
            p.u8(req).u16(hdl.map_or(0, u16::from)).u8(err);
        });
        self.send(pdu).await
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
    #[inline]
    async fn send(&self, sdu: Sdu<T>) -> Result<()> {
        Ok(self.0.send(sdu).await?)
    }
}
