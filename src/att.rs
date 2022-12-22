//! Attribute Protocol ([Vol 3] Part F).

#![allow(dead_code)] // TODO: Remove

use std::fmt::Debug;
use std::sync::Arc;
use std::time::Duration;

use structbuf::{Pack, Packer, Unpacker};
use tracing::{debug, error, warn};

pub(crate) use {consts::*, handle::*, pdu::*, perm::*, server::*};

use crate::gap::Uuid;
use crate::l2cap::{BasicChan, Sdu};
use crate::{host, l2cap};

mod consts;
mod handle;
mod pdu;
mod perm;
mod server;

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
    pub req: Opcode,
    pub hdl: Option<Handle>,
    pub err: ErrorCode,
}

/// ATT bearer ([Vol 3] Part F, Section 3.2.11).
#[derive(Debug)]
pub struct Bearer<T: host::Transport> {
    ch: BasicChan<T>,
    srv: Arc<Server>,
    min_mtu: u16,
}

impl<T: host::Transport> Bearer<T> {
    /// Creates an ATT bearer by associating an L2CAP channel with an ATT
    /// server.
    #[inline]
    #[must_use]
    pub(super) const fn new(ch: BasicChan<T>, srv: Arc<Server>) -> Self {
        let min_mtu = ch.mtu();
        Self { ch, srv, min_mtu }
    }

    /// Returns the next command, request, notification, or indication PDU. This
    /// method is not cancel safe. Dropping the returned future may result in a
    /// failure to send an internal response.
    pub async fn recv(&mut self) -> Result<Pdu<T>> {
        loop {
            let sdu = self.ch.recv().await?;
            // [Vol 3] Part F, Section 3.3
            let Some(&op) = sdu.as_ref().first() else {
                warn!("Empty PDU");
                self.raw_error_rsp(0, None, ErrorCode::InvalidPdu).await?;
                continue;
            };
            let Some(op) = Opcode::try_from(op).ok() else {
                warn!("Unknown ATT opcode: {op}");
                self.raw_error_rsp(op, None, ErrorCode::RequestNotSupported).await?;
                continue;
            };
            if matches!(op.typ(), PduType::Rsp | PduType::Cfm) {
                warn!("Unexpected ATT PDU: {op}");
                continue;
            }
            let pdu = Pdu(sdu);
            match op {
                Opcode::ExchangeMtuReq => self.handle_exchange_mtu_req(pdu).await,
                Opcode::FindInformationReq => self.handle_find_information_req(pdu).await,
                // TODO: Validate signature?
                _ => return Ok(pdu),
            }
        }
    }

    /// Calls function `f` to perform additional validation of the received
    /// request.
    #[inline]
    pub async fn validate<P: Send, R>(
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
        self.error_rsp(e.req, e.hdl, e.err).await?;
        Err(e.into())
    }

    /// Handles `ATT_EXCHANGE_MTU_REQ` ([Vol 3] Part F, Section 3.4.2.1).
    async fn handle_exchange_mtu_req(&mut self, pdu: Pdu<T>) {
        let Ok((cli_mtu, srv_mtu)) = self.validate(pdu.exchange_mtu_req(), |mtu| {
            Ok(((self.min_mtu <= mtu).then_some(mtu), self.ch.preferred_mtu()))
        }).await else { return };
        // TODO: Increase server MTU if the controller's ACL data packet limits
        // are too small.
        if let Err(e) = self.exchange_mtu_rsp(srv_mtu).await {
            error!("MTU exchange failed: {e}");
            return;
        }
        if let Some(cli_mtu) = cli_mtu {
            let min = cli_mtu.min(srv_mtu);
            debug!("{} ATT_MTU={min}", self.ch.cid());
            self.ch.set_mtu(min);
        }
    }

    /// Handles `ATT_FIND_INFORMATION_REQ` ([Vol 3] Part F, Section 3.4.3.1).
    async fn handle_find_information_req(&self, pdu: Pdu<T>) {
        let Ok(it) = self.validate(pdu.find_information_req(), |hdls| {
            self.srv.types(hdls).ok_or_else(|| pdu.hdl_err(Some(hdls.start), ErrorCode::AttributeNotFound))
        }).await else { return };
        if let Err(e) = self.find_information_rsp(it).await {
            error!("Find information response error: {e}");
        }
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
            self.ch.recv_filter(|mut pdu| {
                let op = pdu.u8();
                op == want || (op == err && err != 0)
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
                req: Opcode::try_from(p.u8()).unwrap_or(Opcode::ErrorRsp),
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
        let mut sdu = self.ch.new_sdu();
        f(sdu.append().u8(op));
        sdu
    }

    /// Sends an SDU over the channel.
    #[inline]
    async fn send(&self, sdu: Sdu<T>) -> Result<()> {
        Ok(self.ch.send(sdu).await?)
    }
}
