//! Attribute Protocol ([Vol 3] Part F).

#![allow(dead_code)] // TODO: Remove

use std::fmt::Debug;
use std::sync::Arc;
use std::time::Duration;

use bytes::{Buf, BufMut, BytesMut};
use tracing::warn;

pub(crate) use {consts::*, handle::*, perm::*, server::*};

use crate::gap::Uuid;
use crate::l2cap::{BasicChan, Sdu};
use crate::{host, l2cap};

mod consts;
mod handle;
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

/// Error response ([Vol 3] Part F, Section 3.4.1.1).
#[derive(Copy, Clone, Debug, thiserror::Error)]
#[error("ATT {req} failed with {err} ({hdl:?})")]
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
}

impl<T: host::Transport> Bearer<T> {
    /// Creates an ATT bearer by associating an L2CAP channel with an ATT
    /// server.
    #[inline]
    #[must_use]
    pub(super) const fn new(ch: BasicChan<T>, srv: Arc<Server>) -> Self {
        Self { ch, srv }
    }

    /// Returns the next command, request, notification, or indication PDU. This
    /// method is not cancel safe. Dropping the returned future may result in a
    /// failure to send an error response.
    pub async fn recv(&mut self) -> Result<Pdu<T>> {
        loop {
            let sdu = self.ch.recv().await?;
            // [Vol 3] Part F, Section 3.3
            let Some(&op) = sdu.as_ref().first() else {
                warn!("Empty PDU");
                self.error_rsp(0, None, ErrorCode::InvalidPdu).await?;
                continue;
            };
            let Some(op) = Opcode::try_from(op).ok() else {
                warn!("Unknown ATT opcode: {op}");
                if !Opcode::is_cmd(op) {
                    self.error_rsp(op, None, ErrorCode::RequestNotSupported).await?;
                }
                continue;
            };
            if matches!(op.typ(), PduType::Rsp | PduType::Cfm) {
                warn!("Unexpected ATT PDU: {op}");
                continue;
            }
            // TODO: Validate signature?
            return Ok(Pdu { br: self, op, sdu });
        }
    }

    /// Sends an `ATT_ERROR_RSP` PDU in response to a request that cannot be
    /// performed ([Vol 3] Part F, Section 3.4.1.1).
    async fn error_rsp(&mut self, req: u8, hdl: Option<Handle>, err: ErrorCode) -> Result<()> {
        assert!(!Opcode::is_cmd(req));
        let sdu = self.new_sdu(Opcode::ErrorRsp, |b| {
            b.put_u8(req);
            b.put_u16_le(hdl.map_or(0, u16::from));
            b.put_u8(err.into());
        });
        self.send(sdu).await
    }

    /// Returns an outbound SDU, calling `f` to encode it after writing the
    /// opcode.
    #[inline]
    fn new_sdu(&mut self, op: Opcode, f: impl FnOnce(&mut BytesMut)) -> Sdu<T> {
        let mut sdu = self.ch.new_sdu();
        let b = sdu.buf_mut();
        b.put_u8(op.into());
        f(b);
        sdu
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
            self.ch.recv_filter(|b| {
                let Some(&op) = b.first() else { return false };
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
        let mut b = sdu.as_ref();
        if b.get_u8() == want {
            Ok(sdu)
        } else {
            Err(Error::Att(ErrorRsp {
                req: Opcode::try_from(b.get_u8()).unwrap_or(Opcode::ErrorRsp),
                hdl: Handle::new(b.get_u16_le()),
                err: ErrorCode::try_from(b.get_u8()).unwrap_or(ErrorCode::UnlikelyError),
            }))
        }
    }

    /// Sends an SDU over the channel.
    #[inline]
    async fn send(&mut self, sdu: Sdu<T>) -> Result<()> {
        Ok(self.ch.send(sdu).await?)
    }
}

/// Received ATT protocol data unit ([Vol 3] Part F, Section 3.3).
#[derive(Debug)]
pub struct Pdu<'a, T: host::Transport> {
    br: &'a mut Bearer<T>,
    op: Opcode,
    sdu: Sdu<T>,
}

impl<T: host::Transport> Pdu<'_, T> {
    /// Returns the PDU opcode.
    #[inline]
    pub const fn opcode(&self) -> Opcode {
        self.op
    }

    /// Sends an `ATT_ERROR_RSP` PDU in response to a request that cannot be
    /// performed ([Vol 3] Part F, Section 3.4.1.1).
    pub async fn error_rsp(self, hdl: Option<Handle>, err: ErrorCode) -> Result<()> {
        self.br.error_rsp(self.op as _, hdl, err).await
    }
}

impl<T: host::Transport> AsRef<[u8]> for Pdu<'_, T> {
    /// Returns PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        //self.pdu.as_ref().get_unchecked()
        self.sdu.as_ref()
    }
}
