//! Attribute Protocol ([Vol 3] Part F).

use std::fmt::{Debug, Display, Formatter};
use std::time::Duration;

use structbuf::{Pack, Packer, Unpacker};
use tracing::{debug, error, trace, warn};

pub use {consts::*, handle::*, perm::*};

use crate::gap::Uuid;
use crate::l2cap::{BasicChan, Cid, LeCid, Payload};
use crate::{hci, l2cap};

mod consts;
mod handle;
mod pdu;
mod perm;

/// Error type returned by the ATT layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    L2Cap(#[from] l2cap::Error),
    #[error(transparent)]
    Att(#[from] ErrorRsp),
    #[error("timeout while waiting for {0}")]
    Timeout(Opcode),
    #[error("notification/indication session closed")]
    NotifyClosed,
}

/// Common ATT result type.
pub type Result<T> = std::result::Result<T, Error>;

/// PDU response result.
pub type RspResult<T> = std::result::Result<T, ErrorRsp>;

/// Received ATT protocol data unit ([Vol 3] Part F, Section 3.3).
#[derive(Debug)]
#[repr(transparent)]
pub struct Pdu(Payload);

/// Response PDU.
#[derive(Debug)]
#[repr(transparent)]
pub struct Rsp(Payload);

/// `ATT_ERROR_RSP` PDU ([Vol 3] Part F, Section 3.4.1.1).
#[derive(Clone, Copy, Debug, thiserror::Error)]
pub struct ErrorRsp {
    req: u8,
    hdl: Option<Handle>,
    err: ErrorCode,
}

impl ErrorRsp {
    /// Creates a new error response.
    #[inline(always)]
    #[must_use]
    pub(crate) const fn new(req: u8, hdl: Option<Handle>, err: ErrorCode) -> Self {
        Self { req, hdl, err }
    }
}

impl Display for ErrorRsp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use num_enum::TryFromPrimitive;
        f.write_str("ATT ")?;
        match Opcode::try_from_primitive(self.req) {
            Ok(op) => write!(f, "{op} "),
            Err(_) => write!(f, "request {:#04X} ", self.req),
        }?;
        if let Some(hdl) = self.hdl {
            write!(f, "for handle {:#06X} ", u16::from(hdl))?;
        }
        write!(f, "failed with {}", self.err)
    }
}

/// ATT bearer ([Vol 3] Part F, Section 3.2.11).
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct Bearer(BasicChan);

impl Bearer {
    /// Creates an ATT bearer by associating an L2CAP channel with an ATT
    /// server.
    #[inline]
    #[must_use]
    pub(crate) const fn new(ch: BasicChan) -> Self {
        Self(ch)
    }

    /// Returns the channel ID.
    #[inline(always)]
    #[must_use]
    pub fn cid(&self) -> LeCid {
        self.0.cid()
    }

    /// Returns the connection watch channel.
    #[inline(always)]
    pub(crate) fn conn(&self) -> &hci::ConnWatch {
        self.0.conn()
    }

    /// Returns the current MTU.
    #[inline(always)]
    #[must_use]
    pub const fn mtu(&self) -> u16 {
        self.0.mtu()
    }

    /// Returns the next command, request, notification, or indication PDU. This
    /// method is cancel safe.
    pub async fn recv(&mut self) -> Result<Pdu> {
        let pdu = self.0.recv().await?;
        // [Vol 3] Part F, Section 3.3
        let Some(&op) = pdu.as_ref().first() else {
            warn!("Empty PDU");
            return Err(ErrorRsp::new(0, None, ErrorCode::InvalidPdu).into());
        };
        let Some(op) = Opcode::try_from(op).ok() else {
            warn!("Unknown opcode: {op}");
            return Err(ErrorRsp::new(op, None, ErrorCode::RequestNotSupported).into());
        };
        if matches!(op.typ(), PduType::Rsp | PduType::Cfm) {
            // TODO: This should probably be ignored.
            warn!("Unexpected PDU: {op}");
            return Err(ErrorRsp::new(op as _, None, ErrorCode::UnlikelyError).into());
        }
        // TODO: Validate signature?
        Ok(Pdu(pdu))
    }

    /// Returns the access request being made by the specified `pdu`. The
    /// request contains information about the access type (read/write) and the
    /// client's current authentication/authorization/encryption status.
    ///
    /// # Panics
    ///
    /// Panics if the `pdu` is not a read/write request.
    #[inline]
    pub(crate) fn access_req(&self, pdu: &Pdu) -> Request {
        let sec = self.0.conn().borrow().sec;
        pdu.opcode().request(sec)
    }

    /// Sends a response PDU or an `ATT_ERROR_RSP` if the request could not be
    /// completed ([Vol 3] Part F, Section 3.4.1.1). Command-related errors are
    /// ignored.
    pub async fn send_rsp(&mut self, r: RspResult<Rsp>) -> Result<()> {
        let rsp = match r {
            Ok(r) => r.0,
            Err(e) => {
                warn!("{e}");
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

    /// Performs MTU exchange ([Vol 3] Part F, Section 3.2.8 and 3.4.2.1).
    pub(crate) async fn exchange_mtu(&mut self) -> Result<()> {
        if self.0.cid().chan != Cid::ATT {
            return Ok(());
        }
        let local = self.0.preferred_mtu();
        let req = self.pack(Opcode::ExchangeMtuReq, |p| {
            p.u16(local);
        });
        self.send(req).await?;
        loop {
            let pdu = match self.recv_rsp(Opcode::ExchangeMtuRsp).await {
                Ok(p) => Pdu(p),
                // [Vol 3] Part G, Section 4.3.1
                Err(Error::Att(ErrorRsp {
                    err: ErrorCode::RequestNotSupported,
                    ..
                })) => {
                    debug!("Remote host does not support ATT_EXCHANGE_MTU_REQ");
                    return Ok(());
                }
                Err(e) => return Err(e),
            };
            match pdu.opcode() {
                Opcode::ExchangeMtuReq => {
                    self.handle_exchange_mtu_req(&pdu).await?;
                }
                Opcode::ExchangeMtuRsp => {
                    let remote = pdu.unpack(Opcode::ExchangeMtuRsp, |p| Ok(p.u16()))?;
                    debug!("{} remote preferred MTU: {}", self.cid(), remote);
                    self.0.set_mtu(local.min(remote));
                    return Ok(());
                }
                _ => unreachable!(),
            }
        }
    }

    /// Handles `ATT_EXCHANGE_MTU_REQ` ([Vol 3] Part F, Section 3.4.2.1).
    pub(crate) async fn handle_exchange_mtu_req(&mut self, pdu: &Pdu) -> Result<()> {
        let r = (pdu.unpack(Opcode::ExchangeMtuReq, |p| Ok(p.u16()))).and_then(|remote| {
            debug!("{} remote preferred MTU: {}", self.cid(), remote);
            let local = self.0.preferred_mtu();
            self.0.set_mtu(local.min(remote));
            self.rsp(Opcode::ExchangeMtuRsp, |p| {
                p.u16(local);
                Ok(())
            })
        });
        self.send_rsp(r).await
    }

    /// Receives a response or confirmation PDU ([Vol 3] Part F, Section 3.4.9).
    /// If `rsp` is `ExchangeMtuRsp`, then this will also return any received
    /// `ExchangeMtuReq` to avoid a deadlock.
    async fn recv_rsp(&mut self, rsp: Opcode) -> Result<Payload> {
        let want = u8::from(rsp);
        let err = matches!(rsp.typ(), PduType::Rsp).then_some(Opcode::ErrorRsp as u8);
        // Transaction timeout ([Vol 3] Part F, Section 3.3.3)
        let r = tokio::time::timeout(
            Duration::from_secs(30),
            self.0.recv_filter(|mut pdu| {
                let have = pdu.u8();
                have == want
                    || (Some(have) == err && pdu.u8() == want)
                    || (want == Opcode::ExchangeMtuRsp as u8
                        && have == Opcode::ExchangeMtuReq as u8)
            }),
        );
        let pdu = match r.await {
            Ok(Ok(pdu)) => pdu,
            Ok(Err(e)) => return Err(e.into()),
            Err(_) => {
                self.0.set_error();
                return Err(Error::Timeout(rsp));
            }
        };
        let mut p = Unpacker::new(pdu.as_ref());
        if Some(p.u8()) == err {
            Err(Error::Att(ErrorRsp {
                req: p.u8(),
                hdl: Handle::new(p.u16()),
                err: ErrorCode::try_from(p.u8()).unwrap_or(ErrorCode::UnlikelyError),
            }))
        } else {
            Ok(pdu)
        }
    }

    /// Returns a response PDU.
    #[allow(clippy::unnecessary_wraps)]
    #[inline(always)]
    fn rsp(&self, op: Opcode, f: impl FnOnce(&mut Packer) -> RspResult<()>) -> RspResult<Rsp> {
        let mut pdu = self.0.new_payload();
        f(pdu.append().u8(op))?;
        trace!("{op}: {:02X?}", pdu.as_ref());
        Ok(Rsp(pdu))
    }

    /// Returns an outbound PDU, calling `f` to encode it after writing the
    /// opcode.
    #[inline(always)]
    fn pack(&self, op: Opcode, f: impl FnOnce(&mut Packer)) -> Payload {
        let mut pdu = self.0.new_payload();
        f(pdu.append().u8(op));
        trace!("{op}: {:02X?}", pdu.as_ref());
        pdu
    }

    /// Sends a PDU over the channel.
    #[inline(always)]
    async fn send(&mut self, pdu: Payload) -> Result<()> {
        Ok(self.0.send(pdu).await?)
    }
}

/// An Iterator-like trait that yields requested attribute handles and values.
pub trait ValueIter<H> {
    /// Returns whether another value is available or an error if it could not
    /// be read.
    fn more(&mut self) -> RspResult<bool>;

    /// Returns the current handle or handle group.
    #[must_use]
    fn handle(&self) -> H {
        unimplemented!()
    }

    /// Returns the current value.
    #[must_use]
    fn value(&self) -> &[u8];
}

/// [`ValueIter`] that calls a closure to generate values.
pub(crate) struct ValueFn<'a, H, F> {
    f: F,
    v: Option<(H, &'a [u8])>,
}

impl<'a, H, F: FnMut() -> Option<(H, &'a [u8])>> ValueFn<'a, H, F> {
    /// Creates a [`ValueIter`] from function `f`.
    #[inline(always)]
    #[must_use]
    pub const fn new(f: F) -> Self {
        Self { f, v: None }
    }
}

impl<'a, H: Copy, F: FnMut() -> Option<(H, &'a [u8])>> ValueIter<H> for ValueFn<'a, H, F> {
    fn more(&mut self) -> RspResult<bool> {
        let Some(val) = (self.f)() else { return Ok(false) };
        self.v = Some(val);
        Ok(true)
    }

    #[inline(always)]
    fn handle(&self) -> H {
        self.v.unwrap().0
    }

    #[inline(always)]
    fn value(&self) -> &[u8] {
        self.v.unwrap().1
    }
}
