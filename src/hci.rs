//! Host Controller Interface.

use std::fmt::{Debug, Display, Formatter};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use tracing::{trace, warn};

pub use {codes::*, info::*};

use crate::host;
use crate::host::Transfer;

mod codes;
mod info;

#[cfg(test)]
mod tests;

/// Error type returned by the HCI layer.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Host(#[from] host::Error),
    #[error("HCI error: {status}")]
    Hci {
        #[from]
        status: Status,
        // TODO: Add backtrace once stabilized
    },
    #[error("invalid event: {0:?}")]
    InvalidEvent(Bytes),
    #[error("unknown event [code={code:02x}, subevent={subevent:02x}]: {params:?}")]
    UnknownEvent {
        code: u8,
        subevent: u8,
        params: Bytes,
    },
    #[error("command error [opcode={opcode}, status={status}]")]
    Command { opcode: Opcode, status: Status },
}

impl From<CmdStatus> for Error {
    fn from(s: CmdStatus) -> Self {
        Error::Command {
            opcode: s.opcode,
            status: s.status,
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

/// Host-side of a Host Controller Interface.
#[derive(Debug)]
pub struct Host<T: host::Transport> {
    t: T,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI using transport layer `t`.
    pub fn new(t: T) -> Self {
        Self { t }
    }

    /// Executes an HCI command.
    async fn cmd<R: Debug>(
        &mut self,
        opcode: Opcode,
        enc: impl FnOnce(Cmd),
        mut dec: impl FnMut(Evt) -> Result<R>,
    ) -> Result<R> {
        self.t
            .cmd(|b| enc(Cmd::new(opcode, b)))?
            .result()
            .await
            .map_err(|e| {
                warn!("Command {opcode} failed: {e}");
                e
            })?;
        self.wait_event(|mut evt| match evt.cmd_status() {
            Some(st) if st.opcode == opcode => Some(if st.status.is_ok() {
                dec(evt)
            } else {
                Err(Error::from(st))
            }),
            _ => None,
        })
        .await
    }

    /// Returns the next HCI event.
    async fn wait_event<R: Debug>(
        &mut self,
        mut f: impl FnMut(Evt) -> Option<Result<R>>,
    ) -> Result<R> {
        // TODO: Reuse event
        loop {
            let mut t = self.t.evt()?;
            t.result().await?;
            if let Some(r) = f(Evt::try_from(t.buf().as_ref())?) {
                return r;
            }
        }
    }
}

/// HCI command header and buffer sizes ([Vol 4] Part E, Section 5.4.1).
const CMD_HDR: usize = 3;
pub(crate) const CMD_BUF: usize = CMD_HDR + u8::MAX as usize;

/// HCI command encoder.
#[derive(Debug)]
struct Cmd<'a> {
    b: &'a mut BytesMut,
}

impl<'a> Cmd<'a> {
    fn new(opcode: Opcode, b: &'a mut BytesMut) -> Self {
        let mut cmd = Self { b };
        cmd.u16(opcode.0).u8(0);
        cmd
    }

    fn u8(&mut self, v: u8) -> &mut Self {
        self.b.put_u8(v);
        self
    }

    fn u16(&mut self, v: u16) -> &mut Self {
        self.b.put_u16_le(v);
        self
    }
}

/// HCI event header and buffer sizes ([Vol 4] Part E, Section 5.4.4).
const EVT_HDR: usize = 2;
pub(crate) const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

/// HCI event decoder.
#[derive(Debug)]
struct Evt<'a> {
    typ: EvtType,
    cmd_status: Option<CmdStatus>,
    tail: &'a [u8],
}

impl Evt<'_> {
    /// Returns the event type.
    #[inline]
    #[cfg(test)]
    fn typ(&self) -> EvtType {
        self.typ
    }

    /// Returns any unparsed bytes.
    #[inline]
    #[cfg(test)]
    fn tail(&self) -> &[u8] {
        self.tail
    }

    /// Returns basic information from `CommandComplete` or `CommandStatus`
    /// events. Returns [`None`] for other event types.
    #[inline]
    fn cmd_status(&mut self) -> Option<CmdStatus> {
        self.cmd_status
    }

    #[inline]
    fn u8(&mut self) -> u8 {
        self.tail.get_u8()
    }

    #[inline]
    fn u16(&mut self) -> u16 {
        self.tail.get_u16_le()
    }
}

impl<'a> TryFrom<&'a [u8]> for Evt<'a> {
    type Error = Error;

    /// Tries to parse event header from `orig`. The subevent code for LE events
    /// is also consumed.
    fn try_from(orig: &'a [u8]) -> Result<Self> {
        trace!("Event: {:02x?}", orig);
        if orig.len() < EVT_HDR {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(orig)));
        }
        let mut tail = orig;
        let (code, len) = (tail.get_u8(), tail.get_u8());
        if tail.len() != len as usize {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(orig)));
        }
        let typ = match EventCode::from_repr(code) {
            Some(EventCode::LeMetaEvent) => {
                // After the header is validated, we allow further decoding
                // calls to panic if there is missing data.
                let subevent = tail.get_u8();
                match SubeventCode::from_repr(subevent) {
                    Some(subevent) => EvtType::Le(subevent),
                    None => {
                        return Err(Error::UnknownEvent {
                            code: code as _,
                            subevent,
                            params: Bytes::copy_from_slice(tail),
                        })
                    }
                }
            }
            Some(code) => EvtType::Hci(code),
            None => {
                return Err(Error::UnknownEvent {
                    code,
                    subevent: 0,
                    params: Bytes::copy_from_slice(tail),
                })
            }
        };
        let cmd_status = match typ {
            EvtType::Hci(EventCode::CommandComplete) => Some(CmdStatus {
                quota: CmdQuota(tail.get_u8()),
                opcode: Opcode(tail.get_u16_le()),
                status: if !tail.is_empty() {
                    Status::from(tail.get_u8())
                } else {
                    Status::Success
                },
            }),
            EvtType::Hci(EventCode::CommandStatus) => Some(CmdStatus {
                status: Status::from(tail.get_u8()),
                quota: CmdQuota(tail.get_u8()),
                opcode: Opcode(tail.get_u16_le()),
            }),
            _ => None,
        };
        Ok(Self {
            typ,
            cmd_status,
            tail,
        })
    }
}

/// Combined event code.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EvtType {
    Hci(EventCode),
    Le(SubeventCode),
}

/// Basic information contained in a `CommandComplete` or `CommandStatus`
/// events.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct CmdStatus {
    quota: CmdQuota,
    opcode: Opcode,
    status: Status,
}

/// Number of HCI command packets the host can send to the controller.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CmdQuota(u8);

/// Command opcode.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Opcode(pub u16);

impl Opcode {
    /// Returns an opcode from Opcode Group Field and Opcode Command Field.
    #[inline]
    pub fn new(ogf: u16, ocf: u16) -> Self {
        Self(ogf << 10 | ocf)
    }

    /// Returns an informational parameter command opcode.
    #[inline]
    pub fn info(ocf: u16) -> Self {
        Self::new(0x04, ocf)
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:04x}", self.0)
    }
}
