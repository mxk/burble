//! Host Controller Interface.

use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

use bytes::{BufMut, Bytes, BytesMut};
use tracing::warn;

pub use {codes::*, event::*, info::*};

use crate::host;
use crate::host::Transfer;

mod codes;
mod event;
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
    transport: Arc<T>,
    router: Arc<EventRouter<T>>,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI using transport layer `t`.
    pub fn new(t: T) -> Self {
        let transport = Arc::new(t);
        let router = EventRouter::new(transport.clone());
        Self { transport, router }
    }

    /// Receives and routes HCI events to registered callbacks. The returned
    /// future must be polled in order for commands to finish executing.
    pub async fn next_event(&self) -> Result<EventGuard<T>> {
        Ok(self.router.recv_event().await?)
    }

    /// Executes an HCI command.
    async fn cmd(&self, opcode: Opcode, enc: impl FnOnce(Cmd)) -> Result<EventWaiterGuard<T>> {
        let w = self.router.clone().register(Filter::Command(opcode));
        self.transport
            .cmd(|b| enc(Cmd::new(opcode, b)))?
            .result()
            .await
            .map_err(|e| {
                warn!("Command {opcode} failed: {e}");
                e
            })?;
        Ok(w)
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
