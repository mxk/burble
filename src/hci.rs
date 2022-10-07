//! Host Controller Interface.

use std::fmt::Debug;
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
    #[error("{opcode} command failed: {status}")]
    CommandFailed { opcode: Opcode, status: Status },
    #[error("{opcode} command aborted")]
    CommandAborted { opcode: Opcode },
}

impl From<CommandStatus> for Error {
    fn from(st: CommandStatus) -> Self {
        Error::CommandFailed {
            opcode: st.opcode,
            status: st.status,
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

/// Host-side of a Host Controller Interface.
#[derive(Clone, Debug)]
pub struct Host<T: host::Transport> {
    transport: Arc<T>,
    router: Arc<EventRouter<T>>,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI host using transport layer `t`.
    pub fn new(t: T) -> Self {
        let transport = Arc::new(t);
        let router = EventRouter::new(transport.clone());
        Self { transport, router }
    }

    /// Receives the next HCI event, routes it to registered waiters, and
    /// returns it to the caller. No events, including command completion, are
    /// received unless the returned future is polled, and no new events can be
    /// received until the returned guard is dropped. If there are multiple
    /// concurrent calls to this method, only one will resolve to the received
    /// event and the others will continue waiting.
    pub async fn event(&self) -> Result<EventGuard<T>> {
        self.router.recv_event().await
    }

    /// Executes an HCI command.
    async fn cmd(&self, opcode: Opcode, enc: impl FnOnce(Command)) -> Result<EventGuard<T>> {
        let mut waiter = self.router.clone().register(EventFilter::Command(opcode));
        let mut cmd = self.transport.cmd(|b| enc(Command::new(opcode, b)))?;
        cmd.result().await.map_err(|e| {
            warn!("{opcode:?} failed: {e}");
            e
        })?;
        waiter.next().await.ok_or(Error::CommandAborted { opcode })
    }
}

/// HCI command header and buffer sizes ([Vol 4] Part E, Section 5.4.1).
const CMD_HDR: usize = 3;
pub(crate) const CMD_BUF: usize = CMD_HDR + u8::MAX as usize;

/// HCI command encoder.
#[derive(Debug)]
struct Command<'a> {
    b: &'a mut BytesMut,
}

impl<'a> Command<'a> {
    fn new(opcode: Opcode, b: &'a mut BytesMut) -> Self {
        let mut cmd = Self { b };
        cmd.u16(opcode as _).u8(0);
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
pub struct CommandQuota(u8);
