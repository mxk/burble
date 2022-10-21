//! Host Controller Interface.

use std::fmt::Debug;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};

use bytes::Bytes;
use tokio_util::sync::CancellationToken;
use tracing::{debug, trace, warn};

pub use {cmd::*, codes::*, event::*};

use crate::host;
use crate::host::Transfer;

mod cmd;
mod codes;
mod event;

#[cfg(test)]
mod tests;

/// Error type returned by the HCI layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
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
    #[error("event filter conflict (duplicate commands issued?)")]
    FilterConflict,
    #[error("command quota exceeded")]
    CommandQuotaExceeded,
    #[error("{opcode} command failed: {status}")]
    CommandFailed { opcode: Opcode, status: Status },
    #[error("{opcode} command aborted: {status}")]
    CommandAborted { opcode: Opcode, status: Status },
    #[error("non-command event: {typ}")]
    NonCommandEvent { typ: EventType },
}

impl Error {
    /// Returns the HCI status code, if any.
    pub const fn status(&self) -> Option<Status> {
        use Error::*;
        match *self {
            Hci { status } | CommandFailed { status, .. } | CommandAborted { status, .. } => {
                Some(status)
            }
            Host(_)
            | InvalidEvent(_)
            | UnknownEvent { .. }
            | FilterConflict
            | CommandQuotaExceeded
            | NonCommandEvent { .. } => None,
        }
    }
}

impl From<CommandStatus> for Error {
    #[inline]
    fn from(st: CommandStatus) -> Self {
        Self::CommandFailed {
            opcode: st.opcode,
            status: st.status,
        }
    }
}

/// Common HCI result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Host-side of a Host Controller Interface.
#[derive(Clone, Debug)]
pub struct Host<T: host::Transport> {
    transport: Arc<T>,
    router: Arc<EventRouter<T>>,
    buf_info: LeBufferInfo,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI host using transport layer `t`.
    #[must_use]
    pub fn new(transport: Arc<T>) -> Self {
        Self {
            transport: Arc::clone(&transport),
            router: EventRouter::new(transport),
            buf_info: LeBufferInfo::default(),
        }
    }

    /// Performs a reset and basic controller initialization.
    pub async fn init(&mut self) -> Result<()> {
        self.reset().await?;
        // [Vol 4] Part E, Section 4.1 and [Vol 4] Part E, Section 7.8.2
        self.buf_info = self.le_read_buffer_size().await?;
        if self.buf_info.acl_max_pkts == 0 {
            let bi = self.read_buffer_size().await?;
            self.buf_info.acl_max_len = bi.acl_max_len;
            self.buf_info.acl_max_pkts = bi.acl_max_pkts;
        }
        debug!("Controller buffers: {:?}", self.buf_info);

        self.cmd_with(Opcode::WriteLeHostSupport, |mut cmd| {
            cmd.u8(0x01).u8(0);
        })
        .await?
        .map(|_| ())
        .or_else(|e| match e.status() {
            // TODO: Why InvalidCommandParameters?
            Some(Status::UnknownCommand | Status::InvalidCommandParameters) => Ok(()),
            _ => Err(e),
        })?;
        Ok(())
    }

    /// Receives the next HCI event, routes it to registered waiters, and
    /// returns it to the caller. This is a low-level method. In most cases,
    /// `enable_events` is a better choice.
    ///
    /// No events are received, including command completion, unless the
    /// returned future is polled, and no new events can be received until the
    /// returned guard is dropped. If there are multiple concurrent calls to
    /// this method, only one will resolve to the received event and the others
    /// will continue to wait.
    #[inline]
    pub async fn event(&self) -> Result<EventGuard<T>> {
        self.router.recv_event().await
    }

    /// Executes an HCI command and returns the command completion event. The
    /// caller must check the completion status to determine whether the command
    /// was successful.
    async fn cmd(&self, opcode: Opcode) -> Result<EventGuard<T>> {
        self.cmd_with(opcode, |_cmd| ()).await
    }

    // TODO: Move parameter encoding to Command?

    /// Executes an HCI command, calling `enc` to provide parameters, and
    /// returns the command completion event. The caller must check the
    /// completion status to determine whether the command was successful.
    async fn cmd_with(
        &self,
        opcode: Opcode,
        enc: impl FnOnce(Command) + Send,
    ) -> Result<EventGuard<T>> {
        let mut cmd = self.transport.command();
        let off = cmd.buf_mut().len();
        enc(Command::new(opcode, cmd.buf_mut()));
        trace!("Command: {:02x?}", &cmd.buf_mut()[off..]);
        let mut waiter = Arc::clone(&self.router).register(EventFilter::Command(opcode))?;
        cmd.submit()?.await.result().unwrap().map_err(|e| {
            warn!("{opcode:?} failed: {e}");
            e
        })?;
        waiter.next().await.ok_or(Error::CommandAborted {
            opcode,
            status: Status::UnspecifiedError,
        })
    }
}

impl<T: host::Transport + 'static> Host<T> {
    /// Spawns a task that continuously receives HCI events until an error is
    /// encountered. The task is canceled when the returned future is dropped.
    #[inline]
    #[must_use]
    pub fn enable_events(&self) -> EventReceiverTask {
        EventReceiverTask::new(Arc::clone(&self.router))
    }
}
