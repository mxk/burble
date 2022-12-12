use std::ops::{Deref, DerefMut};

use structbuf::StructBuf;
use tracing::{trace, warn};

pub use {hci_control::*, info_params::*, le::*};

use crate::host::Transfer;

use super::*;

mod hci_control;
mod info_params;
mod le;

/// HCI command encoder.
#[derive(Debug)]
pub(super) struct Command<T: host::Transport> {
    router: Arc<EventRouter<T>>,
    opcode: Opcode,
    xfer: T::Transfer,
}

impl<T: host::Transport> Command<T> {
    /// Creates a new HCI command.
    #[must_use]
    pub fn new(host: &Host<T>, opcode: Opcode) -> Self {
        // TODO: Transfer reuse
        let mut cmd = Self {
            router: Arc::clone(&host.router),
            opcode,
            xfer: host.transport.command(),
        };
        cmd.pack().u16(opcode).u8(0); // Final length is set in exec()
        cmd
    }

    /// Executes the command and returns its completion event. The caller must
    /// check the completion status to determine whether the command was
    /// successful.
    pub async fn exec(mut self) -> Result<EventGuard<T>> {
        let hdr_len = self.xfer.hdr_len();
        let buf = &mut self.xfer.buf_mut()[hdr_len..];
        buf[CMD_HDR - 1] = u8::try_from(buf.len() - CMD_HDR).expect("command too long");
        trace!("Command: {:02X?}", buf);
        // Event registration must happen first to ensure that the command quota
        // is not exceeded, to check for any conflicting commands, and to
        // guarantee that the completion event will not be missed.
        let waiter = self.router.register(EventFilter::Command(self.opcode))?;
        self.xfer.submit()?.await.result().unwrap().map_err(|e| {
            warn!("Failed to submit {} command: {e}", self.opcode);
            e
        })?;
        // [Vol 4] Part E, Section 4.4
        loop {
            let g = waiter.next().await.map_err(|e| Error::CommandAborted {
                opcode: self.opcode,
                status: e.status().unwrap_or(Status::UnspecifiedError),
            })?;
            if g.typ() == EventType::Hci(EventCode::CommandComplete) {
                return Ok(g);
            } else if let Err(e) = g.ok() {
                return Err(e); // Failed CommandStatus
            }
        }
    }
}

impl<T: host::Transport> Deref for Command<T> {
    type Target = StructBuf;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.xfer.buf()
    }
}

impl<T: host::Transport> DerefMut for Command<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.xfer.buf_mut()
    }
}
