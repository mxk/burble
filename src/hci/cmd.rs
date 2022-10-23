use bytes::{BufMut, BytesMut};

pub use {hci_control::*, info_params::*, le::*};

use crate::host::Transfer;

use super::*;

mod hci_control;
mod info_params;
mod le;

/// HCI command header and buffer sizes ([Vol 4] Part E, Section 5.4.1).
const CMD_HDR: usize = 3;
pub(crate) const CMD_BUF: usize = CMD_HDR + u8::MAX as usize;

/// HCI command encoder.
#[derive(Debug)]
pub(super) struct Command<'a, T: host::Transport> {
    host: &'a Host<T>,
    xfer: T::Transfer,
    opcode: Opcode,
    hdr: usize,
}

impl<'a, T: host::Transport> Command<'a, T> {
    /// Allocates a new HCI command.
    #[must_use]
    pub fn new(host: &'a Host<T>, opcode: Opcode) -> Self {
        // TODO: Transfer reuse
        let xfer = host.transport.command();
        let hdr = xfer.as_ref().len();
        let mut cmd = Self {
            host,
            xfer,
            opcode,
            hdr,
        };
        cmd.u16(opcode as _).u8(0); // Final length is set in exec()
        cmd
    }

    /// Calls `f` to set command parameters.
    #[inline]
    #[must_use]
    pub fn params(mut self, f: impl FnOnce(&mut Self)) -> Self {
        f(&mut self);
        self
    }

    /// Executes the command and returns its completion event. The caller must
    /// check the completion status to determine whether the command was
    /// successful.
    pub async fn exec(mut self) -> Result<EventGuard<T>> {
        let buf = &mut self.xfer.buf_mut()[self.hdr..];
        buf[CMD_HDR - 1] = u8::try_from(buf.len() - CMD_HDR).expect("command too long");
        trace!("Command: {:02x?}", buf);
        // Event registration must happen first to ensure that the command quota
        // is not exceeded, to check for any conflicting commands, and to
        // guarantee that the completion event will not be missed.
        let mut waiter =
            Arc::clone(&self.host.router).register(EventFilter::Command(self.opcode))?;
        self.xfer.submit()?.await.result().unwrap().map_err(|e| {
            warn!("Failed to submit {} command: {e}", self.opcode);
            e
        })?;
        // TODO: Handle CommandStatus events
        waiter.next().await.ok_or(Error::CommandAborted {
            opcode: self.opcode,
            status: Status::UnspecifiedError,
        })
    }

    /// Returns the command transfer buffer.
    #[inline]
    #[must_use]
    pub fn buf(&mut self) -> &mut BytesMut {
        self.xfer.buf_mut()
    }

    /// Writes a u8 parameter to the command buffer.
    #[inline]
    pub fn u8(&mut self, v: u8) -> &mut Self {
        self.buf().put_u8(v);
        self
    }

    /// Writes a u16 parameter to the command buffer.
    #[inline]
    pub fn u16(&mut self, v: u16) -> &mut Self {
        self.buf().put_u16_le(v);
        self
    }
}
