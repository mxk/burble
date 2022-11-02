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
pub(super) struct Command<T: host::Transport> {
    router: Arc<EventRouter<T>>,
    xfer: T::Transfer,
    opcode: Opcode,
    hdr: usize,
}

impl<T: host::Transport> Command<T> {
    /// Allocates a new HCI command.
    #[must_use]
    pub fn new(host: &Host<T>, opcode: Opcode) -> Self {
        // TODO: Transfer reuse
        let mut cmd = Self {
            router: Arc::clone(&host.router),
            xfer: host.transport.command(),
            opcode,
            hdr: 0,
        };
        cmd.hdr = cmd.xfer.as_ref().len();
        cmd.u16(opcode).u8(0); // Final length is set in exec()
        cmd
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
        let mut waiter = Arc::clone(&self.router).register(EventFilter::Command(self.opcode))?;
        self.xfer.submit()?.await.result().unwrap().map_err(|e| {
            warn!("Failed to submit {} command: {e}", self.opcode);
            e
        })?;
        // [Vol 4] Part E, Section 4.4
        loop {
            let g = waiter.next().await.ok_or(Error::CommandAborted {
                opcode: self.opcode,
                status: Status::UnspecifiedError,
            })?;
            if g.typ() == EventType::Hci(EventCode::CommandComplete) {
                return Ok(g);
            } else if let Err(e) = g.ok() {
                return Err(e); // Failed CommandStatus
            }
        }
    }

    /// Returns the command transfer buffer.
    #[inline]
    #[must_use]
    pub fn buf(&mut self) -> &mut BytesMut {
        self.xfer.buf_mut()
    }

    /// Writes a `bool` parameter to the command buffer.
    #[inline]
    pub fn bool<V: Into<bool>>(&mut self, v: V) -> &mut Self {
        self.buf().put_u8(u8::from(v.into()));
        self
    }

    /// Writes an `i8` parameter to the command buffer.
    #[inline]
    pub fn i8<V: Into<i8>>(&mut self, v: V) -> &mut Self {
        self.buf().put_i8(v.into());
        self
    }

    /// Writes a `u8` parameter to the command buffer.
    #[inline]
    pub fn u8<V: Into<u8>>(&mut self, v: V) -> &mut Self {
        self.buf().put_u8(v.into());
        self
    }

    /// Writes a `u16` parameter to the command buffer.
    #[inline]
    pub fn u16<V: Into<u16>>(&mut self, v: V) -> &mut Self {
        self.buf().put_u16_le(v.into());
        self
    }

    /// Writes a `u24` parameter to the command buffer.
    #[inline]
    pub fn u24<V: Into<u32>>(&mut self, v: V) -> &mut Self {
        let v = v.into().to_le_bytes();
        assert_eq!(v[3], 0);
        self.slice(&v[..3])
    }

    /// Writes raw byte slice to the command buffer.
    #[inline]
    pub fn slice<V: AsRef<[u8]>>(&mut self, v: V) -> &mut Self {
        self.buf().put_slice(v.as_ref());
        self
    }
}
