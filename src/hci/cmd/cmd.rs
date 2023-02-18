use structbuf::Pack;
use tracing::trace;

pub use {hci_control::*, info_params::*, le::*};

use super::*;

mod hci_control;
mod info_params;
mod le;

/// HCI command encoder.
#[derive(Debug)]
pub(super) struct Command {
    router: Arc<EventRouter>,
    opcode: Opcode,
    xfer: Box<dyn host::Transfer>,
    host_cmd: Arc<CommandTransfer>,
}

impl Command {
    /// Creates a new HCI command.
    #[must_use]
    pub fn new(host: &Host, opcode: Opcode) -> Self {
        let mut cmd = Self {
            router: Arc::clone(&host.router),
            opcode,
            xfer: host.new_cmd(),
            host_cmd: Arc::clone(&host.cmd),
        };
        cmd.append().u16(opcode).u8(0); // Final length is set in exec()
        cmd
    }

    /// Executes the command and returns its completion event. The caller must
    /// check the completion status to determine whether the command was
    /// successful.
    pub async fn exec(mut self) -> Result<Event> {
        let xfer = self.xfer.as_mut();
        let n = u8::try_from(xfer.as_ref().len() - CMD_HDR).expect("command too long");
        xfer.at(CMD_HDR - 1).u8(n);
        trace!("Command: {:02X?}", xfer.as_ref());
        // Event registration must happen first to ensure that the command quota
        // is not exceeded, to check for any conflicting commands, and to
        // guarantee that the completion event will not be missed.
        let mut events = self.router.events(self.opcode)?;
        // TODO: Block on CommandQuotaExceeded
        // TODO: Restore cmd_quota if submit() fails? Probably doesn't matter.
        *self.host_cmd.lock() = Some(self.xfer.submit()?.await?);
        // [Vol 4] Part E, Section 4.4
        loop {
            let e = events.next().await.map_err(|e| Error::CommandAborted {
                opcode: self.opcode,
                status: e.status().unwrap_or(Status::UnspecifiedError),
            })?;
            if e.typ() == EventType::Hci(EventCode::CommandComplete) {
                return Ok(e);
            } else if let Err(e) = e.cmd_ok() {
                return Err(e); // Failed CommandStatus
            }
        }
    }
}

impl Pack for Command {
    #[inline]
    fn append(&mut self) -> Packer {
        self.xfer.append()
    }

    #[inline]
    fn at(&mut self, i: usize) -> Packer {
        self.xfer.at(CMD_HDR + i)
    }
}
