use structbuf::Pack;
use tokio::time::timeout;
use tracing::trace;

pub use {hci::*, le::*};

use super::*;

mod hci;
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
        let cmd_guard = self.router.reserve(self.opcode).await;
        *self.host_cmd.lock() = Some(self.xfer.exec().await?);
        let mut events = cmd_guard.submitted();
        // Handle command status and completion events with a one-second timeout
        // ([Vol 4] Part E, Section 4.4).
        loop {
            let evt = match timeout(Duration::from_secs(1), events.next()).await {
                Ok(r) => r.map_err(|e| Error::CommandAborted {
                    opcode: self.opcode,
                    status: e.status().unwrap_or(Status::UnspecifiedError),
                })?,
                Err(_timeout) => {
                    return Err(Error::CommandTimeout {
                        opcode: self.opcode,
                    })
                }
            };
            if matches!(evt.code(), EventCode::CommandComplete) {
                return Ok(evt);
            } else if let Err(e) = evt.cmd_ok() {
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
