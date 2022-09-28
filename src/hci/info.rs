use crate::{Hci, Opcode, Transport};
use crate::hci::{Cmd, Evt};

use super::Result;

// Informational parameter commands ([Vol 4] Part E, Section 7.4)
impl<T: Transport> Hci<T> {
    /// Reads the values for the version information for the local Controller.
    pub fn read_local_version(&self) -> Result<LocalVersion> {
        let mut cmd = Cmd::new(Opcode::info(0x0001));
        self.t.write_cmd(cmd.as_bytes())?;
        loop {
            let mut ev = Evt::try_from(self.t.read_event()?)?;
            match ev.cmd_status() {
                Some(st) if st.opcode == cmd.opcode => {
                    if !st.status.is_ok() {
                        return Err(st.into());
                    }
                }
                _ => continue,
            }
            return Ok(LocalVersion {
                hci_version: ev.u8(),
                hci_subversion: ev.u16(),
                lmp_version: ev.u8(),
                company_id: ev.u16(),
                lmp_subversion: ev.u16(),
            });
        }
    }
}

/// HCI_Read_Local_Version_Information return parameters.
#[derive(Clone, Copy, Debug)]
pub struct LocalVersion {
    pub hci_version: u8,
    pub hci_subversion: u16,
    pub lmp_version: u8,
    pub company_id: u16,
    pub lmp_subversion: u16,
}
