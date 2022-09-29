use super::*;

/// Informational parameter commands ([Vol 4] Part E, Section 7.4)
impl<T: host::Transport> Host<T> {
    /// Reads version information for the local controller.
    pub fn read_local_version(&self) -> Result<LocalVersion> {
        let mut cmd = Cmd::new(Opcode::info(0x0001));
        self.t.write_cmd(cmd.as_bytes())?;
        loop {
            let mut evt = Evt::new();
            let n = self.t.read_event(evt.reset())?;
            evt.ready(n)?;
            match evt.cmd_status() {
                Some(st) if st.opcode == cmd.opcode => {
                    if !st.status.is_ok() {
                        return Err(st.into());
                    }
                }
                _ => continue,
            }
            return Ok(LocalVersion {
                hci_version: evt.u8(),
                hci_subversion: evt.u16(),
                lmp_version: evt.u8(),
                company_id: evt.u16(),
                lmp_subversion: evt.u16(),
            });
        }
    }
}

/// `HCI_Read_Local_Version_Information` return parameters.
#[derive(Clone, Copy, Debug)]
pub struct LocalVersion {
    pub hci_version: u8,
    pub hci_subversion: u16,
    pub lmp_version: u8,
    pub company_id: u16,
    pub lmp_subversion: u16,
}
