use super::*;

/// Informational parameter commands ([Vol 4] Part E, Section 7.4)
impl<T: host::Transport> Host<T> {
    /// Reads version information for the local controller.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        let mut r = self.cmd(Opcode::info(0x0001), |_| {}).await?;
        r.map_ok(|mut evt| LocalVersion {
            hci_version: evt.u8(),
            hci_subversion: evt.u16(),
            lmp_version: evt.u8(),
            company_id: evt.u16(),
            lmp_subversion: evt.u16(),
        })
        .await
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
