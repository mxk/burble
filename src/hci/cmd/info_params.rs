use crate::hci::*;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl<T: host::Transport> Host<T> {
    /// Returns version information for the local controller.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.cmd(Opcode::ReadLocalVersionInformation, |_| {})
            .await?
            .map(LocalVersion::from)
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

impl From<Event<'_>> for LocalVersion {
    fn from(mut e: Event) -> Self {
        Self {
            hci_version: e.u8(),
            hci_subversion: e.u16(),
            lmp_version: e.u8(),
            company_id: e.u16(),
            lmp_subversion: e.u16(),
        }
    }
}
