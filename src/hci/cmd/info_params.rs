use crate::dev::Addr;
use crate::hci::*;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl<T: host::Transport> Host<T> {
    /// Returns the controller's version information.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.cmd(Opcode::ReadLocalVersionInformation, |_| {})
            .await?
            .map(LocalVersion::from)
    }

    /// Returns the controller's public address.
    pub async fn read_bd_addr(&self) -> Result<Addr> {
        self.cmd(Opcode::ReadBdAddr, |_| {})
            .await?
            .map(|mut e| Addr::Public(e.addr()))
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
