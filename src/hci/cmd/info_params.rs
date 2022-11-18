use crate::hci::*;
use crate::le::Addr;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl<T: host::Transport> Host<T> {
    /// Returns the controller's version information.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.exec(Opcode::ReadLocalVersionInformation).await?.into()
    }

    /// Returns the controller's ACL and SCO packet size and count limits.
    pub async fn read_buffer_size(&self) -> Result<BufferInfo> {
        self.exec(Opcode::ReadBufferSize).await?.into()
    }

    /// Returns the controller's public address.
    pub async fn read_bd_addr(&self) -> Result<Addr> {
        let r = self.exec(Opcode::ReadBdAddr);
        Ok(Addr::Public(r.await?.ok()?.addr()))
    }
}

/// `HCI_Read_Local_Version_Information` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct LocalVersion {
    pub hci_version: CoreVersion,
    pub hci_subversion: u16,
    pub lmp_version: CoreVersion,
    pub company_id: u16,
    pub lmp_subversion: u16,
}

impl From<&mut Event<'_>> for LocalVersion {
    fn from(e: &mut Event) -> Self {
        Self {
            hci_version: CoreVersion::from(e.u8()),
            hci_subversion: e.u16(),
            lmp_version: CoreVersion::from(e.u8()),
            company_id: e.u16(),
            lmp_subversion: e.u16(),
        }
    }
}

/// `HCI_Read_Buffer_Size` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct BufferInfo {
    pub acl_max_len: usize,
    pub acl_max_pkts: usize,
    pub sco_max_len: usize,
    pub sco_max_pkts: usize,
}

impl From<&mut Event<'_>> for BufferInfo {
    fn from(e: &mut Event) -> Self {
        Self {
            acl_max_len: usize::from(e.u16()),
            sco_max_len: usize::from(e.u8()),
            acl_max_pkts: usize::from(e.u16()),
            sco_max_pkts: usize::from(e.u16()),
        }
    }
}
