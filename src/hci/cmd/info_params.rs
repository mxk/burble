use crate::dev::Addr;
use crate::hci::*;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl<T: host::Transport> Host<T> {
    /// Returns the controller's version information.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.cmd(Opcode::ReadLocalVersionInformation).await?.into()
    }

    /// Returns the controller's ACL and SCO packet size and count limits.
    pub async fn read_buffer_size(&self) -> Result<BufferInfo> {
        self.cmd(Opcode::ReadBufferSize).await?.into()
    }

    /// Returns the controller's public address.
    pub async fn read_bd_addr(&self) -> Result<Addr> {
        self.cmd(Opcode::ReadBdAddr)
            .await?
            .map(|mut e| Addr::Public(e.addr()))
    }
}

/// `HCI_Read_Local_Version_Information` return parameters.
#[derive(Clone, Copy, Debug, Default)]
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

/// `HCI_Read_Buffer_Size` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct BufferInfo {
    pub acl_max_len: usize,
    pub acl_max_pkts: usize,
    pub sco_max_len: usize,
    pub sco_max_pkts: usize,
}

impl From<Event<'_>> for BufferInfo {
    fn from(mut e: Event) -> Self {
        Self {
            acl_max_len: e.u16() as _,
            sco_max_len: e.u8() as _,
            acl_max_pkts: e.u16() as _,
            sco_max_pkts: e.u16() as _,
        }
    }
}
