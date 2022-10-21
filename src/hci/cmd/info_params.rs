use crate::dev::Addr;
use crate::hci::*;

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
        let evt = self.exec(Opcode::ReadBdAddr).await?;
        Ok(Addr::Public(evt.cmd_ok()?.addr()))
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

impl From<&mut Event<'_>> for LocalVersion {
    fn from(e: &mut Event) -> Self {
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

impl From<&mut Event<'_>> for BufferInfo {
    fn from(e: &mut Event) -> Self {
        Self {
            acl_max_len: e.u16() as _,
            sco_max_len: e.u8() as _,
            acl_max_pkts: e.u16() as _,
            sco_max_pkts: e.u16() as _,
        }
    }
}
