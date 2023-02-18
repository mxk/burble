use structbuf::Unpacker;

use crate::hci::*;
use crate::le::Addr;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl Host {
    /// Returns the controller's version information.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.exec(Opcode::ReadLocalVersionInformation).await?.ok()
    }

    /// Returns the controller's packet size and count limits.
    pub async fn read_buffer_size(&self) -> Result<BufferSize> {
        self.exec(Opcode::ReadBufferSize).await?.ok()
    }

    /// Returns the controller's public address.
    pub async fn read_bd_addr(&self) -> Result<Addr> {
        let r = self.exec(Opcode::ReadBdAddr);
        r.await?.map_ok(|_, p| Addr::Public(p.addr()))
    }
}

/// `HCI_Read_Local_Version_Information` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct LocalVersion {
    pub hci_version: CoreVersion,
    pub hci_subversion: u16,
    pub lmp_version: CoreVersion,
    pub company_id: CompanyId,
    pub lmp_subversion: u16,
}

impl FromEvent for LocalVersion {
    fn unpack(_: &Event, p: &mut Unpacker) -> Self {
        Self {
            hci_version: CoreVersion::from(p.u8()),
            hci_subversion: p.u16(),
            lmp_version: CoreVersion::from(p.u8()),
            company_id: CompanyId(p.u16()),
            lmp_subversion: p.u16(),
        }
    }
}

/// `HCI_Read_Buffer_Size` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct BufferSize {
    pub acl_data_len: u16,
    pub acl_num_pkts: u16,
}

impl FromEvent for BufferSize {
    fn unpack(_: &Event, p: &mut Unpacker) -> Self {
        let (acl_data_len, _sco_data_len) = (p.u16(), p.u8());
        let (acl_num_pkts, _sco_num_pkts) = (p.u16(), p.u16());
        Self {
            acl_data_len,
            acl_num_pkts,
        }
    }
}
