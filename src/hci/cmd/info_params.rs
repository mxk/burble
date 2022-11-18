use crate::hci::*;
use crate::le::Addr;

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl<T: host::Transport> Host<T> {
    /// Returns the controller's version information.
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.exec(Opcode::ReadLocalVersionInformation).await?.into()
    }

    /// Returns the controller's ACL and SCO packet size and count limits.
    pub async fn read_buffer_size(&self) -> Result<BufferSize> {
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
pub struct BufferSize {
    pub acl_data_len: AclDataLen,
    pub acl_num_pkts: u16,
}

impl From<&mut Event<'_>> for BufferSize {
    fn from(e: &mut Event) -> Self {
        let acl_data_len = AclDataLen(e.u16());
        let _sco_data_len = e.u8();
        let acl_num_pkts = e.u16();
        let _sco_num_pkts = e.u16();
        Self {
            acl_data_len,
            acl_num_pkts,
        }
    }
}

/// Maximum size of data in an ACL data packet excluding the header.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct AclDataLen(pub(crate) u16);

impl From<AclDataLen> for u16 {
    #[inline]
    fn from(v: AclDataLen) -> Self {
        v.0
    }
}

impl From<AclDataLen> for usize {
    #[inline]
    fn from(v: AclDataLen) -> Self {
        Self::from(v.0)
    }
}
