use structbuf::Unpacker;

use crate::hci::*;
use crate::le;

/// HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3).
impl Host {
    /// Configures which events can be generated by the controller
    /// ([Vol 4] Part E, Section 7.3.1).
    pub async fn set_event_mask(&self, enable: &EventMask) -> Result<()> {
        let r = self.exec_params(Opcode::SetEventMask, |cmd| {
            cmd.u64(enable.p1);
        });
        r.await?.ok()
    }

    /// Resets the controller's link manager, baseband, and link layer
    /// ([Vol 4] Part E, Section 7.3.2).
    pub async fn reset(&self) -> Result<()> {
        self.exec(Opcode::Reset).await?.ok()
    }

    /// Turns flow control on or off for data sent from the controller to the
    /// host ([Vol 4] Part E, Section 7.3.38).
    pub async fn set_controller_to_host_flow_control(&self, enable: bool) -> Result<()> {
        let r = self.exec_params(Opcode::SetControllerToHostFlowControl, |cmd| {
            cmd.u8(u8::from(enable));
        });
        r.await?.ok()
    }

    /// Sets the maximum size of the data portion of ACL and SCO data packets
    /// sent from the controller to the host ([Vol 4] Part E, Section 7.3.39).
    pub async fn host_buffer_size(&self, bs: BufferSize) -> Result<()> {
        let r = self.exec_params(Opcode::HostBufferSize, |cmd| {
            cmd.u16(bs.acl_data_len);
            cmd.u8(0);
            cmd.u16(bs.acl_num_pkts);
            cmd.u16(0_u16);
        });
        r.await?.ok()
    }

    /// Configures which events can be generated by the controller
    /// ([Vol 4] Part E, Section 7.3.69).
    pub async fn set_event_mask_page_2(&self, enable: &EventMask) -> Result<()> {
        let r = self.exec_params(Opcode::SetEventMaskPage2, |cmd| {
            cmd.u64(enable.p2);
        });
        r.await?.ok()
    }

    /// Sets the LE Supported (Host) Link Manager Protocol feature bit
    /// ([Vol 4] Part E, Section 7.3.79).
    pub async fn write_le_host_support(&self, enable: bool) -> Result<()> {
        let r = self.exec_params(Opcode::WriteLeHostSupport, |cmd| {
            cmd.bool(enable).u8(0);
        });
        r.await?.ok()
    }
}

/// Informational parameters commands ([Vol 4] Part E, Section 7.4).
impl Host {
    /// Returns the controller's version information
    /// ([Vol 4] Part E, Section 7.4.1).
    pub async fn read_local_version(&self) -> Result<LocalVersion> {
        self.exec(Opcode::ReadLocalVersionInformation).await?.ok()
    }

    /// Returns the commands supported by the local controller
    /// ([Vol 4] Part E, Section 7.4.2).
    pub async fn read_local_supported_commands(&self) -> Result<SupportedCommands> {
        let r = self.exec(Opcode::ReadLocalSupportedCommands);
        // SAFETY: All bit patterns are valid
        r.await?.map_ok(|_, p| unsafe { p.read() })
    }

    /// Returns the controller's packet size and count limits
    /// ([Vol 4] Part E, Section 7.4.5).
    pub async fn read_buffer_size(&self) -> Result<BufferSize> {
        self.exec(Opcode::ReadBufferSize).await?.ok()
    }

    /// Returns the controller's public address ([Vol 4] Part E, Section 7.4.6).
    pub async fn read_bd_addr(&self) -> Result<le::Addr> {
        let r = self.exec(Opcode::ReadBdAddr);
        r.await?.map_ok(|_, p| le::Addr::Public(p.addr()))
    }
}

/// `HCI_Set_Event_Mask`, `HCI_Set_Event_Mask_Page_2`, and
/// `HCI_LE_Set_Event_Mask` command parameters
/// ([Vol 4] Part E, Section 7.3.1, 7.3.69, 7.8.1).
#[derive(Clone, Copy, Debug, Default)]
pub struct EventMask {
    pub(in crate::hci) p1: u64,
    pub(in crate::hci) p2: u64,
    pub(in crate::hci) le: u64,
}

impl FromIterator<EventCode> for EventMask {
    /// Creates an event mask from an iterator of events to enable.
    #[must_use]
    fn from_iter<T: IntoIterator<Item = EventCode>>(it: T) -> Self {
        let mut m = Self::default();
        for c in it {
            c.set(&mut m, true);
        }
        m
    }
}

/// `HCI_Host_Buffer_Size` and `HCI_Read_Buffer_Size` command/return parameters
/// ([Vol 4] Part E, Section 7.3.39, 7.4.5).
#[derive(Clone, Copy, Debug, Default)]
pub struct BufferSize {
    pub acl_data_len: u16,
    pub acl_num_pkts: u16,
}

impl FromEvent for BufferSize {
    #[inline]
    fn unpack(_: &Event, p: &mut Unpacker) -> Self {
        let (acl_data_len, _sco_data_len) = (p.u16(), p.u8());
        let (acl_num_pkts, _sco_num_pkts) = (p.u16(), p.u16());
        Self {
            acl_data_len,
            acl_num_pkts,
        }
    }
}

/// `HCI_Read_Local_Version_Information` return parameters
/// ([Vol 4] Part E, Section 7.4.1).
#[derive(Clone, Copy, Debug, Default)]
pub struct LocalVersion {
    pub hci_version: CoreVersion,
    pub hci_subversion: u16,
    pub lmp_version: CoreVersion,
    pub company_id: CompanyId,
    pub lmp_subversion: u16,
}

impl FromEvent for LocalVersion {
    #[inline]
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

/// `HCI_Read_Local_Supported_Commands` return parameter
/// ([Vol 4] Part E, Section 7.4.2).
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct SupportedCommands([u8; 64]);

impl SupportedCommands {
    /// Returns whether the specified command is supported.
    #[inline]
    #[must_use]
    pub fn is_supported(&self, cmd: Opcode) -> bool {
        let (octet, mask) = cmd.mask();
        // SAFETY: octet < 64
        unsafe { self.0.get_unchecked(octet) & mask != 0 || mask == 0 }
    }
}

impl Default for SupportedCommands {
    #[inline(always)]
    fn default() -> Self {
        Self([0; 64])
    }
}
