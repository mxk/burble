use crate::hci::*;

// LE Controller commands ([Vol 4] Part E, Section 7.8).
impl<T: host::Transport> Host<T> {
    /// Returns the controller's ACL and ISO packet size and count limits. ISO
    /// limits may be missing if the controller does not support v2 of this
    /// command.
    pub async fn le_read_buffer_size(&self) -> Result<LeBufferInfo> {
        // TODO: Use supported features to determine which version to use?
        {
            let evt = self.cmd(Opcode::LeReadBufferSizeV2).await?;
            if evt.status() != Status::UnknownCommand {
                return evt.into();
            }
        }
        self.cmd(Opcode::LeReadBufferSize).await?.into()
    }
}

/// `HCI_LE_Read_Buffer_Size [v2]` return parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct LeBufferInfo {
    pub acl_max_len: usize,
    pub acl_max_pkts: usize,
    pub iso_max_len: usize,
    pub iso_max_pkts: usize,
}

impl From<Event<'_>> for LeBufferInfo {
    fn from(mut e: Event) -> Self {
        if e.opcode() == Opcode::LeReadBufferSize {
            Self {
                acl_max_len: e.u16() as _,
                acl_max_pkts: e.u8() as _,
                ..Self::default()
            }
        } else {
            Self {
                acl_max_len: e.u16() as _,
                acl_max_pkts: e.u8() as _,
                iso_max_len: e.u16() as _,
                iso_max_pkts: e.u8() as _,
            }
        }
    }
}
