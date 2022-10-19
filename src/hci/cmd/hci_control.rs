use crate::hci::*;

/// HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3).
impl<T: host::Transport> Host<T> {
    /// Reset the Controller's link manager, baseband, and link layer.
    pub async fn reset(&self) -> Result<()> {
        self.cmd(Opcode::Reset, |_| ()).await?.map(|_| ())
    }
}
