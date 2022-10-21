use crate::hci::*;

/// HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3).
impl<T: host::Transport> Host<T> {
    /// Reset the controller's link manager, baseband, and link layer.
    pub async fn reset(&self) -> Result<()> {
        self.exec(Opcode::Reset).await?.into()
    }
}
