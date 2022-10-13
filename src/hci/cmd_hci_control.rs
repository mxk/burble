use super::*;

/// HCI Control and Baseband commands ([Vol 4] Part E, Section 7.3).
impl<T: host::Transport> Host<T> {
    /// Reset the Controller's link manager, baseband, and link layer.
    pub async fn reset(&self) -> Result<()> {
        let mut evt = self.cmd(Opcode::Reset, |_| {}).await?;
        evt.map_ok(|_| ())
    }
}
