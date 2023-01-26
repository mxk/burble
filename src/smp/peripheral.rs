use crate::host;
use crate::l2cap::BasicChan;

use super::*;

/// Peripheral role security manager implementing LE security mode 1 level 4
/// "Authenticated LE Secure Connections pairing" in "Secure Connections Only"
/// mode ([Vol 3] Part C, Section 10.2.1 and 10.2.4).
#[derive(Debug)]
pub struct Peripheral<T: host::Transport> {
    ch: BasicChan<T>,
}

impl<T: host::Transport> Peripheral<T> {
    /// Creates a new peripheral security manager.
    #[inline]
    pub(crate) const fn new(ch: BasicChan<T>) -> Self {
        Self { ch }
    }

    /// Returns the next command. This method is cancel safe.
    #[inline]
    async fn recv(&self) -> Result<Command> {
        Ok(Command::try_from(self.ch.recv().await?)?)
    }
}
