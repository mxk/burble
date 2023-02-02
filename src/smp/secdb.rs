use std::fmt::Debug;
use std::io;

use burble_crypto::LTK;

use crate::le::Addr;

/// Security keys for a peer device.
#[derive(Debug, Eq, PartialEq)]
pub struct Keys {
    pub ltk: LTK,
}

/// Interface to security database storage.
pub trait Store: Debug {
    /// Saves keys for a peer address.
    fn save(&self, peer: Addr, keys: &Keys) -> io::Result<()>;
    /// Loads keys for a peer address.
    fn load(&self, peer: Addr) -> io::Result<Keys>;
}
