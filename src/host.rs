//! Controller enumeration and transport layer interface.

use std::fmt::Debug;

pub use usb::*;

mod usb;

/// Local host errors.
#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum Error {
    #[error("USB error: {source}")]
    Usb {
        #[from]
        source: rusb::Error,
        // TODO: Add backtrace once stabilized
    },
}

impl Error {
    /// Returns whether the error is a result of a timeout.
    pub fn is_timeout(&self) -> bool {
        matches!(
            self,
            Self::Usb {
                source: rusb::Error::Timeout
            }
        )
    }
}

type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Send + Sync + 'static {
    fn write_cmd(&self, b: &[u8]) -> Result<()>;
    fn write_async_data(&self, b: &[u8]) -> Result<()>;
    fn read_event(&self, b: &mut [u8]) -> Result<usize>;
}
