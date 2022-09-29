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

type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Send + Sync {
    fn write_cmd(&self, b: &[u8]) -> Result<()>;
    fn write_async_data(&self, b: &[u8]) -> Result<()>;
    fn read_event(&self, b: &mut [u8]) -> Result<usize>;
}
