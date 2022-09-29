//! Controller enumeration and transport layer interface.

use std::fmt::Debug;

use bytes::Bytes;

pub use usb::*;

mod usb;

/// Local host errors.
#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum Error {
    #[error("USB error")]
    Usb {
        #[from]
        source: rusb::Error,
    },
}

type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Send + Sync {
    fn write_cmd(&self, b: &[u8]) -> Result<()>;
    fn write_async_data(&self, b: &[u8]) -> Result<()>;
    fn read_event(&self) -> Result<Bytes>;
}
