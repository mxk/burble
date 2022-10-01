//! Controller enumeration and transport layer interface.

use std::fmt::Debug;
use std::future::Future;

use bytes::BytesMut;

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
pub trait Transport {
    type Transfer: Transfer;

    /// Submits an HCI command, calling `f` to fill the command buffer.
    fn cmd(&self, f: impl FnOnce(&mut BytesMut)) -> Result<Self::Transfer>;

    /// Requests the next HCI event.
    fn evt(&self) -> Result<Self::Transfer>;
}

/// Asynchronous I/O transfer. Dropping the transfer without awaiting its result
/// may cause the transfer to be cancelled.
pub trait Transfer {
    type Future: Future<Output = Result<()>>;

    /// Returns a future that resolves to the transfer result.
    fn result(&mut self) -> Self::Future;

    /// Returns the transfer buffer. It panics if the transfer is still in
    /// progress. The buffer may be allocated in a DMA region, so the caller
    /// must not perform any operations that may result in reallocation.
    fn buf(&mut self) -> &mut BytesMut;
}
