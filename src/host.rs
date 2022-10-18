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

/// Common host result type.
pub type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Send + Sync {
    type Transfer: Transfer + Debug;
    type Future: Future<Output = Self::Transfer> + Send + Unpin;

    /// Returns a new transfer for an HCI command, calling `f` to fill the
    /// command buffer.
    fn cmd(&self, f: impl FnOnce(&mut BytesMut)) -> Self::Transfer;

    /// Returns a new transfer for an HCI event.
    fn evt(&self) -> Self::Transfer;

    /// Submits a transfer for execution. The transfer may be cancelled if the
    /// returned future is dropped before completion.
    fn submit(&self, xfer: Self::Transfer) -> Result<Self::Future>;
}

/// Asynchronous I/O transfer.
pub trait Transfer: Send + Sync {
    /// Returns a shared reference to the transfer buffer.
    fn buf(&self) -> &[u8];

    /// Returns a mutable reference to the transfer buffer. A newly allocated
    /// transfer may start with a non-empty buffer. The header, if any, must not
    /// be modified. The buffer may be allocated in a DMA region, so the caller
    /// must not perform any operations that result in reallocation.
    fn buf_mut(&mut self) -> &mut BytesMut;

    /// Returns the transfer result or [`None`] if the transfer is not yet
    /// submitted.
    fn result(&self) -> Option<Result<()>>;
}
