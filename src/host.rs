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

// TODO: Explicit submit() step?

/// HCI transport layer.
pub trait Transport {
    type Transfer: Transfer + Debug;

    /// Submits an HCI command, calling `f` to fill the command buffer.
    fn cmd(&self, f: impl FnOnce(&mut BytesMut)) -> Result<Self::Transfer>;

    /// Requests the next HCI event.
    fn evt(&self) -> Result<Self::Transfer>;
}

/// Asynchronous I/O transfer.
pub trait Transfer {
    type Future: Future<Output = Result<()>>;

    /// Returns a future that resolves to the transfer result. The transfer is
    /// cancelled if it is dropped without awaiting the result.
    fn result(&mut self) -> Self::Future;

    /// Returns the transfer buffer. It panics if the transfer is still in
    /// progress. The buffer may be allocated in a DMA region, so the caller
    /// must not perform any operations that may result in reallocation.
    fn buf(&mut self) -> &mut BytesMut;

    /// Passes the transfer result and buffer to `f` in a shared context.
    fn map<T>(&self, f: impl FnOnce(<Self::Future as Future>::Output, &[u8]) -> T) -> T;
}
