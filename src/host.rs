//! Host transport layer ([Vol 4] Parts A-D)

use std::fmt::Debug;
use std::future::Future;

use bytes::BytesMut;

pub use usb::*;

mod usb;

/// Local host errors.
#[derive(Clone, Copy, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error("usb error: {source}")]
    Usb {
        #[from]
        source: rusb::Error,
        // TODO: Add backtrace once stabilized
    },
}

impl Error {
    /// Returns whether the error is a result of a timeout.
    #[inline]
    #[must_use]
    pub const fn is_timeout(&self) -> bool {
        matches!(
            self,
            &Self::Usb {
                source: rusb::Error::Timeout
            }
        )
    }
}

/// Common host result type.
pub type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Clone + Debug + Send + Sync {
    type Transfer: Transfer;

    /// Returns an outbound command transfer.
    fn command(&self) -> Self::Transfer;

    /// Returns an inbound event transfer.
    fn event(&self) -> Self::Transfer;

    /// Returns an inbound Asynchronous Connection-Oriented transfer.
    fn acl_in(&self, max_data_len: usize) -> Self::Transfer;

    /// Returns an outbound Asynchronous Connection-Oriented transfer.
    fn acl_out(&self, max_data_len: usize) -> Self::Transfer;
}

/// Asynchronous I/O transfer.
pub trait Transfer: AsRef<[u8]> + Debug + Send + Sync {
    type Future: Future<Output = Self> + Debug + Send + Unpin;

    // TODO: Use Limit for buf_mut?

    /// Returns a mutable reference to the transfer buffer. A newly allocated
    /// transfer may start with a non-empty buffer. The header, if any, must not
    /// be modified. The buffer may be allocated in a DMA region, so the caller
    /// must not perform any operations that result in reallocation.
    fn buf_mut(&mut self) -> &mut BytesMut;

    /// Submits a transfer for execution. The transfer may be cancelled by
    /// dropping the returned future.
    fn submit(self) -> Result<Self::Future>;

    /// Returns the transfer result or [`None`] if the transfer was not yet
    /// submitted.
    fn result(&self) -> Option<Result<()>>;

    /// Resets the transfer to its original state, allowing it to be reused.
    fn reset(&mut self);
}
