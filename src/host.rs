//! Host transport layer ([Vol 4] Parts A-D)

use std::fmt::Debug;
use std::future::Future;

pub use usb::*;

use crate::util::LimitedBuf;

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

    /// Returns an Asynchronous Connection-Oriented transfer.
    fn acl(&self, dir: Direction, max_data_len: usize) -> Self::Transfer;
}

/// Asynchronous I/O transfer.
pub trait Transfer: AsRef<[u8]> + Debug + Send + Sync {
    type Future: Future<Output = Self> + Debug + Send + Unpin;

    /// Returns a reference to the transfer buffer.
    fn buf(&self) -> &LimitedBuf;

    /// Returns a mutable reference to the transfer buffer. A newly allocated
    /// transfer may start with a non-empty buffer. The header, if any, must not
    /// be modified.
    fn buf_mut(&mut self) -> &mut LimitedBuf;

    /// Returns the length of the header that precedes the payload in the
    /// transfer buffer.
    fn hdr_len(&self) -> usize;

    /// Submits the transfer for execution. The transfer may be cancelled by
    /// dropping the returned future.
    fn submit(self) -> Result<Self::Future>;

    /// Returns the transfer result or [`None`] if the transfer was not yet
    /// submitted.
    fn result(&self) -> Option<Result<()>>;

    /// Resets the transfer to its original state, allowing it to be reused.
    fn reset(&mut self);
}

/// Transfer direction.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    In,
    Out,
}
