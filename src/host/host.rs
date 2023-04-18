//! Host transport layer ([Vol 4] Parts A-D).

use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::task::{ready, Context, Poll};

#[cfg(feature = "usb")]
pub use usb::*;

#[cfg(feature = "usb")]
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

/// Transfer type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TransferType {
    Command,
    Event,
    Acl(Direction),
}

impl TransferType {
    /// Returns the transfer direction.
    #[inline]
    #[must_use]
    pub const fn dir(self) -> Direction {
        match self {
            Self::Command => Direction::Out,
            Self::Event => Direction::In,
            Self::Acl(dir) => dir,
        }
    }
}

/// Transfer direction.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    /// Controller to host transfer.
    In,
    /// Host to controller transfer.
    Out,
}

/// HCI transport layer.
pub trait Transport: Debug + Send + Sync {
    /// Returns an outbound command transfer.
    fn command(&self) -> Box<dyn Transfer>;

    /// Returns an inbound event transfer.
    fn event(&self) -> Box<dyn Transfer>;

    /// Returns an Asynchronous Connection-Oriented transfer.
    fn acl(&self, dir: Direction, max_data_len: u16) -> Box<dyn Transfer>;
}

/// Asynchronous I/O transfer.
pub trait Transfer:
    AsRef<[u8]> + Debug + Send + Sync + structbuf::Pack + structbuf::Unpack
{
    /// Returns the transfer type.
    fn typ(&self) -> TransferType;

    /// Submits the transfer for execution. The transfer may be cancelled by
    /// dropping the returned future.
    fn submit(self: Box<Self>) -> Result<TransferFuture>;

    /// Resets the transfer to its original state, allowing it to be reused.
    fn reset(&mut self);
}

/// Submitted asynchronous I/O transfer.
pub trait ActiveTransfer: Debug + Future<Output = Result<()>> + Send {
    /// Returns the original transfer after successful completion.
    fn ready(self: Box<Self>) -> Box<dyn Transfer>;
}

/// Future that resolves to a completed transfer.
#[derive(Debug)]
#[repr(transparent)]
pub struct TransferFuture(Option<Box<dyn ActiveTransfer>>);

impl Future for TransferFuture {
    type Output = Result<Box<dyn Transfer>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let opt = &mut self.get_mut().0;
        let fut = (opt.as_mut().expect("poll of a finished transfer")).as_mut();
        // SAFETY: Future is pinned via `self`
        ready!(unsafe { Pin::new_unchecked(fut) }.poll(cx))?;
        // SAFETY: Option is `Some`
        Poll::Ready(Ok(unsafe { opt.take().unwrap_unchecked() }.ready()))
    }
}
