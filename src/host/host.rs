//! Host transport layer ([Vol 4] Parts A-D).

use std::fmt::Debug;
use std::future::Future;
use std::hint::unreachable_unchecked;
use std::mem;
use std::pin::Pin;
use std::task::{ready, Context, Poll};

use futures_core::FusedFuture;

#[cfg(feature = "usb")]
pub use usb::*;

use crate::hci;

#[cfg(feature = "usb")]
mod usb;

/// Local host errors.
#[derive(Clone, Copy, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    /// Controller cannot be accessed due to a permission problem.
    #[error("controller access denied")]
    Access,
    /// Controller connection was lost, probably because the controller was
    /// disconnected.
    #[error("controller connection broken")]
    Broken,
    /// Communication timed out, but the operation can be retried.
    #[error("controller timeout")]
    Timeout,
    /// Controller could not be found or configured.
    #[error("controller not found")]
    NotFound,
    /// Other fatal controller error.
    #[error("{0}")]
    Other(&'static str),
}

/// Common host result type.
pub type Result<T> = std::result::Result<T, Error>;

/// HCI transport layer.
pub trait Transport: Debug + Send + Sync {
    /// Returns an outbound command transfer.
    #[must_use]
    fn command(&self) -> Box<dyn Transfer>;

    /// Returns an inbound event transfer.
    #[must_use]
    fn event(&self) -> Box<dyn Transfer>;

    /// Returns an Asynchronous Connection-Oriented transfer.
    #[must_use]
    fn acl(&self, dir: hci::Direction, max_data_len: u16) -> Box<dyn Transfer>;
}

/// Asynchronous I/O transfer.
pub trait Transfer: AsRef<[u8]> + Debug + Send + Sync + structbuf::Pack {
    /// Returns the transfer type.
    #[must_use]
    fn typ(&self) -> hci::TransferType;

    /// Submits the transfer for execution. The transfer may be cancelled by
    /// dropping the returned future.
    fn exec(self: Box<Self>) -> Exec;

    /// Resets the transfer to its original state, allowing it to be reused.
    fn reset(&mut self);
}

/// Submitted asynchronous I/O transfer.
pub trait PendingTransfer: Debug + Future<Output = Result<()>> + Send {
    /// Returns the transfer after successful completion.
    ///
    /// # Safety
    ///
    /// This method may only be called after the future resolves to an `Ok`
    /// result.
    unsafe fn ready(self: Pin<Box<Self>>) -> Box<dyn Transfer>;
}

/// Transfer execution future.
#[derive(Debug)]
#[repr(transparent)]
pub struct Exec(State);

/// Transfer execution state.
#[derive(Debug)]
enum State {
    Pending(Pin<Box<dyn PendingTransfer>>),
    Ready(Result<Box<dyn Transfer>>),
    Invalid,
}

impl Exec {
    /// Creates a pending transfer future.
    #[inline(always)]
    #[must_use]
    pub fn pending(t: Pin<Box<dyn PendingTransfer>>) -> Self {
        Self(State::Pending(t))
    }

    /// Creates a ready transfer future.
    #[inline(always)]
    #[must_use]
    pub fn ready(r: Result<Box<dyn Transfer>>) -> Self {
        Self(State::Ready(r))
    }
}

impl Future for Exec {
    type Output = Result<Box<dyn Transfer>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let state = &mut self.get_mut().0;
        Poll::Ready(match *state {
            State::Pending(ref mut xfer) => {
                let r = ready!(xfer.as_mut().poll(cx));
                let State::Pending(xfer) = mem::replace(state, State::Invalid) else {
                    // SAFETY: `state` is Pending
                    unsafe { unreachable_unchecked() };
                };
                // SAFETY: ready() is only called if `r` is `Ok`
                r.map(|_| unsafe { xfer.ready() })
            }
            State::Ready(_) => {
                let State::Ready(r) = mem::replace(state, State::Invalid) else {
                    // SAFETY: `state` is Ready
                    unsafe { unreachable_unchecked() };
                };
                r
            }
            State::Invalid => panic!("poll of a finished transfer"),
        })
    }
}

impl FusedFuture for Exec {
    #[inline(always)]
    fn is_terminated(&self) -> bool {
        matches!(self.0, State::Invalid)
    }
}
