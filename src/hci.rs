//! Host Controller Interface ([Vol 4] Part E).

use std::fmt::Debug;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::Duration;

use bytes::Bytes;
use tokio_util::sync::CancellationToken;
use tracing::{debug, trace, warn};

pub use {adv::*, cmd::*, conn::*, consts::*, event::*, handle::*};

use crate::host;

mod adv;
mod cmd;
mod conn;
mod consts;
mod event;
mod handle;

#[cfg(test)]
mod tests;

/// Error type returned by the HCI layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    Host(#[from] host::Error),
    #[error("HCI error: {status}")]
    Hci {
        #[from]
        status: Status,
        // TODO: Add backtrace once stabilized
    },
    #[error("invalid event: {0:?}")]
    InvalidEvent(Bytes),
    #[error("unknown event [code={code:02x}, subevent={subevent:02x}]: {params:?}")]
    UnknownEvent {
        code: u8,
        subevent: u8,
        params: Bytes,
    },
    #[error("event filter conflict (duplicate commands issued?)")]
    FilterConflict,
    #[error("command quota exceeded")]
    CommandQuotaExceeded,
    #[error("{opcode} command failed: {status}")]
    CommandFailed { opcode: Opcode, status: Status },
    #[error("{opcode} command aborted: {status}")]
    CommandAborted { opcode: Opcode, status: Status },
    #[error("non-command event: {typ}")]
    NonCommandEvent { typ: EventType },
}

impl Error {
    /// Returns the HCI status code, if any.
    pub const fn status(&self) -> Option<Status> {
        use Error::*;
        match *self {
            Hci { status } | CommandFailed { status, .. } | CommandAborted { status, .. } => {
                Some(status)
            }
            Host(_)
            | InvalidEvent(_)
            | UnknownEvent { .. }
            | FilterConflict
            | CommandQuotaExceeded
            | NonCommandEvent { .. } => None,
        }
    }
}

/// Common HCI result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Host-side of a Host Controller Interface.
#[derive(Clone, Debug)]
pub struct Host<T: host::Transport> {
    transport: T,
    router: Arc<EventRouter<T>>,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI host using transport layer `t`.
    #[inline]
    #[must_use]
    pub fn new(transport: T) -> Self {
        Self {
            transport,
            router: Arc::default(),
        }
    }

    /// Performs a reset and basic controller initialization
    /// ([Vol 6] Part D, Section 2.1).
    pub async fn init(&self) -> Result<()> {
        // TODO: USB endpoints need to be reset. Otherwise, old events are
        // delivered. Try using libusb_set_configuration.
        self.reset().await?;

        // Unmask all events.
        // SAFETY: The controller ignores any reserved bits set to 1.
        self.set_event_mask(EventMask::default()).await?;
        let _ignore_unknown = self.set_event_mask_page_2(EventMaskPage2::default()).await;
        self.le_set_event_mask(LeEventMask::default()).await?;

        // [Vol 4] Part E, Section 4.1 and [Vol 4] Part E, Section 7.8.2
        // TODO: Move to L2CAP
        let mut buf_info = self.le_read_buffer_size().await?;
        if buf_info.acl_max_pkts == 0 {
            let bi = self.read_buffer_size().await?;
            buf_info.acl_max_len = bi.acl_max_len;
            buf_info.acl_max_pkts = bi.acl_max_pkts;
        }
        debug!("Controller buffers: {:?}", buf_info);

        let _ignore_unknown = self.write_le_host_support(true).await;
        Ok(())
    }

    /// Receives the next HCI event, routes it to registered waiters, and
    /// returns it to the caller. This is a low-level method. In most cases,
    /// `enable_events` is a better choice.
    ///
    /// No events are received, including command completion, unless the
    /// returned future is polled, and no new events can be received until the
    /// returned guard is dropped. If there are multiple concurrent calls to
    /// this method, only one will resolve to the received event and the others
    /// will continue to wait.
    #[inline]
    pub async fn event(&self) -> Result<EventGuard<T>> {
        self.router.recv_event(&self.transport).await
    }

    /// Executes a command with no parameters and returns the command completion
    /// event.
    #[inline]
    async fn exec(&self, opcode: Opcode) -> Result<EventGuard<T>> {
        Command::new(self, opcode).exec().await
    }

    /// Executes a command, calling `f` to provide parameters, and returns the
    /// command completion event.
    #[inline]
    async fn exec_params(
        &self,
        opcode: Opcode,
        f: impl FnOnce(&mut Command<T>) + Send,
    ) -> Result<EventGuard<T>> {
        let mut cmd = Command::new(self, opcode);
        f(&mut cmd);
        cmd.exec().await
    }
}

impl<T: host::Transport + 'static> Host<T> {
    /// Spawns a task that continuously receives HCI events until an error is
    /// encountered. The task is canceled when the returned future is dropped.
    #[inline]
    #[must_use]
    pub fn enable_events(&self) -> EventReceiverTask {
        EventReceiverTask::new(self.clone())
    }
}

/// Converts the count of 10ms `ticks` into [`Duration`].
#[inline]
pub(crate) fn duration_10ms(ticks: u16) -> Duration {
    Duration::from_millis(u64::from(ticks) * 10)
}

/// Converts the count of 1.25ms `ticks` into [`Duration`].
#[inline]
pub(crate) fn duration_1250us(ticks: u16) -> Duration {
    Duration::from_micros(u64::from(ticks) * 1250)
}

/// Returns the number of 10ms ticks in `d` (rounding down) or `None` if the
/// value overflows `u16`.
#[inline]
pub(crate) fn ticks_10ms(d: Duration) -> Option<u16> {
    ticks_ms(d, 10)
}

/// Returns the number of 1.25ms ticks in `d` (rounding down) or `None` if the
/// value overflows `u16`.
#[inline]
pub(crate) fn ticks_1250us(d: Duration) -> Option<u16> {
    ticks_us(d, 1250)
}

/// Returns the number of 0.625ms ticks in `d` (rounding down) or `None` if the
/// value overflows `u32`.
#[inline]
pub(crate) fn ticks_625us(d: Duration) -> Option<u32> {
    ticks_us(d, 625)
}

/// Converts duration `d` into the count of `tick` milliseconds, rounding down.
/// Returns `None` if the count overflows `T`.
#[inline]
fn ticks_ms<T>(d: Duration, tick: u16) -> Option<T>
where
    T: Default + Eq + Ord + TryFrom<u128>,
{
    if d.is_zero() {
        return Some(T::default());
    }
    T::try_from((d.as_millis() / u128::from(tick)).max(1)).ok()
}

/// Converts duration `d` into the count of `tick` microseconds, rounding down.
/// Returns `None` if the count overflows `T`.
#[inline]
fn ticks_us<T>(d: Duration, tick: u16) -> Option<T>
where
    T: Default + Ord + TryFrom<u128>,
{
    if d.is_zero() {
        return Some(T::default());
    }
    T::try_from((d.as_micros() / u128::from(tick)).max(1)).ok()
}
