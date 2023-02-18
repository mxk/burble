//! Host Controller Interface ([Vol 4] Part E).

use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};
use std::time::Duration;

use structbuf::{Pack, Packer};
use strum::IntoEnumIterator;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error};

pub use {adv::*, cmd::*, consts::*, event::*, handle::*};

use crate::host;

mod adv;
#[path = "cmd/cmd.rs"]
mod cmd;
mod consts;
#[path = "event/event.rs"]
mod event;
mod handle;

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
    InvalidEvent(Vec<u8>),
    #[error("unknown event [code={code:#04X}, subevent={subevent:#04X}]: {params:?}")]
    UnknownEvent {
        code: u8,
        subevent: u8,
        params: Vec<u8>,
    },
    #[error("duplicate {opcode} commands issued")]
    DuplicateCommands { opcode: Opcode },
    #[error("command quota exceeded")]
    CommandQuotaExceeded,
    #[error("{opcode} command failed: {status}")]
    CommandFailed { opcode: Opcode, status: Status },
    #[error("{opcode} command aborted: {status}")]
    CommandAborted { opcode: Opcode, status: Status },
}

impl Error {
    /// Returns the HCI status code, if any.
    #[must_use]
    pub const fn status(&self) -> Option<Status> {
        use Error::*;
        match *self {
            Hci { status } | CommandFailed { status, .. } | CommandAborted { status, .. } => {
                Some(status)
            }
            Host(_)
            | InvalidEvent(_)
            | UnknownEvent { .. }
            | DuplicateCommands { .. }
            | CommandQuotaExceeded => None,
        }
    }
}

/// Common HCI result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Reusable HCI command transfer.
type CommandTransfer = parking_lot::Mutex<Option<Box<dyn host::Transfer>>>;

/// Host-side of a Host Controller Interface.
#[derive(Clone, Debug)]
pub struct Host {
    transport: Arc<dyn host::Transport>,
    router: Arc<EventRouter>,
    cmd: Arc<CommandTransfer>,
}

impl Host {
    /// Creates an HCI host using transport layer `t`.
    #[inline]
    #[must_use]
    pub fn new(t: Arc<dyn host::Transport>) -> Self {
        Self {
            transport: t,
            router: EventRouter::new(),
            cmd: Arc::new(CommandTransfer::default()),
        }
    }

    /// Returns the underlying transport.
    #[inline]
    #[must_use]
    pub const fn transport(&self) -> &Arc<dyn host::Transport> {
        &self.transport
    }

    /// Returns an event receiver.
    #[inline]
    pub(crate) fn events(&self) -> Result<EventStream> {
        self.router.events(Opcode::None)
    }

    /// Performs a reset and basic controller initialization
    /// ([Vol 6] Part D, Section 2.1).
    pub async fn init(&self) -> Result<()> {
        // TODO: USB endpoints need to be reset. Otherwise, old events are
        // delivered. Try using libusb_set_configuration.
        self.reset().await?;

        // Unmask all events.
        self.set_event_mask(EventMask::enable(EventCode::iter()))
            .await?;
        let _ignore_unknown = self
            .set_event_mask_page_2(EventMask2::enable(EventCode::iter()))
            .await;
        self.le_set_event_mask(LeEventMask::enable(SubeventCode::iter()))
            .await?;

        let _ignore_unknown = self.write_le_host_support(true).await;
        Ok(())
    }

    /// Receives the next HCI event, routes it to registered waiters, and
    /// returns it to the caller. This is a low-level method. In most cases,
    /// [`Self::event_loop`] is a better choice.
    ///
    /// No events are received, including command completion, unless the
    /// returned future is polled, and no new events can be received until the
    /// returned event handle is dropped.
    ///
    /// This method is not cancel safe because dropping the future may cause an
    /// event to be lost.
    ///
    /// # Panics
    ///
    /// Panics if there are multiple concurrent event receivers.
    #[inline]
    async fn next_event(&self) -> Result<Event> {
        self.router.next(self.transport.as_ref()).await
    }

    /// Spawns a task that continuously receives HCI events until a fatal error
    /// is encountered. The task is canceled when the returned future is
    /// dropped.
    #[inline]
    #[must_use]
    pub fn event_loop(&self) -> EventLoop {
        let c = CancellationToken::new();
        EventLoop {
            h: tokio::spawn(EventLoop::run(self.clone(), c.clone())),
            c: c.clone(),
            _g: c.drop_guard(),
        }
    }

    /// Executes a command with no parameters and returns the command completion
    /// event.
    #[inline]
    async fn exec(&self, opcode: Opcode) -> Result<Event> {
        self.exec_params(opcode, |_| {}).await
    }

    /// Executes a command, calling `f` to provide parameters, and returns the
    /// command completion event.
    #[inline]
    async fn exec_params(
        &self,
        opcode: Opcode,
        f: impl FnOnce(&mut Packer) + Send,
    ) -> Result<Event> {
        let mut cmd = Command::new(self, opcode);
        f(&mut cmd.append());
        cmd.exec().await.map_err(|e| {
            error!("{opcode} error: {e}");
            e
        })
    }

    /// Returns a new command transfer.
    #[inline]
    fn new_cmd(&self) -> Box<dyn host::Transfer> {
        self.cmd.lock().take().map_or_else(
            || self.transport.command(),
            |mut cmd| {
                cmd.reset();
                cmd
            },
        )
    }
}

/// Future that continuously receives HCI events.
#[derive(Debug)]
pub struct EventLoop {
    h: tokio::task::JoinHandle<Result<()>>,
    c: CancellationToken,
    _g: tokio_util::sync::DropGuard,
}

impl EventLoop {
    /// Stops event processing.
    #[inline]
    pub async fn stop(self) -> Result<()> {
        self.c.cancel();
        self.h.await.unwrap()
    }

    /// Receives HCI events until cancellation.
    async fn run(h: Host, c: CancellationToken) -> Result<()> {
        debug!("Event loop started");
        loop {
            let r: Result<Event> = tokio::select! {
                r = h.next_event() => r,
                _ = c.cancelled() => {
                    debug!("Event loop terminating");
                    return Ok(());
                }
            };
            if let Err(e) = r {
                // TODO: Ignore certain errors, like short write
                error!("Event loop error: {e}");
                return Err(e);
            }
        }
    }
}

impl Future for EventLoop {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(ready!(Pin::new(&mut self.h).poll(cx)).unwrap())
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
