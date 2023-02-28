//! Host Controller Interface ([Vol 4] Part E).

use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};
use std::time::Duration;

use bitflags::bitflags;
use structbuf::{Pack, Packer};
use strum::IntoEnumIterator;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error};

pub use {adv::*, cmd::*, consts::*, event::*, handle::*};

use crate::{host, le, SyncArcMutexGuard, SyncMutex};

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
    #[error("invalid event: {0:02X?}")]
    InvalidEvent(Vec<u8>),
    #[error("unknown event [code={code:#04X}, subcode={subcode:#04X}]: {params:02X?}")]
    UnknownEvent {
        code: u8,
        subcode: u8,
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
type CommandTransfer = SyncMutex<Option<Box<dyn host::Transfer>>>;

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
    pub(crate) fn events(&self) -> EventStream {
        self.router.events(Opcode::None).unwrap() // Never returns an error
    }

    /// Returns connection information for the specified handle or [`None`] if
    /// the handle is invalid.
    #[inline(always)]
    pub(crate) fn conn(&self, hdl: ConnHandle) -> Option<SyncArcMutexGuard<Conn>> {
        self.router.conn(hdl)
    }

    /// Performs a reset and basic controller initialization
    /// ([Vol 6] Part D, Section 2.1).
    pub async fn init(&self) -> Result<()> {
        // TODO: USB endpoints need to be reset. Otherwise, old events are
        // delivered. Try using libusb_set_configuration.
        self.reset().await?;

        // Unmask all events.
        let all = EventCode::iter().collect();
        self.set_event_mask(&all).await?;
        let _ignore_unknown = self.set_event_mask_page_2(&all).await;
        self.le_set_event_mask(&all).await?;

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

/// Shared connection information.
pub(crate) type ArcConnInfo = Arc<SyncMutex<Conn>>;

/// Information about an established connection.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Conn {
    /// Local role.
    pub role: Role,
    /// Local public or random address. This is needed by the security manager
    /// during pairing. [`LeConnectionComplete`] event does not provide this
    /// information, so it must be set by the component that created the
    /// connection.
    pub local_addr: Option<le::Addr>,
    /// Remote public or random address (identity address if IRK is used).
    pub peer_addr: le::Addr,
    /// Connection security properties.
    pub sec: ConnSec,
}

impl Conn {
    /// Creates new connection info.
    #[inline(always)]
    const fn new(e: &LeConnectionComplete) -> Self {
        Self {
            role: e.role,
            local_addr: None,
            peer_addr: e.peer_addr,
            sec: ConnSec::empty(),
        }
    }

    /// Returns whether the connection is authenticated (has MITM protection).
    #[inline(always)]
    pub const fn authn(&self) -> bool {
        self.sec.contains(ConnSec::AUTHN)
    }

    /// Returns whether the remote device is authorized.
    #[inline(always)]
    pub const fn authz(&self) -> bool {
        self.sec.contains(ConnSec::AUTHZ)
    }

    /// Returns whether a trusted relationship exists between the devices.
    #[inline(always)]
    pub const fn bond(&self) -> bool {
        self.sec.contains(ConnSec::BOND)
    }

    /// Returns the encryption key length.
    #[inline(always)]
    pub const fn key_len(&self) -> u8 {
        self.sec.intersection(ConnSec::KEY_LEN).bits
    }
}

bitflags! {
    /// Connection security properties.
    #[allow(clippy::unsafe_derive_deserialize)]
    #[derive(Default, serde::Deserialize, serde::Serialize)]
    #[repr(transparent)]
    #[serde(transparent)]
    pub(crate) struct ConnSec: u8 {
        /// Authentication flag.
        const AUTHN = 1 << 0;
        /// Authorization flag.
        const AUTHZ = 1 << 1;
        /// Trusted relationship flag ([Vol 3] Part C, Section 9.4).
        const BOND = 1 << 2;
        /// Encryption key length mask.
        const KEY_LEN = 0x1F << 3;
    }
}

impl ConnSec {
    /// Creates a key length property.
    #[inline(always)]
    pub const fn key_len(n: u8) -> Self {
        assert!(56 <= n && n <= 128 && n % 8 == 0);
        // SAFETY: All bits are valid
        unsafe { Self::from_bits_unchecked(n) }
    }
}

impl Display for ConnSec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let k = self.intersection(Self::KEY_LEN).bits;
        if k == 0 {
            return f.write_str("Unencrypted");
        }
        let mut t = f.debug_tuple("Encrypted");
        t.field(&format_args!("{k}-bit"));
        if self.contains(Self::AUTHN) {
            t.field(&"AUTHN");
        }
        if self.contains(Self::AUTHZ) {
            t.field(&"AUTHZ");
        }
        if self.contains(Self::BOND) {
            t.field(&"BOND");
        }
        t.finish()
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
