//! Host Controller Interface ([Vol 4] Part E).

use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};
use std::time::Duration;

use once_cell::sync::Lazy;
use structbuf::{Pack, Packer};
use tracing::{debug, error, warn};

pub use {adv::*, cmd::*, consts::*, event::*, handle::*};

use crate::le::Addr;
use crate::{host, smp, SyncMutex};

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
    #[error("controller initialization error: {0}")]
    Init(&'static str),
    #[error("invalid event: {0:02X?}")]
    InvalidEvent(Vec<u8>),
    #[error("unknown event [code={code:#04X}, subcode={subcode:#04X}]: {params:02X?}")]
    UnknownEvent {
        code: u8,
        subcode: u8,
        params: Vec<u8>,
    },
    #[error("{opcode} command failed: {status}")]
    CommandFailed { opcode: Opcode, status: Status },
    #[error("{opcode} command aborted: {status}")]
    CommandAborted { opcode: Opcode, status: Status },
    #[error("{opcode} command timeout")]
    CommandTimeout { opcode: Opcode },
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
            Host(_) | Init(_) | InvalidEvent(_) | UnknownEvent { .. } | CommandTimeout { .. } => {
                None
            }
        }
    }

    /// Returns whether the error is a result of a timeout.
    #[must_use]
    pub const fn is_timeout(&self) -> bool {
        use Error::*;
        match *self {
            Host(host::Error::Timeout) | CommandTimeout { .. } => true,
            Host(_)
            | Hci { .. }
            | Init(_)
            | InvalidEvent(_)
            | UnknownEvent { .. }
            | CommandFailed { .. }
            | CommandAborted { .. } => false,
        }
    }
}

/// Common HCI result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Reusable HCI command transfer.
type CommandTransfer = SyncMutex<Option<Box<dyn host::Transfer>>>;

/// Transfer direction.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    /// Controller to host transfer.
    ToHost,
    /// Host to controller transfer.
    FromHost,
}

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
            Self::Command => Direction::FromHost,
            Self::Event => Direction::ToHost,
            Self::Acl(dir) => dir,
        }
    }
}

/// Host-side of a Host Controller Interface.
#[derive(Clone, Debug)]
pub struct Host {
    transport: Arc<dyn host::Transport>,
    info: Arc<ControllerInfo>,
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
            info: Arc::default(),
            router: EventRouter::new(),
            cmd: Arc::new(CommandTransfer::default()),
        }
    }

    /// Returns the underlying transport.
    #[inline(always)]
    #[must_use]
    pub(crate) const fn transport(&self) -> &Arc<dyn host::Transport> {
        &self.transport
    }

    /// Returns controller information.
    #[inline(always)]
    pub(crate) fn info(&self) -> &ControllerInfo {
        &self.info
    }

    /// Returns an event stream that will yield non-command events.
    #[inline(always)]
    pub(crate) fn events(&self) -> EventStream {
        self.router.events()
    }

    /// Returns connection information for the specified handle or [`None`] if
    /// the handle is invalid.
    #[inline(always)]
    pub(crate) fn conn(&self, hdl: ConnHandle) -> Option<ConnWatch> {
        self.router.conn(hdl)
    }

    /// Calls `f` to update connection parameters. This is a no-op if the handle
    /// is invalid.
    #[inline(always)]
    pub(crate) fn update_conn(&self, hdl: ConnHandle, f: impl FnOnce(&mut Conn)) {
        self.router.update_conn(hdl, f);
    }

    /// Resets and initializes the controller ([Vol 6] Part D, Section 2.1). The
    /// event loop must be running prior to calling this method.
    pub async fn init(&mut self, event_mask: &EventMask) -> Result<()> {
        fn info_mut(this: &mut Host) -> &mut ControllerInfo {
            Arc::get_mut(&mut this.info).expect("host is shared")
        }

        // Reset after allowing the event loop to discard any unexpected events
        tokio::time::sleep(Duration::from_millis(100)).await;
        debug!("HCI reset...");
        self.reset().await?;

        // Get controller information
        info_mut(self).cmd = match self.read_local_supported_commands().await {
            // The first command after a reset may time out, so we retry it once
            Err(e) if e.is_timeout() => self.read_local_supported_commands().await,
            r => r,
        }?;
        info_mut(self).ver = self.read_local_version().await?;
        debug!("Controller version: {:?}", self.info.ver);
        if self.info.ver.hci_version < CoreVersion::V5_0 {
            return Err(Error::Init("pre-v5 controller"));
        }
        info_mut(self).lmp_features = self.read_local_supported_features().await?;
        debug!("Controller LMP features: {:?}", self.info.lmp_features);
        if !self.info.lmp_features.contains(LmpFeature::LE_SUPPORTED) {
            return Err(Error::Init("non-LE controller"));
        }
        info_mut(self).le_features = self.le_read_local_supported_features().await?;
        debug!("Controller LE features: {:?}", self.info.le_features);
        info_mut(self).states = self.le_read_supported_states().await?;
        debug!("Controller LE states: {:#044b}", self.info.states.0);

        // [Vol 4] Part E, Section 4.1 and [Vol 4] Part E, Section 7.8.2
        if self.info.states.supports_connection_state() {
            let mut buf = self.le_read_buffer_size().await?;
            debug!("Controller LE buffers: {:?}", buf);
            #[allow(clippy::cast_possible_truncation)]
            if buf.acl_data_len == 0 || buf.acl_num_pkts == 0 {
                let shared = self.read_buffer_size().await?;
                debug!("Controller BR/EDR/LE buffers: {:?}", shared);
                buf.acl_data_len = shared.acl_data_len;
                buf.acl_num_pkts = shared.acl_num_pkts.max(u16::from(u8::MAX)) as _;
                if buf.acl_data_len == 0 || buf.acl_num_pkts == 0 {
                    return Err(Error::Init("invalid buffer parameters"));
                }
            }
            info_mut(self).buf = buf;
        } else {
            warn!("Controller does not support Connection State");
        }
        info_mut(self).addr = self.read_bd_addr().await?;
        debug!("Controller address: {:?}", self.info.addr);

        // Enable requested events
        event_mask.apply(self).await
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
    #[inline(always)]
    async fn next_event(&self) -> Result<Event> {
        self.router.next(self.transport.as_ref()).await
    }

    /// Spawns a task that continuously receives HCI events until a fatal error
    /// is encountered. The task is canceled when the returned future is
    /// dropped.
    #[inline]
    #[must_use]
    pub fn event_loop(&self) -> EventLoop {
        let ct = tokio_util::sync::CancellationToken::new();
        let mut host = self.clone();
        // Drop ControllerInfo reference to allow exclusive access in init()
        host.info = Arc::clone(&NO_CONTROLLER_INFO);
        EventLoop {
            join: tokio::spawn(EventLoop::run(host, ct.clone())),
            cancel: ct.clone(),
            _guard: ct.drop_guard(),
        }
    }

    /// Executes a command with no parameters and returns the command completion
    /// event.
    #[inline(always)]
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
        // Allow all commands until cmd is initialized in init()
        if !self.info.cmd.is_supported(opcode) && !self.info.cmd.is_empty() {
            warn!("Ignoring unsupported command: {opcode}");
            return Err(Error::CommandFailed {
                opcode,
                status: Status::UnknownCommand,
            });
        }
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

/// Static controller information.
#[derive(Debug, Default)]
#[must_use]
pub struct ControllerInfo {
    cmd: SupportedCommands,
    ver: LocalVersion,
    lmp_features: LmpFeature,
    le_features: LeFeature,
    states: LeStateCombinations,
    buf: LeBufferSize,
    addr: Addr,
}

/// Invalid controller information.
static NO_CONTROLLER_INFO: Lazy<Arc<ControllerInfo>> = Lazy::new(Arc::default);

impl ControllerInfo {
    /// Returns information about commands supported by the controller.
    #[inline(always)]
    #[must_use]
    pub const fn commands(&self) -> &SupportedCommands {
        &self.cmd
    }

    /// Returns controller version information.
    #[inline(always)]
    #[must_use]
    pub const fn version(&self) -> LocalVersion {
        self.ver
    }

    /// Returns controller feature support information.
    #[inline(always)]
    #[must_use]
    pub const fn features(&self) -> LeFeature {
        self.le_features
    }

    /// Returns controller buffer size information.
    #[inline(always)]
    #[must_use]
    pub const fn buffer_size(&self) -> LeBufferSize {
        self.buf
    }
}

/// Future that continuously receives HCI events.
#[derive(Debug)]
pub struct EventLoop {
    join: tokio::task::JoinHandle<Result<()>>,
    cancel: tokio_util::sync::CancellationToken,
    _guard: tokio_util::sync::DropGuard,
}

impl EventLoop {
    /// Stops event processing.
    #[inline]
    pub async fn stop(self) -> Result<()> {
        self.cancel.cancel();
        self.join.await.expect("event loop panic")
    }

    /// Receives HCI events until cancellation.
    async fn run(h: Host, c: tokio_util::sync::CancellationToken) -> Result<()> {
        debug!("Event loop started");
        let r = loop {
            let r: Result<Event> = tokio::select! {
                r = h.next_event() => r,
                _ = c.cancelled() => {
                    debug!("Event loop terminating");
                    break Ok(());
                }
            };
            if let Err(e) = r {
                // TODO: Ignore certain errors, like short write
                error!("Event loop error: {e}");
                break Err(e);
            }
        };
        // We reset the controller when the event loop exits to ensure that it
        // stops all communication. Doing this at the transport layer would be
        // better, but WinUSB does not support actual device resets.
        // TODO: Is there a better place to do this?
        let mut reset = h.new_cmd();
        reset.append().u16(Opcode::Reset).u8(0);
        match reset.exec().await {
            Ok(_) => debug!("Submitted controller reset command"),
            Err(e) => warn!("Failed to reset controller: {e}"),
        }
        r
    }
}

impl Future for EventLoop {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(ready!(Pin::new(&mut self.join).poll(cx)).unwrap())
    }
}

/// Connection watch channel used for broadcasting connection state changes.
pub(crate) type ConnWatch = tokio::sync::watch::Receiver<Conn>;

/// Information about an established connection.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Conn {
    /// Local role.
    pub role: Role,
    /// Local public or random address. This is used by the security manager
    /// during pairing. [`LeConnectionComplete`] event does not provide this
    /// information, so it must be set by the component that created the
    /// connection.
    pub local_addr: Addr,
    /// Remote public or random address (identity address if IRK is used).
    pub peer_addr: Addr,
    /// Current connection security properties.
    pub sec: ConnSec,
    /// Bond ID set by the Security Manager when a connection is created to
    /// indicate the existence of a trusted relationship with the peer. A change
    /// in the ID invalidates any cached data.
    pub bond_id: Option<smp::BondId>,
    /// Reason parameter from the [`DisconnectionComplete`] event.
    pub disconnect_reason: Option<Status>,
}

impl Conn {
    /// Creates new connection info.
    #[inline(always)]
    fn new(e: &LeConnectionComplete) -> Self {
        Self {
            role: e.role,
            local_addr: Addr::default(),
            peer_addr: e.peer_addr,
            sec: ConnSec::empty(),
            bond_id: None,
            disconnect_reason: None,
        }
    }
}

bitflags::bitflags! {
    /// Connection security properties.
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
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
        Self::from_bits_retain(n)
    }
}

impl Display for ConnSec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let k = self.intersection(Self::KEY_LEN).bits();
        if k == 0 {
            return f.write_str("Unencrypted");
        }
        let mut t = f.debug_tuple("Encrypted");
        t.field(&format_args!("{k}-bit"));
        if self.contains(Self::AUTHN) {
            t.field(&format_args!("AUTHN"));
        }
        if self.contains(Self::AUTHZ) {
            t.field(&format_args!("AUTHZ"));
        }
        if self.contains(Self::BOND) {
            t.field(&format_args!("BOND"));
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
