use std::collections::VecDeque;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll, Waker};
use std::time::Duration;

use structbuf::Unpacker;
use tokio::time::timeout;
use tokio_util::sync::CancellationToken;
use tracing::{debug, trace};

pub use {hci::*, le::*};

use crate::{host, le::RawAddr};

use super::*;

mod hci;
mod le;

#[cfg(test)]
mod tests;

/// HCI event decoder.
#[derive(Clone, Debug, Default)]
#[must_use]
pub struct Event<'a> {
    typ: EventType,
    status: Status,
    cmd_quota: u8,
    opcode: Opcode,
    handle: u16,
    params: Unpacker<'a>,
}

impl Event<'_> {
    /// Returns the event type.
    #[inline(always)]
    #[must_use]
    pub const fn typ(&self) -> EventType {
        self.typ
    }

    /// Returns the event status or [`Status::Success`] for events without a
    /// status parameter.
    #[inline(always)]
    #[must_use]
    pub const fn status(&self) -> Status {
        self.status
    }

    /// Returns the opcode from `CommandComplete` or `CommandStatus` events.
    /// [`Opcode::None`] may be returned for either event that only updates the
    /// command quota and for non-command events.
    #[inline(always)]
    #[must_use]
    pub const fn opcode(&self) -> Opcode {
        self.opcode
    }

    /// Returns the associated advertising handle or `None` for non-advertising
    /// events.
    #[inline]
    #[must_use]
    pub fn adv_handle(&self) -> Option<AdvHandle> {
        #[allow(clippy::cast_possible_truncation)]
        if self.typ.param_fmt().contains(EventFmt::ADV_HANDLE) {
            AdvHandle::new(self.handle as u8)
        } else {
            None
        }
    }

    /// Returns the associated connection handle or `None` for non-connection
    /// events.
    #[inline]
    #[must_use]
    pub fn conn_handle(&self) -> Option<ConnHandle> {
        if self.typ.param_fmt().contains(EventFmt::CONN_HANDLE) {
            ConnHandle::new(self.handle)
        } else {
            None
        }
    }

    /// Returns the next `BD_ADDR`.
    #[inline]
    pub fn addr(&mut self) -> RawAddr {
        // SAFETY: All bit patterns are valid
        unsafe { self.params.read() }
    }
}

impl<'a> Deref for Event<'a> {
    type Target = Unpacker<'a>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.params
    }
}

impl DerefMut for Event<'_> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.params
    }
}

impl<'a> TryFrom<&'a [u8]> for Event<'a> {
    type Error = Error;

    /// Tries to parse event header from `orig`. The subevent code for LE
    /// events, event status, and handle parameters are also consumed.
    fn try_from(orig: &'a [u8]) -> Result<Self> {
        let mut evt = Unpacker::new(orig);
        let Some(mut hdr) = evt.skip(EVT_HDR) else {
            return Err(Error::InvalidEvent(Vec::from(orig)));
        };
        let code = hdr.u8();
        if evt.len() != usize::from(hdr.u8()) {
            return Err(Error::InvalidEvent(Vec::from(orig)));
        }
        let typ = match EventCode::try_from(code) {
            Ok(EventCode::LeMetaEvent) => {
                let subevent = evt.u8();
                match SubeventCode::try_from(subevent) {
                    Ok(subevent) => EventType::Le(subevent),
                    Err(_) => {
                        return Err(Error::UnknownEvent {
                            code,
                            subevent,
                            params: Vec::from(evt.as_ref()),
                        })
                    }
                }
            }
            Ok(code) => EventType::Hci(code),
            Err(_) => {
                return Err(Error::UnknownEvent {
                    code,
                    subevent: 0,
                    params: Vec::from(evt.as_ref()),
                })
            }
        };
        let mut evt = Self {
            typ,
            params: evt,
            ..Self::default()
        };
        match typ {
            EventType::Hci(EventCode::CommandComplete) => {
                evt.cmd_quota = evt.u8();
                evt.opcode = Opcode::from(evt.u16());
                if !evt.params.is_empty() {
                    evt.status = Status::from(evt.u8());
                }
            }
            EventType::Hci(EventCode::CommandStatus) => {
                evt.status = Status::from(evt.u8());
                evt.cmd_quota = evt.u8();
                evt.opcode = Opcode::from(evt.u16());
            }
            _ => {
                let pf = typ.param_fmt();
                if pf.intersects(EventFmt::STATUS) {
                    evt.status = Status::from(evt.u8());
                }
                if pf.intersects(EventFmt::HANDLE) {
                    evt.handle = if pf.intersects(EventFmt::ADV_HANDLE | EventFmt::BIG_HANDLE) {
                        u16::from(evt.u8())
                    } else {
                        evt.u16()
                    };
                }
            }
        }
        Ok(evt)
    }
}

impl From<&mut Event<'_>> for () {
    /// Converts events without any additional parameters.
    #[inline]
    fn from(_: &mut Event) -> Self {}
}

/// HCI event or LE subevent code.
#[derive(Clone, Copy, Debug, Eq, PartialEq, strum::Display)]
#[allow(clippy::exhaustive_enums)]
pub enum EventType {
    Hci(EventCode),
    Le(SubeventCode),
}

impl EventType {
    /// Returns whether the event type is either `CommandStatus` or
    /// `CommandComplete`.
    #[inline]
    #[must_use]
    pub const fn is_cmd(self) -> bool {
        use EventCode::{CommandComplete, CommandStatus};
        matches!(self, Self::Hci(CommandComplete | CommandStatus))
    }

    /// Returns the format of the associated event parameters.
    #[inline]
    #[must_use]
    pub const fn param_fmt(self) -> EventFmt {
        match self {
            Self::Hci(c) => c.param_fmt(),
            Self::Le(c) => c.param_fmt(),
        }
    }
}

impl Default for EventType {
    /// Returns an invalid `EventType`.
    #[inline]
    fn default() -> Self {
        Self::Hci(EventCode::LeMetaEvent)
    }
}

/// Received event router. When an event is received, all registered waiters
/// with matching filters are notified in a broadcast fashion, and must process
/// the event asynchronously before the next receive operation can happen.
#[derive(Debug, Default)]
pub(super) struct EventRouter {
    waiters: parking_lot::Mutex<Waiters>,
    recv: tokio::sync::Mutex<SharedEventTransfer>,
}

impl EventRouter {
    /// Registers an event waiter with filter `f`.
    pub fn register(self: &Arc<Self>, f: EventFilter) -> Result<EventReceiver> {
        let mut ws = self.waiters.lock();
        if ws.queue.iter().any(|w| f.conflicts_with(&w.filter)) {
            return Err(Error::FilterConflict);
        }
        if let EventFilter::Command(opcode) = f {
            if ws.cmd_quota == 0 {
                return Err(Error::CommandQuotaExceeded);
            }
            if opcode == Opcode::Reset {
                ws.cmd_quota = 0; // [Vol 4] Part E, Section 7.3.2
            } else {
                ws.cmd_quota -= 1;
            }
        }
        let id = ws.next_id;
        ws.next_id = ws.next_id.checked_add(1).unwrap();
        ws.queue.push_back(Waiter {
            id,
            filter: f,
            ready: None,
            waker: None,
        });
        drop(ws);
        Ok(EventReceiver {
            router: Arc::clone(self),
            id,
        })
    }

    /// Receives the next event, notifies registered waiters, and returns the
    /// event to the caller.
    pub async fn recv_event(&self, t: &dyn host::Transport) -> Result<EventGuard> {
        // Ensure that there are no calls to RwLock::write_owned() when
        // RwLock::try_read_owned() is called. Since Tokio's RwLock is
        // write-preferring, acquiring read locks would fail if there is a
        // writer waiting.
        let rwlock = self.recv.lock().await;

        // Wait for all read locks to be released and receive the next event.
        // Clippy checks that EventGuards are not held across await points, so
        // the caller can't deadlock, but it's possible that another Waiter with
        // an event is not being awaited. The timeout should catch that.
        let mut recv = (timeout(Duration::from_secs(3), Arc::clone(&rwlock).write_owned()).await)
            .expect("recv_event stalled (EventGuard held for too long)");
        recv.next(t).await.map_err(|e| {
            if let Error::Host(e) = e {
                let mut ws = self.waiters.lock();
                ws.err = Some(e);
                for w in &mut ws.queue {
                    if let Some(waker) = w.waker.take() {
                        waker.wake();
                    }
                }
            }
            e
        })?;

        // TODO: One second command timeout ([Vol 4] Part E, Section 4.4)

        // Lock router before downgrading to a read lock
        let mut ws = self.waiters.lock();
        let evt = EventGuard::new(recv.downgrade());

        // Provide EventGuards to registered waiters and notify them
        if evt.typ.is_cmd() {
            ws.cmd_quota = evt.cmd_quota;
        }
        let mut received = false;
        for w in ws.queue.iter_mut().filter(|w| evt.matches(&w.filter)) {
            // try_read_owned() is guaranteed to succeed since we are
            // holding our own read lock and there are no writers waiting.
            w.ready = Some(Arc::clone(&rwlock).try_read_owned().unwrap());
            if let Some(waker) = w.waker.take() {
                waker.wake();
            }
            received = true;
        }
        if !received {
            trace!("Ignored event: {evt:?}");
        }
        Ok(evt)
    }
}

/// Future that continuously receives HCI events.
#[derive(Debug)]
pub struct EventTransferTask {
    h: tokio::task::JoinHandle<Result<()>>,
    c: CancellationToken,
    _g: tokio_util::sync::DropGuard,
}

impl EventTransferTask {
    /// Creates a new event receiver task.
    pub(super) fn new(host: Host) -> Self {
        let c = CancellationToken::new();
        Self {
            h: tokio::spawn(Self::run(host, c.clone())),
            c: c.clone(),
            _g: c.drop_guard(),
        }
    }

    /// Stops event processing.
    pub async fn disable(self) -> Result<()> {
        self.c.cancel();
        self.h.await.unwrap()
    }

    /// Receives HCI events until cancellation.
    async fn run(h: Host, c: CancellationToken) -> Result<()> {
        debug!("Event receiver task started");
        loop {
            let r: Result<EventGuard> = tokio::select! {
                r = h.event() => r,
                _ = c.cancelled() => {
                    debug!("Event receiver task terminating");
                    return Ok(());
                }
            };
            if let Err(e) = r {
                // TODO: Ignore certain errors, like short write
                debug!("Event receiver task error: {e}");
                return Err(e);
            }
        }
    }
}

impl Future for EventTransferTask {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(ready!(Pin::new(&mut self.h).poll(cx)).unwrap())
    }
}

// TODO: Use event masks for filtering. These should also determine which events
// are unmasked on the controller.

/// Defines event matching criteria.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum EventFilter {
    Command(Opcode),
    AdvManager,
    ChanManager,
    SecDb,
}

impl EventFilter {
    /// Returns whether two filters would create an ambiguity in event delivery
    /// if both were registered.
    #[inline]
    fn conflicts_with(&self, other: &Self) -> bool {
        self == other
    }
}

/// Queue of event waiters. `VecDeque` is used because there are likely to be
/// only a few waiters with frequent insertion and removal for commands, so it
/// is likely to outperform `HashMap` and `BTreeMap`.
#[derive(Debug)]
struct Waiters {
    queue: VecDeque<Waiter>,
    err: Option<host::Error>,
    next_id: u64,
    cmd_quota: u8,
}

impl Default for Waiters {
    #[inline]
    fn default() -> Self {
        Self {
            queue: VecDeque::with_capacity(4),
            err: None,
            next_id: 0,
            cmd_quota: 1, // [Vol 4] Part E, Section 4.4
        }
    }
}

/// Registered event waiter.
#[derive(Debug)]
struct Waiter {
    id: u64,
    filter: EventFilter,
    ready: Option<tokio::sync::OwnedRwLockReadGuard<EventTransfer>>,
    waker: Option<Waker>,
}

/// Guard that unregisters the event waiter when dropped.
#[derive(Debug)]
pub(crate) struct EventReceiver {
    id: u64,
    router: Arc<EventRouter>,
}

impl EventReceiver {
    /// Returns a future that resolves to the next event.
    #[inline(always)]
    pub fn next(&mut self) -> NextEvent {
        NextEvent(self)
    }
}

/// Next event future. This future is cancel safe.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct NextEvent<'a>(&'a mut EventReceiver);

impl Future for NextEvent<'_> {
    type Output = Result<EventGuard>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut ws = self.0.router.waiters.lock();
        if let Some(e) = ws.err {
            return Poll::Ready(Err(e.into()));
        }
        let w = (ws.queue.iter_mut())
            .find(|w| w.id == self.0.id)
            .expect("unregistered event waiter");
        if let Some(ready) = w.ready.take() {
            Poll::Ready(Ok(EventGuard::new(ready)))
        } else {
            w.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

impl Drop for EventReceiver {
    fn drop(&mut self) {
        let mut ws = self.router.waiters.lock();
        if let Some(i) = ws.queue.iter().position(|w| w.id == self.id) {
            // Order doesn't matter since we don't control the order in which
            // async tasks are executed.
            ws.queue.swap_remove_back(i);
        }
    }
}

/// Guard providing access to a received event. The next event cannot be
/// received until all guards are dropped. It is `!Send` to prevent it from
/// being held across `await` points.
#[derive(Debug)]
#[must_use]
pub struct EventGuard {
    r: tokio::sync::OwnedRwLockReadGuard<EventTransfer>,
    _not_send: PhantomData<*const ()>,
}

impl EventGuard {
    /// Creates an event read lock that is !Send and !Sync.
    #[inline]
    fn new(r: tokio::sync::OwnedRwLockReadGuard<EventTransfer>) -> Self {
        Self {
            r,
            _not_send: PhantomData,
        }
    }

    /// Returns the received event.
    #[inline]
    pub fn get(&self) -> Event {
        // SAFETY: EventGuard can only contain a valid Event
        unsafe { self.r.event().unwrap_unchecked() }
    }

    /// Returns the received event if it represents successful command
    /// completion.
    #[inline]
    pub fn ok(&self) -> Result<Event> {
        if !self.typ.is_cmd() {
            return Err(Error::NonCommandEvent { typ: self.typ });
        }
        if !self.status.is_ok() {
            return Err(Error::CommandFailed {
                opcode: self.opcode,
                status: self.status,
            });
        }
        Ok(self.get())
    }

    /// Returns whether the event matches filter `f`.
    #[allow(clippy::match_same_arms)]
    fn matches(&self, f: &EventFilter) -> bool {
        use {EventFilter::*, EventType::*};
        match self.typ {
            Hci(EventCode::DisconnectionComplete | EventCode::NumberOfCompletedPackets) => {
                matches!(*f, ChanManager)
            }
            Hci(EventCode::CommandComplete | EventCode::CommandStatus) => {
                matches!(*f, Command(op) if op == self.opcode)
            }
            Le(SubeventCode::ConnectionComplete | SubeventCode::EnhancedConnectionComplete) => {
                match *f {
                    Command(_) => false,
                    AdvManager => {
                        LeConnectionComplete::from(&mut self.get()).role == Role::Peripheral
                    }
                    ChanManager => true,
                    SecDb => true,
                }
            }
            Le(SubeventCode::LongTermKeyRequest) => matches!(*f, SecDb),
            Le(SubeventCode::AdvertisingSetTerminated) => matches!(*f, AdvManager),
            _ => false,
        }
    }
}

impl Deref for EventGuard {
    type Target = Event<'static>;

    /// Provides access to `Event` metadata without parameters. Use
    /// [`Self::get()`] or [`Self::ok()`] to get a decodable event.
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.r.evt
    }
}

// TODO: This should work for all events?

impl<T: for<'a, 'b> From<&'a mut Event<'b>>> From<EventGuard> for Result<T> {
    /// Converts `CommandComplete` parameters to a concrete type.
    #[inline]
    fn from(g: EventGuard) -> Self {
        let mut evt = g.ok()?;
        let r = T::from(&mut evt);
        debug_assert!(evt.params.is_empty(), "unconsumed event");
        debug_assert!(evt.params.is_ok(), "corrupt event");
        Ok(r)
    }
}

type SharedEventTransfer = Arc<tokio::sync::RwLock<EventTransfer>>;

/// HCI event transfer.
#[derive(Debug, Default)]
struct EventTransfer {
    xfer: Option<Box<dyn host::Transfer>>,
    evt: Event<'static>,
    params: usize,
}

impl EventTransfer {
    /// Receives and validates the next event.
    async fn next(&mut self, t: &dyn host::Transport) -> Result<()> {
        self.params = 0;
        let xfer = {
            let xfer = self.xfer.take().map_or_else(
                || t.event(),
                |mut xfer| {
                    xfer.reset();
                    xfer
                },
            );
            self.xfer = Some(xfer.submit()?.await?);
            // SAFETY: xfer can't be None
            unsafe { self.xfer.as_ref().unwrap_unchecked().as_ref() }
        };
        let buf = xfer.as_ref();
        let r = Event::try_from(buf);
        trace!("{r:?}");
        r.map(|evt| {
            self.evt = Event {
                params: Unpacker::default(),
                ..evt
            };
            self.params = buf.len() - evt.params.len();
        })
    }

    /// Returns the received event or [`None`] if the most recent call to
    /// `next()` failed.
    #[inline]
    #[must_use]
    fn event(&self) -> Option<Event> {
        (self.params >= EVT_HDR).then_some(Event {
            // SAFETY: We have a valid Event and EVT_HDR <= self.params <= buf.len()
            params: Unpacker::new(unsafe {
                let xfer = self.xfer.as_ref().unwrap_unchecked().as_ref();
                xfer.as_ref().get_unchecked(self.params..)
            }),
            ..self.evt
        })
    }
}
