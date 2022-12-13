use std::collections::VecDeque;
use std::future::Future;
use std::marker::PhantomData;
use std::ops::Deref;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};
use std::time::Duration;

use structbuf::Unpacker;
use tokio::time::timeout;
use tokio_util::sync::CancellationToken;
use tracing::{debug, trace};

pub use {hci::*, le::*};

use crate::{host, host::Transfer, le::RawAddr};

use super::*;

mod hci;
mod le;

// TODO: Merge Event and EventGuard
// TODO: Remove u* and i* Event methods

/// HCI event decoder.
#[derive(Clone, Debug, Default)]
pub struct Event<'a> {
    typ: EventType,
    status: Status,
    cmd_quota: u8,
    opcode: Opcode,
    handle: u16,
    tail: Unpacker<'a>,
}

impl Event<'_> {
    /// Returns the event type.
    #[inline]
    #[must_use]
    pub const fn typ(&self) -> EventType {
        self.typ
    }

    /// Returns the event status or [`Status::Success`] for events without a
    /// status parameter.
    #[inline]
    #[must_use]
    pub const fn status(&self) -> Status {
        self.status
    }

    /// Returns the number of commands that the host is allowed to send to the
    /// controller from `CommandComplete` or `CommandStatus` events.
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub(super) const fn cmd_quota(&self) -> u8 {
        self.cmd_quota
    }

    /// Returns the opcode from `CommandComplete` or `CommandStatus` events.
    /// [`Opcode::None`] may be returned for either event that only updates the
    /// command quota and for non-command events.
    #[inline]
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

    /// Returns any unparsed bytes.
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub fn tail(&self) -> &[u8] {
        self.tail.as_ref()
    }

    /// Returns the next u8.
    #[inline]
    pub fn u8(&mut self) -> u8 {
        self.tail.u8()
    }

    /// Returns the next i8.
    #[inline]
    pub fn i8(&mut self) -> i8 {
        self.tail.i8()
    }

    /// Returns the next u16.
    #[inline]
    pub fn u16(&mut self) -> u16 {
        self.tail.u16()
    }

    /// Returns the next `BD_ADDR`.
    #[inline]
    pub fn addr(&mut self) -> RawAddr {
        RawAddr::from(*self.array())
    }

    /// Returns a reference to the next array of length `N`.
    #[inline]
    pub fn array<const N: usize>(&mut self) -> &[u8; N] {
        // SAFETY: unwrap() returns &[u8; N]
        unsafe { &*self.tail.skip(N).unwrap().as_ref().as_ptr().cast() }
    }
}

impl<'a> TryFrom<&'a [u8]> for Event<'a> {
    type Error = Error;

    /// Tries to parse event header from `orig`. The subevent code for LE
    /// events, event status, and handle parameters are also consumed.
    fn try_from(orig: &'a [u8]) -> Result<Self> {
        let mut p = Unpacker::new(orig);
        let Some(mut hdr) = p.skip(EVT_HDR) else {
            return Err(Error::InvalidEvent(Vec::from(orig)));
        };
        let code = hdr.u8();
        if p.len() != usize::from(hdr.u8()) {
            return Err(Error::InvalidEvent(Vec::from(orig)));
        }
        let typ = match EventCode::try_from(code) {
            Ok(EventCode::LeMetaEvent) => {
                let subevent = p.u8();
                match SubeventCode::try_from(subevent) {
                    Ok(subevent) => EventType::Le(subevent),
                    Err(_) => {
                        return Err(Error::UnknownEvent {
                            code,
                            subevent,
                            params: Vec::from(p.as_ref()),
                        })
                    }
                }
            }
            Ok(code) => EventType::Hci(code),
            Err(_) => {
                return Err(Error::UnknownEvent {
                    code,
                    subevent: 0,
                    params: Vec::from(p.as_ref()),
                })
            }
        };
        let mut evt = Self {
            typ,
            tail: p,
            ..Self::default()
        };
        match typ {
            EventType::Hci(EventCode::CommandComplete) => {
                evt.cmd_quota = evt.u8();
                evt.opcode = Opcode::from(evt.u16());
                if !evt.tail.is_empty() {
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

/// Basic information from `CommandComplete` or `CommandStatus` events.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CommandStatus {
    pub cmd_quota: u8,
    pub opcode: Opcode,
    pub status: Status,
}

/// Received event router. When an event is received, all registered waiters
/// with matching filters are notified in a broadcast fashion, and must process
/// the event asynchronously before the next receive operation can happen.
#[derive(Debug)]
pub(super) struct EventRouter<T: host::Transport> {
    waiters: parking_lot::Mutex<Waiters<T>>, // TODO: Switch to Condvar
    recv: tokio::sync::Mutex<SharedEventReceiver<T>>,

    // Notification should ideally be limited to each waiter, but we need
    // condvar-like functionality where we register for notification while
    // holding the waiters lock, drop the lock, and then await. That doesn't
    // work if we're referencing a synchronization primitive protected by the
    // lock, so we keep this separate.
    notify: tokio::sync::Notify,
}

impl<T: host::Transport> EventRouter<T> {
    /// Registers an event waiter with filter `f`.
    pub fn register(self: &Arc<Self>, f: EventFilter) -> Result<EventWaiterGuard<T>> {
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
        });
        drop(ws);
        Ok(EventWaiterGuard {
            router: Arc::clone(self),
            id,
        })
    }

    /// Receives the next event, notifies registered waiters, and returns the
    /// event to the caller.
    pub async fn recv_event(&self, transport: &T) -> Result<EventGuard<T>> {
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
        recv.next(transport).await?;

        // TODO: Remove and notify receivers for certain fatal errors, like lost
        // device.

        // TODO: One second command timeout ([Vol 4] Part E, Section 4.4)

        // Lock router before downgrading to a read lock
        let mut waiters = self.waiters.lock();
        let evt = EventGuard::new(recv.downgrade());

        // Provide EventGuards to registered waiters and notify them
        if evt.typ.is_cmd() {
            waiters.cmd_quota = evt.cmd_quota;
        }
        let mut notify = false;
        for w in waiters.queue.iter_mut().filter(|w| evt.matches(&w.filter)) {
            // try_read_owned() is guaranteed to succeed since we are
            // holding our own read lock and there are no writers waiting.
            w.ready = Some(Arc::clone(&rwlock).try_read_owned().unwrap());
            notify = true;
        }
        if notify {
            self.notify.notify_waiters();
        } else {
            trace!("Ignored event: {evt:?}");
        }
        Ok(evt)
    }
}

impl<T: host::Transport> Default for EventRouter<T> {
    #[inline]
    fn default() -> Self {
        Self {
            waiters: parking_lot::Mutex::default(),
            recv: tokio::sync::Mutex::default(),
            notify: tokio::sync::Notify::default(),
        }
    }
}

/// Future that continuously receives HCI events.
#[derive(Debug)]
pub struct EventReceiverTask {
    h: tokio::task::JoinHandle<Result<()>>,
    c: CancellationToken,
    _g: tokio_util::sync::DropGuard,
}

impl EventReceiverTask {
    /// Creates a new event receiver task.
    pub(super) fn new<T: host::Transport + 'static>(host: Host<T>) -> Self {
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
    async fn run<T: host::Transport>(h: Host<T>, c: CancellationToken) -> Result<()> {
        debug!("Event receiver task started");
        loop {
            let r: Result<EventGuard<T>> = tokio::select! {
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

impl Future for EventReceiverTask {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(ready!(Pin::new(&mut self.h).poll(cx)).unwrap())
    }
}

/// Defines event matching criteria.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum EventFilter {
    Command(Opcode),
    AdvManager,
    ChanManager,
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
struct Waiters<T: host::Transport> {
    queue: VecDeque<Waiter<T>>,
    next_id: u64,
    cmd_quota: u8,
}

impl<T: host::Transport> Default for Waiters<T> {
    #[inline]
    fn default() -> Self {
        Self {
            queue: VecDeque::with_capacity(4),
            next_id: 0,
            cmd_quota: 1, // [Vol 4] Part E, Section 4.4
        }
    }
}

/// Registered event waiter.
#[derive(Debug)]
struct Waiter<T: host::Transport> {
    id: u64,
    filter: EventFilter,
    ready: Option<tokio::sync::OwnedRwLockReadGuard<EventReceiver<T>>>,
}

/// Guard that unregisters the event waiter when dropped.
#[derive(Debug)]
pub(crate) struct EventWaiterGuard<T: host::Transport> {
    router: Arc<EventRouter<T>>,
    id: u64, // TODO: Use direct pointer registration rather than ids
}

impl<T: host::Transport> EventWaiterGuard<T> {
    /// Returns the next matching event or an error if the waiter is no longer
    /// registered (e.g. if the controller is lost). This method is cancel safe.
    pub async fn next(&self) -> Result<EventGuard<T>> {
        loop {
            let ready_or_notify = {
                let mut ws = self.router.waiters.lock();
                match ws.queue.iter_mut().find(|w| w.id == self.id) {
                    Some(w) => w.ready.take().ok_or_else(|| self.router.notify.notified()),
                    None => return Err(Status::UnspecifiedError.into()),
                }
            };
            match ready_or_notify {
                Ok(ready) => return Ok(EventGuard::new(ready)),
                Err(notify) => notify.await,
            };
        }
    }
}

impl<T: host::Transport> Drop for EventWaiterGuard<T> {
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
pub struct EventGuard<T: host::Transport> {
    r: tokio::sync::OwnedRwLockReadGuard<EventReceiver<T>>,
    _not_send: PhantomData<*const ()>,
}

impl<T: host::Transport> EventGuard<T> {
    /// Creates a read lock that is !Send and !Sync.
    #[inline]
    #[must_use]
    fn new(r: tokio::sync::OwnedRwLockReadGuard<EventReceiver<T>>) -> Self {
        Self {
            r,
            _not_send: PhantomData,
        }
    }

    /// Returns the received event.
    #[inline]
    #[must_use]
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
    fn matches(&self, f: &EventFilter) -> bool {
        use {EventFilter::*, EventType::*};
        match self.typ {
            Hci(EventCode::DisconnectionComplete | EventCode::NumberOfCompletedPackets) => {
                *f == ChanManager
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
                }
            }
            Le(SubeventCode::AdvertisingSetTerminated) => *f == AdvManager,
            _ => false,
        }
    }
}

impl<T: host::Transport> Deref for EventGuard<T> {
    type Target = Event<'static>;

    /// Provides access to `Event` metadata. The `tail()` will be empty. Use
    /// [`Self::get()`] or [`Self::ok()`] to get a decodable event.
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.r.evt
    }
}

impl<T: host::Transport, R: for<'a, 'b> From<&'a mut Event<'b>>> From<EventGuard<T>> for Result<R> {
    /// Converts `CommandComplete` parameters to a concrete type.
    #[inline]
    fn from(g: EventGuard<T>) -> Self {
        let mut evt = g.ok()?;
        let r = R::from(&mut evt);
        debug_assert_eq!(evt.tail.len(), 0, "unconsumed event");
        Ok(r)
    }
}

type SharedEventReceiver<T> = Arc<tokio::sync::RwLock<EventReceiver<T>>>;

/// HCI event receiver.
#[derive(Debug)]
struct EventReceiver<T: host::Transport> {
    xfer: Option<T::Transfer>,
    evt: Event<'static>,
    tail: usize,
}

impl<T: host::Transport> EventReceiver<T> {
    /// Receives and validates the next event.
    async fn next(&mut self, transport: &T) -> Result<()> {
        self.tail = 0;
        let xfer = {
            let mut xfer = self.xfer.take().unwrap_or_else(|| transport.event());
            xfer.reset();
            self.xfer = Some(xfer.submit()?.await);
            // SAFETY: xfer can't be None
            unsafe { self.xfer.as_ref().unwrap_unchecked() }
        };
        xfer.result().unwrap()?;
        let buf = xfer.as_ref();
        let r = Event::try_from(buf);
        trace!("{r:?}");
        r.map(|evt| {
            self.evt = Event {
                tail: Unpacker::default(),
                ..evt
            };
            self.tail = buf.len() - evt.tail.len();
        })
    }

    /// Returns the received event or [`None`] if the most recent call to
    /// `next()` failed.
    #[inline]
    #[must_use]
    fn event(&self) -> Option<Event> {
        if self.tail < EVT_HDR {
            return None;
        }
        // SAFETY: We have a valid Event and EVT_HDR <= self.tail <= buf.len()
        let tail = Unpacker::new(unsafe {
            self.xfer
                .as_ref()
                .unwrap_unchecked()
                .as_ref()
                .get_unchecked(self.tail..)
        });
        Some(Event { tail, ..self.evt })
    }
}

impl<T: host::Transport> Default for EventReceiver<T> {
    #[inline]
    fn default() -> Self {
        Self {
            xfer: None,
            evt: Event::default(),
            tail: 0,
        }
    }
}
