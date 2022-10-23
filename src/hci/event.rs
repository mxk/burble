use std::collections::VecDeque;
use std::future::Future;
use std::marker::PhantomData;
use std::sync::Arc;
use std::time::Duration;

use bytes::Buf;
use tokio::time::timeout;
use tracing::trace;

use crate::dev::RawAddr;
use crate::host;
use crate::host::Transfer;

use super::*;

/// HCI event header and buffer sizes ([Vol 4] Part E, Section 5.4.4).
const EVT_HDR: usize = 2;
pub(crate) const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

/// HCI event decoder.
#[derive(Clone, Debug, Default)]
pub struct Event<'a> {
    typ: EventType,
    cmd_quota: u8,
    opcode: Opcode,
    status: Status,
    tail: &'a [u8],
}

impl Event<'_> {
    /// Returns the event type.
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub const fn typ(&self) -> EventType {
        self.typ
    }

    /// Returns the number of commands that the host is allowed to send to the
    /// controller from `CommandComplete` or `CommandStatus` events.
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub(super) const fn cmd_quota(&self) -> u8 {
        self.cmd_quota
    }

    /// Returns the opcode from `CommandComplete` or `CommandStatus` events, or
    /// [`Opcode::None`] for non-command events and either command event that
    /// only updates the command quota.
    #[inline]
    #[must_use]
    pub const fn opcode(&self) -> Opcode {
        self.opcode
    }

    /// Returns the command status for `CommandComplete` or `CommandStatus`
    /// events, or [`Status::Success`] for non-command events.
    #[inline]
    #[must_use]
    pub const fn status(&self) -> Status {
        self.status
    }

    /// Returns any unparsed bytes.
    #[cfg(test)]
    #[inline]
    #[must_use]
    pub const fn tail(&self) -> &[u8] {
        self.tail
    }

    /// Returns the next u8.
    #[inline]
    pub fn u8(&mut self) -> u8 {
        self.tail.get_u8()
    }

    /// Returns the next u16.
    #[inline]
    pub fn u16(&mut self) -> u16 {
        self.tail.get_u16_le()
    }

    /// Returns the next `BD_ADDR`.
    #[inline]
    pub fn addr(&mut self) -> RawAddr {
        RawAddr::from(*self.array())
    }

    /// Returns a reference to the next array of length `N`.
    #[inline]
    pub fn array<const N: usize>(&mut self) -> &[u8; N] {
        // TODO: Use split_array_ref when stabilized
        let (head, tail) = self.tail.split_at(N);
        self.tail = tail;
        // SAFETY: head is &[u8; N] (checked by split_at)
        unsafe { &*head.as_ptr().cast() }
    }
}

impl<'a> TryFrom<&'a [u8]> for Event<'a> {
    type Error = Error;

    /// Tries to parse event header from `orig`. The subevent code for LE events
    /// and command status information are also consumed.
    fn try_from(orig: &'a [u8]) -> Result<Self> {
        if orig.len() < EVT_HDR {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(orig)));
        }
        let mut tail = orig;
        let (code, len) = (tail.get_u8(), tail.get_u8());
        if tail.len() != len as usize {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(orig)));
        }
        let typ = match EventCode::from_repr(code) {
            Some(EventCode::LeMetaEvent) => {
                // After the header is validated, we allow further decoding
                // calls to panic if there is missing data.
                let subevent = tail.get_u8();
                match SubeventCode::from_repr(subevent) {
                    Some(subevent) => EventType::Le(subevent),
                    None => {
                        return Err(Error::UnknownEvent {
                            code: code as _,
                            subevent,
                            params: Bytes::copy_from_slice(tail),
                        })
                    }
                }
            }
            Some(code) => EventType::Hci(code),
            None => {
                return Err(Error::UnknownEvent {
                    code,
                    subevent: 0,
                    params: Bytes::copy_from_slice(tail),
                })
            }
        };
        let mut evt = Self {
            typ,
            cmd_quota: 0,
            opcode: Opcode::None,
            status: Status::Success,
            tail,
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
            _ => {}
        };
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
    waiters: parking_lot::Mutex<Waiters<T>>,
    recv: tokio::sync::Mutex<SharedEventReceiver<T>>,

    // Notification should ideally be limited to each waiter, but we need
    // condvar-like functionality where we register for notification while
    // holding the waiters lock, drop the lock, and then await. That doesn't
    // work if we're referencing a synchronization primitive protected by the
    // lock, so we keep this separate.
    notify: tokio::sync::Notify,
}

impl<T: host::Transport> EventRouter<T> {
    /// Register a new event waiter with filter `f`.
    pub fn register(&self, f: EventFilter) -> Result<EventWaiterGuard<T>> {
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
        Ok(EventWaiterGuard { router: self, id })
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
        let mut recv =
            match timeout(Duration::from_secs(3), Arc::clone(&rwlock).write_owned()).await {
                Ok(recv) => recv,
                Err(_) => panic!("recv_event stalled (EventGuard held for too long)"),
            };
        recv.next(transport).await?;

        // TODO: Remove and notify receivers for certain fatal errors, like lost
        // device.

        // TODO: One second command timeout ([Vol 4] Part E, Section 4.4)

        // Lock router before downgrading to a read lock
        let mut waiters = self.waiters.lock();
        let guard = EventGuard::new(recv.downgrade());

        // Provide EventGuards to registered waiters and notify them
        let evt = guard.get();
        if evt.typ.is_cmd() {
            waiters.cmd_quota = evt.cmd_quota;
        }
        let mut notify = false;
        for w in waiters.queue.iter_mut().filter(|w| w.filter.matches(&evt)) {
            // try_read_owned() is guaranteed to succeed since we are
            // holding our own read lock and there are no writers waiting.
            w.ready = Some(Arc::clone(&rwlock).try_read_owned().unwrap());
            notify = true;
        }
        if notify {
            self.notify.notify_waiters();
        }
        Ok(guard)
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
    /// Returns a new event monitor.
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

    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(ready!(Pin::new(&mut self.h).poll(ctx)).unwrap())
    }
}

/// Defines event matching criteria.
#[derive(Clone, Debug)]
pub(crate) enum EventFilter {
    _Any,
    Command(Opcode),
}

impl EventFilter {
    /// Returns whether two filters would create an ambiguity in event delivery
    /// if both were registered.
    fn conflicts_with(&self, other: &Self) -> bool {
        use EventFilter::*;
        match (self, other) {
            (&Command(lhs), &Command(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    /// Returns whether `evt` matches the filter.
    fn matches(&self, evt: &Event) -> bool {
        use EventFilter::*;
        match *self {
            _Any => true,
            Command(opcode) => evt.opcode == opcode && evt.typ.is_cmd(),
        }
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
pub(super) struct EventWaiterGuard<'a, T: host::Transport> {
    router: &'a EventRouter<T>,
    id: u64,
}

impl<T: host::Transport> EventWaiterGuard<'_, T> {
    /// Returns the next matching event or [`None`] if the waiter is no longer
    /// registered (e.g. if the controller is lost).
    pub async fn next(&mut self) -> Option<EventGuard<T>> {
        loop {
            let ready_or_notify = {
                let mut ws = self.router.waiters.lock();
                match ws.queue.iter_mut().find(|w| w.id == self.id) {
                    Some(w) => w.ready.take().ok_or_else(|| self.router.notify.notified()),
                    None => return None,
                }
            };
            match ready_or_notify {
                Ok(ready) => return Some(EventGuard::new(ready)),
                Err(notify) => notify.await,
            };
        }
    }
}

impl<T: host::Transport> Drop for EventWaiterGuard<'_, T> {
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
    /// Returns a read lock that is !Send and !Sync.
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

    /// Returns the command status or `Status::Success` for non-command events.
    #[inline]
    #[must_use]
    pub fn status(&self) -> Status {
        self.r.evt.status
    }

    /// Returns the received event if it represents successful command
    /// completion.
    #[inline]
    pub fn cmd_ok(&self) -> Result<Event> {
        let evt = self.get();
        if !evt.typ.is_cmd() {
            return Err(Error::NonCommandEvent { typ: evt.typ });
        }
        if evt.status != Status::Success {
            return Err(Error::CommandFailed {
                opcode: evt.opcode,
                status: evt.status,
            });
        }
        Ok(evt)
    }
}

impl<T: host::Transport, R: for<'a, 'b> From<&'a mut Event<'b>>> From<EventGuard<T>> for Result<R> {
    /// Converts `CommandComplete` parameters to a concrete type.
    #[inline]
    fn from(g: EventGuard<T>) -> Self {
        let mut evt = g.cmd_ok()?;
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
            self.evt = Event { tail: &[], ..evt };
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
        let tail = unsafe {
            self.xfer
                .as_ref()
                .unwrap_unchecked()
                .as_ref()
                .get_unchecked(self.tail..)
        };
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
