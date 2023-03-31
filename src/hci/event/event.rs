use std::collections::VecDeque;
use std::fmt::Formatter;
use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};
use std::time::Duration;

use structbuf::Unpacker;
use tokio::time::timeout;
use tracing::{trace, warn};

pub use {hci::*, le::*};

use crate::le::RawAddr;
use crate::{host, AsyncMutex, AsyncRwLock, SyncMutex, SyncMutexGuard};

use super::*;

mod hci;
mod le;

#[cfg(test)]
mod tests;

/// Common event header. [`EventCode`] determines the validity of other fields.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct EventHeader {
    code: EventCode,
    status: Status,
    cmd_quota: u8,
    opcode: Opcode,
    handle: u16,
}

impl EventHeader {
    /// Unpacks and validates the header of a received event, returning the
    /// header and any remaining parameters.
    fn unpack(raw: &[u8]) -> Result<(Self, &[u8])> {
        let mut p = Unpacker::new(raw);
        let (code, len, rem) = (p.u8(), p.u8(), p.len());
        let subcode = if code == EventCode::LeMetaEvent as u8 {
            p.u8()
        } else {
            0
        };
        if usize::from(len) != rem || !p.is_ok() {
            return Err(Error::InvalidEvent(Vec::from(raw)));
        }
        let code = match EventCode::try_from(u16::from(subcode) << 8 | u16::from(code)) {
            Ok(code) if !matches!(code, EventCode::LeMetaEvent) => code,
            _ => {
                return Err(Error::UnknownEvent {
                    code,
                    subcode,
                    params: Vec::from(p.as_ref()),
                })
            }
        };
        let mut hdr = Self {
            code,
            ..Self::default()
        };
        match code {
            EventCode::CommandComplete => {
                hdr.cmd_quota = p.u8();
                hdr.opcode = Opcode::from(p.u16());
                if !p.is_empty() {
                    hdr.status = Status::from(p.u8());
                }
            }
            EventCode::CommandStatus => {
                hdr.status = Status::from(p.u8());
                hdr.cmd_quota = p.u8();
                hdr.opcode = Opcode::from(p.u16());
            }
            _ => {
                let pf = code.param_fmt();
                if pf.contains(EventFmt::STATUS) {
                    hdr.status = Status::from(p.u8());
                }
                if pf.intersects(EventFmt::HANDLE) {
                    hdr.handle = if pf.intersects(EventFmt::ADV_HANDLE | EventFmt::BIG_HANDLE) {
                        u16::from(p.u8())
                    } else {
                        p.u16()
                    };
                }
            }
        }
        if p.is_ok() {
            Ok((hdr, p.into_inner()))
        } else {
            Err(Error::InvalidEvent(Vec::from(raw)))
        }
    }
}

impl Default for EventHeader {
    #[inline]
    fn default() -> Self {
        Self {
            code: EventCode::LeMetaEvent, // Would never be unpacked
            status: Status::Success,
            cmd_quota: 0,
            opcode: Opcode::None,
            handle: 0,
        }
    }
}

/// HCI event handle. Provides access to common event metadata and methods for
/// unpacking event-specific parameters. The next event cannot be received until
/// all existing handles are dropped.
///
/// It is `!Send` to prevent it from being held across `await` points.
#[must_use]
#[repr(transparent)]
pub struct Event(
    EventRef,
    PhantomData<*const ()>, // !Send
);

impl Event {
    /// Creates a new event handle after unpacking and validating the header.
    #[inline]
    fn new(mut t: tokio::sync::OwnedRwLockWriteGuard<EventTransfer>) -> Result<Self> {
        let raw = t.xfer.as_deref().expect("invalid event transfer").as_ref();
        let (hdr, params) = EventHeader::unpack(raw)?;
        trace!("{hdr:?} {params:02X?}");
        (t.hdr, t.params_off) = (hdr, raw.len() - params.len());
        Ok(Self(t.downgrade(), PhantomData))
    }

    /// Returns the event code.
    #[inline(always)]
    #[must_use]
    pub fn code(&self) -> EventCode {
        self.0.hdr.code
    }

    /// Returns the event status or [`Status::Success`] for events without a
    /// status parameter.
    #[inline(always)]
    #[must_use]
    pub fn status(&self) -> Status {
        self.0.hdr.status
    }

    /// Returns the opcode from `CommandComplete` or `CommandStatus` events.
    /// [`Opcode::None`] is returned for non-command events, including
    /// `CommandComplete` and `CommandStatus` events that only update the
    /// command quota.
    #[inline(always)]
    #[must_use]
    pub fn opcode(&self) -> Opcode {
        self.0.hdr.opcode
    }

    /// Returns the associated connection handle or `None` for non-connection
    /// events.
    #[inline]
    #[must_use]
    pub fn conn_handle(&self) -> Option<ConnHandle> {
        if self.code().param_fmt().contains(EventFmt::CONN_HANDLE) {
            ConnHandle::new(self.0.hdr.handle)
        } else {
            None
        }
    }

    /// Returns the associated advertising handle or `None` for non-advertising
    /// events.
    #[inline]
    #[must_use]
    pub fn adv_handle(&self) -> Option<AdvHandle> {
        #[allow(clippy::cast_possible_truncation)]
        if self.code().param_fmt().contains(EventFmt::ADV_HANDLE) {
            AdvHandle::new(self.0.hdr.handle as u8)
        } else {
            None
        }
    }

    /// Unpacks event parameters.
    ///
    /// # Panics
    ///
    /// Panics if `T` is not a representation of the event.
    #[inline]
    #[must_use]
    pub(crate) fn get<T: FromEvent>(&self) -> T {
        debug_assert!(
            T::matches(self.code()),
            "event type mismatch for {}",
            self.code()
        );
        self.unpack(T::unpack)
    }

    /// Unpacks successful command completion or status parameters. Returns an
    /// error if the command failed.
    ///
    /// # Panics
    ///
    /// Panics for non-command events or if `T` is not a representation of the
    /// event.
    #[inline]
    pub(crate) fn ok<T: FromEvent>(&self) -> Result<T> {
        self.cmd_ok().map(|_| self.get())
    }

    /// Calls `f` to unpack successful command completion or status parameters.
    /// Returns an error if the command failed.
    ///
    /// # Panics
    ///
    /// Panics for non-command events.
    #[inline]
    pub(crate) fn map_ok<T>(&self, f: impl FnOnce(&Self, &mut Unpacker) -> T) -> Result<T> {
        self.cmd_ok().map(|_| self.unpack(f))
    }

    /// Ensures that the event represent successful command status or
    /// completion.
    ///
    /// # Panics
    ///
    /// Panics for non-command events.
    pub(super) fn cmd_ok(&self) -> Result<()> {
        assert!(self.code().is_cmd(), "non-command {} event", self.code());
        if self.status().is_ok() {
            Ok(())
        } else {
            Err(Error::CommandFailed {
                opcode: self.opcode(),
                status: self.status(),
            })
        }
    }

    /// Calls `f` to unpack event parameters.
    #[inline]
    #[must_use]
    fn unpack<T>(&self, f: impl FnOnce(&Self, &mut Unpacker) -> T) -> T {
        let mut p = self.0.params();
        let v = f(self, &mut p);
        debug_assert!(p.is_ok(), "unexpected {} event format", self.code());
        debug_assert!(p.is_empty(), "unconsumed {} event", self.code());
        v
    }
}

impl Debug for Event {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:02X?}", self.0.hdr, self.0.params().as_ref())
    }
}

/// Trait for unpacking event parameters.
pub(crate) trait FromEvent {
    /// Returns whether `unpack` supports event code `c`.
    #[inline(always)]
    #[must_use]
    fn matches(c: EventCode) -> bool {
        matches!(c, EventCode::CommandComplete)
    }

    /// Unpacks event parameters. Implementations must consume `p` fully without
    /// reading past the end.
    #[must_use]
    fn unpack(e: &Event, p: &mut Unpacker) -> Self;
}

/// Unpacker for command completion events without parameters.
impl FromEvent for () {
    #[inline(always)]
    fn unpack(_: &Event, _: &mut Unpacker) -> Self {}
}

/// Extension trait providing [`Event`]-specific [`Unpacker`] methods.
pub(crate) trait EventUnpacker {
    /// Returns the next `BD_ADDR`.
    fn addr(&mut self) -> RawAddr;
}

impl EventUnpacker for Unpacker<'_> {
    #[inline(always)]
    fn addr(&mut self) -> RawAddr {
        // SAFETY: All bit patterns are valid
        unsafe { self.read() }
    }
}

/// Event receiver and router. When an event is received, all registered
/// receivers are notified in a broadcast fashion, and must process and drop the
/// event before the next one can be received. Command status and completion
/// events are delivered to exactly one receiver with the matching [`Opcode`].
#[derive(Debug)]
pub(super) struct EventRouter {
    monitor: SyncMutex<Monitor>,
    xfer: AsyncMutex<Arc<AsyncRwLock<EventTransfer>>>,
}

impl EventRouter {
    /// Creates a new event router.
    #[inline]
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            monitor: SyncMutex::default(),
            xfer: AsyncMutex::new(Arc::new(AsyncRwLock::with_max_readers(
                EventTransfer::default(),
                64,
            ))),
        })
    }

    /// Returns a future that resolves when the specified command can be
    /// submitted.
    #[inline(always)]
    pub fn reserve(self: &Arc<Self>, opcode: Opcode) -> Reserve {
        debug_assert!(opcode.is_some());
        Reserve {
            router: Some(Arc::clone(self)),
            opcode,
        }
    }

    /// Returns a non-command event stream.
    #[inline(always)]
    pub fn events(self: &Arc<Self>) -> EventStream {
        self.events_locked(self.monitor.lock(), Opcode::None)
    }

    /// Registers a new event stream.
    #[inline]
    fn events_locked(
        self: &Arc<Self>,
        mut m: SyncMutexGuard<Monitor>,
        opcode: Opcode,
    ) -> EventStream {
        let id = m.next_id;
        m.next_id = m.next_id.checked_add(1).expect("overflow");
        m.queue.push_back(Receiver {
            id,
            opcode,
            ..Receiver::default()
        });
        EventStream {
            router: Arc::clone(self),
            id,
        }
    }

    /// Returns connection watch channel for the specified handle or [`None`] if
    /// the handle is invalid.
    #[inline]
    pub fn conn(&self, hdl: ConnHandle) -> Option<ConnWatch> {
        let m = self.monitor.lock();
        (m.conns.get(&hdl)).map(tokio::sync::watch::Sender::subscribe)
    }

    /// Calls `f` to update connection parameters. This is a no-op if the handle
    /// is invalid.
    #[inline]
    pub fn update_conn(&self, hdl: ConnHandle, f: impl FnOnce(&mut Conn)) {
        let m = self.monitor.lock();
        if let Some(s) = m.conns.get(&hdl) {
            s.send_modify(f);
        }
    }

    /// Receives the next event, notifies registered receivers, and returns the
    /// event to the caller.
    ///
    /// # Panics
    ///
    /// Panics if there are multiple concurrent event receivers.
    pub async fn next(&self, t: &dyn host::Transport) -> Result<Event> {
        // Ensure that there is only one receiver. Since Tokio's RwLock is
        // write-preferring, RwLock::try_read_owned() would fail if another
        // receiver is blocked on RwLock::write_owned().
        let rwlock = self.xfer.try_lock().expect("multiple event receivers");

        // Wait for all event handles to be dropped and receive the next event.
        // Clippy checks that Event handles are not held across await points, so
        // the caller can't deadlock, but it's possible that another Receiver
        // with an event is not being polled. The timeout will detect this.
        let mut xfer = timeout(Duration::from_secs(3), Arc::clone(&rwlock).write_owned())
            .await
            .expect("EventRouter stalled (Event not handled)");
        xfer.next(t).await.map_err(|e| {
            // Fatal transport error
            let mut m = self.monitor.lock();
            m.err = Some(e);
            for r in &mut m.queue {
                r.ready = None;
                if let Some(w) = r.waker.take() {
                    w.wake();
                }
            }
            e
        })?;

        let evt = Event::new(xfer)?;
        self.monitor.lock().notify(&rwlock, &evt);
        Ok(evt)
    }
}

/// Receiver queue and router state monitor.
#[derive(Debug)]
struct Monitor {
    conns: BTreeMap<ConnHandle, tokio::sync::watch::Sender<Conn>>,
    queue: VecDeque<Receiver>,
    err: Option<host::Error>,
    next_id: u64,
    cmd_quota: u8,
    cmd_wakers: Vec<Waker>,
}

impl Monitor {
    /// Updates command quota, possibly waking any blocked commands.
    #[inline]
    fn set_cmd_quota(&mut self, new: u8) {
        self.cmd_quota = new;
        if new > 0 {
            // Commands don't unregister their wakers if dropped, so we have to
            // wake them all to guarantee that at least one can execute.
            self.wake_cmds();
        }
    }

    /// Wakes all command waiters.
    #[inline]
    fn wake_cmds(&mut self) {
        while let Some(w) = self.cmd_wakers.pop() {
            w.wake();
        }
    }

    /// Notifies registered receives of a new event.
    fn notify(&mut self, xfer: &Arc<AsyncRwLock<EventTransfer>>, evt: &Event) {
        use EventCode::*;
        let hdr = &evt.0.hdr;
        if hdr.code.is_cmd() {
            if hdr.cmd_quota == 0 && hdr.opcode.is_none() && !hdr.status.is_ok() {
                // RTL8761BUV sends this invalid event (makes no sense to have a
                // non-zero status for the zero opcode) when the controller had
                // an unconsumed event from a previous connection. We let the
                // command timeout take care of any retry logic, and we ignore
                // the command quota because the controller never increases it
                // later, permanently stalling the HCI interface.
                warn!("Ignored corrupt command completion: {hdr:?}");
                self.set_cmd_quota(1);
                return;
            }
            self.set_cmd_quota(hdr.cmd_quota);
            if hdr.opcode.is_some() {
                (self.queue.iter_mut().find(|r| r.opcode == hdr.opcode)).map_or_else(
                    || warn!("Ignored {} command completion", hdr.opcode),
                    |r| r.ready(xfer),
                );
            }
            return;
        }
        match hdr.code {
            DisconnectionComplete => {
                if hdr.status.is_ok() {
                    let e: super::DisconnectionComplete = evt.get();
                    if let Some(s) = self.conns.remove(&e.handle) {
                        s.send_modify(|cn| cn.disconnect_reason = Some(e.reason));
                    }
                }
            }
            HardwareError => {
                error!("Controller hardware error: {:#04X}", evt.0.params().u8());
            }
            LeConnectionComplete | LeEnhancedConnectionComplete => {
                if hdr.status.is_ok() {
                    let e: super::LeConnectionComplete = evt.get();
                    let (cn, _) = tokio::sync::watch::channel(Conn::new(&e));
                    let old = self.conns.insert(e.handle, cn);
                    assert!(old.is_none(), "duplicate connection handle");
                }
            }
            _ => {}
        }
        let mut received = false;
        for r in self.queue.iter_mut().filter(|r| r.opcode.is_none()) {
            r.ready(xfer);
            received = true;
        }
        if !received {
            trace!("Ignored event: {evt:?}");
        }
    }

    /// Returns the receiver with the specified `id`.
    #[inline(always)]
    fn get(&mut self, id: u64) -> &mut Receiver {
        (self.queue.iter_mut().find(|r| r.id == id)).expect("unregistered event receiver")
    }
}

impl Default for Monitor {
    #[inline]
    fn default() -> Self {
        Self {
            conns: BTreeMap::new(),
            queue: VecDeque::with_capacity(8),
            err: None,
            next_id: 0,
            cmd_quota: 1, // [Vol 4] Part E, Section 4.4
            cmd_wakers: Vec::new(),
        }
    }
}

/// Registered event receiver.
#[derive(Debug, Default)]
struct Receiver {
    id: u64,
    opcode: Opcode,
    ready: Option<EventRef>,
    waker: Option<Waker>,
}

impl Receiver {
    /// Provides a new event to and notifies the receiver.
    ///
    /// # Panics
    ///
    /// Panics if `try_read_owned()` fails. The caller prevents this by holding
    /// another read lock and ensuring that there are no writers waiting.
    #[inline]
    fn ready(&mut self, xfer: &Arc<AsyncRwLock<EventTransfer>>) {
        let xfer = Arc::clone(xfer).try_read_owned().unwrap();
        self.ready = Some(xfer);
        if let Some(w) = self.waker.take() {
            w.wake();
        }
    }
}

/// Future that resolves when the command can be submitted after ensuring that
/// there are no quota or opcode conflicts.
#[derive(Debug)]
#[must_use]
pub(super) struct Reserve {
    router: Option<Arc<EventRouter>>,
    opcode: Opcode,
}

impl Future for Reserve {
    type Output = CmdGuard;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let router = self.router.as_ref().expect("poll of resolved future");
        let mut m = router.monitor.lock();
        // The spec doesn't say whether commands with the same opcode can be
        // executed concurrently, but it's safer to avoid.
        if m.queue.iter().any(|r| r.opcode == self.opcode) || m.cmd_quota == 0 {
            m.cmd_wakers.push(cx.waker().clone());
            return Poll::Pending;
        }
        if matches!(self.opcode, Opcode::Reset) {
            m.cmd_quota = 0; // [Vol 4] Part E, Section 7.3.2
        } else {
            m.cmd_quota -= 1;
        }
        let events = router.events_locked(m, self.opcode);
        Poll::Ready(CmdGuard {
            router: self.router.take(),
            events: Some(events),
        })
    }
}

/// Command quota reservation guard that restores the quota if the command is
/// not submitted successfully.
#[derive(Debug)]
#[must_use]
pub(super) struct CmdGuard {
    router: Option<Arc<EventRouter>>,
    events: Option<EventStream>,
}

impl CmdGuard {
    /// Consumes reserved command quota and returns the command event stream.
    /// This should be called after the command transfer is completed.
    #[inline(always)]
    pub fn submitted(mut self) -> EventStream {
        self.router = None;
        self.events.take().expect("guard already consumed")
    }
}

impl Drop for CmdGuard {
    #[inline]
    fn drop(&mut self) {
        let Some(router) = self.router.take() else { return };
        let mut m = router.monitor.lock();
        let new = m.cmd_quota.saturating_add(1);
        m.set_cmd_quota(new);
    }
}

/// HCI event stream. The owner is guaranteed to receive all matching events
/// after the stream is created, and must continuously poll [`NextEvent`] future
/// to avoid blocking event delivery.
#[derive(Debug)]
pub(crate) struct EventStream {
    id: u64,
    router: Arc<EventRouter>,
}

impl EventStream {
    /// Returns a future that resolves to the next event.
    #[inline(always)]
    pub fn next(&mut self) -> NextEvent<Event> {
        NextEvent(self, PhantomData)
    }

    /// Returns a future that resolves when the next event is ready. This is
    /// useful with [`tokio::select`] when it's not possible to use `next()` due
    /// to `Send` constraints.
    #[allow(dead_code)] // TODO: Remove
    #[inline(always)]
    pub fn ready(&mut self) -> NextEvent<()> {
        NextEvent(self, PhantomData)
    }

    /// Tries to get the next event without blocking.
    #[allow(dead_code)] // TODO: Remove
    #[inline(always)]
    pub fn get(&mut self) -> Option<Result<Event>> {
        match self.poll(None) {
            Poll::Ready(r) => Some(r),
            Poll::Pending => None,
        }
    }

    /// Calls `f` to update connection parameters. This is a no-op if the handle
    /// is invalid.
    #[inline]
    pub fn update_conn(&self, hdl: ConnHandle, f: impl FnOnce(&mut Conn)) {
        self.router.update_conn(hdl, f);
    }

    /// Polls for the next event.
    pub(super) fn poll(&mut self, cx: Option<&mut Context<'_>>) -> Poll<Result<Event>> {
        let mut m = self.router.monitor.lock();
        if let Some(e) = m.err {
            return Poll::Ready(Err(Error::Host(e)));
        }
        let r = m.get(self.id);
        if let Some(xfer) = r.ready.take() {
            return Poll::Ready(Ok(Event(xfer, PhantomData)));
        }
        if let Some(cx) = cx {
            r.waker = Some(cx.waker().clone());
        }
        Poll::Pending
    }

    /// Polls for whether the next event is ready.
    pub(super) fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<()> {
        let mut m = self.router.monitor.lock();
        if m.err.is_some() {
            return Poll::Ready(());
        }
        let mut r = m.get(self.id);
        if r.ready.is_some() {
            return Poll::Ready(());
        }
        r.waker = Some(cx.waker().clone());
        Poll::Pending
    }
}

impl Drop for EventStream {
    fn drop(&mut self) {
        let mut m = self.router.monitor.lock();
        if let Some(i) = m.queue.iter().position(|r| r.id == self.id) {
            // Order doesn't matter since we don't control the order in which
            // async tasks are executed.
            let Some(r) = m.queue.swap_remove_back(i) else { unreachable!() };
            if r.opcode.is_some() {
                m.wake_cmds();
            }
        }
    }
}

/// Next event future. This future is cancel safe.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct NextEvent<'a, T>(&'a mut EventStream, PhantomData<T>);

// SAFETY: We don't actually hold T
unsafe impl<T> Send for NextEvent<'_, T> {}

impl Future for NextEvent<'_, Event> {
    type Output = Result<Event>;

    #[inline(always)]
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.0.poll(Some(cx))
    }
}

impl Future for NextEvent<'_, ()> {
    type Output = ();

    #[inline(always)]
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.0.poll_ready(cx)
    }
}

impl<T> Drop for NextEvent<'_, T> {
    fn drop(&mut self) {
        let mut m = self.0.router.monitor.lock();
        if let Some(r) = m.queue.iter_mut().find(|r| r.id == self.0.id) {
            r.waker = None;
        }
    }
}

/// A read-only event reference.
type EventRef = tokio::sync::OwnedRwLockReadGuard<EventTransfer>;

/// HCI event transfer.
#[derive(Debug, Default)]
struct EventTransfer {
    xfer: Option<Box<dyn host::Transfer>>,
    hdr: EventHeader,
    params_off: usize,
}

impl EventTransfer {
    /// Receives the next event.
    async fn next(&mut self, t: &dyn host::Transport) -> host::Result<()> {
        self.params_off = 0;
        let xfer = self.xfer.take().map_or_else(
            || t.event(),
            |mut xfer| {
                xfer.reset();
                xfer
            },
        );
        self.xfer = Some(xfer.submit()?.await?);
        Ok(())
    }

    /// Returns the event parameters after the header or an invalid [`Unpacker`]
    /// if there is no valid event.
    #[inline]
    fn params(&self) -> Unpacker {
        if self.params_off < EVT_HDR {
            return Unpacker::invalid();
        }
        // SAFETY: `Event::new` ensured that we have a valid event with
        // `EVT_HDR <= self.params_off <= xfer.len()`
        Unpacker::new(unsafe {
            let xfer = self.xfer.as_deref().unwrap_unchecked();
            xfer.as_ref().get_unchecked(self.params_off..)
        })
    }
}
