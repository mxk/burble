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

use crate::{host, le::RawAddr, AsyncMutex, AsyncRwLock, SyncArcMutexGuard, SyncMutex};

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
    tokio::sync::OwnedRwLockReadGuard<EventTransfer>,
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
/// receivers with matching filters are notified in a broadcast fashion, and
/// must process the event asynchronously before the next event can be received.
#[derive(Debug)]
pub(super) struct EventRouter {
    recv: SyncMutex<Receivers>,
    xfer: AsyncMutex<Arc<AsyncRwLock<EventTransfer>>>,
}

impl EventRouter {
    /// Creates a new event router.
    #[inline]
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            recv: SyncMutex::default(),
            xfer: AsyncMutex::new(Arc::new(AsyncRwLock::with_max_readers(
                EventTransfer::default(),
                64,
            ))),
        })
    }

    // TODO: Use event masks for filtering. These should also determine which
    // events are unmasked on the controller.

    /// Returns an event receiver. Commands must specify their opcode.
    pub fn events(self: &Arc<Self>, opcode: Opcode) -> Result<EventStream> {
        let mut rs = self.recv.lock();
        if opcode.is_some() {
            if rs.queue.iter().any(|r| r.opcode == opcode) {
                // The spec doesn't specify whether commands with the same
                // opcode can be issued concurrently, but it's safer to avoid.
                return Err(Error::DuplicateCommands { opcode });
            }
            if rs.cmd_quota == 0 {
                return Err(Error::CommandQuotaExceeded);
            }
            if opcode == Opcode::Reset {
                rs.cmd_quota = 0; // [Vol 4] Part E, Section 7.3.2
            } else {
                rs.cmd_quota -= 1;
            }
        }
        let id = rs.next_id;
        rs.next_id += 1;
        rs.queue.push_back(Receiver {
            id,
            opcode,
            ..Receiver::default()
        });
        Ok(EventStream {
            router: Arc::clone(self),
            id,
        })
    }

    /// Returns connection information for the specified handle or [`None`] if
    /// the handle is invalid.
    #[inline]
    pub fn conn(&self, hdl: ConnHandle) -> Option<SyncArcMutexGuard<Conn>> {
        self.recv.lock().conns.get(&hdl).map(SyncMutex::lock_arc)
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
            let mut rs = self.recv.lock();
            rs.err = Some(e);
            for r in &mut rs.queue {
                if let Some(w) = r.waker.take() {
                    w.wake();
                }
            }
            e
        })?;

        // TODO: One second command timeout ([Vol 4] Part E, Section 4.4)
        let evt = Event::new(xfer)?;
        self.recv.lock().notify(&rwlock, &evt);
        Ok(evt)
    }
}

/// Receiver queue and internal router state. `VecDeque` is used because there
/// are likely to be only a few receivers with frequent insertions/removals for
/// commands, so it is likely to outperform `HashMap` and `BTreeMap`.
#[derive(Debug)]
struct Receivers {
    conns: BTreeMap<ConnHandle, ArcConnInfo>,
    queue: VecDeque<Receiver>,
    err: Option<host::Error>,
    next_id: u64,
    cmd_quota: u8,
    _cmd_wakers: Vec<Waker>, // TODO: Implement
}

impl Receivers {
    // Notifies registered receives of a new event.
    fn notify(&mut self, xfer: &Arc<AsyncRwLock<EventTransfer>>, evt: &Event) {
        let hdr = &evt.0.hdr;
        if hdr.code.is_cmd() {
            self.cmd_quota = hdr.cmd_quota;
            if hdr.opcode.is_some() {
                (self.queue.iter_mut().find(|r| r.opcode == hdr.opcode)).map_or_else(
                    || warn!("Ignored {} command completion", hdr.opcode),
                    |r| r.ready(xfer),
                );
            }
            return;
        }
        if hdr.status.is_ok() {
            // Update global connection information
            match hdr.code {
                EventCode::LeConnectionComplete | EventCode::LeEnhancedConnectionComplete => {
                    let e: LeConnectionComplete = evt.get();
                    let cn = Arc::new(SyncMutex::new(Conn::new(&e)));
                    let old = self.conns.insert(e.handle, cn);
                    assert!(old.is_none(), "duplicate connection handle");
                }
                EventCode::DisconnectionComplete => {
                    // TODO: Store reason?
                    self.conns.remove(&ConnHandle::new(hdr.handle).unwrap());
                }
                _ => {}
            }
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
}

impl Default for Receivers {
    #[inline]
    fn default() -> Self {
        Self {
            conns: BTreeMap::new(),
            queue: VecDeque::with_capacity(8),
            err: None,
            next_id: 0,
            cmd_quota: 1, // [Vol 4] Part E, Section 4.4
            _cmd_wakers: Vec::new(),
        }
    }
}

/// Registered event receiver.
#[derive(Debug, Default)]
struct Receiver {
    id: u64,
    opcode: Opcode,
    ready: Option<tokio::sync::OwnedRwLockReadGuard<EventTransfer>>,
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
        self.ready = Some(Arc::clone(xfer).try_read_owned().unwrap());
        if let Some(w) = self.waker.take() {
            w.wake();
        }
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
    pub fn next(&mut self) -> NextEvent {
        NextEvent(self)
    }

    /// Returns connection information for the specified handle or [`None`] if
    /// the handle is invalid.
    #[inline(always)]
    pub fn conn(&self, hdl: ConnHandle) -> Option<SyncArcMutexGuard<Conn>> {
        self.router.conn(hdl)
    }

    /// Polls for the next event.
    pub fn poll(&mut self, cx: &mut Context<'_>) -> Poll<<NextEvent as Future>::Output> {
        let mut rs = self.router.recv.lock();
        if let Some(e) = rs.err {
            return Poll::Ready(Err(Error::Host(e)));
        }
        let r = (rs.queue.iter_mut().find(|r| r.id == self.id)).expect("unregistered event stream");
        if let Some(xfer) = r.ready.take() {
            Poll::Ready(Ok(Event(xfer, PhantomData)))
        } else {
            r.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

impl Drop for EventStream {
    fn drop(&mut self) {
        let mut rs = self.router.recv.lock();
        if let Some(i) = rs.queue.iter().position(|r| r.id == self.id) {
            // Order doesn't matter since we don't control the order in which
            // async tasks are executed.
            rs.queue.swap_remove_back(i);
        }
    }
}

/// Next event future. This future is cancel safe.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct NextEvent<'a>(&'a mut EventStream);

impl Future for NextEvent<'_> {
    type Output = Result<Event>;

    #[inline(always)]
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.0.poll(cx)
    }
}

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
