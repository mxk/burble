use std::collections::VecDeque;
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

use crate::{host, le::RawAddr};

use super::*;

mod hci;
mod le;

#[cfg(test)]
mod tests;

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
        matches!(
            self,
            Self::Hci(EventCode::CommandComplete | EventCode::CommandStatus)
        )
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

/// Common event header. [`EventType`] determines the validity of other fields.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct EventHeader {
    typ: EventType,
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
        let (code, len) = (p.u8(), p.u8());
        if p.len() != usize::from(len) || !p.is_ok() {
            return Err(Error::InvalidEvent(Vec::from(raw)));
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
        let mut hdr = Self {
            typ,
            ..Self::default()
        };
        match typ {
            EventType::Hci(EventCode::CommandComplete) => {
                hdr.cmd_quota = p.u8();
                hdr.opcode = Opcode::from(p.u16());
                if !p.is_empty() {
                    hdr.status = Status::from(p.u8());
                }
            }
            EventType::Hci(EventCode::CommandStatus) => {
                hdr.status = Status::from(p.u8());
                hdr.cmd_quota = p.u8();
                hdr.opcode = Opcode::from(p.u16());
            }
            _ => {
                let pf = typ.param_fmt();
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

/// HCI event handle. Provides access to common event metadata and methods for
/// unpacking event parameters. The next event cannot be received until all
/// existing event handles are dropped.
///
/// It is `!Send` to prevent it from being held across `await` points.
#[derive(Debug)]
#[must_use]
#[repr(transparent)]
pub struct Event(
    tokio::sync::OwnedRwLockReadGuard<EventTransfer>,
    PhantomData<*const ()>, // !Send
);

impl Event {
    /// Creates a new event handle after unpacking and validating the event
    /// header.
    #[inline]
    fn new(mut t: tokio::sync::OwnedRwLockWriteGuard<EventTransfer>) -> Result<Self> {
        let raw = t.xfer.as_deref().expect("invalid event").as_ref();
        let (hdr, params) = EventHeader::unpack(raw)?;
        trace!("{hdr:?} {params:02X?}");
        (t.hdr, t.params_off) = (hdr, raw.len() - params.len());
        Ok(Self(t.downgrade(), PhantomData))
    }

    /// Returns the event type.
    #[inline(always)]
    #[must_use]
    pub fn typ(&self) -> EventType {
        self.0.hdr.typ
    }

    /// Returns the event status or [`Status::Success`] for events without a
    /// status parameter.
    #[inline(always)]
    #[must_use]
    pub fn status(&self) -> Status {
        self.0.hdr.status
    }

    /// Returns the opcode from `CommandComplete` or `CommandStatus` events.
    /// [`Opcode::None`] is returned for either event that only updates the
    /// command quota and for non-command events.
    #[inline(always)]
    #[must_use]
    pub fn opcode(&self) -> Opcode {
        self.0.hdr.opcode
    }

    /// Returns the associated advertising handle or `None` for non-advertising
    /// events.
    #[inline]
    #[must_use]
    pub fn adv_handle(&self) -> Option<AdvHandle> {
        #[allow(clippy::cast_possible_truncation)]
        if self.typ().param_fmt().contains(EventFmt::ADV_HANDLE) {
            AdvHandle::new(self.0.hdr.handle as u8)
        } else {
            None
        }
    }

    /// Returns the associated connection handle or `None` for non-connection
    /// events.
    #[inline]
    #[must_use]
    pub fn conn_handle(&self) -> Option<ConnHandle> {
        if self.typ().param_fmt().contains(EventFmt::CONN_HANDLE) {
            ConnHandle::new(self.0.hdr.handle)
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
        debug_assert!(T::matches(self), "event type mismatch for {}", self.typ());
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
    /// Panics for non-command events or if `T` is not a representation of the
    /// event.
    #[inline]
    pub(crate) fn map_ok<T>(&self, f: impl FnOnce(&Self, &mut Unpacker) -> T) -> Result<T> {
        self.cmd_ok().map(|_| self.unpack(f))
    }

    /// Ensures that the event represent successful command status or
    /// completion.
    pub(super) fn cmd_ok(&self) -> Result<()> {
        assert!(self.typ().is_cmd(), "non-command {} event", self.typ());
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
    ///
    /// # Panics
    ///
    /// Panics if `f` does not consume all event parameters or reads past the
    /// end.
    #[inline]
    #[must_use]
    fn unpack<T>(&self, f: impl FnOnce(&Self, &mut Unpacker) -> T) -> T {
        let mut p = self.0.params();
        let v = f(self, &mut p);
        debug_assert!(p.is_ok(), "corrupt {} event", self.typ());
        debug_assert!(p.is_empty(), "unconsumed {} event", self.typ());
        v
    }
}

/// Trait for unpacking event parameters.
pub(crate) trait FromEvent {
    /// Returns whether event `e` can be unpacked by this type.
    #[inline(always)]
    #[must_use]
    fn matches(e: &Event) -> bool {
        let _ = e;
        true // Default to true for command completions and `()`
    }

    /// Unpacks event parameters.
    #[must_use]
    fn unpack(e: &Event, p: &mut Unpacker) -> Self;
}

/// Unpacker for events without parameters.
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

/// Received event router. When an event is received, all registered waiters
/// with matching filters are notified in a broadcast fashion, and must process
/// the event asynchronously before the next receive operation can happen.
#[derive(Debug)]
pub(super) struct EventRouter {
    waiters: parking_lot::Mutex<Waiters>,
    xfer: tokio::sync::Mutex<Arc<tokio::sync::RwLock<EventTransfer>>>,
}

impl EventRouter {
    /// Creates a new event router.
    #[inline]
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            waiters: parking_lot::Mutex::default(),
            xfer: tokio::sync::Mutex::new(Arc::new(tokio::sync::RwLock::with_max_readers(
                EventTransfer::default(),
                64,
            ))),
        })
    }

    // TODO: Use event masks for filtering. These should also determine which
    // events are unmasked on the controller.

    /// Returns an event receiver. Commands must specify their opcode.
    pub fn events(self: &Arc<Self>, opcode: Opcode) -> Result<EventStream> {
        let mut ws = self.waiters.lock();
        if opcode.is_some() {
            if ws.queue.iter().any(|w| w.opcode == opcode) {
                return Err(Error::DuplicateCommands { opcode });
            }
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
        ws.next_id += 1;
        ws.queue.push_back(Waiter {
            id,
            opcode,
            ..Waiter::default()
        });
        Ok(EventStream {
            router: Arc::clone(self),
            id,
        })
    }

    /// Receives the next event, notifies registered waiters, and returns the
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
        // the caller can't deadlock, but it's possible that another Waiter with
        // an event is not being polled. The timeout will detect that.
        let mut xfer = timeout(Duration::from_secs(3), Arc::clone(&rwlock).write_owned())
            .await
            .expect("EventRouter stalled (Event not handled)");
        xfer.next(t).await.map_err(|e| {
            // Fatal transport error
            let mut ws = self.waiters.lock();
            ws.err = Some(e);
            for w in &mut ws.queue {
                if let Some(waker) = w.waker.take() {
                    waker.wake();
                }
            }
            e
        })?;
        let evt = Event::new(xfer)?;
        let hdr = &evt.0.hdr;

        // TODO: One second command timeout ([Vol 4] Part E, Section 4.4)

        // Provide Events to registered waiters and notify them
        let mut ws = self.waiters.lock();
        if hdr.typ.is_cmd() {
            ws.cmd_quota = hdr.cmd_quota;
            if hdr.opcode.is_some() {
                (ws.queue.iter_mut().find(|w| w.opcode == hdr.opcode)).map_or_else(
                    || warn!("Ignored {} command completion", hdr.opcode),
                    |w| w.ready(&rwlock),
                );
            }
        } else {
            let mut received = false;
            for w in ws.queue.iter_mut().filter(|w| w.opcode.is_none()) {
                w.ready(&rwlock);
                received = true;
            }
            if !received {
                trace!("Ignored event: {evt:?}");
            }
        }
        Ok(evt)
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
#[derive(Debug, Default)]
struct Waiter {
    id: u64,
    opcode: Opcode,
    ready: Option<tokio::sync::OwnedRwLockReadGuard<EventTransfer>>,
    waker: Option<Waker>,
}

impl Waiter {
    /// Provides a new event to the waiter and notifies the receiver.
    ///
    /// # Panics
    ///
    /// Panics if `try_read_owned()` fails. The caller can prevent this by
    /// holding another read lock and ensuring that there are no writers
    /// waiting.
    #[inline]
    fn ready(&mut self, xfer: &Arc<tokio::sync::RwLock<EventTransfer>>) {
        self.ready = Some(Arc::clone(xfer).try_read_owned().unwrap());
        if let Some(w) = self.waker.take() {
            w.wake();
        }
    }
}

/// HCI event stream. The owner is guaranteed to receive all relevant events
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
}

/// Next event future. This future is cancel safe.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct NextEvent<'a>(&'a mut EventStream);

impl Future for NextEvent<'_> {
    type Output = Result<Event>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut ws = self.0.router.waiters.lock();
        if let Some(e) = ws.err {
            return Poll::Ready(Err(Error::Host(e)));
        }
        let w = (ws.queue.iter_mut())
            .find(|w| w.id == self.0.id)
            .expect("unregistered event waiter");
        if let Some(xfer) = w.ready.take() {
            Poll::Ready(Ok(Event(xfer, PhantomData)))
        } else {
            w.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

impl Drop for EventStream {
    fn drop(&mut self) {
        let mut ws = self.router.waiters.lock();
        if let Some(i) = ws.queue.iter().position(|w| w.id == self.id) {
            // Order doesn't matter since we don't control the order in which
            // async tasks are executed.
            ws.queue.swap_remove_back(i);
        }
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
        self.hdr = EventHeader::default();
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
        // SAFETY: We have a valid Event and
        // `EVT_HDR <= self.params_off <= xfer.len()`
        Unpacker::new(unsafe {
            let xfer = self.xfer.as_deref().unwrap_unchecked();
            xfer.as_ref().get_unchecked(self.params_off..)
        })
    }
}
