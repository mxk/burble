use std::collections::VecDeque;
use std::sync::Arc;

use bytes::Buf;
use parking_lot::Mutex;
use tracing::trace;

use crate::host;

use super::*;

/// HCI event header and buffer sizes ([Vol 4] Part E, Section 5.4.4).
const EVT_HDR: usize = 2;
pub(crate) const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

/// HCI event decoder.
#[derive(Clone, Debug)]
pub struct Event<'a> {
    #[allow(dead_code)] // TODO: Remove
    typ: EventType,
    cmd_status: Option<CommandStatus>,
    tail: &'a [u8],
}

impl Event<'_> {
    /// Returns the event type.
    #[inline]
    #[cfg(test)]
    pub fn typ(&self) -> EventType {
        self.typ
    }

    /// Returns any unparsed bytes.
    #[inline]
    #[cfg(test)]
    pub fn tail(&self) -> &[u8] {
        self.tail
    }

    /// Returns basic information from `CommandComplete` or `CommandStatus`
    /// events. Returns [`None`] for other event types.
    #[inline]
    pub fn cmd_status(&self) -> Option<CommandStatus> {
        self.cmd_status
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
        let cmd_status = match typ {
            EventType::Hci(EventCode::CommandComplete) => Some(CommandStatus {
                quota: CommandQuota(tail.get_u8()),
                opcode: Opcode::from(tail.get_u16_le()),
                status: if !tail.is_empty() {
                    Status::from(tail.get_u8())
                } else {
                    Status::Success
                },
            }),
            EventType::Hci(EventCode::CommandStatus) => Some(CommandStatus {
                status: Status::from(tail.get_u8()),
                quota: CommandQuota(tail.get_u8()),
                opcode: Opcode::from(tail.get_u16_le()),
            }),
            _ => None,
        };
        Ok(Self {
            typ,
            cmd_status,
            tail,
        })
    }
}

/// HCI event or LE subevent code.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EventType {
    Hci(EventCode),
    Le(SubeventCode),
}

/// Basic information from `CommandComplete` or `CommandStatus` events.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CommandStatus {
    pub quota: CommandQuota,
    pub opcode: Opcode,
    pub status: Status,
}

type SharedTransfer<T> = Arc<tokio::sync::RwLock<EventTransfer<T>>>;

/// Received event router. When an event is received, all registered waiters
/// with matching filters are notified in a broadcast fashion, and must process
/// the event asynchronously before the next receive operation can happen.
#[derive(Debug)]
pub(super) struct EventRouter<T: host::Transport> {
    waiters: Mutex<Waiters<T>>,
    xfer: tokio::sync::Mutex<SharedTransfer<T>>,

    // Notification should ideally be limited to each waiter, but we need
    // condvar-like functionality where we register for notification while
    // holding the waiters lock, drop the lock, and then await. That doesn't
    // work if we're referencing a synchronization primitive protected by the
    // lock, so we keep this separate.
    notify: tokio::sync::Notify,
}

impl<T: host::Transport> EventRouter<T> {
    /// Returns a new event router using transport `t`.
    pub fn new(t: Arc<T>) -> Arc<Self> {
        Arc::new(Self {
            waiters: Mutex::new(Waiters {
                queue: VecDeque::with_capacity(4),
                next_id: 0,
            }),
            xfer: tokio::sync::Mutex::new(EventTransfer::new(t)),
            notify: tokio::sync::Notify::new(),
        })
    }

    /// Register a new event waiter with filter `f`.
    pub fn register(self: Arc<Self>, f: EventFilter) -> Result<EventWaiterGuard<T>> {
        let mut ws = self.waiters.lock();
        if ws.queue.iter().any(|w| f.conflicts_with(&w.filter)) {
            return Err(Error::FilterConflict);
        }
        let id = ws.next_id;
        ws.next_id = ws.next_id.checked_add(1).unwrap();
        ws.queue.push_back(Waiter {
            id,
            filter: f,
            ready: None,
        });
        drop(ws);
        Ok(EventWaiterGuard { id, router: self })
    }

    /// Receives the next event, notifies registered waiters, and returns the
    /// event to the caller.
    pub async fn recv_event(&self) -> Result<EventGuard<T>> {
        // This mutex ensures there are no calls to RwLock::write_owned() after
        // an event is received and RwLock::try_read_owned() is being called.
        // Since Tokio's RwLock is write-preferring, acquiring read locks would
        // fail if there is a writer waiting.
        let xfer = self.xfer.lock().await;

        // Wait for all read locks to be released and receive the next event
        let mut recv = xfer.clone().write_owned().await;
        recv.next().await?;

        // TODO: Remove and notify receivers for certain fatal errors, like lost
        // device.

        // Lock router before downgrading to a read lock
        let mut ws = self.waiters.lock();
        let evt = EventGuard(recv.downgrade());

        // Provide EventGuards to registered waiters and notify them
        let mut notify = false;
        evt.map(|evt| {
            for w in ws.queue.iter_mut().filter(|w| w.filter.matches(&evt)) {
                // try_read_owned() is guaranteed to succeed since we are
                // holding our own read lock and there are no writers waiting.
                w.ready = Some(EventGuard(xfer.clone().try_read_owned().unwrap()));
                notify = true;
            }
        });
        if notify {
            self.notify.notify_waiters();
        }
        drop(ws);
        Ok(evt)
    }
}

/// Defines event matching criteria.
#[derive(Clone, Debug)]
pub(crate) enum EventFilter {
    #[allow(dead_code)] // TODO: Remove
    Any,
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
        match self {
            Any => true,
            &Command(opcode) => matches!(evt.cmd_status, Some(st) if st.opcode == opcode),
        }
    }
}

/// Queue of event waiters. VecDeque is used because there are likely to be
/// only a few waiters with frequent insertion and removal for commands, so it
/// is likely to outperform HashMap and BTreeMap.
#[derive(Debug)]
struct Waiters<T: host::Transport> {
    queue: VecDeque<Waiter<T>>,
    next_id: u64,
}

/// Registered event waiter.
#[derive(Debug)]
struct Waiter<T: host::Transport> {
    id: u64,
    filter: EventFilter,
    ready: Option<EventGuard<T>>,
}

/// Guard that unregisters the event waiter when dropped.
#[derive(Debug)]
pub(super) struct EventWaiterGuard<T: host::Transport> {
    id: u64,
    router: Arc<EventRouter<T>>,
}

impl<T: host::Transport> EventWaiterGuard<T> {
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
                Ok(ready) => return Some(ready),
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
/// received until all existing guards are dropped.
#[derive(Debug)]
pub struct EventGuard<T: host::Transport>(tokio::sync::OwnedRwLockReadGuard<EventTransfer<T>>);

impl<T: host::Transport> EventGuard<T> {
    /// Calls `f` on the received event.
    #[inline]
    pub fn map<R>(&self, f: impl FnOnce(Event) -> R) -> R {
        let xfer = self.0.xfer.as_ref().unwrap();
        xfer.map(|_, b| f(Event::try_from(b).unwrap()))
    }

    /// Calls `f` on the received event if the event represents successful
    /// command completion.
    pub fn map_ok<R>(&mut self, f: impl FnOnce(Event) -> R) -> Result<R> {
        self.map(|evt| match evt.cmd_status() {
            Some(CommandStatus {
                status: Status::Success,
                ..
            }) => Ok(f(evt)),
            Some(st) => Err(Error::from(st)),
            // This shouldn't happen since our filter should only match command
            // completion or status events.
            None => Err(Error::from(Status::UnsupportedFeatureOrParameterValue)),
        })
    }
}

/// HCI event transfer.
#[derive(Debug)]
struct EventTransfer<T: host::Transport> {
    t: Arc<T>,
    xfer: Option<T::Transfer>,
    restart: bool,
}

impl<T: host::Transport> EventTransfer<T> {
    fn new(t: Arc<T>) -> SharedTransfer<T> {
        Arc::new(tokio::sync::RwLock::new(EventTransfer {
            t,
            xfer: None,
            restart: true,
        }))
    }

    /// Receives and validates the next event.
    async fn next(&mut self) -> Result<()> {
        if self.restart {
            self.xfer = Some(self.t.evt()?);
            self.restart = false;
        }
        let xfer = self.xfer.as_mut().unwrap();
        let r = xfer.result().await;
        self.restart = true;
        r?;
        let b = xfer.buf();
        let evt = Event::try_from(b.as_ref());
        trace!("{evt:?}");
        evt.map(|_evt| ())
    }
}
