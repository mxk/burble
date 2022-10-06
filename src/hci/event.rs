use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use bytes::Buf;
use tracing::trace;

use crate::host;

use super::*;

/// HCI event header and buffer sizes ([Vol 4] Part E, Section 5.4.4).
const EVT_HDR: usize = 2;
pub(crate) const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

/// HCI event decoder.
#[derive(Clone, Debug)]
pub struct Event<'a> {
    typ: EventType,
    cmd_status: Option<CmdStatus>,
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
    pub fn cmd_status(&self) -> Option<CmdStatus> {
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
            EventType::Hci(EventCode::CommandComplete) => Some(CmdStatus {
                quota: CmdQuota(tail.get_u8()),
                opcode: Opcode(tail.get_u16_le()),
                status: if !tail.is_empty() {
                    Status::from(tail.get_u8())
                } else {
                    Status::Success
                },
            }),
            EventType::Hci(EventCode::CommandStatus) => Some(CmdStatus {
                status: Status::from(tail.get_u8()),
                quota: CmdQuota(tail.get_u8()),
                opcode: Opcode(tail.get_u16_le()),
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

/// Basic information contained in a `CommandComplete` or `CommandStatus`
/// events.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct CmdStatus {
    pub quota: CmdQuota,
    pub opcode: Opcode,
    pub status: Status,
}

type LockedTransfer<T> = Arc<tokio::sync::RwLock<EventTransfer<T>>>;

/// Routes received events to registered waiters. When an event is received,
/// all registered waiters with filters that match the event are notified and
/// must process the event asynchronously before the next receive operation can
/// happen. This design is similar in concept to [`tokio::sync::broadcast`].
#[derive(Debug)]
pub(super) struct EventRouter<T: host::Transport> {
    waiters: Mutex<Waiters<T>>,
    xfer: LockedTransfer<T>,
    notify: tokio::sync::Notify,
}

#[derive(Debug)]
struct Waiters<T: host::Transport> {
    map: HashMap<u64, Waiter<T>>,
    next_key: u64,
}

impl<T: host::Transport> EventRouter<T> {
    /// Returns a new event router
    pub fn new(t: Arc<T>) -> Arc<Self> {
        let waiters = Mutex::new(Waiters {
            map: HashMap::new(),
            next_key: 0,
        });
        Arc::new(Self {
            waiters,
            xfer: Arc::new(tokio::sync::RwLock::new(EventTransfer::new(t))),
            notify: tokio::sync::Notify::new(),
        })
    }

    /// Register a new event waiter with filter `f`.
    pub fn register(self: Arc<Self>, f: Filter) -> EventWaiterGuard<T> {
        let mut r = self.waiters.lock().unwrap();
        let key = r.next_key;
        r.next_key = r.next_key.checked_add(1).unwrap();
        let w = Waiter {
            filter: f,
            ready: None,
        };
        r.map.insert(key, w);
        drop(r);
        EventWaiterGuard { key, r: self }
    }

    /// Receives HID events and routes them to registered waiters.
    pub async fn recv_event(&self) -> Result<EventGuard<T>> {
        // Wait for all read locks to be released and receive the next event
        let mut xfer = self.xfer.clone().write_owned().await;
        xfer.next().await?;

        // Lock router before downgrading to a read lock
        let mut waiters = self.waiters.lock().unwrap();
        let g = EventGuard(xfer.downgrade());
        let mut notify = false;

        // Provide EventGuards to registered waiters
        g.map(|evt| {
            for w in waiters.map.values_mut().filter(|w| w.filter.matches(&evt)) {
                // TODO: try_read_owned may fail if another recv_event call is
                // waiting for the write lock.
                w.ready = Some(EventGuard::from(self.xfer.clone()));
                notify = true;
            }
        });

        // Wake all waiters, if any
        if notify {
            self.notify.notify_waiters();
        }
        Ok(g)
    }
}

/// Registered event waiter.
#[derive(Debug)]
struct Waiter<T: host::Transport> {
    filter: Filter,
    ready: Option<EventGuard<T>>,
}

/// Guard that unregisters the event waiter when dropped.
#[derive(Debug)]
pub(super) struct EventWaiterGuard<T: host::Transport> {
    key: u64,
    r: Arc<EventRouter<T>>,
}

impl<T: host::Transport> EventWaiterGuard<T> {
    /// Calls `f` on the next matching event.
    pub async fn map<R>(&mut self, f: impl FnOnce(Event) -> R) -> R {
        loop {
            let ready_or_wait = {
                let mut r = self.r.waiters.lock().unwrap();
                let w = r.map.get_mut(&self.key).unwrap();
                w.ready.take().ok_or_else(|| self.r.notify.notified())
            };
            match ready_or_wait {
                Ok(ready) => return ready.map(f),
                Err(notify) => notify.await,
            };
        }
    }

    /// Calls `f` on the next matching event.
    pub async fn map_ok<R>(&mut self, f: impl FnOnce(Event) -> R) -> Result<R> {
        self.map(|evt| match evt.cmd_status() {
            Some(CmdStatus {
                status: Status::Success,
                ..
            }) => Ok(f(evt)),
            Some(st) => Err(Error::from(st)),
            // TODO: Not a command error, though this shouldn't happen since
            // only command completion or status should match our filter
            None => Err(Error::from(Status::InvalidCommandParameters)),
        })
        .await
    }
}

impl<T: host::Transport> Drop for EventWaiterGuard<T> {
    fn drop(&mut self) {
        let mut s = self.r.waiters.lock().unwrap();
        s.map.remove(&self.key);
    }
}

/// Received event guard. The next event cannot be received until all existing
/// guards are dropped.
#[derive(Debug)]
pub struct EventGuard<T: host::Transport>(tokio::sync::OwnedRwLockReadGuard<EventTransfer<T>>);

impl<T: host::Transport> EventGuard<T> {
    fn map<R>(&self, f: impl FnOnce(Event) -> R) -> R {
        self.0
            .xfer
            .as_ref()
            .unwrap()
            // TODO: Avoid repeat validation?
            .map(|_, b| f(Event::try_from(b).unwrap_or_else(|_| unreachable!())))
    }
}

impl<T: host::Transport> From<Arc<tokio::sync::RwLock<EventTransfer<T>>>> for EventGuard<T> {
    fn from(et: Arc<tokio::sync::RwLock<EventTransfer<T>>>) -> Self {
        Self(et.try_read_owned().unwrap())
    }
}

/// Event receive transfer.
#[derive(Debug)]
struct EventTransfer<T: host::Transport> {
    t: Arc<T>,
    xfer: Option<T::Transfer>,
    ready: bool,
}

impl<T: host::Transport> EventTransfer<T> {
    fn new(t: Arc<T>) -> Self {
        EventTransfer {
            t,
            xfer: None,
            ready: false,
        }
    }

    async fn next(&mut self) -> Result<()> {
        if self.ready || self.xfer.is_none() {
            self.ready = false;
            self.xfer = Some(self.t.evt()?);
        }
        let xfer = self.xfer.as_mut().unwrap();
        xfer.result().await?;
        let b = xfer.buf();
        trace!("Event: {:02x?}", b.as_ref());
        Event::try_from(b.as_ref())?;
        self.ready = true;
        Ok(())
    }
}

/// Defines event matching criteria.
#[derive(Debug)]
pub(crate) enum Filter {
    Command(Opcode),
}

impl Filter {
    fn matches(&self, evt: &Event<'_>) -> bool {
        match self {
            &Self::Command(opcode) => matches!(evt.cmd_status, Some(st) if st.opcode == opcode),
        }
    }
}
