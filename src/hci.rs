//! Host Controller Interface.

use std::fmt::{Display, Formatter};
use std::io::Cursor;
use std::sync::Arc;
use std::{slice, thread};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use tokio::sync::mpsc;
use tracing::{debug, trace};

pub use {codes::*, info::*};

use crate::host;

mod codes;
mod info;

#[cfg(test)]
mod tests;

/// Error type returned by the HCI layer.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Host(#[from] host::Error),
    #[error("HCI error: {status}")]
    Hci {
        #[from]
        status: Status,
        // TODO: Add backtrace once stabilized
    },
    #[error("invalid event: {0:?}")]
    InvalidEvent(Bytes),
    #[error("unknown event [code={code:02x}, subevent={subevent:02x}]: {params:?}")]
    UnknownEvent {
        code: u8,
        subevent: u8,
        params: Bytes,
    },
    #[error("command error [opcode={opcode}, status={status}]")]
    Command { opcode: Opcode, status: Status },
}

impl From<CmdStatus> for Error {
    fn from(s: CmdStatus) -> Self {
        Error::Command {
            opcode: s.opcode,
            status: s.status,
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

/// Host-side of a Host Controller Interface.
#[derive(Debug)]
pub struct Host<T> {
    t: Arc<T>,
    evt_ch: Option<mpsc::Receiver<Result<Evt>>>,
    evt_thr: Option<thread::JoinHandle<()>>,
}

impl<T: host::Transport> Host<T> {
    /// Returns an HCI using transport layer `t`.
    pub fn new(t: T) -> Self {
        let (tx, rx) = mpsc::channel(1);
        let t = Arc::new(t);
        let evt_hdl = {
            let t = t.clone();
            thread::spawn(move || Self::event_thread(t, tx))
        };
        Self {
            t,
            evt_ch: Some(rx),
            evt_thr: Some(evt_hdl),
        }
    }

    async fn next_event(&mut self) -> Result<Evt> {
        match self.evt_ch.as_mut().unwrap().recv().await {
            Some(r) => r,
            None => Err(Error::Hci {
                status: Status::UnspecifiedError, // TODO: Closed error
            }),
        }
    }

    fn event_thread(t: Arc<T>, tx: mpsc::Sender<Result<Evt>>) {
        debug!("HCI event thread started");
        let mut evt = Some(Evt::new());
        loop {
            if evt.is_none() {
                evt = Some(Evt::new());
            }
            let buf = evt.as_mut().unwrap();
            let r = tx.blocking_send(match t.read_event(buf.reset()) {
                Ok(n) => match buf.ready(n) {
                    Ok(()) => Ok(evt.take().unwrap()),
                    Err(e) => Err(e),
                },
                Err(e) => {
                    if e.is_timeout() {
                        if tx.is_closed() {
                            break;
                        }
                        continue;
                    }
                    Err(Error::from(e))
                }
            });
            if r.is_err() {
                break;
            }
        }
        debug!("HCI event thread terminating");
    }
}

impl<T> Drop for Host<T> {
    fn drop(&mut self) {
        drop(self.evt_ch.take());
        if let Some(h) = self.evt_thr.take() {
            let _ = h.join();
        }
    }
}

/// HCI command encoder.
#[derive(Debug)]
struct Cmd {
    opcode: Opcode,
    b: BytesMut,
}

const CMD_HDR: usize = 3;

impl Cmd {
    fn new(opcode: Opcode) -> Self {
        let mut cmd = Self {
            opcode,
            b: BytesMut::with_capacity(CMD_HDR + u8::MAX as usize), // [Vol 4] Part E, Section 5.4.1
        };
        cmd.u16(opcode.0).u8(0);
        cmd
    }

    fn u8(&mut self, v: u8) -> &mut Self {
        self.b.put_u8(v);
        self
    }

    fn u16(&mut self, v: u16) -> &mut Self {
        self.b.put_u16_le(v);
        self
    }

    fn as_bytes(&mut self) -> &[u8] {
        let n = self.b.len() - CMD_HDR;
        assert_eq!(n as u8 as usize, n);
        let s = self.b.as_mut();
        s[CMD_HDR - 1] = n as _;
        s
    }
}

const EVT_HDR: usize = 2;
const EVT_BUF: usize = EVT_HDR + u8::MAX as usize;

/// HCI event buffer.
#[derive(Clone, Debug, Default)]
struct Evt {
    typ: EvtType,
    cur: Cursor<BytesMut>,
}

impl Evt {
    /// Allocates a new event buffer.
    fn new() -> Self {
        let mut buf = BytesMut::zeroed(EVT_BUF);
        unsafe { buf.set_len(0) }
        Self {
            typ: EvtType::Pending,
            cur: Cursor::new(buf),
        }
    }

    /// Resets the decoder and returns a buffer for receiving the next event.
    fn reset(&mut self) -> &mut [u8] {
        self.typ = EvtType::Pending;
        self.cur.set_position(0);
        let buf = self.cur.get_mut();
        unsafe {
            buf.set_len(0);
            slice::from_raw_parts_mut(buf.as_mut_ptr(), buf.capacity())
        }
    }

    /// Parses event header after receiving `n` bytes into the buffer. The
    /// subevent code for LE events is also consumed.
    fn ready(&mut self, n: usize) -> Result<()> {
        unsafe { self.cur.get_mut().set_len(n) };
        trace!("Event: {:02x?}", self.as_ref());
        if n < EVT_HDR {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(self.as_ref())));
        }
        let (code, len) = (self.u8(), self.u8());
        if self.cur.remaining() != len as usize {
            return Err(Error::InvalidEvent(Bytes::copy_from_slice(self.as_ref())));
        }
        self.typ = match EventCode::from_repr(code) {
            Some(EventCode::LeMetaEvent) => {
                // After the header is validated, we allow further decoding
                // calls to panic if there is missing data.
                let subevent = self.u8();
                match SubeventCode::from_repr(subevent) {
                    Some(subevent) => EvtType::Le(subevent),
                    None => {
                        return Err(Error::UnknownEvent {
                            code: code as _,
                            subevent,
                            params: Bytes::copy_from_slice(self.tail()),
                        })
                    }
                }
            }
            Some(code) => EvtType::Hci(code),
            None => {
                return Err(Error::UnknownEvent {
                    code,
                    subevent: 0,
                    params: Bytes::copy_from_slice(self.tail()),
                })
            }
        };
        Ok(())
    }

    /// Returns the event type.
    fn typ(&self) -> EvtType {
        self.typ
    }

    /// Returns any unparsed bytes.
    fn tail(&self) -> &[u8] {
        &self.as_ref()[self.cur.position() as usize..]
    }

    /// Returns basic information from `CommandComplete` or `CommandStatus`
    /// events. Returns `None` for other event types.
    fn cmd_status(&mut self) -> Option<CmdStatus> {
        match self.typ {
            EvtType::Hci(EventCode::CommandComplete) => Some(CmdStatus {
                quota: CmdQuota(self.u8()),
                opcode: Opcode(self.u16()),
                status: if self.cur.has_remaining() {
                    Status::from(self.u8())
                } else {
                    Status::Success // Opcode 0x0000
                },
            }),
            EvtType::Hci(EventCode::CommandStatus) => Some(CmdStatus {
                status: Status::from(self.u8()),
                quota: CmdQuota(self.u8()),
                opcode: Opcode(self.u16()),
            }),
            _ => None,
        }
    }

    #[inline]
    fn u8(&mut self) -> u8 {
        self.cur.get_u8()
    }

    #[inline]
    fn u16(&mut self) -> u16 {
        self.cur.get_u16_le()
    }
}

impl AsRef<[u8]> for Evt {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.cur.get_ref().as_ref()
    }
}

/// Combined event code.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum EvtType {
    #[default]
    Pending,
    Hci(EventCode),
    Le(SubeventCode),
}

/// Basic information contained in a `CommandComplete` or `CommandStatus`
/// events.
#[derive(Clone, Copy, Debug, Default)]
struct CmdStatus {
    quota: CmdQuota,
    opcode: Opcode,
    status: Status,
}

/// Number of HCI command packets the host can send to the controller.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct CmdQuota(u8);

/// Command opcode.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Opcode(pub u16);

impl Opcode {
    /// Creates an opcode from Opcode Group Field and Opcode Command Field.
    #[inline]
    pub fn new(ogf: u16, ocf: u16) -> Self {
        Self(ogf << 10 | ocf)
    }

    /// Creates an informational parameter commands opcode.
    #[inline]
    pub fn info(ocf: u16) -> Self {
        Self::new(0x04, ocf)
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:04x}", self.0)
    }
}
