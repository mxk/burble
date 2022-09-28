use std::fmt::{Display, Formatter};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use tracing::trace;

pub use {codes::*, info::*};

use crate::{HostError, Transport};

mod codes;
mod info;

/// Error type returned by the HCI layer.
#[derive(Clone, Debug, thiserror::Error)]
pub enum HciError {
    #[error("HCI error ({status})")]
    Hci {
        #[from]
        status: Status,
    },
    #[error(transparent)]
    Host {
        #[from]
        source: HostError,
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

impl From<CmdStatus> for HciError {
    fn from(s: CmdStatus) -> Self {
        HciError::Command {
            opcode: s.opcode,
            status: s.status,
        }
    }
}

type Result<T> = std::result::Result<T, HciError>;

/// Host Controller Interface.
#[derive(Debug)]
pub struct Hci<T> {
    t: T,
}

impl<T: Transport> Hci<T> {
    /// Returns an HCI using transport layer `t`.
    pub fn new(t: T) -> Self {
        Self { t }
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

/// Combined event code.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EvtType {
    Hci(EventCode),
    Le(SubeventCode),
}

/// HCI event decoder.
#[derive(Clone, Debug)]
struct Evt {
    typ: EvtType,
    orig: Bytes,
    b: Bytes,
}

impl Evt {
    /// Returns basic information from `CommandComplete` or `CommandStatus`
    /// events. Returns `None` for other event types.
    fn cmd_status(&mut self) -> Option<CmdStatus> {
        match self.typ {
            EvtType::Hci(EventCode::CommandComplete) => Some(CmdStatus {
                quota: CmdQuota(self.u8()),
                opcode: Opcode(self.u16()),
                status: if !self.b.is_empty() {
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

    fn u8(&mut self) -> u8 {
        self.b.get_u8()
    }

    fn u16(&mut self) -> u16 {
        self.b.get_u16_le()
    }
}

impl TryFrom<Bytes> for Evt {
    type Error = HciError;

    fn try_from(mut b: Bytes) -> Result<Self> {
        trace!("Event: {:02x?}", b.as_ref());
        let orig = b.clone();
        if b.len() < EVT_HDR {
            return Err(HciError::InvalidEvent(orig));
        }
        let (code, len) = (b.get_u8(), b.get_u8());
        if b.len() != len as usize {
            return Err(HciError::InvalidEvent(orig));
        }
        let typ = match EventCode::from_repr(code) {
            Some(code) => match code {
                EventCode::LeMetaEvent => {
                    if b.is_empty() {
                        return Err(HciError::InvalidEvent(orig));
                    }
                    let subevent = b.get_u8();
                    match SubeventCode::from_repr(subevent) {
                        Some(subevent) => EvtType::Le(subevent),
                        None => {
                            return Err(HciError::UnknownEvent {
                                code: code as _,
                                subevent,
                                params: b,
                            })
                        }
                    }
                }
                _ => EvtType::Hci(code),
            },
            None => {
                return Err(HciError::UnknownEvent {
                    code,
                    subevent: 0,
                    params: b,
                })
            }
        };
        Ok(Self { typ, orig, b })
    }
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
