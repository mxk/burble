use bytes::{BufMut, BytesMut};

pub use {hci_control::*, info_params::*, le::*};

use super::*;

mod hci_control;
mod info_params;
mod le;

/// HCI command header and buffer sizes ([Vol 4] Part E, Section 5.4.1).
const CMD_HDR: usize = 3;
pub(crate) const CMD_BUF: usize = CMD_HDR + u8::MAX as usize;

/// HCI command encoder.
#[derive(Debug)]
pub(super) struct Command<'a> {
    b: &'a mut BytesMut,
}

impl<'a> Command<'a> {
    pub fn new(opcode: Opcode, b: &'a mut BytesMut) -> Self {
        let mut cmd = Self { b };
        cmd.u16(opcode as _).u8(0);
        cmd
    }

    pub fn u8(&mut self, v: u8) -> &mut Self {
        self.b.put_u8(v);
        self
    }

    pub fn u16(&mut self, v: u16) -> &mut Self {
        self.b.put_u16_le(v);
        self
    }
}
