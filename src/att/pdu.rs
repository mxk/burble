use crate::util::Unpacker;

use super::*;

/// PDU response result.
pub type RspResult<T> = std::result::Result<T, ErrorRsp>;

/// Received ATT protocol data unit ([Vol 3] Part F, Section 3.3).
#[derive(Debug)]
pub struct Pdu<'a, T: host::Transport> {
    br: &'a mut Bearer<T>,
    op: Opcode,
    sdu: Sdu<T>,
}

impl<'a, T: host::Transport> Pdu<'a, T> {
    /// Creates a PDU from the received L2CAP SDU.
    #[inline]
    pub(super) fn new(br: &'a mut Bearer<T>, op: Opcode, sdu: Sdu<T>) -> Self {
        Self { br, op, sdu }
    }

    /// Returns the PDU opcode.
    #[inline]
    pub const fn opcode(&self) -> Opcode {
        self.op
    }

    /// Sends an `ATT_ERROR_RSP` PDU in response to a request that cannot be
    /// performed ([Vol 3] Part F, Section 3.4.1.1). Command-related errors are
    /// ignored.
    #[inline]
    pub async fn error_rsp(self, e: ErrorRsp) -> Result<()> {
        self.br.error_rsp(e.req as u8, e.hdl, e.err).await
    }

    /// Returns the result of calling `f` to unpack the PDU.
    #[inline]
    fn unpack<'b, V>(&'b self, op: Opcode, f: impl FnOnce(&mut Unpacker<'b>) -> V) -> RspResult<V> {
        debug_assert_eq!(self.op, op);
        let mut p = Unpacker::new(self.sdu.as_ref());
        p.skip(1); // Opcode
        p.map_or(Err(op.err(ErrorCode::InvalidPdu)), |p| Ok(f(p)))
    }
}

impl<T: host::Transport> AsRef<[u8]> for Pdu<'_, T> {
    /// Returns PDU bytes.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.sdu.as_ref()
    }
}

/// PDU parameter decoding methods.
impl<T: host::Transport> Pdu<'_, T> {
    /// Returns `ATT_EXCHANGE_MTU_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.2.1).
    pub fn exchange_mtu_req(&self) -> RspResult<u16> {
        self.unpack(Opcode::ExchangeMtuReq, Unpacker::u16)
    }

    /// Returns `ATT_FIND_INFORMATION_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.1).
    pub(super) fn find_information_req(&self) -> RspResult<(Handle, Handle)> {
        let (start, end) = self.unpack(Opcode::FindInformationReq, |p| {
            (Handle::new(p.u16()), Handle::new(p.u16()))
        })?;
        if start.is_none() || start > end {
            return Err(self.op.hdl_err(start, ErrorCode::InvalidHandle));
        }
        let (start, end) = (start.unwrap(), end.unwrap());
        Ok((start, end))
    }
}

/// PDU parameter encoding methods.
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_EXCHANGE_MTU_RSP` PDU ([Vol 3] Part F, Section 3.4.2.2).
    pub async fn exchange_mtu_rsp(&mut self, mtu: u16) -> Result<()> {
        let sdu = self.new_pdu(Opcode::ExchangeMtuRsp, |p| {
            p.put_u16_le(mtu);
        });
        self.send(sdu).await
    }
}
