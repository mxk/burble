use structbuf::Unpack;

use crate::gap::Uuid16;

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

    /// Returns a non-handle error response.
    #[inline]
    const fn err(&self, err: ErrorCode) -> ErrorRsp {
        self.op.err(err)
    }

    /// Returns a handle-specific error response.
    #[inline]
    const fn hdl_err(&self, hdl: Option<Handle>, err: ErrorCode) -> ErrorRsp {
        self.op.hdl_err(hdl, err)
    }

    /// Returns the result of calling `f` to unpack the PDU.
    #[inline]
    fn unpack<'b, V>(
        &'b self,
        op: Opcode,
        f: impl FnOnce(&mut Unpacker<'b>) -> RspResult<V>,
    ) -> RspResult<V> {
        debug_assert_eq!(self.op, op);
        let p = self.sdu.unpack().split_at(1).1; // Skip opcode
        p.map_or(Err(op.err(ErrorCode::InvalidPdu)), f)
    }

    /// Unpacks start/end handle range ([Vol 3] Part F, Section 3.4.3.1).
    #[inline]
    fn handle_range(&self, p: &mut Unpacker) -> RspResult<HandleRange> {
        match (Handle::new(p.u16()), Handle::new(p.u16())) {
            (Some(start), Some(end)) if start <= end => Ok(HandleRange::new(start, end)),
            (start, _) => Err(self.hdl_err(start, ErrorCode::InvalidHandle)),
        }
    }

    /// Unpacks a 128-bit UUID.
    #[inline]
    fn uuid(&self, p: &mut Unpacker, err_hdl: Option<Handle>) -> RspResult<Uuid> {
        Uuid::new(p.u128()).ok_or_else(|| self.hdl_err(err_hdl, ErrorCode::AttributeNotFound))
    }

    /// Unpacks a 16-bit UUID.
    #[inline]
    fn uuid16(&self, p: &mut Unpacker, err_hdl: Option<Handle>) -> RspResult<Uuid16> {
        Uuid16::new(p.u16()).ok_or_else(|| self.hdl_err(err_hdl, ErrorCode::AttributeNotFound))
    }
}

impl<T: host::Transport> AsRef<[u8]> for Pdu<'_, T> {
    /// Returns PDU bytes.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.sdu.as_ref()
    }
}

/// MTU exchange parameter decoders ([Vol 3] Part F, Section 3.4.2).
impl<T: host::Transport> Pdu<'_, T> {
    /// Returns `ATT_EXCHANGE_MTU_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.2.1).
    pub fn exchange_mtu_req(&self) -> RspResult<u16> {
        self.unpack(Opcode::ExchangeMtuReq, |p| Ok(p.u16()))
    }
}

/// MTU exchange parameter encoders ([Vol 3] Part F, Section 3.4.2).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_EXCHANGE_MTU_RSP` PDU ([Vol 3] Part F, Section 3.4.2.2).
    pub async fn exchange_mtu_rsp(&self, mtu: u16) -> Result<()> {
        let rsp = self.pack(Opcode::ExchangeMtuRsp, |p| {
            p.u16(mtu);
        });
        self.send(rsp).await
    }
}

/// Find information parameter decoders ([Vol 3] Part F, Section 3.4.3).
impl<T: host::Transport> Pdu<'_, T> {
    /// Returns `ATT_FIND_INFORMATION_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.1).
    pub(super) fn find_information_req(&self) -> RspResult<HandleRange> {
        self.unpack(Opcode::FindInformationReq, |p| self.handle_range(p))
    }

    /// Returns `ATT_FIND_BY_TYPE_VALUE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.3).
    pub(super) fn find_by_type_value_req(&self) -> RspResult<(HandleRange, Uuid16, &[u8])> {
        self.unpack(Opcode::FindByTypeValueReq, |p| {
            let range = self.handle_range(p)?;
            Ok((
                range,
                self.uuid16(p, Some(range.start))?,
                p.take().into_inner().unwrap_or_default(),
            ))
        })
    }
}

/// Find information parameter encoders ([Vol 3] Part F, Section 3.4.3).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_FIND_INFORMATION_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.3.2).
    pub(super) async fn find_information_rsp(
        &self,
        it: impl Iterator<Item = (Handle, Uuid)> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::FindInformationRsp, move |p| {
            let mut it = it.peekable();
            let first = it
                .peek()
                .expect("ATT_FIND_INFORMATION_RSP must contain at least one entry");
            let fmt = 0x01 + u8::from(first.1.as_u16().is_none());
            p.u8(fmt);
            if fmt == 0x01 {
                for (h, u) in it
                    .take(p.remaining() / (2 + 2))
                    .map_while(|(h, u)| u.as_u16().map(|u| (h, u)))
                {
                    p.u16(h).u16(u);
                }
            } else {
                for (h, u) in it
                    .take(p.remaining() / (2 + 16))
                    .take_while(|&(_, u)| u.as_u16().is_none())
                {
                    p.u16(h).u128(u);
                }
            }
        });
        self.send(rsp).await
    }

    /// Sends an `ATT_FIND_BY_TYPE_VALUE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.3.4).
    pub(super) async fn find_by_type_value_rsp(
        &self,
        it: impl Iterator<Item = (Handle, Option<Handle>)> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::FindByTypeValueRsp, move |p| {
            for (hdl, group_end) in it.take(p.remaining() / (2 + 2)) {
                p.u16(hdl).u16(group_end.unwrap_or(hdl));
            }
        });
        self.send(rsp).await
    }
}

/// Reading attributes parameter decoders ([Vol 3] Part F, Section 3.4.4).
impl<T: host::Transport> Pdu<'_, T> {
    /// Returns `ATT_READ_BY_TYPE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.1).
    pub(super) fn read_by_type_req(&self) -> RspResult<(HandleRange, Uuid)> {
        self.unpack(Opcode::ReadByTypeReq, |p| {
            let range = self.handle_range(p)?;
            Ok((
                range,
                if p.len() == 2 {
                    self.uuid16(p, Some(range.start))?.as_uuid()
                } else {
                    self.uuid(p, Some(range.start))?
                },
            ))
        })
    }
}

/// Reading attributes parameter encoders ([Vol 3] Part F, Section 3.4.4).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_READ_BY_TYPE_RSP` PDU ([Vol 3] Part F, Section 3.4.4.2).
    pub(super) async fn read_by_type_rsp(
        &self,
        it: impl Iterator<Item = (Handle, &[u8])> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::FindInformationRsp, move |p| {
            let mut it = it.peekable();
            let first = (it.peek()).expect("ATT_READ_BY_TYPE_RSP must contain at least one entry");
            let n = (first.1.len())
                .min(p.remaining() - 3)
                .min(u8::MAX as usize - 2);
            #[allow(clippy::cast_possible_truncation)]
            p.u8(2 + n as u8);
            for (h, v) in it
                .take(p.remaining() / (2 + n))
                .take_while(|&(_, v)| v.len() >= n)
            {
                p.u16(h).put(&v[..n]);
            }
        });
        self.send(rsp).await
    }
}
