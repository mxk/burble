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

    /// Unpacks one handle ([Vol 3] Part F, Section 3.4.4.3).
    #[inline]
    fn handle(&self, p: &mut Unpacker) -> RspResult<Handle> {
        Handle::new(p.u16()).ok_or_else(|| self.err(ErrorCode::InvalidHandle))
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
    pub fn read_by_type_req(&self) -> RspResult<(HandleRange, Uuid)> {
        self.read_by_type_op(Opcode::ReadByTypeReq)
    }

    #[inline]
    fn read_by_type_op(&self, op: Opcode) -> RspResult<(HandleRange, Uuid)> {
        self.unpack(op, |p| {
            let range = self.handle_range(p)?;
            let uuid = if p.len() == 2 {
                self.uuid16(p, Some(range.start))?.as_uuid()
            } else {
                self.uuid(p, Some(range.start))?
            };
            Ok((range, uuid))
        })
    }

    /// Returns `ATT_READ_REQ` PDU parameters ([Vol 3] Part F, Section 3.4.4.3).
    pub fn read_req(&self) -> RspResult<Handle> {
        self.unpack(Opcode::ReadReq, |p| self.handle(p))
    }

    /// Returns `ATT_READ_BLOB_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.5).
    pub fn read_blob_req(&self) -> RspResult<(Handle, u16)> {
        self.unpack(Opcode::ReadBlobReq, |p| Ok((self.handle(p)?, p.u16())))
    }

    /// Returns `ATT_READ_MULTIPLE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.7).
    pub fn read_multiple_req(&self) -> RspResult<Vec<Handle>> {
        self.read_multiple_op(Opcode::ReadMultipleReq)
    }

    #[inline]
    fn read_multiple_op(&self, op: Opcode) -> RspResult<Vec<Handle>> {
        self.unpack(op, |p| {
            let mut v = Vec::with_capacity((p.len() + 1) / 2);
            while !p.is_empty() {
                v.push(self.handle(p)?);
            }
            if v.len() >= 2 {
                Ok(v)
            } else {
                Err(self.err(ErrorCode::InvalidHandle))
            }
        })
    }

    /// Returns `ATT_READ_BY_GROUP_TYPE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.9).
    pub fn read_by_group_type_req(&self) -> RspResult<(HandleRange, Uuid)> {
        self.read_by_type_op(Opcode::ReadByGroupTypeReq)
    }

    /// Returns `ATT_READ_MULTIPLE_VARIABLE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.11).
    pub fn read_multiple_variable_req(&self) -> RspResult<Vec<Handle>> {
        self.read_multiple_op(Opcode::ReadMultipleVariableReq)
    }
}

/// Reading attributes parameter encoders ([Vol 3] Part F, Section 3.4.4).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_READ_BY_TYPE_RSP` PDU ([Vol 3] Part F, Section 3.4.4.2).
    pub async fn read_by_type_rsp(
        &self,
        it: impl Iterator<Item = (Handle, &[u8])> + Send,
    ) -> Result<()> {
        self.read_by_type_op::<2, (Handle, &[u8])>(
            Opcode::ReadByTypeRsp,
            it,
            |&(_, v)| v.len(),
            |p, n, (h, v)| {
                p.u16(h).put(&v[..n]);
            },
        )
        .await
    }

    #[inline]
    async fn read_by_type_op<const HDR: usize, V>(
        &self,
        op: Opcode,
        it: impl Iterator<Item = V> + Send,
        len: impl Fn(&V) -> usize + Send,
        put: impl Fn(&mut Packer, usize, V) + Send,
    ) -> Result<()> {
        let rsp = self.pack(op, move |p| {
            let mut it = it.peekable();
            let first = it
                .peek()
                .unwrap_or_else(|| panic!("{op} must contain at least one entry"));
            let n = len(first)
                .min(p.remaining() - (1 + HDR))
                .min(u8::MAX as usize - HDR);
            #[allow(clippy::cast_possible_truncation)]
            p.u8((HDR + n) as u8);
            for v in it
                .take(p.remaining() / (HDR + n))
                .take_while(|v| len(v) >= n)
            {
                put(p, n, v);
            }
        });
        self.send(rsp).await
    }

    /// Sends an `ATT_READ_RSP` PDU ([Vol 3] Part F, Section 3.4.4.4).
    pub async fn read_rsp(&self, v: &[u8]) -> Result<()> {
        self.read_op(Opcode::ReadRsp, v).await
    }

    /// Sends an `ATT_READ_BLOB_RSP` PDU ([Vol 3] Part F, Section 3.4.4.6).
    pub async fn read_blob_rsp(&self, v: &[u8]) -> Result<()> {
        self.read_op(Opcode::ReadBlobRsp, v).await
    }

    #[inline]
    async fn read_op(&self, op: Opcode, v: &[u8]) -> Result<()> {
        let rsp = self.pack(op, move |p| {
            p.put(&v[..v.len().min(p.remaining())]);
        });
        self.send(rsp).await
    }

    /// Sends an `ATT_READ_MULTIPLE_RSP` PDU ([Vol 3] Part F, Section 3.4.4.8).
    pub async fn read_multiple_rsp(
        &self,
        mut it: impl Iterator<Item = &[u8]> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::ReadMultipleRsp, move |p| {
            while p.remaining() > 0 {
                let Some(v) = it.next() else { break };
                p.put(&v[..v.len().min(p.remaining())]);
            }
        });
        self.send(rsp).await
    }

    /// Sends an `ATT_READ_BY_GROUP_TYPE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.4.10).
    pub async fn read_by_group_type_rsp(
        &self,
        it: impl Iterator<Item = (Handle, Handle, &[u8])> + Send,
    ) -> Result<()> {
        self.read_by_type_op::<4, (Handle, Handle, &[u8])>(
            Opcode::ReadByTypeRsp,
            it,
            |&(_, _, v)| v.len(),
            |p, n, (h, g, v)| {
                p.u16(h).u16(g).put(&v[..n]);
            },
        )
        .await
    }

    /// Sends an `ATT_READ_MULTIPLE_VARIABLE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.4.12).
    pub async fn read_multiple_variable_rsp(
        &self,
        mut it: impl Iterator<Item = &[u8]> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::ReadMultipleVariableRsp, move |p| {
            while p.remaining() >= 2 {
                let Some(v) = it.next() else { break };
                p.u16(u16::try_from(v.len()).expect("attribute value too long"));
                p.put(&v[..v.len().min(p.remaining())]);
            }
        });
        self.send(rsp).await
    }
}
