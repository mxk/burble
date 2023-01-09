use structbuf::Unpack;

use crate::gap::Uuid16;

use super::*;

/// PDU response result.
pub type RspResult<T> = std::result::Result<T, ErrorRsp>;

/// Received ATT protocol data unit ([Vol 3] Part F, Section 3.3).
#[derive(Debug)]
#[repr(transparent)]
pub struct Pdu<T: host::Transport>(pub(super) Sdu<T>);

impl<T: host::Transport> Pdu<T> {
    /// Returns the PDU opcode.
    #[inline]
    pub fn opcode(&self) -> Opcode {
        // SAFETY: Bearer::recv() validated the opcode
        unsafe { std::mem::transmute(*self.0.as_ref().get_unchecked(0)) }
    }

    /// Returns a non-handle error response.
    #[inline]
    pub fn err(&self, err: ErrorCode) -> ErrorRsp {
        self.opcode().err(err)
    }

    /// Returns a handle-specific error response.
    #[inline]
    pub fn hdl_err(&self, hdl: impl Into<Option<Handle>>, err: ErrorCode) -> ErrorRsp {
        self.opcode().hdl_err(hdl.into(), err)
    }

    /// Returns the result of calling `f` to unpack the PDU.
    #[inline]
    fn unpack<'b, V>(
        &'b self,
        op: Opcode,
        f: impl FnOnce(&mut Unpacker<'b>) -> RspResult<V>,
    ) -> RspResult<V> {
        debug_assert_eq!(self.opcode(), op);
        let p = self.0.unpack().split_at(1).1; // Skip opcode
        p.map_or(Err(self.err(ErrorCode::InvalidPdu)), f)
    }

    /// Unpacks start/end handle range ([Vol 3] Part F, Section 3.4.3.1).
    #[inline]
    fn handle_range(&self, p: &mut Unpacker) -> RspResult<HandleRange> {
        match (Handle::new(p.u16()), Handle::new(p.u16())) {
            (Some(start), Some(end)) if start <= end => Ok(HandleRange::new(start, end)),
            (start, _) => Err(self.hdl_err(start, ErrorCode::InvalidHandle)),
        }
    }

    /// Unpacks one handle ([Vol 3] Part F, Section 3.4.4.3).
    #[inline]
    fn handle(&self, p: &mut Unpacker) -> RspResult<Handle> {
        Handle::new(p.u16()).ok_or_else(|| self.err(ErrorCode::InvalidHandle))
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

impl<T: host::Transport> From<Pdu<T>> for Opcode {
    #[inline(always)]
    fn from(pdu: Pdu<T>) -> Self {
        pdu.opcode()
    }
}

//
// Error handling ([Vol 3] Part F, Section 3.4.1)
//

/// Error handling encoders ([Vol 3] Part F, Section 3.4.1).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_ERROR_RSP` PDU ([Vol 3] Part F, Section 3.4.1.1).
    #[inline]
    pub async fn error_rsp(
        &self,
        req: impl Into<Opcode> + Send,
        hdl: impl Into<Option<Handle>> + Send,
        err: ErrorCode,
    ) -> Result<()> {
        self.raw_error_rsp(req.into() as u8, hdl.into(), err).await
    }
}

//
// MTU exchange ([Vol 3] Part F, Section 3.4.2)
//

/// MTU exchange decoders ([Vol 3] Part F, Section 3.4.2).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_EXCHANGE_MTU_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.2.1).
    pub(super) fn exchange_mtu_req(&self) -> RspResult<u16> {
        self.unpack(Opcode::ExchangeMtuReq, |p| Ok(p.u16()))
    }
}

/// MTU exchange encoders ([Vol 3] Part F, Section 3.4.2).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_EXCHANGE_MTU_RSP` PDU ([Vol 3] Part F, Section 3.4.2.2).
    pub(super) async fn exchange_mtu_rsp(&self, mtu: u16) -> Result<()> {
        let rsp = self.pack(Opcode::ExchangeMtuRsp, |p| {
            p.u16(mtu);
        });
        self.send(rsp).await
    }
}

//
// Find information ([Vol 3] Part F, Section 3.4.3)
//

/// Find information decoders ([Vol 3] Part F, Section 3.4.3).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_FIND_INFORMATION_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.1).
    pub fn find_information_req(&self) -> RspResult<HandleRange> {
        self.unpack(Opcode::FindInformationReq, |p| self.handle_range(p))
    }

    /// Returns `ATT_FIND_BY_TYPE_VALUE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.3).
    pub fn find_by_type_value_req(&self) -> RspResult<(HandleRange, Uuid16, &[u8])> {
        self.unpack(Opcode::FindByTypeValueReq, |p| {
            let range = self.handle_range(p)?;
            Ok((range, self.uuid16(p, Some(range.start()))?, take(p)))
        })
    }
}

/// Find information encoders ([Vol 3] Part F, Section 3.4.3).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_FIND_INFORMATION_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.3.2).
    pub async fn find_information_rsp(
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
    pub async fn find_by_type_value_rsp(
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

//
// Reading attributes ([Vol 3] Part F, Section 3.4.4)
//

/// Reading attributes decoders ([Vol 3] Part F, Section 3.4.4).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_READ_BY_TYPE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.1).
    pub fn read_by_type_req(&self) -> RspResult<(HandleRange, Uuid)> {
        self.read_by_type_op(Opcode::ReadByTypeReq)
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

    #[inline]
    fn read_by_type_op(&self, op: Opcode) -> RspResult<(HandleRange, Uuid)> {
        self.unpack(op, |p| {
            let range = self.handle_range(p)?;
            let uuid = if p.len() == 2 {
                self.uuid16(p, Some(range.start()))?.as_uuid()
            } else {
                self.uuid(p, Some(range.start()))?
            };
            Ok((range, uuid))
        })
    }

    #[inline]
    fn read_multiple_op(&self, op: Opcode) -> RspResult<Vec<Handle>> {
        self.unpack(op, |p| {
            let mut v = Vec::with_capacity(p.len().saturating_add(1) / 2);
            while !p.is_empty() {
                v.push(self.handle(p)?);
            }
            if v.len() >= 2 {
                Ok(v)
            } else {
                Err(self.err(ErrorCode::InvalidPdu))
            }
        })
    }
}

/// Reading attributes encoders ([Vol 3] Part F, Section 3.4.4).
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

    /// Sends an `ATT_READ_RSP` PDU ([Vol 3] Part F, Section 3.4.4.4).
    pub async fn read_rsp(&self, v: &[u8]) -> Result<()> {
        self.read_op(Opcode::ReadRsp, v).await
    }

    /// Sends an `ATT_READ_BLOB_RSP` PDU ([Vol 3] Part F, Section 3.4.4.6).
    pub async fn read_blob_rsp(&self, v: &[u8]) -> Result<()> {
        self.read_op(Opcode::ReadBlobRsp, v).await
    }

    /// Sends an `ATT_READ_MULTIPLE_RSP` PDU ([Vol 3] Part F, Section 3.4.4.8).
    pub async fn read_multiple_rsp(
        &self,
        mut it: impl Iterator<Item = &[u8]> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::ReadMultipleRsp, move |p| {
            while p.remaining() > 0 {
                let Some(v) = it.next() else { break };
                put_truncate(p, v);
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
            Opcode::ReadByGroupTypeRsp,
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
                put_truncate(p, v);
            }
        });
        self.send(rsp).await
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
            let same_len = len(first);
            let n = same_len
                .min(p.remaining() - (1 + HDR))
                .min(u8::MAX as usize - HDR);
            #[allow(clippy::cast_possible_truncation)]
            p.u8((HDR + n) as u8);
            for v in it
                .take(p.remaining() / (HDR + n))
                .take_while(|v| len(v) == same_len)
            {
                put(p, n, v);
            }
        });
        self.send(rsp).await
    }

    #[inline]
    async fn read_op(&self, op: Opcode, v: &[u8]) -> Result<()> {
        self.send(self.pack(op, |p| put_truncate(p, v))).await
    }
}

//
// Writing attributes ([Vol 3] Part F, Section 3.4.5)
//

/// Writing attributes parameter decoders ([Vol 3] Part F, Section 3.4.5).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_WRITE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.5.1).
    pub fn write_req(&self) -> RspResult<(Handle, &[u8])> {
        self.write_op(Opcode::WriteReq)
    }

    /// Returns `ATT_WRITE_CMD` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.5.3).
    pub fn write_cmd(&self) -> RspResult<(Handle, &[u8])> {
        self.write_op(Opcode::WriteCmd)
    }

    #[inline]
    fn write_op(&self, op: Opcode) -> RspResult<(Handle, &[u8])> {
        // TODO: Strip signature for ATT_SIGNED_WRITE_CMD?
        self.unpack(op, |p| Ok((self.handle(p)?, take(p))))
    }
}

/// Writing attributes encoders ([Vol 3] Part F, Section 3.4.5).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_WRITE_RSP` PDU ([Vol 3] Part F, Section 3.4.5.2).
    pub async fn write_rsp(&self) -> Result<()> {
        self.send(self.pack(Opcode::WriteRsp, |_| ())).await
    }
}

//
// Queued writes ([Vol 3] Part F, Section 3.4.6)
//

/// Queued writes decoders ([Vol 3] Part F, Section 3.4.6).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_PREPARE_WRITE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.6.1).
    pub fn prepare_write_req(&self) -> RspResult<(Handle, u16, &[u8])> {
        self.unpack(Opcode::PrepareWriteReq, |p| {
            Ok((self.handle(p)?, p.u16(), take(p)))
        })
    }

    /// Returns `ATT_EXECUTE_WRITE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.6.3).
    pub fn execute_write_req(&self) -> RspResult<bool> {
        self.unpack(Opcode::ExecuteWriteReq, |p| Ok(p.bool()))
    }
}

/// Queued writes encoders ([Vol 3] Part F, Section 3.4.6).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_PREPARE_WRITE_RSP` PDU ([Vol 3] Part F, Section 3.4.6.2).
    pub async fn prepare_write_rsp(&self, hdl: Handle, off: u16, v: &[u8]) -> Result<()> {
        let pdu = self.pack(Opcode::PrepareWriteRsp, |p| {
            p.u16(hdl).u16(off).put(v);
        });
        self.send(pdu).await
    }

    /// Sends an `ATT_EXECUTE_WRITE_RSP` PDU ([Vol 3] Part F, Section 3.4.6.4).
    pub async fn execute_write_rsp(&self) -> Result<()> {
        self.send(self.pack(Opcode::ExecuteWriteRsp, |_| ())).await
    }
}

//
// Server initiated ([Vol 3] Part F, Section 3.4.7)
//

/// Server initiated decoders ([Vol 3] Part F, Section 3.4.7).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_HANDLE_VALUE_CFM` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.7.3).
    pub fn handle_value_cfm(&self) -> RspResult<()> {
        self.unpack(Opcode::HandleValueCfm, |_| Ok(()))
    }
}

/// Server initiated encoders ([Vol 3] Part F, Section 3.4.7).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_HANDLE_VALUE_NTF` PDU ([Vol 3] Part F, Section 3.4.7.1).
    pub async fn handle_value_ntf(&self, hdl: Handle, v: &[u8]) -> Result<()> {
        self.send(self.pack(Opcode::HandleValueNtf, |p| put_truncate(p.u16(hdl), v)))
            .await
    }

    /// Sends an `ATT_HANDLE_VALUE_IND` PDU ([Vol 3] Part F, Section 3.4.7.2).
    pub async fn handle_value_ind(&self, hdl: Handle, v: &[u8]) -> Result<()> {
        self.send(self.pack(Opcode::HandleValueInd, |p| put_truncate(p.u16(hdl), v)))
            .await
    }

    /// Sends an `ATT_MULTIPLE_HANDLE_VALUE_NTF` PDU
    /// ([Vol 3] Part F, Section 3.4.7.4).
    pub async fn multiple_handle_value_ntf(
        &self,
        it: impl Iterator<Item = (Handle, &[u8])> + Send,
    ) -> Result<()> {
        let rsp = self.pack(Opcode::MultipleHandleValueNtf, move |p| {
            for (hdl, v) in it {
                p.u16(hdl)
                    .u16(u16::try_from(v.len()).expect("attribute value too long"))
                    .put(v);
            }
        });
        self.send(rsp).await
    }
}

/// Consumes any remaining bytes in `p`.
#[inline]
fn take<'a>(p: &mut Unpacker<'a>) -> &'a [u8] {
    p.take().into_inner().unwrap_or_default()
}

/// Writes `v` to `p`, possibly truncating the written value.
#[inline]
fn put_truncate(p: &mut Packer, v: &[u8]) {
    let n = p.remaining();
    p.put(if v.len() <= n {
        v
    } else {
        // SAFETY: n < v.len()
        unsafe { v.get_unchecked(..n) }
    });
}
