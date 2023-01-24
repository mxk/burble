use std::mem;

use structbuf::Unpack;
use tracing::trace;

use {ErrorCode::*, Opcode::*};

use crate::gap::Uuid16;

use super::*;

impl<T: host::Transport> Pdu<T> {
    /// Returns the PDU opcode.
    #[inline]
    pub fn opcode(&self) -> Opcode {
        // SAFETY: Bearer::recv() validated the opcode
        unsafe { mem::transmute(*self.0.as_ref().get_unchecked(0)) }
    }

    /// Returns a non-handle error response.
    #[inline(always)]
    pub fn err<R>(&self, err: ErrorCode) -> RspResult<R> {
        self.opcode().err(err)
    }

    /// Returns a handle-specific error response.
    #[inline(always)]
    pub fn hdl_err<R>(&self, err: ErrorCode, hdl: Handle) -> RspResult<R> {
        self.opcode().hdl_err(err, hdl)
    }

    /// Returns the result of calling `f` to unpack the PDU.
    #[inline]
    fn unpack<'b, V: Debug>(
        &'b self,
        op: Opcode,
        f: impl FnOnce(&mut Unpacker<'b>) -> RspResult<V>,
    ) -> RspResult<V> {
        debug_assert_eq!(self.opcode(), op);
        let p = self.0.unpack().split_at(1).1; // Skip opcode
        p.map_or(self.err(InvalidPdu), f).map(|r| {
            trace!("{op}: {r:?}");
            r
        })
    }

    /// Unpacks start/end handle range ([Vol 3] Part F, Section 3.4.3.1).
    #[inline]
    fn handle_range(&self, p: &mut Unpacker) -> RspResult<HandleRange> {
        match (Handle::new(p.u16()), Handle::new(p.u16())) {
            (Some(start), Some(end)) if start <= end => Ok(HandleRange::new(start, end)),
            (Some(start), _) => self.hdl_err(InvalidHandle, start),
            (None, _) => self.err(InvalidHandle),
        }
    }

    /// Unpacks one handle ([Vol 3] Part F, Section 3.4.4.3).
    #[inline]
    fn handle(&self, p: &mut Unpacker) -> RspResult<Handle> {
        Handle::new(p.u16()).map_or_else(|| self.err(InvalidHandle), Ok)
    }

    /// Unpacks a 128-bit UUID.
    #[inline]
    fn uuid(&self, p: &mut Unpacker, hdl: Handle) -> RspResult<Uuid> {
        Uuid::new(p.u128()).map_or_else(|| self.hdl_err(AttributeNotFound, hdl), Ok)
    }

    /// Unpacks a 16-bit UUID.
    #[inline]
    fn uuid16(&self, p: &mut Unpacker, hdl: Handle) -> RspResult<Uuid16> {
        Uuid16::new(p.u16()).map_or_else(|| self.hdl_err(AttributeNotFound, hdl), Ok)
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
        self.unpack(ExchangeMtuReq, |p| Ok(p.u16()))
    }
}

/// MTU exchange encoders ([Vol 3] Part F, Section 3.4.2).
impl<T: host::Transport> Bearer<T> {
    /// Returns an `ATT_EXCHANGE_MTU_RSP` PDU ([Vol 3] Part F, Section 3.4.2.2).
    pub(super) fn exchange_mtu_rsp(&self, mtu: u16) -> RspResult<Rsp<T>> {
        self.rsp(ExchangeMtuRsp, |p| {
            p.u16(mtu);
        })
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
        self.unpack(FindInformationReq, |p| self.handle_range(p))
    }

    /// Returns `ATT_FIND_BY_TYPE_VALUE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.3.3).
    pub fn find_by_type_value_req(&self) -> RspResult<(HandleRange, Uuid16, &[u8])> {
        self.unpack(FindByTypeValueReq, |p| {
            let range = self.handle_range(p)?;
            Ok((range, self.uuid16(p, range.start())?, take(p)))
        })
    }
}

/// Find information encoders ([Vol 3] Part F, Section 3.4.3).
impl<T: host::Transport> Bearer<T> {
    /// Returns an `ATT_FIND_INFORMATION_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.3.2).
    pub fn find_information_rsp(
        &self,
        start: Handle,
        it: impl Iterator<Item = (Handle, Uuid)>,
    ) -> RspResult<Rsp<T>> {
        let mut it = it.peekable();
        let Some(&(_, u)) = it.peek() else {
            return FindInformationReq.hdl_err(AttributeNotFound, start);
        };
        let fmt = 0x01 + u8::from(u.as_u16().is_none());
        self.rsp(FindInformationRsp, |p| {
            p.u8(fmt);
            if fmt == 0x01 {
                for (h, u) in it
                    .take(p.remaining() / (2 + 2))
                    .map_while(|(h, u)| u.as_uuid16().map(|u| (h, u)))
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
        })
    }

    /// Returns an `ATT_FIND_BY_TYPE_VALUE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.3.4).
    pub fn find_by_type_value_rsp(
        &self,
        it: impl Iterator<Item = (Handle, Option<Handle>)>,
    ) -> RspResult<Rsp<T>> {
        self.rsp(FindByTypeValueRsp, |p| {
            for (hdl, group_end) in it.take(p.remaining() / (2 + 2)) {
                p.u16(hdl).u16(group_end.unwrap_or(hdl));
            }
        })
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
        self.read_by_type_op(ReadByTypeReq)
    }

    /// Returns `ATT_READ_REQ` PDU parameters ([Vol 3] Part F, Section 3.4.4.3).
    pub fn read_req(&self) -> RspResult<Handle> {
        self.unpack(ReadReq, |p| self.handle(p))
    }

    /// Returns `ATT_READ_BLOB_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.5).
    pub fn read_blob_req(&self) -> RspResult<(Handle, u16)> {
        self.unpack(ReadBlobReq, |p| Ok((self.handle(p)?, p.u16())))
    }

    /// Returns `ATT_READ_MULTIPLE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.7).
    pub fn read_multiple_req(&self) -> RspResult<Vec<Handle>> {
        self.read_multiple_op(ReadMultipleReq)
    }

    /// Returns `ATT_READ_BY_GROUP_TYPE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.9).
    pub fn read_by_group_type_req(&self) -> RspResult<(HandleRange, Uuid)> {
        self.read_by_type_op(ReadByGroupTypeReq)
    }

    /// Returns `ATT_READ_MULTIPLE_VARIABLE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.4.11).
    pub fn read_multiple_variable_req(&self) -> RspResult<Vec<Handle>> {
        self.read_multiple_op(ReadMultipleVariableReq)
    }

    #[inline]
    fn read_by_type_op(&self, op: Opcode) -> RspResult<(HandleRange, Uuid)> {
        self.unpack(op, |p| {
            let range = self.handle_range(p)?;
            let typ = if p.len() == 2 {
                self.uuid16(p, range.start())?.as_uuid()
            } else {
                self.uuid(p, range.start())?
            };
            Ok((range, typ))
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
                self.err(InvalidPdu)
            }
        })
    }
}

/// Reading attributes encoders ([Vol 3] Part F, Section 3.4.4).
impl<T: host::Transport> Bearer<T> {
    /// Returns an `ATT_READ_BY_TYPE_RSP` PDU ([Vol 3] Part F, Section 3.4.4.2).
    #[allow(single_use_lifetimes)]
    pub fn read_by_type_rsp<'a>(
        &self,
        start: Handle,
        it: impl Iterator<Item = (Handle, &'a [u8])>,
    ) -> RspResult<Rsp<T>> {
        self.read_by_type_op(ReadByTypeRsp, start, it, |p, hdl| {
            p.u16(hdl);
        })
    }

    /// Returns an `ATT_READ_RSP` PDU ([Vol 3] Part F, Section 3.4.4.4).
    pub fn read_rsp(&self, v: &[u8]) -> RspResult<Rsp<T>> {
        self.read_op(ReadRsp, v)
    }

    /// Returns an `ATT_READ_BLOB_RSP` PDU ([Vol 3] Part F, Section 3.4.4.6).
    pub fn read_blob_rsp(&self, v: &[u8]) -> RspResult<Rsp<T>> {
        self.read_op(ReadBlobRsp, v)
    }

    /// Returns an `ATT_READ_MULTIPLE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.4.8).
    #[allow(single_use_lifetimes)]
    pub fn read_multiple_rsp<'a>(
        &self,
        mut it: impl Iterator<Item = &'a [u8]>,
    ) -> RspResult<Rsp<T>> {
        self.rsp(ReadMultipleRsp, |p| {
            while p.remaining() > 0 {
                let Some(v) = it.next() else { break };
                put_truncate(p, v);
            }
        })
    }

    /// Returns an `ATT_READ_BY_GROUP_TYPE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.4.10).
    #[allow(single_use_lifetimes)]
    pub fn read_by_group_type_rsp<'a>(
        &self,
        start: Handle,
        it: impl Iterator<Item = (HandleRange, &'a [u8])>,
    ) -> RspResult<Rsp<T>> {
        self.read_by_type_op(ReadByGroupTypeRsp, start, it, |p, hdls| {
            p.u16(hdls.start()).u16(hdls.end());
        })
    }

    /// Returns an `ATT_READ_MULTIPLE_VARIABLE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.4.12).
    #[allow(single_use_lifetimes)]
    pub fn read_multiple_variable_rsp<'a>(
        &self,
        mut it: impl Iterator<Item = &'a [u8]>,
    ) -> RspResult<Rsp<T>> {
        self.rsp(ReadMultipleVariableRsp, |p| {
            while p.remaining() >= 2 {
                let Some(v) = it.next() else { break };
                p.u16(u16::try_from(v.len()).expect("attribute value too long"));
                put_truncate(p, v);
            }
        })
    }

    #[allow(single_use_lifetimes)]
    #[inline]
    fn read_by_type_op<'a, H: Copy>(
        &self,
        op: Opcode,
        start: Handle,
        it: impl Iterator<Item = (H, &'a [u8])>,
        put_hdl: impl Fn(&mut Packer, H),
    ) -> RspResult<Rsp<T>> {
        let mut it = it.peekable();
        let Some(&(_, v)) = it.peek() else {
            return Err(super::ErrorRsp::new(u8::from(op) - 1, Some(start), AttributeNotFound))
        };
        let same_len = v.len();
        let hdr = mem::size_of::<H>();
        Ok(Rsp(self.pack(op, |p| {
            let n = same_len
                .min(p.remaining() - (1 + hdr))
                .min(u8::MAX as usize - hdr);
            #[allow(clippy::cast_possible_truncation)]
            p.u8((hdr + n) as u8);
            for (h, v) in it
                .take(p.remaining() / (hdr + n))
                .take_while(|&(_, v)| v.len() == same_len)
            {
                put_hdl(p, h);
                // SAFETY: `n <= v.len()`
                p.put(unsafe { v.get_unchecked(..n) });
            }
        })))
    }

    #[inline]
    fn read_op(&self, op: Opcode, v: &[u8]) -> RspResult<Rsp<T>> {
        self.rsp(op, |p| put_truncate(p, v))
    }
}

//
// Writing attributes ([Vol 3] Part F, Section 3.4.5)
//

/// Writing attributes parameter decoders ([Vol 3] Part F, Section 3.4.5).
impl<T: host::Transport> Pdu<T> {
    /// Returns `ATT_WRITE_REQ` or `ATT_WRITE_CMD` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.5.1 and 3.4.5.3).
    pub fn write_req(&self) -> RspResult<(Handle, &[u8])> {
        let op = self.opcode();
        debug_assert!(matches!(op, WriteReq | WriteCmd));
        // TODO: Strip signature for ATT_SIGNED_WRITE_CMD?
        self.unpack(op, |p| Ok((self.handle(p)?, take(p))))
    }
}

/// Writing attributes encoders ([Vol 3] Part F, Section 3.4.5).
impl<T: host::Transport> Bearer<T> {
    /// Returns an `ATT_WRITE_RSP` PDU ([Vol 3] Part F, Section 3.4.5.2).
    pub fn write_rsp(&self) -> RspResult<Rsp<T>> {
        self.rsp(WriteRsp, |_| ())
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
        self.unpack(PrepareWriteReq, |p| Ok((self.handle(p)?, p.u16(), take(p))))
    }

    /// Returns `ATT_EXECUTE_WRITE_REQ` PDU parameters
    /// ([Vol 3] Part F, Section 3.4.6.3).
    pub fn execute_write_req(&self) -> RspResult<bool> {
        self.unpack(ExecuteWriteReq, |p| Ok(p.bool()))
    }
}

/// Queued writes encoders ([Vol 3] Part F, Section 3.4.6).
impl<T: host::Transport> Bearer<T> {
    /// Returns an `ATT_PREPARE_WRITE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.6.2).
    pub fn prepare_write_rsp(&self, hdl: Handle, off: u16, v: &[u8]) -> RspResult<Rsp<T>> {
        self.rsp(PrepareWriteRsp, |p| {
            p.u16(hdl).u16(off).put(v);
        })
    }

    /// Returns an `ATT_EXECUTE_WRITE_RSP` PDU
    /// ([Vol 3] Part F, Section 3.4.6.4).
    pub fn execute_write_rsp(&self) -> RspResult<Rsp<T>> {
        self.rsp(ExecuteWriteRsp, |_| ())
    }
}

//
// Server initiated ([Vol 3] Part F, Section 3.4.7)
//

/// Server initiated encoders ([Vol 3] Part F, Section 3.4.7).
impl<T: host::Transport> Bearer<T> {
    /// Sends an `ATT_HANDLE_VALUE_NTF` PDU ([Vol 3] Part F, Section 3.4.7.1).
    pub async fn handle_value_ntf(&self, hdl: Handle, v: &[u8]) -> Result<()> {
        let ntf = self.pack(HandleValueNtf, |p| put_truncate(p.u16(hdl), v));
        self.send(ntf).await
    }

    /// Sends an `ATT_HANDLE_VALUE_IND` PDU and waits for the confirmation
    /// ([Vol 3] Part F, Section 3.4.7.2).
    pub async fn handle_value_ind(&self, hdl: Handle, v: &[u8]) -> Result<()> {
        let ind = self.pack(HandleValueInd, |p| put_truncate(p.u16(hdl), v));
        self.send(ind).await?;
        drop(self.recv_rsp(HandleValueCfm).await?);
        Ok(())
    }

    /// Sends an `ATT_MULTIPLE_HANDLE_VALUE_NTF` PDU
    /// ([Vol 3] Part F, Section 3.4.7.4).
    pub async fn multiple_handle_value_ntf(
        &self,
        it: impl Iterator<Item = (Handle, &[u8])> + Send,
    ) -> Result<()> {
        let ntf = self.pack(MultipleHandleValueNtf, |p| {
            for (hdl, v) in it {
                p.u16(hdl)
                    .u16(u16::try_from(v.len()).expect("attribute value too long"))
                    .put(v);
            }
        });
        self.send(ntf).await
    }
}

/// Consumes any remaining bytes in `p`.
#[inline]
fn take<'a>(p: &mut Unpacker<'a>) -> &'a [u8] {
    p.take().into_inner()
}

/// Writes `v` to `p`, possibly truncating the written value.
#[inline]
fn put_truncate(p: &mut Packer, v: &[u8]) {
    // SAFETY: Range is always <= v.len()
    p.put(unsafe { v.get_unchecked(..v.len().min(p.remaining())) });
}
