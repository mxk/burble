use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::mem::{align_of, size_of};

use scopeguard::{guard, ScopeGuard};
use tracing::{trace, warn};

use crate::hci::ACL_HDR;

use super::*;

/// Received PDU fragment queue. Inbound ACL data transfers are recombined and
/// placed into per-channel queues.
#[derive(Debug)]
pub(super) struct RxQueue<T: host::Transport> {
    /// CID of the current PDU for each connection handle. Used to route
    /// continuation fragments to the appropriate queue.
    cont: HashMap<ConnHandle, Cid>,
    /// Per-channel PDU queue.
    pdu: HashMap<ConnCid, VecDeque<RawPdu<T>>>,
    /// Transfer allocator.
    alloc: Alloc<T>,
}

impl<T: host::Transport> RxQueue<T> {
    /// Creates a new receive transfer queue.
    #[inline]
    #[must_use]
    pub fn new(max_frag_len: usize) -> Self {
        Self {
            cont: HashMap::new(),
            pdu: HashMap::new(),
            alloc: Alloc::rx(max_frag_len),
        }
    }

    /// Registers a new channel, allowing it to receive data.
    pub fn register_chan(&mut self, cc: ConnCid) {
        assert!(cc.is_valid());
        assert!(!self.pdu.contains_key(&cc));
        if let Entry::Vacant(e) = self.cont.entry(cc.hdl) {
            e.insert(Cid::INVALID);
        }
        self.pdu.insert(cc, VecDeque::new());
    }

    /// Removes channel registration, dropping any unconsumed PDU fragments.
    pub fn remove_chan(&mut self, cc: ConnCid) {
        if let Some(queue) = self.pdu.remove(&cc) {
            self.alloc
                .free_xfers(queue.into_iter().filter_map(|mut pdu| pdu.xfer.take()));
        }
        if !self.pdu.keys().any(|other| other.hdl == cc.hdl) {
            self.cont.remove(&cc.hdl); // Last channel for this connection
        }
    }

    /// Recombines a received PDU fragment. It returns `Some(ConnCid)` if a new
    /// complete PDU is ready for that channel. The channel may have other
    /// complete PDUs already available even if `None` is returned.
    pub fn recombine(&mut self, xfer: T::Transfer) -> Option<ConnCid> {
        let xfer = guard(xfer, |xfer| self.alloc.free_xfer(xfer));
        let pkt: &[u8] = xfer.as_ref();
        trace!("PDU fragment: {pkt:02x?}");
        let (cn, is_first, data) = acl_hdr(pkt)?;
        let Entry::Occupied(mut cont) = self.cont.entry(cn) else {
            warn!("PDU fragment for an unknown {cn:?}");
            return None;
        };
        if is_first {
            self.ensure_complete(ConnCid::new(cn, *cont.get()));
            let (pdu_len, cid, payload) = l2cap_hdr(data)?;
            cont.insert(cid);
            let cc = ConnCid::new(cn, cid);
            let Some(queue) = self.pdu.get_mut(&cc) else {
                warn!("PDU fragment for an unknown {cc:?}");
                return None;
            };
            return if pdu_len == payload.len() {
                queue.push_back(RawPdu::complete(ScopeGuard::into_inner(xfer)));
                Some(cc)
            } else {
                queue.push_back(RawPdu::first(pdu_len, pkt));
                None
            };
        }
        let cc = ConnCid::new(cn, *cont.get());
        let Some(pdu) = self.pdu.get_mut(&cc).and_then(|queue| queue.back_mut()) else {
            warn!("Unexpected continuation PDU fragment for {cc:?}");
            return None;
        };
        let rem_len = pdu.rem_len().cmp(&data.len());
        if rem_len.is_ge() {
            pdu.buf.extend_from_slice(data);
            return rem_len.is_eq().then_some(cc);
        }
        self.ensure_complete(cc);
        warn!(
            "PDU fragment for {cc:?} exceeds expected length (want={}, have={})",
            pdu.rem_len(),
            data.len()
        );
        None
    }

    /// Returns the next complete PDU for the specified channel.
    #[inline]
    pub fn next(&mut self, cc: ConnCid) -> Option<RawPdu<T>> {
        let queue = self.pdu.get_mut(&cc)?;
        if queue.front()?.is_complete() {
            queue.pop_front()
        } else {
            None
        }
    }

    /// Removes the last PDU from the queue if it is incomplete.
    #[inline]
    fn ensure_complete(&mut self, cc: ConnCid) {
        if let Some(queue) = self.pdu.get_mut(&cc) {
            if queue.back().map_or(false, |pdu| !pdu.is_complete()) {
                warn!("Incomplete PDU for {cc:?}");
                queue.pop_back().map(|pdu| self.alloc.free_pdu(pdu));
            }
        }
    }
}

/// Parses ACL data packet header ([Vol 4] Part E, Section 5.4.2).
#[inline]
#[must_use]
fn acl_hdr(pkt: &[u8]) -> Option<(ConnHandle, bool, &[u8])> {
    debug_assert_eq!(pkt.as_ptr() as usize % align_of::<u16>(), 0);
    if pkt.len() < ACL_HDR {
        warn!("ACL data packet with missing header: {pkt:02x?}");
        return None;
    }
    let (hdr, data) = pkt.split_at(ACL_HDR);
    // SAFETY: hdr is a properly aligned &[u16; 2]
    let hdr: &[u16; ACL_HDR / size_of::<u16>()] = unsafe { &*hdr.as_ptr().cast() };
    let (hdl, len) = (u16::from_le(hdr[0]), u16::from_le(hdr[1]));
    if data.len() != usize::from(len) {
        warn!("ACL data packet length mismatch: {pkt:02x?}");
        return None;
    }
    let is_first = (hdl >> 12) & 0b11 != 0b01;
    if data.len() < L2CAP_HDR {
        if is_first {
            warn!("ACL data packet with missing L2CAP header: {pkt:02x?}");
            return None;
        } else if data.is_empty() {
            warn!("ACL data packet without payload: {pkt:02x?}");
            return None;
        }
    }
    Some((ConnHandle::from_raw(hdl & 0xFFF), is_first, data))
}

/// Parses basic L2CAP header from ACL data packet payload
/// ([Vol 3] Part A, Section 3.1).
#[inline]
#[must_use]
fn l2cap_hdr(data: &[u8]) -> Option<(usize, Cid, &[u8])> {
    let (hdr, payload) = data.split_at(L2CAP_HDR);
    // SAFETY: acl_hdr() validated data alignment and minimum length
    let hdr: &[u16; L2CAP_HDR / size_of::<u16>()] = unsafe { &*hdr.as_ptr().cast() };
    let (len, cid) = (u16::from_le(hdr[0]), u16::from_le(hdr[1]));
    if usize::from(len) < payload.len() {
        warn!("PDU length mismatch: {data:02x?}");
        return None;
    }
    Some((usize::from(len), Cid::from_raw(cid), payload))
}
