use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::mem::align_of;

use bytes::Buf;
use tracing::{error, trace, warn};

use super::*;

/// Inbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    alloc: Arc<Alloc<T>>,
    queue: parking_lot::Mutex<Queue<T>>,
    event: tokio::sync::Notify,
}

impl<T: host::Transport> State<T> {
    /// Returns a new transfer state.
    #[must_use]
    pub fn new(transport: T, acl_data_len: u16) -> Self {
        Self {
            alloc: Alloc::new(transport, host::Direction::In, acl_data_len),
            queue: parking_lot::Mutex::new(Queue::new()),
            event: tokio::sync::Notify::new(),
        }
    }
}

/// Inbound PDU queue. Inbound ACL data transfers are recombined and placed into
/// per-channel queues.
#[derive(Debug)]
pub(super) struct Queue<T: host::Transport> {
    /// CID of the current PDU for each logical link. Used to route continuation
    /// fragments to the appropriate queue ([Vol 3] Part A, Section 7.2.1).
    cont: HashMap<LeU, Cid>,
    /// Per-channel PDU queue.
    queue: HashMap<LeUCid, VecDeque<RawPdu<T>>>,
}

impl<T: host::Transport> Queue<T> {
    /// Maximum number of PDUs that may be queued per channel. Reaching this
    /// limit likely means that the channel is broken and isn't receiving data.
    const MAX_PDUS: usize = 128;

    /// Creates a new receive transfer queue.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            cont: HashMap::new(),
            queue: HashMap::new(),
        }
    }

    /// Registers a new channel, allowing it to receive data.
    pub fn register_chan(&mut self, lc: LeUCid) {
        assert!(!self.queue.contains_key(&lc));
        if let Entry::Vacant(e) = self.cont.entry(lc.link) {
            e.insert(Cid::INVALID);
        }
        self.queue.insert(lc, VecDeque::new());
    }

    /// Removes channel registration, dropping any unconsumed PDU fragments.
    pub fn remove_chan(&mut self, lc: LeUCid) {
        self.queue.remove(&lc);
        if !self.queue.keys().any(|other| other.link == lc.link) {
            self.cont.remove(&lc.link); // Last channel for this logical link
        }
    }

    /// Recombines a received PDU fragment ([Vol 3] Part A, Section 7.2.2). It
    /// returns `Some` if a new complete PDU is ready for that channel. The
    /// channel may have other complete PDUs already available even if `None` is
    /// returned.
    pub fn recombine(&mut self, xfer: AclTransfer<T>) -> Option<LeUCid> {
        let pkt: &[u8] = xfer.as_ref();
        trace!("PDU fragment: {pkt:02x?}");
        let (link, is_first, data) = acl_hdr(pkt)?;
        let Some(&prev_cid) = self.cont.get(&link) else {
            warn!("PDU fragment for an unknown {link:?}");
            return None;
        };
        if is_first {
            self.ensure_complete(LeUCid::new(link, prev_cid));
            let (pdu_len, cid, payload) = l2cap_hdr(data)?;
            self.cont.insert(link, cid);
            let lc = LeUCid::new(link, cid);
            let Some(queue) = self.queue.get_mut(&lc) else {
                warn!("PDU fragment for an unknown {lc:?}");
                return None;
            };
            if queue.len() > Self::MAX_PDUS {
                // TODO: Indicate that the channel is unreliable
                error!("PDU queue overflow for {lc:?}");
                return None;
            }
            return if pdu_len == payload.len() {
                queue.push_back(RawPdu::complete(xfer));
                Some(lc)
            } else {
                queue.push_back(RawPdu::first(pdu_len, pkt));
                None
            };
        }
        let lc = LeUCid::new(link, prev_cid);
        let Some(pdu) = self.queue.get_mut(&lc).and_then(VecDeque::back_mut) else {
            // TODO: Indicate that the channel is unreliable
            warn!("Unexpected continuation PDU fragment for {lc:?}");
            return None;
        };
        let rem_len = pdu.rem_len();
        let cmp = rem_len.cmp(&data.len());
        if cmp.is_ge() {
            pdu.buf_mut().extend_from_slice(data);
            return cmp.is_eq().then_some(lc);
        }
        self.ensure_complete(lc);
        // TODO: Indicate that the channel is unreliable
        warn!(
            "PDU fragment for {lc:?} exceeds expected length (want={rem_len}, have={})",
            data.len()
        );
        None
    }

    /// Returns the next complete PDU for the specified channel.
    #[inline]
    pub fn next(&mut self, lc: LeUCid) -> Option<RawPdu<T>> {
        let queue = self.queue.get_mut(&lc)?;
        if queue.front()?.is_complete() {
            queue.pop_front()
        } else {
            None
        }
    }

    /// Removes the last PDU from the queue if it is incomplete.
    #[inline]
    fn ensure_complete(&mut self, lc: LeUCid) {
        if let Some(queue) = self.queue.get_mut(&lc) {
            if queue.back().map_or(false, |pdu| !pdu.is_complete()) {
                warn!("Incomplete PDU for {lc:?}");
                queue.pop_back();
            }
        }
    }
}

/// Parses ACL data packet header ([Vol 4] Part E, Section 5.4.2).
#[inline]
#[must_use]
fn acl_hdr(pkt: &[u8]) -> Option<(LeU, bool, &[u8])> {
    debug_assert_eq!(pkt.as_ptr() as usize % align_of::<u16>(), 0);
    if pkt.len() < ACL_HDR {
        warn!("ACL data packet with missing header: {pkt:02x?}");
        return None;
    }
    let (mut hdr, data) = pkt.split_at(ACL_HDR); // TODO: Use split_at_unchecked
    let (cn, len) = (hdr.get_u16_le(), hdr.get_u16_le());
    if data.len() != usize::from(len) {
        warn!("ACL data packet length mismatch: {pkt:02x?}");
        return None;
    }
    let is_first = (cn >> 12) & 0b11 != 0b01;
    if data.len() < L2CAP_HDR {
        if is_first {
            warn!("ACL data packet with missing L2CAP header: {pkt:02x?}");
            return None;
        } else if data.is_empty() {
            warn!("ACL data packet without payload: {pkt:02x?}");
            return None;
        }
    }
    Some((LeU::from_raw(cn), is_first, data))
}

/// Parses basic L2CAP header from ACL data packet payload
/// ([Vol 3] Part A, Section 3.1).
#[inline]
#[must_use]
fn l2cap_hdr(data: &[u8]) -> Option<(usize, Cid, &[u8])> {
    let (mut hdr, payload) = data.split_at(L2CAP_HDR);
    let (len, cid) = (hdr.get_u16_le(), hdr.get_u16_le());
    if usize::from(len) < payload.len() {
        warn!("PDU length mismatch: {data:02x?}");
        return None;
    }
    Some((usize::from(len), Cid::from_raw(cid), payload))
}
