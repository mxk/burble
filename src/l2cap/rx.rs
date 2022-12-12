//! Receive side of the Resource Manager.

use std::u16;

use structbuf::Unpacker;
use tracing::{error, trace, warn};

use crate::util::CondvarGuard;

use super::*;

/// Inbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    xfer: tokio::sync::mpsc::Receiver<host::Result<AclTransfer<T>>>,
    recv: parking_lot::Mutex<Receiver<T>>, // TODO: Get rid of Mutex
}

impl<T: host::Transport + 'static> State<T> {
    /// Creates a new inbound transfer state.
    #[must_use]
    pub fn new(transport: T, acl_data_len: u16) -> Self {
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        tokio::task::spawn(Self::recv_task(transport, acl_data_len, tx));
        Self {
            xfer: rx,
            recv: parking_lot::Mutex::new(Receiver::new()),
        }
    }

    /// Receives one or more PDU fragments, and returns the CID of the channel
    /// that has a new complete PDU. This method is cancel safe.
    #[inline]
    pub async fn recv(&mut self) -> Result<LeCid> {
        loop {
            let xfer = self.xfer.recv().await.unwrap()?;
            if let Some(cid) = self.recv.lock().recombine(xfer) {
                return Ok(cid);
            }
        }
    }

    /// Registers a new LE-U channel.
    #[inline]
    pub fn register_chan(&self, ch: &Arc<RawChan<T>>) {
        self.recv.lock().register_chan(Arc::clone(ch));
    }

    /// Removes LE-U channel registration.
    #[inline]
    pub fn remove_chan(&self, cid: LeCid) {
        self.recv.lock().remove_chan(cid);
    }

    /// Receives ACL data packets and sends them via a channel. This makes
    /// `recv()` cancel safe.
    async fn recv_task(
        transport: T,
        acl_data_len: u16,
        ch: tokio::sync::mpsc::Sender<host::Result<AclTransfer<T>>>,
    ) {
        let alloc = Alloc::new(transport, host::Direction::In, acl_data_len);
        loop {
            let r = tokio::select! {
                r = alloc.xfer().submit() => r,
                _ = ch.closed() => break,
            };
            if ch.send(r).await.is_err() {
                break;
            }
        }
    }
}

/// Inbound PDU receiver. Recombines PDU fragments and routes the PDUs to their
/// channels.
#[derive(Debug)]
pub(super) struct Receiver<T: host::Transport> {
    /// CID of the current PDU for each logical link. Used to route continuation
    /// fragments to the appropriate channel ([Vol 3] Part A, Section 7.2.1).
    cont: HashMap<LeU, Option<Cid>>,
    /// Registered channels.
    chans: HashMap<LeCid, Chan<T>>,
}

impl<T: host::Transport> Receiver<T> {
    /// Creates a new PDU receiver.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            cont: HashMap::new(),
            chans: HashMap::new(),
        }
    }

    /// Registers a new LE-U channel, allowing it to receive data.
    fn register_chan(&mut self, ch: Arc<RawChan<T>>) {
        trace!("Adding channel: {}", ch.cid);
        self.cont.entry(ch.cid.link).or_default();
        assert!(self.chans.insert(ch.cid, Chan::new(ch)).is_none());
    }

    /// Removes channel registration. Any incomplete PDU is discarded.
    fn remove_chan(&mut self, cid: LeCid) {
        let Some(_) = self.chans.remove(&cid) else { return };
        trace!("Removing channel: {}", cid);
        if !self.chans.keys().any(|other| other.link == cid.link) {
            self.cont.remove(&cid.link); // Last channel for this logical link
            return;
        }
        let cont = self.cont.get_mut(&cid.link).unwrap();
        if *cont == Some(cid.chan) {
            *cont = None;
        }
    }

    /// Recombines a received PDU fragment, and returns the CID of the channel
    /// that has a new complete PDU ([Vol 3] Part A, Section 7.2.2).
    fn recombine(&mut self, xfer: AclTransfer<T>) -> Option<LeCid> {
        let pkt = xfer.as_ref();
        let Some((link, l2cap_hdr, data)) = parse_hdr(pkt) else { return None };
        let Some(cont_cid) = self.cont.get_mut(&link) else {
            warn!("PDU fragment for an unknown {link}: {pkt:02X?}");
            return None;
        };
        if let Some((pdu_len, cid)) = l2cap_hdr {
            if let Some(cid) = *cont_cid {
                // TODO: ChanManager will not notice this in handle_signal()
                (self.chans.get_mut(&link.chan(cid)).unwrap()).ensure_complete();
                *cont_cid = None;
            }
            if !cid.is_le() {
                // [Vol 3] Part A, Section 3
                warn!("PDU fragment for an invalid {cid}: {pkt:02X?}");
                return None;
            }
            let cid = link.chan(cid);
            let Some(ch) = self.chans.get_mut(&cid) else {
                warn!("PDU fragment for an unknown {cid}: {pkt:02X?}");
                return None;
            };
            trace!("PDU fragment for {cid}: {pkt:02X?}");
            return ch.first(pdu_len, xfer).or_else(|| {
                if !ch.buf.is_full() {
                    *cont_cid = Some(cid.chan);
                }
                None
            });
        }
        let Some(cid) = *cont_cid else {
            warn!("Unexpected continuation PDU fragment for {link}: {pkt:02X?}");
            return None;
        };
        trace!("Cont. PDU fragment for {cid}: {pkt:02X?}");
        let ch = self.chans.get_mut(&link.chan(cid)).unwrap();
        let r = ch.cont(data);
        if ch.buf.is_full() {
            *cont_cid = None;
        }
        r
    }
}

/// Channel PDU recombination state.
#[derive(Debug)]
struct Chan<T: host::Transport> {
    /// Destination channel.
    raw: Arc<RawChan<T>>,
    /// PDU recombination buffer.
    buf: StructBuf,
}

impl<T: host::Transport> Chan<T> {
    /// Maximum number of PDUs that may be queued. Reaching this limit likely
    /// means that the channel is broken and isn't receiving data.
    const MAX_PDUS: usize = 64;

    /// Creates PDU receive state for channel `ch`.
    #[inline]
    #[must_use]
    pub fn new(ch: Arc<RawChan<T>>) -> Self {
        Self {
            raw: ch,
            buf: StructBuf::new(0),
        }
    }

    /// Sets the channel error flag if the most recent PDU is incomplete.
    #[inline]
    fn ensure_complete(&mut self) {
        if self.buf.lim() > 0 {
            self.buf = StructBuf::new(0);
            error!("Incomplete PDU for {}", self.raw.cid);
            self.raw.set_error();
        }
    }

    /// Receives the first, possibly complete, PDU fragment.
    pub fn first(&mut self, pdu_len: u16, xfer: AclTransfer<T>) -> Option<LeCid> {
        self.ensure_complete();
        let cs = self.raw.state.lock();
        if !cs.is_ok() {
            return None;
        }
        let frame_len = L2CAP_HDR + usize::from(pdu_len);
        if frame_len > cs.max_frame_len {
            error!(
                "PDU for {} exceeds maximum frame size ({} > {})",
                self.raw.cid, frame_len, cs.max_frame_len
            );
            chan::State::set_fatal(cs, Status::ERROR);
            return None;
        }
        let complete_len = ACL_HDR + frame_len;
        if xfer.as_ref().len() == complete_len {
            self.complete(cs, RawBuf::Transfer(xfer))
        } else {
            self.buf = StructBuf::with_capacity(complete_len);
            self.buf.put_at(0, xfer.as_ref());
            None
        }
    }

    /// Receives a continuation PDU fragment.
    pub fn cont(&mut self, acl_data: &[u8]) -> Option<LeCid> {
        let mut p = self.buf.pack();
        if !p.can_put(acl_data.len()) {
            error!(
                "PDU fragment for {} exceeds expected length ({} > {})",
                self.raw.cid,
                acl_data.len(),
                self.buf.remaining()
            );
            self.buf = StructBuf::new(0);
            self.raw.set_error();
            return None;
        }
        p.put(acl_data);
        if self.buf.is_full() {
            let buf = RawBuf::Buf(mem::replace(&mut self.buf, StructBuf::new(0)));
            self.complete(self.raw.state.lock(), buf)
        } else {
            None
        }
    }

    /// Adds a complete PDU to the channel queue and notifies any waiters.
    pub fn complete(&self, mut cs: CondvarGuard<chan::State<T>>, raw: RawBuf<T>) -> Option<LeCid> {
        if !cs.is_ok() {
            return None;
        }
        if cs.pdu.len() == Self::MAX_PDUS {
            error!("PDU queue overflow for {}", self.raw.cid);
            chan::State::set_fatal(cs, Status::ERROR);
            return None;
        }
        trace!("New PDU for {}", self.raw.cid);
        cs.pdu.push_back(raw);
        if cs.pdu.len() == 1 {
            cs.notify_all();
        }
        Some(self.raw.cid)
    }
}

/// Parses and validates ACL data packet and basic L2CAP headers
/// ([Vol 4] Part E, Section 5.4.2 and [Vol 3] Part A, Section 3.1).
#[allow(clippy::type_complexity)]
#[must_use]
fn parse_hdr(pkt: &[u8]) -> Option<(LeU, Option<(u16, Cid)>, &[u8])> {
    let mut p = Unpacker::new(pkt);
    let Some(mut hdr) = p.skip(ACL_HDR) else {
        error!("ACL data packet with missing header: {pkt:02X?}");
        return None;
    };
    let cn_flag = hdr.u16();
    let Some(cn) = hci::ConnHandle::new(cn_flag) else {
        error!("ACL data packet for an invalid connection handle: {pkt:02X?}");
        return None;
    };
    if p.len() != usize::from(hdr.u16()) {
        error!("ACL data packet length mismatch: {pkt:02X?}");
        return None;
    }
    let is_first = (cn_flag >> hci::ConnHandle::BITS) & 0b11 != 0b01;
    let l2cap_hdr = if is_first {
        let Some(mut hdr) = p.skip(L2CAP_HDR) else {
            error!("ACL data packet with missing L2CAP header: {pkt:02X?}");
            return None;
        };
        let pdu_len = hdr.u16();
        if usize::from(pdu_len) < p.len() {
            error!("ACL data packet with an invalid PDU length: {pkt:02X?}");
            return None;
        }
        let Some(cid) = Cid::new(hdr.u16()) else {
            error!("ACL data packet for an invalid CID: {pkt:02X?}");
            return None;
        };
        Some((pdu_len, cid))
    } else if p.is_empty() {
        warn!("ACL data packet without payload: {pkt:02X?}");
        return None;
    } else {
        None
    };
    p.into_inner().map(|data| (LeU::new(cn), l2cap_hdr, data))
}
