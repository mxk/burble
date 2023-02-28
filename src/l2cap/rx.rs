//! Receive side of the Resource Manager.

use structbuf::Unpacker;
use tracing::{error, trace, warn};

use super::*;

/// Inbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State {
    xfer: tokio::sync::mpsc::Receiver<host::Result<AclTransfer>>,
    recv: SyncMutex<Receiver>, // TODO: Get rid of Mutex
}

impl State {
    /// Creates a new inbound transfer state.
    #[must_use]
    pub fn new(t: &Arc<dyn host::Transport>, acl_data_len: u16) -> Self {
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        tokio::task::spawn(Self::recv_task(Arc::clone(t), acl_data_len, tx));
        Self {
            xfer: rx,
            recv: SyncMutex::new(Receiver::new()),
        }
    }

    /// Receives one or more PDU fragments, and returns the CID of the channel
    /// that has a new complete PDU. This method is cancel safe.
    #[inline]
    pub async fn recv(&mut self) -> Result<()> {
        loop {
            let xfer = self.xfer.recv().await.unwrap()?;
            self.recv.lock().recombine(xfer);
        }
    }

    /// Registers a new LE-U channel.
    #[inline]
    pub fn register_chan(&self, ch: &Arc<RawChan>) {
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
        t: Arc<dyn host::Transport>,
        acl_data_len: u16,
        ch: tokio::sync::mpsc::Sender<host::Result<AclTransfer>>,
    ) {
        let alloc = Alloc::new(t, host::Direction::In, acl_data_len);
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
pub(super) struct Receiver {
    /// CID of the current PDU for each logical link. Used to route continuation
    /// fragments to the appropriate channel ([Vol 3] Part A, Section 7.2.1).
    cont: HashMap<LeU, Option<Cid>>,
    /// Registered channels.
    chans: HashMap<LeCid, Chan>,
}

impl Receiver {
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
    fn register_chan(&mut self, ch: Arc<RawChan>) {
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
    fn recombine(&mut self, xfer: AclTransfer) {
        let pkt = xfer.as_ref();
        let Some((link, l2cap_hdr, data)) = parse_hdr(pkt) else { return };
        let Some(cont_cid) = self.cont.get_mut(&link) else {
            warn!("PDU fragment for an unknown {link}: {pkt:02X?}");
            return;
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
                return;
            }
            let cid = link.chan(cid);
            let Some(ch) = self.chans.get_mut(&cid) else {
                warn!("PDU fragment for an unknown {cid}: {pkt:02X?}");
                return;
            };
            trace!("{cid}: {:02X?}", &pkt[4 + 4..]); // Skip ACL and L2CAP headers
            ch.first(pdu_len, xfer);
            if !ch.buf.is_none() {
                *cont_cid = Some(cid.chan);
            }
            return;
        }
        let Some(cid) = *cont_cid else {
            warn!("Unexpected continuation PDU fragment for {link}: {pkt:02X?}");
            return;
        };
        trace!("Cont. PDU fragment for {cid}: {pkt:02X?}");
        let ch = self.chans.get_mut(&link.chan(cid)).unwrap();
        ch.cont(data);
        if ch.buf.is_none() {
            *cont_cid = None;
        }
    }
}

/// Channel PDU recombination state.
#[derive(Debug)]
struct Chan {
    /// Destination channel.
    raw: Arc<RawChan>,
    /// PDU recombination buffer.
    buf: StructBuf,
}

impl Chan {
    /// Creates PDU receive state for channel `ch`.
    #[inline]
    #[must_use]
    pub const fn new(ch: Arc<RawChan>) -> Self {
        Self {
            raw: ch,
            buf: StructBuf::none(),
        }
    }

    /// Sets the channel error flag if the most recent PDU is incomplete.
    #[inline]
    fn ensure_complete(&mut self) {
        if !self.buf.is_none() {
            self.buf = StructBuf::none();
            error!("Incomplete PDU for {}", self.raw.cid);
            self.raw.set_error();
        }
    }

    /// Receives the first, possibly incomplete, PDU fragment.
    pub fn first(&mut self, pdu_len: u16, xfer: AclTransfer) {
        self.ensure_complete();
        let mut cs = self.raw.state.lock();
        let frame_len = L2CAP_HDR + usize::from(pdu_len);
        if cs.can_recv(self.raw.cid, frame_len) {
            if xfer.as_ref().len() == ACL_HDR + frame_len {
                cs.push(self.raw.cid, Frame::complete(xfer));
            } else {
                self.buf = Frame::first(&xfer, frame_len);
            }
        }
    }

    /// Receives a continuation PDU fragment.
    pub fn cont(&mut self, acl_data: &[u8]) {
        let mut p = self.buf.append();
        if !p.can_put(acl_data.len()) {
            error!(
                "PDU fragment for {} exceeds expected length ({} > {})",
                self.raw.cid,
                acl_data.len(),
                self.buf.remaining()
            );
            self.buf = StructBuf::none();
            self.raw.set_error();
            return;
        }
        p.put(acl_data);
        if self.buf.is_full() {
            let buf = Frame::Buf(self.buf.take());
            self.raw.state.lock().push(self.raw.cid, buf);
        }
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
    Some((LeU::new(cn), l2cap_hdr, p.into_inner()))
}
