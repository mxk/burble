//! Receive side of the Resource Manager.

use structbuf::Unpacker;
use tracing::{error, trace, warn};

use super::*;

/// Inbound PDU receiver. Recombines PDU fragments and routes the PDUs to their
/// channels.
#[derive(Debug)]
pub(super) struct Receiver {
    /// Received transfer channel.
    xfer: tokio::sync::mpsc::Receiver<Box<dyn host::Transfer>>,
    /// Receive loop handle.
    join: Option<tokio::task::JoinHandle<host::Result<()>>>,
    /// CID of the current PDU for each logical link. Used to route continuation
    /// fragments to the appropriate channel ([Vol 3] Part A, Section 7.2.1).
    cont: HashMap<LeU, Option<Cid>>,
    /// Registered channels.
    chans: HashMap<LeCid, ChanBuf>,
}

impl Receiver {
    /// Creates a new inbound PDU receiver.
    #[inline]
    #[must_use]
    pub fn new(t: &Arc<dyn host::Transport>, acl_data_len: u16) -> Self {
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        let alloc = Alloc::new(t, host::Direction::In, acl_data_len);
        Self {
            xfer: rx,
            join: Some(tokio::task::spawn(Self::recv_loop(alloc, tx))),
            cont: HashMap::new(),
            chans: HashMap::new(),
        }
    }

    /// Receives PDU fragments until a fatal transport error is encountered.
    /// This method is cancel safe.
    #[inline]
    pub async fn recv(&mut self) -> host::Result<()> {
        while let Some(xfer) = self.xfer.recv().await {
            // TODO: Reuse transfer
            self.recombine(xfer);
        }
        let join = self.join.take().expect("receive loop already joined");
        join.await.expect("receive loop panic")
    }

    /// Registers an LE-U channel, allowing it to receive data.
    pub fn register_chan(&mut self, ch: &Arc<RawChan>) {
        trace!("Adding channel: {}", ch.cid);
        self.cont.entry(ch.cid.link).or_default();
        let prev = self.chans.insert(ch.cid, ChanBuf::new(ch));
        debug_assert!(prev.is_none());
    }

    /// Removes channel registration. Any incomplete PDU is discarded.
    pub fn remove_chan(&mut self, cid: LeCid) {
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

    /// Recombines a received PDU fragment ([Vol 3] Part A, Section 7.2.2).
    fn recombine(&mut self, xfer: Box<dyn host::Transfer>) {
        let pkt = (*xfer).as_ref();
        let Some((link, l2cap_hdr, data)) = parse_hdr(pkt) else { return };
        let Some(cont_cid) = self.cont.get_mut(&link) else {
            warn!("PDU fragment for an unknown {link}: {pkt:02X?}");
            return;
        };
        if let Some((pdu_len, cid)) = l2cap_hdr {
            if let Some(cid) = *cont_cid {
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
            let is_first = usize::from(pdu_len) != data.len();
            trace!(
                "{cid}{}: {:02X?}",
                if is_first { " (first)" } else { "" },
                &pkt[ACL_HDR + L2CAP_HDR..]
            );
            ch.first(pdu_len, xfer);
            if !ch.buf.is_none() {
                *cont_cid = Some(cid.chan);
            }
        } else {
            let Some(cid) = *cont_cid else {
                warn!("Unexpected PDU continuation fragment for {link}: {pkt:02X?}");
                return;
            };
            trace!("{cid} (cont.): {:02X?}", &pkt[ACL_HDR..]);
            let ch = self.chans.get_mut(&link.chan(cid)).unwrap();
            ch.cont(data);
            if ch.buf.is_none() {
                *cont_cid = None;
            }
        }
    }

    /// Receives ACL data packets and sends them via a channel. This makes
    /// [`Self::recv()`] cancel safe.
    async fn recv_loop(
        alloc: Alloc,
        tx: tokio::sync::mpsc::Sender<Box<dyn host::Transfer>>,
    ) -> host::Result<()> {
        loop {
            tokio::select! {
                xfer = alloc.xfer().submit()? => {
                    if tx.send(xfer?).await.is_err() {
                        break;
                    }
                }
                _ = tx.closed() => break,
            }
        }
        Ok(())
    }
}

/// Channel PDU recombination buffer.
#[derive(Debug)]
struct ChanBuf {
    /// Destination channel.
    raw: Arc<RawChan>,
    /// PDU recombination buffer.
    buf: StructBuf,
}

impl ChanBuf {
    /// Creates a new PDU recombination buffer.
    #[inline(always)]
    #[must_use]
    pub fn new(ch: &Arc<RawChan>) -> Self {
        Self {
            raw: Arc::clone(ch),
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
    pub fn first(&mut self, pdu_len: u16, xfer: Box<dyn host::Transfer>) {
        self.ensure_complete();
        let frame_len = L2CAP_HDR + usize::from(pdu_len);
        let mut cs = self.raw.state.lock();
        if !cs.can_recv(self.raw.cid, frame_len) {
            return;
        }
        if (*xfer).as_ref().len() == ACL_HDR + frame_len {
            cs.push(self.raw.cid, Frame::complete(xfer));
        } else {
            self.buf = Frame::first(&*xfer, frame_len);
        }
    }

    /// Receives a continuation PDU fragment.
    pub fn cont(&mut self, acl_data: &[u8]) {
        let mut p = self.buf.append();
        if p.can_put(acl_data.len()) {
            p.put(acl_data);
            if self.buf.is_full() {
                let buf = Frame::Buf(self.buf.take());
                self.raw.state.lock().push(self.raw.cid, buf);
            }
        } else {
            error!(
                "PDU fragment for {} exceeds expected length ({} > {})",
                self.raw.cid,
                acl_data.len(),
                self.buf.remaining()
            );
            self.buf = StructBuf::none();
            self.raw.set_error();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hdr() {
        use super::parse_hdr;
        let link = LeU::new(hci::ConnHandle::new(0xEFF).unwrap());
        let cid = link.chan(Cid::ATT);

        let mut p = StructBuf::new(ACL_HDR + L2CAP_HDR + 2);
        assert_eq!(parse_hdr(p.as_ref()), None);

        p.append().u16(0xF00_u16).u16(0_u16);
        assert_eq!(parse_hdr(p.as_ref()), None);

        p.clear().append().u16(link).u16(0_u16);
        assert_eq!(parse_hdr(p.as_ref()), None);

        p.clear()
            .append()
            .u16(link)
            .u16(4_u16)
            .u16(0_u16)
            .u16(cid.chan);
        assert_eq!(
            parse_hdr(p.as_ref()),
            Some((link, Some((0, cid.chan)), Default::default()))
        );
    }
}
