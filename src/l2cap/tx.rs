use super::*;

/// Outbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    alloc: Arc<Alloc<T>>,
    sched: parking_lot::Mutex<Scheduler>,
    event: tokio::sync::Notify,
}

impl<T: host::Transport> State<T> {
    /// Returns a new transfer state.
    #[must_use]
    pub fn new(transport: T, max_pkts: usize, acl_data_len: u16) -> Self {
        Self {
            alloc: Alloc::new(transport, host::Direction::Out, acl_data_len),
            sched: parking_lot::Mutex::new(Scheduler::new(max_pkts)),
            event: tokio::sync::Notify::new(),
        }
    }

    /// Updates transmit queue status.
    #[inline]
    pub fn on_num_completed(&self, complete: &hci::NumberOfCompletedPackets) {
        let mut sched = self.sched.lock();
        sched.cq.pop(complete);
        self.event.notify_waiters();
    }

    /// Updates transmit queue status after a connection is disconnected
    /// ([Vol 4] Part E, Section 4.3).
    #[inline]
    pub fn on_disconnect(&self, cn: hci::ConnHandle) {
        let mut sched = self.sched.lock();
        sched.cq.remove_link(LeU::new(cn));
        self.event.notify_waiters();
    }
}

/// Outbound PDU scheduler. Uses round-robin scheduling to share the
/// controller's transmit buffer among all logical links.
#[derive(Debug)]
pub(super) struct Scheduler {
    /// Channels that are blocked from sending because another channel on the
    /// same logical link is sending a PDU ([Vol 3] Part A, Section 7.2.1).
    ready: HashMap<LeU, VecDeque<Cid>>,
    /// Channels that are allowed to send PDU fragments as soon as buffer space
    /// becomes available.
    next: VecDeque<LeUCid>,
    /// Controller's transmit queue status.
    cq: ControllerQueue,
}

impl Scheduler {
    /// Creates a new outbound PDU scheduler.
    #[must_use]
    fn new(max_pkts: usize) -> Self {
        Self {
            ready: HashMap::new(),
            next: VecDeque::new(),
            cq: ControllerQueue::new(max_pkts),
        }
    }

    /// Registers a new LE-U logical link, allowing it to send data.
    fn register_link(&mut self, link: LeU) {
        self.ready.entry(link).or_default();
    }

    /// Removes LE-U logical link registration.
    fn remove_link(&mut self, link: LeU) {
        self.ready.remove(&link);
        if let Some(i) = self.next.iter().position(|other| other.link == link) {
            self.next.remove(i);
        }
        self.cq.remove_link(link);
    }

    /// Schedules a channel for sending a PDU.
    fn schedule_sender(&mut self, lc: LeUCid) -> Result<()> {
        let Some(ready) = self.ready.get_mut(&lc.link) else {
            return Err(Error::InvalidConn(lc.link.0));
        };
        let Some(same_cn) = self.next.iter().find(|other| other.link == lc.link) else {
            self.next.push_back(lc); // One sender for this logical link
            return Ok(());
        };
        if same_cn.cid != lc.cid && !ready.contains(&lc.cid) {
            ready.push_back(lc.cid); // Multiple senders for this logical link
            return Ok(());
        }
        unreachable!("One channel must not send two PDUs concurrently");
    }

    /// Returns whether channel `lc` is allowed to send the next PDU fragment.
    #[inline]
    fn may_send(&self, lc: LeUCid) -> Result<bool> {
        if self.ready.contains_key(&lc.link) {
            Ok(!self.cq.is_full() && self.next.front() == Some(&lc))
        } else {
            Err(Error::InvalidConn(lc.link.0))
        }
    }

    /// Removes channel `lc` from the scheduler.
    fn remove_sender(&mut self, lc: LeUCid) {
        let Some(ready) = self.ready.get_mut(&lc.link) else { return };
        let Some(i) = find_val(&self.next, lc) else {
            find_val(ready, lc.cid).map(|i| ready.remove(i));
            return;
        };
        match ready.pop_front() {
            Some(next) => self.next[i].cid = next,
            None => {
                self.next.remove(i);
            }
        }
    }
}

/// Outgoing PDU.
#[derive(Debug)]
pub(super) struct Pdu<T: host::Transport> {
    rm: Arc<ResourceManager<T>>,
    lc: LeUCid,
    raw: RawPdu<T>,
}

impl<T: host::Transport> Pdu<T> {
    /// Allocates a new PDU.
    pub fn new(rm: Arc<ResourceManager<T>>, lc: LeUCid, max_frame_len: usize) -> Self {
        let mut raw = rm.tx.alloc.pdu(max_frame_len);
        raw.buf_mut().put_bytes(0, ACL_HDR + L2CAP_HDR);
        Self { rm, lc, raw }
    }

    /// Returns the PDU buffer, which starts with an ACL data packet header.
    #[inline]
    #[must_use]
    pub fn buf_mut(&mut self) -> &mut BytesMut {
        self.raw.buf_mut()
    }

    /// Sends the PDU, returning as soon as the last fragment is submitted to
    /// the controller.
    #[inline]
    pub async fn send(self) -> Result<()> {
        TxTask::new(self)?.send().await
    }
}

/// Scheduled PDU transmit task.
#[derive(Debug)]
struct TxTask<T: host::Transport> {
    pdu: Pdu<T>,
    xfer: Option<AclTransfer<T>>,
}

impl<T: host::Transport> TxTask<T> {
    /// Schedules the PDU for transmission.
    #[inline]
    fn new(pdu: Pdu<T>) -> Result<Self> {
        pdu.rm.tx.sched.lock().schedule_sender(pdu.lc)?;
        Ok(Self { pdu, xfer: None })
    }

    /// Performs PDU fragmentation and submits each fragment to the controller.
    async fn send(mut self) -> Result<()> {
        // Update basic L2CAP header ([Vol 3] Part A, Section 3.1)
        let data = &mut self.pdu.raw.buf_mut()[ACL_HDR..];
        let (mut l2cap_hdr, payload) = data.split_at_mut(L2CAP_HDR);
        l2cap_hdr.put_u16_le(u16::try_from(payload.len()).unwrap());
        l2cap_hdr.put_u16_le(self.pdu.lc.cid.0);

        // Fast path for single-fragment PDUs
        if matches!(self.pdu.raw, RawPdu::Xfer(_)) {
            self.may_send().await?;
            let xfer = self.pdu.raw.take_xfer().unwrap();
            return (self.send_frag(xfer, false).await)
                .map(|xfer| self.pdu.raw = RawPdu::Xfer(xfer));
        }

        // Fragmentation path
        let mut xfer = self.pdu.rm.tx.alloc.xfer();
        let data = self.pdu.raw.as_ref();
        for (i, frag) in data.chunks(self.pdu.rm.tx.alloc.acl_data_len).enumerate() {
            self.may_send().await?;
            xfer.reset();
            let buf = xfer.buf_mut();
            buf.put_bytes(0, ACL_HDR);
            buf.extend_from_slice(frag);
            xfer = self.send_frag(xfer, i != 0).await?;
        }
        Ok(())
    }

    /// Waits until the controller has space for another ACL data packet and the
    /// current task is the next one scheduled to transmit.
    async fn may_send(&self) -> Result<()> {
        loop {
            let notify = {
                let sched = self.pdu.rm.tx.sched.lock();
                if sched.may_send(self.pdu.lc)? {
                    return Ok(());
                }
                self.pdu.rm.tx.event.notified()
            };
            self.pdu.rm.event(notify).await?;
        }
    }

    async fn send_frag(&self, mut xfer: AclTransfer<T>, cont: bool) -> Result<AclTransfer<T>> {
        // Update HCI ACL data packet header ([Vol 4] Part E, Section 5.4.2)
        let (mut hdr, data) = xfer.buf_mut().split_at_mut(ACL_HDR);
        hdr.put_u16_le(u16::from(cont) << 12 | u16::from(self.pdu.lc.link));
        hdr.put_u16_le(u16::try_from(data.len()).unwrap());
        let xfer = xfer.submit().await?;

        // Pass send permission to the next logical link
        let mut sched = self.pdu.rm.tx.sched.lock();
        sched.cq.push(self.pdu.lc);
        if sched.next.len() > 1 {
            sched.next.rotate_left(1);
            self.pdu.rm.tx.event.notify_waiters();
        }
        Ok(xfer)
    }
}

impl<T: host::Transport> Drop for TxTask<T> {
    fn drop(&mut self) {
        let mut sched = self.pdu.rm.tx.sched.lock();
        sched.remove_sender(self.pdu.lc);
        self.pdu.rm.tx.event.notify_waiters();
    }
}

/// Bookkeeping information about the order of ACL data packets (PDU fragments)
/// submitted to the controller.
#[derive(Debug, Default)]
struct ControllerQueue {
    queue: VecDeque<LeUCid>,
    complete: HashMap<LeU, u16>,
    max_pkts: usize,
}

impl ControllerQueue {
    /// Returns a new transfer queue.
    #[inline]
    #[must_use]
    fn new(max_pkts: usize) -> Self {
        assert!(max_pkts > 0);
        Self {
            queue: VecDeque::with_capacity(max_pkts),
            complete: HashMap::with_capacity(4),
            max_pkts,
        }
    }

    /// Returns whether the transfer queue is full.
    #[inline]
    #[must_use]
    fn is_full(&self) -> bool {
        self.queue.len() >= self.max_pkts
    }

    /// Clears the queue of all packets for the specified link.
    #[inline]
    fn remove_link(&mut self, link: LeU) {
        self.queue.retain(|lc| lc.link != link);
    }

    /// Adds channel `lc` to the queue. This must be called after a successful
    /// data transfer to the controller.
    #[inline]
    fn push(&mut self, lc: LeUCid) {
        assert!(!self.is_full());
        self.queue.push_back(lc);
    }

    /// Removes completed packets from the queue.
    fn pop(&mut self, complete: &hci::NumberOfCompletedPackets) {
        for &(cn, count) in complete.as_ref() {
            if count > 0 {
                self.complete.insert(LeU::new(cn), count);
            }
        }
        self.queue.retain(|lc| {
            let Entry::Occupied(mut e) = self.complete.entry(lc.link) else {
                return true;
            };
            match e.get_mut() {
                &mut 1 => {
                    e.remove();
                }
                count => *count -= 1,
            }
            false
        });
        assert!(self.complete.is_empty());
    }
}

/// Returns the index of `v` in `q`.
#[allow(clippy::needless_pass_by_value)]
#[inline]
fn find_val<V: Eq>(q: &VecDeque<V>, v: V) -> Option<usize> {
    q.iter().position(|other| *other == v)
}

// TODO: Remove
/// Removes value `v` from `q` and returns its index.
#[inline]
fn remove_val<V: Eq>(q: &mut VecDeque<V>, v: V) {
    find_val(q, v).map(|i| q.remove(i));
}
