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
    pub fn new(transport: T, max_pkts: usize, max_frag_len: usize) -> Self {
        Self {
            alloc: Alloc::new(transport, host::Direction::Out, max_frag_len),
            sched: parking_lot::Mutex::new(Scheduler::new(max_pkts)),
            event: tokio::sync::Notify::new(),
        }
    }

    /// Updates transmit queue status.
    #[inline]
    pub fn update(&self, complete: &NumberOfCompletedPackets) {
        let mut sched = self.sched.lock();
        sched.queue.pop(complete);
        self.event.notify_waiters();
    }
}

/// Outbound PDU scheduler. Uses round-robin scheduling to share the
/// controller's transmit buffer among all connection handles.
#[derive(Debug)]
pub(super) struct Scheduler {
    /// Channels that are blocked from sending because another channel on the
    /// same connection is sending a PDU.
    ready: HashMap<ConnHandle, VecDeque<Cid>>,
    /// Channels that are allowed to send PDU fragments as soon as buffer space
    /// becomes available.
    next: VecDeque<ConnCid>,
    /// Controller's transmit queue status.
    queue: ControllerQueue,
}

impl Scheduler {
    /// Creates a new outbound PDU scheduler.
    #[must_use]
    pub fn new(max_pkts: usize) -> Self {
        Self {
            ready: HashMap::new(),
            next: VecDeque::new(),
            queue: ControllerQueue::new(max_pkts),
        }
    }

    /// Registers a new connection, allowing it to send data.
    pub fn register_conn(&mut self, cn: ConnHandle) {
        assert!(cn.is_valid());
        self.ready.entry(cn).or_default();
    }

    /// Removes connection registration.
    pub fn remove_conn(&mut self, cn: ConnHandle) {
        self.ready.remove(&cn);
        if let Some(i) = self.next.iter().position(|other| other.hdl == cn) {
            self.next.remove(i);
        }
    }

    /// Schedules a channel for sending a PDU.
    fn schedule_sender(&mut self, cc: ConnCid) -> Result<()> {
        let Some(ready) = self.ready.get_mut(&cc.hdl) else {
            return Err(super::Error::InvalidConn(cc.hdl));
        };
        let Some(same_cn) = self.next.iter().find(|other| other.hdl == cc.hdl) else {
            self.next.push_back(cc); // One sender for this connection
            return Ok(());
        };
        if same_cn.cid != cc.cid && !ready.contains(&cc.cid) {
            ready.push_back(cc.cid); // Multiple senders for this connection
            return Ok(());
        }
        unreachable!("One channel must not send two PDUs concurrently");
    }

    /// Returns whether channel `cc` is allowed to send the next PDU fragment.
    #[inline]
    fn may_send(&self, cc: ConnCid) -> Result<bool> {
        if self.ready.contains_key(&cc.hdl) {
            Ok(!self.queue.is_full() && self.next.front() == Some(&cc))
        } else {
            Err(super::Error::InvalidConn(cc.hdl))
        }
    }

    /// Removes channel `cc` from the scheduler.
    fn remove_sender(&mut self, cc: ConnCid) {
        let Some(ready) = self.ready.get_mut(&cc.hdl) else { return };
        let Some(i) = find_val(&self.next, cc) else {
            find_val(ready, cc.cid).map(|i| ready.remove(i));
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
    cc: ConnCid,
    raw: RawPdu<T>,
}

impl<T: host::Transport> Pdu<T> {
    /// Allocates a new PDU.
    pub fn new(rm: Arc<ResourceManager<T>>, cc: ConnCid, data_cap: usize) -> Self {
        let mut raw = rm.tx.alloc.pdu(data_cap);
        raw.buf_mut().put_bytes(0, ACL_HDR + L2CAP_HDR);
        Self { rm, cc, raw }
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
        pdu.rm.tx.sched.lock().schedule_sender(pdu.cc)?;
        Ok(Self { pdu, xfer: None })
    }

    /// Performs PDU fragmentation and submits each fragment to the controller.
    async fn send(mut self) -> Result<()> {
        // Update basic L2CAP header ([Vol 3] Part A, Section 3.1)
        let data = &mut self.pdu.raw.buf_mut()[ACL_HDR..];
        let (mut l2cap_hdr, payload) = data.split_at_mut(L2CAP_HDR);
        l2cap_hdr.put_u16_le(u16::try_from(payload.len()).unwrap());
        l2cap_hdr.put_u16_le(self.pdu.cc.cid.0);

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
        for (i, frag) in data.chunks(self.pdu.rm.tx.alloc.max_frag_len).enumerate() {
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
                if sched.may_send(self.pdu.cc)? {
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
        hdr.put_u16_le(u16::from(cont) << 12 | u16::from(self.pdu.cc.hdl));
        hdr.put_u16_le(u16::try_from(data.len()).unwrap());
        let xfer = xfer.submit().await?;

        // Pass send permission to the next connection handle
        let mut sched = self.pdu.rm.tx.sched.lock();
        sched.queue.push(self.pdu.cc);
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
        sched.remove_sender(self.pdu.cc);
        self.pdu.rm.tx.event.notify_waiters();
    }
}

/// Bookkeeping information about the order of ACL data packets (PDU fragments)
/// submitted to the controller.
#[derive(Debug, Default)]
struct ControllerQueue {
    queue: VecDeque<ConnCid>,
    complete: HashMap<ConnHandle, u16>,
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

    /// Returns whether any packets for the specified connection are still
    /// queued.
    #[inline]
    #[must_use]
    fn contains_conn(&self, cn: ConnHandle) -> bool {
        let (a, b) = self.queue.as_slices();
        a.iter().any(|cc| cc.hdl == cn) || b.iter().any(|cc| cc.hdl == cn)
    }

    /// Returns whether any packets for the specified channel are still queued.
    #[inline]
    #[must_use]
    fn contains_chan(&self, cc: ConnCid) -> bool {
        self.queue.contains(&cc)
    }

    /// Adds channel `cc` to the queue. This must be called after a successful
    /// data transfer to the controller.
    #[inline]
    fn push(&mut self, cc: ConnCid) {
        assert!(!self.is_full());
        self.queue.push_back(cc);
    }

    /// Removes completed packets from the queue.
    fn pop(&mut self, complete: &NumberOfCompletedPackets) {
        for &(conn, count) in complete.as_ref() {
            if count > 0 {
                self.complete.insert(conn, count);
            }
        }
        self.queue.retain(|cc| {
            let Entry::Occupied(mut e) = self.complete.entry(cc.hdl) else {
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
