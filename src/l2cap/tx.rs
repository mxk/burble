use super::*;

/// Outbound PDU scheduler. Uses round-robin scheduling to share the
/// controller's transmit buffer among all connection handles.
#[derive(Debug)]
pub(super) struct TxScheduler<T: host::Transport> {
    /// Transfer allocator.
    alloc: Alloc<T>,
    /// Channels that are blocked from sending because another channel on the
    /// same connection is sending its PDU.
    ready: HashMap<ConnHandle, VecDeque<Cid>>,
    /// Channels that are ready to send as soon as buffer space becomes
    /// available.
    next: VecDeque<ConnCid>,
    /// Controller's transmit queue status.
    queue: TxQueue,
}

impl<T: host::Transport> TxScheduler<T> {
    /// Creates a new outbound PDU scheduler.
    #[inline]
    #[must_use]
    pub fn new(max_pkts: usize, max_frag_len: usize) -> Self {
        Self {
            alloc: Alloc::tx(max_frag_len),
            ready: HashMap::new(),
            next: VecDeque::new(),
            queue: TxQueue::new(max_pkts),
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

    /// Updates transmit queue status.
    #[inline]
    pub fn update(&mut self, complete: &NumberOfCompletedPackets) {
        self.queue.pop(complete)
    }

    /// Schedules a channel for sending a PDU.
    fn schedule_sender(&mut self, cc: ConnCid) -> Result<()> {
        assert!(cc.is_valid());
        let Some(ready) = self.ready.get_mut(&cc.hdl) else {
            return Err(Error::InvalidConn(cc.hdl));
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
        if !self.ready.contains_key(&cc.hdl) {
            Err(Error::InvalidConn(cc.hdl))
        } else {
            Ok(!self.queue.is_full() && self.next.front() == Some(&cc))
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
pub(super) struct TxPdu<T: host::Transport> {
    rm: Arc<ResourceManager<T>>,
    cc: ConnCid,
    raw: RawPdu<T>,
}

impl<T: host::Transport> TxPdu<T> {
    /// Allocates a new PDU.
    pub fn new(rm: Arc<ResourceManager<T>>, cc: ConnCid, data_cap: usize) -> Self {
        let mut raw = rm.tx.lock().alloc.pdu(&rm.transport, data_cap);
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

impl<T: host::Transport> Drop for TxPdu<T> {
    fn drop(&mut self) {
        (self.raw.xfer.take()).map(|xfer| self.rm.tx.lock().alloc.free_xfer(xfer));
    }
}

/// Scheduled PDU transmit task.
#[derive(Debug)]
struct TxTask<T: host::Transport> {
    pdu: TxPdu<T>,
    xfer: Option<T::Transfer>,
}

impl<T: host::Transport> TxTask<T> {
    /// Schedules the PDU for transmission.
    #[inline]
    fn new(pdu: TxPdu<T>) -> Result<Self> {
        pdu.rm.tx.lock().schedule_sender(pdu.cc)?;
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
        if self.pdu.raw.xfer.is_some() {
            self.may_send().await?;
            let xfer = self.pdu.raw.xfer.take().unwrap();
            return (self
                .send_frag(xfer, false)
                .await)
                .map(|xfer| self.pdu.raw.xfer = Some(xfer));
        }

        // Fragmentation path
        let max_frag_len = {
            let mut sched = self.pdu.rm.tx.lock();
            self.pdu.raw.xfer = Some(sched.alloc.xfer(&self.pdu.rm.transport));
            sched.alloc.max_frag_len
        };
        let data = &self.pdu.raw.buf.as_ref()[ACL_HDR..];
        for (i, frag) in data.chunks(max_frag_len).enumerate() {
            self.may_send().await?;
            let mut xfer = self.xfer.take().unwrap();
            xfer.reset();
            let buf = xfer.buf_mut();
            buf.put_bytes(0, ACL_HDR);
            buf.extend_from_slice(frag);
            self.send_frag(xfer, i != 0).await.map(|xfer| self.pdu.raw.xfer = Some(xfer))?;
        }
        Ok(())
    }

    /// Waits until the controller has space for another ACL data packet and the
    /// current task is the next one scheduled to transmit.
    async fn may_send(&self) -> Result<()> {
        //self.pdu.rm.tx_wait(|sched| sched.may_send(self.pdu.cc)).await
        Ok(())
    }

    async fn send_frag(&self, mut xfer: T::Transfer, cont: bool) -> Result<T::Transfer> {
        // Update HCI ACL data packet header ([Vol 4] Part E, Section 5.4.2)
        let (mut hdr, data) = xfer.buf_mut().split_at_mut(ACL_HDR);
        hdr.put_u16_le(u16::from(cont) << 12 | u16::from(self.pdu.cc.hdl));
        hdr.put_u16_le(u16::try_from(data.len()).unwrap());
        let xfer = xfer.submit()?.await;

        // Pass send permission to the next connection handle
        let mut sched = self.pdu.rm.tx.lock();
        sched.queue.push(self.pdu.cc);
        if sched.next.len() > 1 {
            sched.next.rotate_left(1);
            self.pdu.rm.tx_event.notify_waiters();
        }
        Ok(xfer)
    }
}

impl<T: host::Transport> Drop for TxTask<T> {
    fn drop(&mut self) {
        let mut sched = self.pdu.rm.tx.lock();
        sched.remove_sender(self.pdu.cc);
        self.pdu.rm.tx_event.notify_waiters();
    }
}

/// Bookkeeping information about the order of ACL data packets (PDU fragments)
/// submitted to the controller.
#[derive(Debug, Default)]
struct TxQueue {
    queue: VecDeque<ConnCid>,
    complete: HashMap<ConnHandle, u16>,
    max_pkts: usize,
}

impl TxQueue {
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
        assert!(self.complete.is_empty())
    }
}

/// Returns the index of `v` in `q`.
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
