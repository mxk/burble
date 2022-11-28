//! Transmit side of the Resource Manager.

use tracing::{trace, warn};

use super::*;

/// Outbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    alloc: Arc<Alloc<T>>,
    sched: parking_lot::Mutex<Scheduler<T>>,
}

impl<T: host::Transport> State<T> {
    /// Creates a new outbound transfer state.
    #[must_use]
    #[inline]
    pub fn new(transport: T, max_pkts: u16, acl_data_len: u16) -> Arc<Self> {
        Arc::new(Self {
            alloc: Alloc::new(transport, host::Direction::Out, acl_data_len),
            sched: parking_lot::Mutex::new(Scheduler::new(max_pkts)),
        })
    }

    /// Creates a new outbound PDU.
    pub fn new_pdu(self: &Arc<Self>, ch: &Arc<RawChan<T>>, max_frame_len: usize) -> Pdu<T> {
        let mut raw = self.alloc.pdu(max_frame_len);
        raw.buf_mut().put_bytes(0, ACL_HDR + L2CAP_HDR);
        Pdu {
            tx: Arc::clone(self),
            ch: Arc::clone(ch),
            raw,
        }
    }

    /// Registers a new LE-U logical link.
    pub fn register_link(&self, link: LeU) {
        self.sched.lock().register_link(link);
    }

    /// Removes LE-U logical link registration.
    pub fn on_disconnect(&self, evt: hci::DisconnectionComplete) {
        self.sched.lock().disconnect_link(LeU::new(evt.handle));
    }

    /// Updates controller's buffer status.
    pub fn on_num_completed(&self, evt: &hci::NumberOfCompletedPackets) {
        let pkts = (evt.as_ref().iter()).map(|&(cn, complete)| (LeU::new(cn), complete));
        self.sched.lock().ack(pkts);
    }

    // TODO: Handle Data Buffer Overflow event?
}

/// Outbound PDU.
#[derive(Debug)]
#[must_use]
pub(crate) struct Pdu<T: host::Transport> {
    tx: Arc<State<T>>,
    ch: Arc<RawChan<T>>,
    raw: RawBuf<T>,
}

impl<T: host::Transport> Pdu<T> {
    /// Returns the PDU buffer, padded with space for the ACL data packet and
    /// basic L2CAP headers.
    #[inline]
    #[must_use]
    pub fn buf_mut(&mut self) -> &mut BytesMut {
        self.raw.buf_mut()
    }

    /// Sends the PDU, returning as soon as the last fragment is submitted to
    /// the controller.
    #[inline]
    pub async fn send(self) -> Result<()> {
        self.tx.sched.lock().schedule(Arc::clone(&self.ch))?;
        SchedulerGuard(self).send().await
    }
}

/// Outbound PDU scheduler. Ensures that the controller's transmit buffer is
/// shared fairly between all logical links. Within each logical link, PDUs are
/// transmitted in FIFO order.
#[derive(Debug)]
struct Scheduler<T: host::Transport> {
    /// Channels that are blocked from sending because another channel on the
    /// same logical link is sending a PDU ([Vol 3] Part A, Section 7.2.1).
    blocked: HashMap<LeU, VecDeque<Arc<RawChan<T>>>>,
    /// Channels from distinct logical links that are ready to send PDU
    /// fragments as soon as controller buffer space is available.
    ready: VecDeque<Arc<RawChan<T>>>,
    /// Current channel with permission to send.
    active: Option<Arc<RawChan<T>>>,
    /// Number of PDU fragments sent to the controller for each logical link,
    /// but not yet acknowledged by `HCI_Number_Of_Completed_Packets` event.
    sent: HashMap<LeU, u16>,
    /// Number of new PDU fragments that the controller can accept immediately.
    quota: u16,
}

impl<T: host::Transport> Scheduler<T> {
    /// Creates a new outbound PDU scheduler. This assumes that the controller
    /// is ready to accept `max_pkts` data packets.
    #[inline]
    #[must_use]
    fn new(max_pkts: u16) -> Self {
        Self {
            blocked: HashMap::new(),
            ready: VecDeque::new(),
            active: None,
            sent: HashMap::new(),
            quota: max_pkts,
        }
    }

    /// Registers a new LE-U logical link, allowing it to send data.
    #[inline]
    fn register_link(&mut self, link: LeU) {
        trace!("Adding logical link: {link}");
        self.blocked.entry(link).or_default();
        self.sent.entry(link).or_default();
    }

    /// Removes an LE-U logical link after an `HCI_Disconnection_Complete`
    /// event. This does not change the status of any affected channels, so
    /// [`SchedulerGuard`]s will block indefinitely until the channels are
    /// closed.
    #[inline]
    fn disconnect_link(&mut self, link: LeU) {
        self.remove_link(link);
        if let Some(sent) = self.sent.remove(&link) {
            self.quota += sent; // [Vol 4] Part E, Section 4.3
            self.reschedule();
        }
    }

    /// Removes an LE-U logical link without assuming that any unacknowledged
    /// packets were flushed from the controller's buffer.
    #[inline]
    fn remove_link(&mut self, link: LeU) {
        trace!("Removing logical link: {link}");
        let Some(blocked) = self.blocked.remove(&link) else { return };
        for ch in blocked {
            ch.state.lock().set_scheduled(false);
        }
        let mut is_active = false;
        if let Some(ch) = self.active.as_ref() {
            if ch.cid.link == link {
                // The channel may be in the process of sending a fragment, and
                // we don't want another channel to start a new transfer before
                // the current one is finished. We stop this channel from
                // sending any more fragments, but keep it active until it is
                // closed and its SchedulerGuard is dropped.
                // TODO: Use an explicit SENDING Status?
                ch.deny_send();
                is_active = true;
            }
        }
        if !is_active {
            if let Some(i) = self.ready.iter().position(|other| other.cid.link == link) {
                (self.ready.remove(i).unwrap().state.lock()).set_scheduled(false);
            }
        }
        if let Entry::Occupied(sent) = self.sent.entry(link) {
            if *sent.get() == 0 {
                sent.remove();
            }
        }
    }

    /// Updates controller's buffer status.
    #[inline]
    fn ack(&mut self, pkts: impl Iterator<Item = (LeU, u16)>) {
        for (link, mut complete) in pkts {
            let Entry::Occupied(mut sent) = self.sent.entry(link) else {
                warn!("ACL data packet completion for an unknown {link} ({complete})");
                continue;
            };
            let rem = sent.get().checked_sub(complete).unwrap_or_else(|| {
                let sent = *sent.get();
                warn!("ACL data packet completion mismatch for {link} (sent={sent}, acked={complete})");
                complete = sent;
                0
            });
            self.quota += complete;
            if self.blocked.contains_key(&link) || rem > 0 {
                sent.insert(rem);
            } else {
                sent.remove(); // remove_link was called
            }
        }
        self.reschedule();
    }

    /// Allows the next ready channel to send its PDU fragment. The caller must
    /// not be holding a lock on any channel.
    fn reschedule(&mut self) {
        if self.quota == 0 || self.active.is_some() {
            return;
        }
        self.active = match self.ready.len() {
            0 => return,
            1 => self.ready.pop_front(),
            // Pick the logical link with the fewest unacknowledged packets to
            // maximize physical link utilization and prevent a stalled link
            // from filling up the controller's buffer. Channels are removed
            // from the front and pushed to the back after sending, resulting in
            // round-robin scheduling when the number of unacknowledged packets
            // is equal.
            _ => self.ready.remove(
                (self.ready.iter().map(|ch| self.sent[&ch.cid.link]))
                    .enumerate()
                    .reduce(|min, cur| if min.1 <= cur.1 { min } else { cur })
                    .map_or(0, |min| min.0),
            ),
        };
        self.active.as_ref().unwrap().allow_send();
    }

    /// Returns whether channel `ch` is the current sender. This is done by
    /// comparing pointers because connection handles can be reused before the
    /// channel is removed from the scheduler ([Vol 4] Part E, Section 5.3).
    #[inline]
    fn is_active(&self, ch: &Arc<RawChan<T>>) -> bool {
        (self.active.as_ref()).map_or(false, |act| Arc::ptr_eq(act, ch))
    }

    /// Schedules channel `ch` for sending a PDU.
    fn schedule(&mut self, ch: Arc<RawChan<T>>) -> Result<()> {
        let Some(blocked) = self.blocked.get_mut(&ch.cid.link) else {
            return Err(Error::InvalidConn(ch.cid.link.into()));
        };
        let mut cs = ch.state.lock();
        assert!(
            !cs.is_scheduled(),
            "A channel may not send two PDUs concurrently"
        );
        cs.err(ch.cid)?;
        if self.quota > 0 && self.active.is_none() {
            // Fast path for the only sender, no notification needed
            cs.set_scheduled_active();
            drop(cs);
            self.active = Some(ch);
            return Ok(());
        }
        cs.set_scheduled(true);
        drop(cs);
        if blocked.is_empty()
            && (self.active.iter().chain(self.ready.iter()))
                .all(|other| other.cid.link != ch.cid.link)
        {
            self.ready.push_back(ch); // First sender for this logical link
        } else {
            blocked.push_back(ch); // Multiple senders for this logical link
        }
        Ok(())
    }

    /// Updates scheduler state after a PDU fragment is sent to the controller.
    fn sent(&mut self, ch: &Arc<RawChan<T>>, more: bool) {
        debug_assert!(self.is_active(ch));
        if !self.blocked.contains_key(&ch.cid.link) {
            // remove_link was called. The channel already can't send, but keep
            // it active until it is closed and removed by SchedulerGuard.
            // TODO: Will `HCI_Number_Of_Completed_Packets` be sent? If so,
            // update quota. May need to restore self.sent entry.
            return;
        }
        self.quota -= 1; // [Vol 4] Part E, Section 4.1.1
        *self.sent.get_mut(&ch.cid.link).unwrap() += 1;
        if !more {
            // Keep the channel active so that remove() takes the fast path
            ch.deny_send();
        } else if self.quota == 0 || !self.ready.is_empty() {
            ch.deny_send();
            self.ready.push_back(self.active.take().unwrap());
            self.reschedule();
        } else {
            // Channel is the only sender and can send another PDU fragment
        }
    }

    /// Removes channel `ch` from the scheduler.
    fn remove(&mut self, ch: &Arc<RawChan<T>>) {
        #[inline]
        fn position<V>(q: &VecDeque<Arc<V>>, v: &Arc<V>) -> Option<usize> {
            q.iter().position(|other| Arc::ptr_eq(other, v))
        }
        if self.is_active(ch) {
            // Fast path for the active channel
            self.active = None;
            ch.state.lock().set_scheduled(false);
            if let Some(next) = (self.blocked.get_mut(&ch.cid.link)).and_then(VecDeque::pop_front) {
                self.ready.push_back(next);
            }
            self.reschedule();
            return;
        }
        let Some(blocked) = self.blocked.get_mut(&ch.cid.link) else { return };
        let mut cs = ch.state.lock();
        if !cs.is_scheduled() {
            // The channel was already removed by remove_link() and a new link
            // was created with the same connection handle.
            return;
        }
        cs.set_scheduled(false);
        let Some(i) = position(&self.ready, ch) else {
            blocked.remove(position(blocked, ch).unwrap());
            return;
        };
        match blocked.pop_front() {
            Some(next) => self.ready[i] = next,
            None => {
                self.ready.remove(i);
            }
        }
    }
}

/// Scheduled PDU send task. Ensures that the channel is removed from the
/// Scheduler when the send operation is done (or dropped).
#[derive(Debug)]
#[repr(transparent)]
struct SchedulerGuard<T: host::Transport>(Pdu<T>);

impl<T: host::Transport> SchedulerGuard<T> {
    /// Performs PDU fragmentation and submits each fragment to the controller.
    async fn send(mut self) -> Result<()> {
        // Update basic L2CAP header ([Vol 3] Part A, Section 3.1)
        let cid = u16::from(self.ch.cid.chan);
        let data = &mut self.raw.buf_mut()[ACL_HDR..];
        let (mut l2cap_hdr, payload) = data.split_at_mut(L2CAP_HDR);
        l2cap_hdr.put_u16_le(u16::try_from(payload.len()).unwrap());
        l2cap_hdr.put_u16_le(cid);

        if let Some(xfer) = self.raw.take_xfer() {
            // Fast path for single-fragment PDUs
            return self.send_frag(xfer, false, false).await.map(|_xfer| ());
        }

        let mut xfer = self.tx.alloc.xfer();
        let frags = self.raw.as_ref().chunks(self.tx.alloc.acl_data_len);
        let last = frags.len() - 1;
        for (i, frag) in frags.enumerate() {
            let buf = xfer.buf_mut();
            buf.put_bytes(0, ACL_HDR);
            buf.extend_from_slice(frag);
            xfer = self.send_frag(xfer, i != 0, i != last).await?;
            if i != last {
                xfer.reset();
            }
        }
        Ok(())
    }

    /// Sends a single PDU fragment.
    async fn send_frag(
        &self,
        mut xfer: AclTransfer<T>,
        is_cont: bool,
        more: bool,
    ) -> Result<AclTransfer<T>> {
        // Update ACL data packet header ([Vol 4] Part E, Section 5.4.2)
        let (mut hdr, data) = xfer.buf_mut().split_at_mut(ACL_HDR);
        hdr.put_u16_le(u16::from(is_cont) << hci::ConnHandle::BITS | u16::from(self.ch.cid.link));
        hdr.put_u16_le(u16::try_from(data.len()).unwrap());
        self.ch.may_send().await?;
        trace!(
            "{}PDU fragment from {}: {:02X?}",
            if is_cont { "Cont. " } else { "" },
            self.ch.cid,
            xfer.as_ref()
        );
        match xfer.submit().await {
            Ok(xfer) => {
                self.tx.sched.lock().sent(&self.ch, more);
                Ok(xfer)
            }
            Err(e) => {
                self.ch.set_error();
                Err(e.into())
            }
        }
    }
}

impl<T: host::Transport> Drop for SchedulerGuard<T> {
    #[inline]
    fn drop(&mut self) {
        self.tx.sched.lock().remove(&self.ch);
    }
}

impl<T: host::Transport> Deref for SchedulerGuard<T> {
    type Target = Pdu<T>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: host::Transport> DerefMut for SchedulerGuard<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
