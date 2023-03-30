//! Transmit side of the Resource Manager.

use structbuf::Pack;
use tracing::{trace, warn};

use super::*;

/// Outbound PDU transfer state.
#[derive(Debug)]
pub(super) struct State {
    alloc: Alloc,
    sched: SyncMutex<Scheduler>,
}

impl State {
    /// Creates a new outbound transfer state.
    #[must_use]
    #[inline]
    pub fn new(t: &Arc<dyn host::Transport>, max_pkts: u8, acl_data_len: u16) -> Arc<Self> {
        Arc::new(Self {
            alloc: Alloc::new(t, host::Direction::Out, acl_data_len),
            sched: SyncMutex::new(Scheduler::new(u16::from(max_pkts))),
        })
    }

    /// Returns the transfer allocator.
    #[inline(always)]
    #[must_use]
    pub const fn alloc(&self) -> &Alloc {
        &self.alloc
    }

    /// Sends the PDU, returning as soon as the last fragment is submitted to
    /// the controller.
    #[inline]
    pub async fn send(self: &Arc<Self>, ch: &Arc<RawChan>, pdu: Frame) -> Result<()> {
        let (tx, ch) = (Arc::clone(self), Arc::clone(ch));
        let guard = self.sched.lock().schedule(tx, ch)?;
        guard.send(pdu).await
    }

    /// Registers a new LE-U logical link.
    pub fn register_link(&self, link: LeU) {
        self.sched.lock().register_link(link);
    }

    /// Removes LE-U logical link registration.
    pub fn handle_disconnect(&self, evt: hci::DisconnectionComplete) {
        self.sched.lock().disconnect_link(LeU::new(evt.handle));
    }

    /// Updates controller's buffer status.
    pub fn handle_num_completed(&self, evt: &hci::NumberOfCompletedPackets) {
        let pkts = (evt.as_ref().iter()).map(|&(cn, complete)| (LeU::new(cn), complete));
        self.sched.lock().ack(pkts);
    }

    // TODO: Handle Data Buffer Overflow event?
}

/// Outbound PDU scheduler. Ensures that the controller's transmit buffer is
/// shared fairly between all logical links. Within each logical link, PDUs are
/// transmitted in FIFO order.
#[derive(Debug)]
struct Scheduler {
    /// Channels that are blocked from sending because another channel on the
    /// same logical link is sending a PDU ([Vol 3] Part A, Section 7.2.1).
    blocked: HashMap<LeU, VecDeque<Arc<RawChan>>>,
    /// Channels from distinct logical links that are ready to send PDU
    /// fragments as soon as controller buffer space is available.
    ready: VecDeque<Arc<RawChan>>,
    /// Current channel with permission to send.
    active: Option<Arc<RawChan>>,
    /// Number of PDU fragments sent to the controller for each logical link,
    /// but not yet acknowledged by `HCI_Number_Of_Completed_Packets` event.
    sent: HashMap<LeU, u16>,
    /// Number of new PDU fragments that the controller can accept immediately.
    quota: u16,
}

impl Scheduler {
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
    fn is_active(&self, ch: &Arc<RawChan>) -> bool {
        (self.active.as_ref()).map_or(false, |act| Arc::ptr_eq(act, ch))
    }

    /// Schedules channel `ch` for sending a PDU.
    fn schedule(&mut self, tx: Arc<State>, ch: Arc<RawChan>) -> Result<SchedulerGuard> {
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
            self.active = Some(Arc::clone(&ch));
        } else {
            cs.set_scheduled(true);
            drop(cs);
            if blocked.is_empty()
                && (self.active.iter().chain(self.ready.iter()))
                    .all(|other| other.cid.link != ch.cid.link)
            {
                // First sender for this logical link
                self.ready.push_back(Arc::clone(&ch));
            } else {
                // Multiple senders for this logical link
                blocked.push_back(Arc::clone(&ch));
            }
        }
        Ok(SchedulerGuard { tx, ch })
    }

    /// Updates scheduler state after a PDU fragment is sent to the controller.
    fn sent(&mut self, ch: &Arc<RawChan>, more: bool) {
        debug_assert!(self.is_active(ch));
        if !self.blocked.contains_key(&ch.cid.link) {
            // remove_link was called. The channel already can't send, but keep
            // it active until it is closed and removed by SchedulerGuard.
            // Testing shows that at least one controller does not report
            // invalid connection handles in HCI_Number_Of_Completed_Packets
            // events, so we do not change self.quota or self.sent.
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
    fn remove(&mut self, ch: &Arc<RawChan>) {
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
#[must_use]
struct SchedulerGuard {
    tx: Arc<State>,
    ch: Arc<RawChan>,
}

impl SchedulerGuard {
    /// Performs PDU fragmentation and submits each fragment to the controller.
    async fn send(self, mut pdu: Frame) -> Result<()> {
        // Update basic L2CAP header ([Vol 3] Part A, Section 3.1)
        let mut hdr = pdu.at(0);
        let pdu_len = u16::try_from(hdr.as_ref().len() - L2CAP_HDR).unwrap();
        hdr.u16(pdu_len).u16(self.ch.cid.chan);

        if let Some(xfer) = pdu.take_xfer() {
            // Fast path for a single-fragment PDU
            debug_assert_eq!(xfer.typ(), host::TransferType::Acl(host::Direction::Out));
            return self.send_frag(xfer, false, false).await.map(|_xfer| ());
        }

        let mut xfer = self.tx.alloc.xfer();
        let frags = pdu.as_ref().chunks(self.tx.alloc.acl_data_len as _);
        let last = frags.len() - 1;
        for (i, frag) in frags.enumerate() {
            xfer.at(ACL_HDR).put(frag);
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
        mut xfer: Box<dyn host::Transfer>,
        is_cont: bool,
        more: bool,
    ) -> Result<Box<dyn host::Transfer>> {
        // Update ACL data packet header ([Vol 4] Part E, Section 5.4.2)
        let data_len = u16::try_from((*xfer).as_ref().len() - ACL_HDR).unwrap();
        xfer.at(0)
            .u16(u16::from(is_cont) << hci::ConnHandle::BITS | u16::from(self.ch.cid.link))
            .u16(data_len);
        self.ch.may_send().await?;
        trace!(
            "{}{}: {:02X?}",
            if is_cont { "Cont. " } else { "" },
            self.ch.cid,
            &(*xfer).as_ref()[4 + 4..] // Skip ACL and L2CAP headers
        );
        let e = match xfer.submit() {
            Ok(fut) => match fut.await {
                Ok(xfer) => {
                    self.tx.sched.lock().sent(&self.ch, more);
                    return Ok(xfer);
                }
                Err(e) => e,
            },
            Err(e) => e,
        };
        self.ch.set_error();
        Err(e.into())
    }
}

impl Drop for SchedulerGuard {
    #[inline]
    fn drop(&mut self) {
        // TODO: Mark the channel as broken if the entire PDU was not sent?
        self.tx.sched.lock().remove(&self.ch);
    }
}
