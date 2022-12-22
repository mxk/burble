use std::collections::VecDeque;
use std::fmt::Debug;
use std::sync::Arc;

use structbuf::{Unpack, Unpacker};

use crate::hci::ACL_LE_MIN_DATA_LEN;
use crate::host;
use crate::util::{Condvar, CondvarGuard};

use super::*;

// TODO: Mark RawChan as closed when BasicChan is dropped?

/// Basic L2CAP channel over an LE-U logical link.
#[derive(Debug)]
pub(crate) struct BasicChan<T: host::Transport> {
    pub(super) raw: Arc<RawChan<T>>,
    tx: Arc<tx::State<T>>,
    mtu: u16,
}

impl<T: host::Transport> BasicChan<T> {
    /// Creates a new channel.
    #[inline]
    pub(super) fn new(cid: LeCid, tx: &Arc<tx::State<T>>, mtu: u16) -> Self {
        assert!(mtu >= L2CAP_LE_MIN_MTU);
        Self {
            raw: RawChan::new(cid, L2CAP_HDR + mtu as usize),
            tx: Arc::clone(tx),
            mtu,
        }
    }

    /// Returns the channel ID.
    #[inline(always)]
    pub fn cid(&self) -> LeCid {
        self.raw.cid
    }

    /// Returns the current MTU.
    #[inline]
    pub const fn mtu(&self) -> u16 {
        self.mtu
    }

    /// Returns the maximum MTU that avoids fragmentation.
    #[allow(clippy::cast_possible_truncation)]
    #[inline]
    pub fn preferred_mtu(&self) -> u16 {
        self.tx.preferred_frame_len() - L2CAP_HDR as u16
    }

    /// Sets new channel MTU.
    pub fn set_mtu(&mut self, mtu: u16) {
        assert!(mtu >= L2CAP_LE_MIN_MTU);
        self.mtu = mtu;
        self.raw.state.lock().max_frame_len = L2CAP_HDR + mtu as usize;
    }

    /// Creates a new outbound SDU.
    #[inline]
    pub fn new_sdu(&self) -> Sdu<T> {
        Sdu::new(self.tx.new_frame(L2CAP_HDR + self.mtu as usize), L2CAP_HDR)
    }

    /// Receives the next inbound SDU. This method is cancel safe.
    pub async fn recv(&mut self) -> Result<Sdu<T>> {
        let mut cs = self.raw.state.lock();
        loop {
            cs.err(self.raw.cid)?;
            if let Some(pdu) = cs.pdu.pop_front() {
                return Ok(Sdu::new(pdu, L2CAP_HDR));
            }
            cs.notified().await;
        }
    }

    /// Returns the next inbound SDU that matches filter `f`. All other SDUs
    /// stay in the buffer.
    pub async fn recv_filter(&mut self, f: impl Fn(Unpacker) -> bool + Send) -> Result<Sdu<T>> {
        let mut cs = self.raw.state.lock();
        let mut skip = 0;
        loop {
            cs.err(self.raw.cid)?;
            let mut it = cs.pdu.iter();
            if skip > 0 {
                // It's safe to skip prior PDUs because we have exclusive access
                // to the channel and the queue can only be modified by new PDUs
                // getting appended.
                it.nth(skip - 1);
            }
            if let Some(i) = it.position(|pdu| f(pdu.unpack().split_at(L2CAP_HDR).1)) {
                // SAFETY: `skip + i` is within bounds
                let pdu = unsafe { cs.pdu.remove(skip + i).unwrap_unchecked() };
                return Ok(Sdu::new(pdu, L2CAP_HDR));
            }
            skip = cs.pdu.len();
            cs.notified().await;
        }
    }

    /// Sends an outbound SDU, returning as soon as the last fragment is
    /// submitted to the controller.
    #[inline]
    pub async fn send(&self, sdu: Sdu<T>) -> Result<()> {
        self.tx.send(&self.raw, sdu.f).await
    }
}

/// Channel state shared between the Channel Manager, Resource Manager, and the
/// channel owner.
#[derive(Debug)]
pub(super) struct RawChan<T: host::Transport> {
    pub cid: LeCid,
    pub state: Condvar<State<T>>,
}

impl<T: host::Transport> RawChan<T> {
    /// Creates new channel state.
    #[inline]
    fn new(cid: LeCid, max_frame_len: usize) -> Arc<Self> {
        Arc::new(Self {
            cid,
            state: Condvar::new(State::new(max_frame_len)),
        })
    }

    /// Allows the channel to send PDU fragments.
    #[inline]
    pub fn allow_send(&self) {
        let mut cs = self.state.lock();
        if !(cs.status).intersects(Status::CLOSED.union(Status::ERROR).union(Status::MAY_SEND)) {
            debug_assert!(cs.status.contains(Status::SCHEDULED));
            cs.status |= Status::MAY_SEND;
            cs.notify_all();
        }
    }

    /// Prevents the channel from sending PDU fragments.
    #[inline]
    pub fn deny_send(&self) {
        self.state.lock().status -= Status::MAY_SEND;
    }

    /// Blocks until the scheduler allows the channel to send PDU fragments.
    #[inline]
    pub async fn may_send(&self) -> Result<()> {
        let mut cs = self.state.lock();
        while !cs.status.contains(Status::MAY_SEND) {
            cs.err(self.cid)?;
            cs.notified().await;
        }
        Ok(())
    }

    /// Sets channel closed flag.
    #[inline]
    pub fn set_closed(&self) {
        State::set_fatal(self.state.lock(), Status::CLOSED);
    }

    /// Sets channel error flag.
    #[inline]
    pub fn set_error(&self) {
        State::set_fatal(self.state.lock(), Status::ERROR);
    }
}

bitflags::bitflags! {
    /// Channel status flags.
    #[repr(transparent)]
    pub(super) struct Status: u8 {
        /// Channel is closed.
        const CLOSED = 1 << 0;
        /// Send or receive error.
        const ERROR = 1 << 1;
        /// Channel is registered with the Scheduler to send a PDU.
        const SCHEDULED = 1 << 2;
        /// Channel is allowed to send a single PDU fragment.
        const MAY_SEND = 1 << 3;
    }
}

// TODO: Move Mutex out of Condvar and use two Condvars to allow separate
// notifications for sender and receiver?

/// Condvar-protected channel state. Since the state affects both send and
/// receive operations, all notifications must be done via `notify_all()`, as
/// `notify_one()` may wake the wrong waiter. When interacting with `tx::State`,
/// the scheduler must always be locked before the channel to avoid a deadlock.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    /// Channel status flags.
    status: Status,
    /// Maximum PDU length, including the L2CAP header.
    pub max_frame_len: usize,
    /// Received PDU queue.
    pub pdu: VecDeque<Frame<T>>,
}

impl<T: host::Transport> State<T> {
    /// Creates new channel state.
    #[inline]
    fn new(max_frame_len: usize) -> Self {
        assert!(max_frame_len >= ACL_LE_MIN_DATA_LEN as usize);
        Self {
            status: Status::empty(),
            max_frame_len,
            pdu: VecDeque::new(),
        }
    }

    /// Returns whether the channel can send and receive data.
    #[inline]
    pub const fn is_ok(&self) -> bool {
        !self.status.intersects(Status::CLOSED.union(Status::ERROR))
    }

    /// Returns whether the channel is registered with the Scheduler.
    #[inline]
    pub const fn is_scheduled(&self) -> bool {
        self.status.contains(Status::SCHEDULED)
    }

    /// Sets the channel scheduled flag.
    #[inline]
    pub fn set_scheduled(&mut self, scheduled: bool) {
        self.status = if scheduled {
            self.status.union(Status::SCHEDULED)
        } else {
            debug_assert!(self.is_scheduled(), "Channel must be scheduled");
            (self.status).difference(Status::SCHEDULED.union(Status::MAY_SEND))
        }
    }

    /// Sets the channel scheduled flag with immediate permission to send.
    #[inline]
    pub fn set_scheduled_active(&mut self) {
        self.status |= Status::SCHEDULED.union(Status::MAY_SEND);
    }

    /// Returns the channel error, if any.
    pub const fn err(&self, cid: LeCid) -> Result<()> {
        if self.is_ok() {
            return Ok(());
        }
        Err(if self.status.contains(Status::CLOSED) {
            Error::ChanClosed(cid)
        } else {
            Error::ChanBroken(cid)
        })
    }

    /// Sets channel closed or error status flag.
    #[inline]
    pub fn set_fatal(mut cs: CondvarGuard<Self>, s: Status) {
        if !cs.status.contains(s) {
            cs.status = cs.status.difference(Status::MAY_SEND) | s;
            cs.notify_all();
        }
    }
}
