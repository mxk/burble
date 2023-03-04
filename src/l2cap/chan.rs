use std::collections::VecDeque;
use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};

use structbuf::{Unpack, Unpacker};
use tracing::{error, info, trace};

use crate::hci::ACL_LE_MIN_DATA_LEN;

use super::*;

// TODO: Mark RawChan as closed when BasicChan is dropped?

/// Basic L2CAP channel over an LE-U logical link.
#[derive(Clone, Debug)]
pub(crate) struct BasicChan {
    pub(super) raw: Arc<RawChan>,
    tx: Arc<tx::State>,
    mtu: u16,
}

impl BasicChan {
    /// Creates a new channel.
    #[inline]
    pub(super) fn new(cid: LeCid, cn: &hci::ConnWatch, tx: &Arc<tx::State>, mtu: u16) -> Self {
        assert!(mtu >= L2CAP_LE_MIN_MTU);
        Self {
            raw: RawChan::new(cid, cn, L2CAP_HDR + mtu as usize),
            tx: Arc::clone(tx),
            mtu,
        }
    }

    /// Returns the channel ID.
    #[inline(always)]
    pub fn cid(&self) -> LeCid {
        self.raw.cid
    }

    /// Returns the connection watch channel.
    #[inline(always)]
    pub(crate) fn conn(&self) -> &hci::ConnWatch {
        &self.raw.cn
    }

    /// Returns the current MTU.
    #[inline(always)]
    pub const fn mtu(&self) -> u16 {
        self.mtu
    }

    /// Returns the maximum MTU that avoids fragmentation.
    #[allow(clippy::cast_possible_truncation)]
    #[inline]
    pub(crate) fn preferred_mtu(&self) -> u16 {
        self.tx.preferred_frame_len() - L2CAP_HDR as u16
    }

    /// Sets new channel MTU.
    pub fn set_mtu(&mut self, mtu: u16) {
        // The MTU can only be changed once, so the current one is the minimum
        // allowed for the channel.
        if mtu <= self.mtu {
            if mtu < self.mtu {
                error!(
                    "Refusing {} MTU change: {} -> {mtu}",
                    self.raw.cid, self.mtu
                );
            }
            return;
        }
        info!("{} MTU change: {} -> {mtu}", self.raw.cid, self.mtu);
        self.mtu = mtu;
        self.raw.state.lock().max_frame_len = L2CAP_HDR + mtu as usize;
    }

    /// Sets channel error flag.
    #[inline(always)]
    pub fn set_error(&self) {
        self.raw.set_error();
    }

    /// Creates a new outbound SDU.
    #[inline]
    pub fn new_payload(&self) -> Payload {
        Payload::new(self.tx.new_frame(L2CAP_HDR + self.mtu as usize), L2CAP_HDR)
    }

    /// Returns a future that will resolve to the next inbound SDU.
    #[inline(always)]
    pub fn recv(&mut self) -> Recv {
        Recv {
            raw: Arc::clone(&self.raw),
            have_lock: false,
        }
    }

    /// Returns the next inbound SDU that matches filter `f`. All other SDUs
    /// stay in the buffer.
    #[inline(always)]
    pub fn recv_filter<F: Fn(Unpacker) -> bool + Send>(&mut self, f: F) -> RecvFilter<F> {
        RecvFilter { r: self.recv(), f }
    }

    /// Sends an outbound SDU, returning as soon as the last fragment is
    /// submitted to the controller.
    #[inline]
    pub async fn send(&mut self, sdu: Payload) -> Result<()> {
        self.tx.send(&self.raw, sdu.f).await
    }
}

/// Channel receive future.
#[derive(Debug)]
pub(crate) struct Recv {
    raw: Arc<RawChan>,
    have_lock: bool,
}

impl Future for Recv {
    type Output = Result<Payload>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.raw.state.lock();
        if let Err(e) = cs.err(self.raw.cid) {
            return Poll::Ready(Err(e));
        }
        if let Some(pdu) = cs.pdu.pop_front() {
            return Poll::Ready(Ok(Payload::new(pdu, L2CAP_HDR)));
        }
        cs.rx_await(cx, self.have_lock);
        drop(cs);
        self.have_lock = true;
        Poll::Pending
    }
}

impl Drop for Recv {
    #[inline]
    fn drop(&mut self) {
        if self.have_lock {
            let mut cs = self.raw.state.lock();
            cs.status.remove(Status::RX_LOCK);
            cs.rx_waker = None;
        }
    }
}

/// Channel receive future with SDU filtering.
#[derive(Debug)]
pub(crate) struct RecvFilter<F> {
    r: Recv,
    f: F,
}

impl<F: Fn(Unpacker) -> bool + Send + Unpin> Future for RecvFilter<F> {
    type Output = Result<Payload>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.r.raw.state.lock();
        if let Err(e) = cs.err(self.r.raw.cid) {
            return Poll::Ready(Err(e));
        }
        let mut it = cs.pdu.iter();
        if let Some(i) = it.position(|pdu| (self.f)(pdu.unpack().split_at(L2CAP_HDR).1)) {
            // SAFETY: `i` is within bounds
            let pdu = unsafe { cs.pdu.remove(i).unwrap_unchecked() };
            return Poll::Ready(Ok(Payload::new(pdu, L2CAP_HDR)));
        }
        cs.rx_await(cx, self.r.have_lock);
        drop(cs);
        self.r.have_lock = true;
        Poll::Pending
    }
}

/// Channel send permission future.
#[derive(Debug)]
pub(crate) struct MaySend<'a> {
    raw: &'a RawChan,
    have_lock: bool,
}

impl Future for MaySend<'_> {
    type Output = Result<()>;

    #[inline]
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.raw.state.lock();
        if cs.status.contains(Status::MAY_SEND) {
            return Poll::Ready(Ok(()));
        }
        if let Err(e) = cs.err(self.raw.cid) {
            return Poll::Ready(Err(e));
        }
        cs.tx_await(cx, self.have_lock);
        drop(cs);
        self.have_lock = true;
        Poll::Pending
    }
}

impl Drop for MaySend<'_> {
    #[inline]
    fn drop(&mut self) {
        if self.have_lock {
            let mut cs = self.raw.state.lock();
            cs.status.remove(Status::TX_LOCK);
            cs.tx_waker = None;
        }
    }
}

/// Channel state shared between the Channel Manager, Resource Manager, and the
/// channel owner.
#[derive(Debug)]
pub(super) struct RawChan {
    pub cid: LeCid,
    pub cn: hci::ConnWatch,
    pub state: SyncMutex<State>,
}

impl RawChan {
    /// Creates new channel state.
    #[inline]
    fn new(cid: LeCid, cn: &hci::ConnWatch, max_frame_len: usize) -> Arc<Self> {
        Arc::new(Self {
            cid,
            cn: cn.clone(),
            state: SyncMutex::new(State::new(max_frame_len)),
        })
    }

    /// Allows the channel to send PDU fragments.
    #[inline]
    pub fn allow_send(&self) {
        let mut cs = self.state.lock();
        if !(cs.status).intersects(Status::CLOSED.union(Status::ERROR).union(Status::MAY_SEND)) {
            debug_assert!(cs.status.contains(Status::SCHEDULED));
            cs.status |= Status::MAY_SEND;
            if let Some(tx) = cs.tx_waker.take() {
                tx.wake();
            }
        }
    }

    /// Prevents the channel from sending PDU fragments.
    #[inline(always)]
    pub fn deny_send(&self) {
        self.state.lock().status.remove(Status::MAY_SEND);
    }

    /// Returns a future that blocks until the scheduler allows the channel to
    /// send a PDU fragment.
    #[inline(always)]
    pub const fn may_send(&self) -> MaySend {
        MaySend {
            raw: self,
            have_lock: false,
        }
    }

    /// Sets channel closed flag.
    #[inline(always)]
    pub fn set_closed(&self) {
        self.state.lock().set_fatal(Status::CLOSED);
    }

    /// Sets channel error flag.
    #[inline(always)]
    pub fn set_error(&self) {
        self.state.lock().set_fatal(Status::ERROR);
    }
}

bitflags::bitflags! {
    /// Channel status flags.
    #[repr(transparent)]
    pub(super) struct Status: u8 {
        /// Channel is closed.
        const CLOSED = 1 << 0;
        /// Fatal send or receive error.
        const ERROR = 1 << 1;
        /// Channel is registered with the Scheduler to send a PDU.
        const SCHEDULED = 1 << 2;
        /// Channel is allowed to send a single PDU fragment.
        const MAY_SEND = 1 << 3;
        /// [`Recv`] future is using rx_waker.
        const RX_LOCK = 1 << 4;
        /// [`MaySend`] future is using tx_waker.
        const TX_LOCK = 1 << 5;
    }
}

/// Mutex-protected channel state. When interacting with [`tx::State`], the
/// scheduler must always be locked before the channel to avoid a deadlock.
#[derive(Debug)]
pub(super) struct State {
    /// Channel status flags.
    status: Status,
    /// Maximum PDU length, including the L2CAP header.
    max_frame_len: usize,
    /// Received PDU queue.
    pdu: VecDeque<Frame>,
    /// Receive task waker.
    rx_waker: Option<Waker>,
    /// Transmit task waker.
    tx_waker: Option<Waker>,
}

impl State {
    /// Maximum number of PDUs that may be queued. Reaching this limit likely
    /// means that the channel is broken and isn't receiving data.
    const MAX_PDUS: usize = 64;

    /// Creates new channel state.
    #[inline]
    fn new(max_frame_len: usize) -> Self {
        assert!(max_frame_len >= ACL_LE_MIN_DATA_LEN as usize);
        Self {
            status: Status::empty(),
            max_frame_len,
            pdu: VecDeque::new(),
            rx_waker: None,
            tx_waker: None,
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
    pub fn set_fatal(&mut self, s: Status) {
        self.status = self.status.union(s).difference(Status::MAY_SEND);
        if let Some(rx) = self.rx_waker.take() {
            rx.wake();
        }
        if let Some(tx) = self.tx_waker.take() {
            tx.wake();
        }
    }

    /// Verifies that the channel can receive the next frame.
    #[inline]
    pub fn can_recv(&mut self, cid: LeCid, frame_len: usize) -> bool {
        if frame_len <= self.max_frame_len {
            return self.is_ok();
        }
        if self.is_ok() {
            error!(
                "PDU for {} exceeds maximum frame length ({} > {})",
                cid, frame_len, self.max_frame_len
            );
            self.set_fatal(Status::ERROR);
        }
        false
    }

    /// Adds a complete PDU to the channel queue.
    #[inline]
    pub fn push(&mut self, cid: LeCid, pdu: Frame) {
        if !self.is_ok() {
            return;
        }
        if self.pdu.len() == Self::MAX_PDUS {
            error!("PDU queue overflow for {}", cid);
            self.set_fatal(Status::ERROR);
            return;
        }
        trace!("New PDU for {}", cid);
        self.pdu.push_back(pdu);
        if let Some(rx) = self.rx_waker.take() {
            rx.wake();
        }
    }

    /// Returns [`Poll::Pending`] after configuring the rx waker.
    #[inline(always)]
    fn rx_await(&mut self, cx: &Context<'_>, have_lock: bool) {
        // We want to ensure that there is at most one receiver at any given
        // time. This can't be based on rx_waker being Some because Recv::poll
        // may be called twice in a row before the waker is signaled and
        // cleared. This happens with tokio::select. The caller takes the lock
        // when this method returns and is responsible for releasing it when
        // dropped.
        assert_eq!(
            have_lock,
            self.status.contains(Status::RX_LOCK),
            "channel receiver lock mismatch"
        );
        self.status.insert(Status::RX_LOCK);
        self.rx_waker = Some(cx.waker().clone());
    }

    /// Returns [`Poll::Pending`] after configuring the tx waker.
    #[inline(always)]
    fn tx_await(&mut self, cx: &Context<'_>, have_lock: bool) {
        assert_eq!(
            have_lock,
            self.status.contains(Status::TX_LOCK),
            "channel sender lock mismatch"
        );
        self.status.insert(Status::TX_LOCK);
        self.tx_waker = Some(cx.waker().clone());
    }
}
