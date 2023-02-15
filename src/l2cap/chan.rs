use std::collections::VecDeque;
use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll, Waker};

use structbuf::{Unpack, Unpacker};
use tracing::{error, trace};

use crate::hci::ACL_LE_MIN_DATA_LEN;
use crate::host;

use super::*;

// TODO: Mark RawChan as closed when BasicChan is dropped?

/// Basic L2CAP channel over an LE-U logical link.
#[derive(Clone, Debug)]
pub(crate) struct BasicChan<T: host::Transport> {
    pub(super) raw: Arc<RawChan<T>>,
    tx: Arc<tx::State<T>>,
    mtu: u16,
}

impl<T: host::Transport> BasicChan<T> {
    /// Creates a new channel.
    #[inline]
    pub(super) fn new(cid: LeCid, cn: &Arc<ConnInfo>, tx: &Arc<tx::State<T>>, mtu: u16) -> Self {
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

    /// Returns connection information.
    #[inline(always)]
    pub(crate) fn conn_info(&self) -> &ConnInfo {
        &self.raw.conn
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
        assert!(mtu >= L2CAP_LE_MIN_MTU);
        self.mtu = mtu;
        self.raw.state.lock().max_frame_len = L2CAP_HDR + mtu as usize;
    }

    /// Creates a new outbound SDU.
    #[inline]
    pub fn new_payload(&self) -> Payload<T> {
        Payload::new(self.tx.new_frame(L2CAP_HDR + self.mtu as usize), L2CAP_HDR)
    }

    /// Returns a future that will resolve to the next inbound SDU.
    #[inline(always)]
    pub fn recv(&mut self) -> Recv<T> {
        Recv(Arc::clone(&self.raw))
    }

    /// Returns the next inbound SDU that matches filter `f`. All other SDUs
    /// stay in the buffer.
    #[inline(always)]
    pub fn recv_filter<F: Fn(Unpacker) -> bool + Send>(&mut self, f: F) -> RecvFilter<T, F> {
        RecvFilter {
            raw: Arc::clone(&self.raw),
            f,
        }
    }

    /// Sends an outbound SDU, returning as soon as the last fragment is
    /// submitted to the controller.
    #[inline]
    pub async fn send(&mut self, sdu: Payload<T>) -> Result<()> {
        self.tx.send(&self.raw, sdu.f).await
    }
}

/// Channel receive future.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct Recv<T: host::Transport>(Arc<RawChan<T>>);

impl<T: host::Transport> Future for Recv<T> {
    type Output = Result<Payload<T>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.0.state.lock();
        if let Err(e) = cs.err(self.0.cid) {
            return Poll::Ready(Err(e));
        }
        if let Some(pdu) = cs.pdu.pop_front() {
            return Poll::Ready(Ok(Payload::new(pdu, L2CAP_HDR)));
        }
        cs.rx_await(cx)
    }
}

/// Channel receive future with SDU filtering.
#[derive(Debug)]
pub(crate) struct RecvFilter<T: host::Transport, F> {
    raw: Arc<RawChan<T>>,
    f: F,
}

impl<T: host::Transport, F: Fn(Unpacker) -> bool + Send> Future for RecvFilter<T, F> {
    type Output = Result<Payload<T>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.raw.state.lock();
        if let Err(e) = cs.err(self.raw.cid) {
            return Poll::Ready(Err(e));
        }
        let mut it = cs.pdu.iter();
        if let Some(i) = it.position(|pdu| (self.f)(pdu.unpack().split_at(L2CAP_HDR).1)) {
            // SAFETY: `i` is within bounds
            let pdu = unsafe { cs.pdu.remove(i).unwrap_unchecked() };
            return Poll::Ready(Ok(Payload::new(pdu, L2CAP_HDR)));
        }
        cs.rx_await(cx)
    }
}

/// Channel state shared between the Channel Manager, Resource Manager, and the
/// channel owner.
#[derive(Debug)]
pub(super) struct RawChan<T: host::Transport> {
    pub cid: LeCid,
    pub conn: Arc<ConnInfo>,
    pub state: parking_lot::Mutex<State<T>>,
}

impl<T: host::Transport> RawChan<T> {
    /// Creates new channel state.
    #[inline]
    fn new(cid: LeCid, cn: &Arc<ConnInfo>, max_frame_len: usize) -> Arc<Self> {
        Arc::new(Self {
            cid,
            conn: Arc::clone(cn),
            state: parking_lot::Mutex::new(State::new(max_frame_len)),
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
    #[inline]
    pub fn deny_send(&self) {
        self.state.lock().status -= Status::MAY_SEND;
    }

    /// Returns a future that blocks until the scheduler allows the channel to
    /// send a PDU fragment.
    #[inline(always)]
    pub const fn may_send(&self) -> MaySend<T> {
        MaySend(self)
    }

    /// Sets channel closed flag.
    #[inline]
    pub fn set_closed(&self) {
        self.state.lock().set_fatal(Status::CLOSED);
    }

    /// Sets channel error flag.
    #[inline]
    pub fn set_error(&self) {
        self.state.lock().set_fatal(Status::ERROR);
    }
}

/// Channel send permission future.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct MaySend<'a, T: host::Transport>(&'a RawChan<T>);

impl<T: host::Transport> Future for MaySend<'_, T> {
    type Output = Result<()>;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut cs = self.0.state.lock();
        if cs.status.contains(Status::MAY_SEND) {
            return Poll::Ready(Ok(()));
        }
        if let Err(e) = cs.err(self.0.cid) {
            return Poll::Ready(Err(e));
        }
        cs.tx_await(cx)
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
    }
}

/// Mutex-protected channel state. When interacting with [`tx::State`], the
/// scheduler must always be locked before the channel to avoid a deadlock.
#[derive(Debug)]
pub(super) struct State<T: host::Transport> {
    /// Channel status flags.
    status: Status,
    /// Maximum PDU length, including the L2CAP header.
    max_frame_len: usize,
    /// Received PDU queue.
    pdu: VecDeque<Frame<T>>,
    /// Receive task waker.
    rx_waker: Option<Waker>,
    /// Transmit task waker.
    tx_waker: Option<Waker>,
}

impl<T: host::Transport> State<T> {
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
    pub fn push(&mut self, cid: LeCid, pdu: Frame<T>) {
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
    fn rx_await<P>(&mut self, cx: &Context<'_>) -> Poll<P> {
        debug_assert!(self.rx_waker.is_none(), "multiple channel receivers");
        self.rx_waker = Some(cx.waker().clone());
        Poll::Pending
    }

    /// Returns [`Poll::Pending`] after configuring the tx waker.
    #[inline(always)]
    fn tx_await<P>(&mut self, cx: &Context<'_>) -> Poll<P> {
        debug_assert!(self.tx_waker.is_none(), "multiple channel senders");
        self.tx_waker = Some(cx.waker().clone());
        Poll::Pending
    }
}
