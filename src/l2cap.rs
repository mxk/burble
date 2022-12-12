//! Logical Link Control and Adaptation Protocol ([Vol 3] Part A).

#![allow(dead_code)] // TODO: Remove

use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::mem;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use tracing::debug;

pub(crate) use chan::*;
pub use {consts::*, handle::*};

use crate::hci::ACL_HDR;
use crate::host::Transfer;
use crate::util::LimitedBuf;
use crate::{hci, host};

mod chan;
mod consts;
mod handle;
mod rx;
mod tx;

// TODO: Remove this assumption
#[allow(clippy::assertions_on_constants)]
const _: () = assert!(usize::BITS > u16::BITS, "usize too small");

/// Error type returned by the L2CAP layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    // TODO: Review error encapsulation, maybe combine error types.
    #[error(transparent)]
    Hci(#[from] hci::Error),
    #[error("invalid {0:?}")]
    InvalidConn(hci::ConnHandle),
    #[error("channel is closed ({0})")]
    ChanClosed(LeCid),
    #[error("channel is broken ({0})")]
    ChanBroken(LeCid),
}

impl From<host::Error> for Error {
    #[inline]
    fn from(e: host::Error) -> Self {
        Self::Hci(e.into())
    }
}

/// Common L2CAP result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Channel manager responsible for maintaining connection and channel state
/// information.
#[derive(Debug)]
pub struct ChanManager<T: host::Transport> {
    ctl: hci::EventWaiterGuard<T>,
    conns: HashMap<LeU, Conn<T>>,
    rm: ResManager<T>,
}

impl<T: host::Transport + 'static> ChanManager<T> {
    /// Creates a new Channel Manager.
    pub async fn new(host: &hci::Host<T>) -> Result<Self> {
        let ctl = host.register(hci::EventFilter::ChanManager)?;
        let rm = ResManager::new(host).await?;
        Ok(Self {
            ctl,
            rm,
            conns: HashMap::new(),
        })
    }

    /// Receives HCI events and ACL data packets until a new connection is
    /// established. This method is cancel safe.
    pub async fn recv(&mut self) -> Result<LeU> {
        loop {
            tokio::select! {
                r = self.ctl.next() => {
                    if let Some(link) = self.handle_event(&mut r?.get()) {
                        return Ok(link);
                    }
                }
                r = self.rm.rx.recv() => self.handle_signal(r?),
            };
        }
    }

    /// Returns the Attribute Protocol (ATT) fixed channel for the specified
    /// LE-U logical link.
    pub(crate) fn att_chan(&mut self, link: LeU) -> Option<BasicChan<T>> {
        self.conns.get_mut(&link).and_then(|cn| cn.att_opt.take())
    }

    /// Returns the Security Manager (SM) fixed channel for the specified LE-U
    /// logical link.
    pub(crate) fn sm_chan(&mut self, link: LeU) -> Option<BasicChan<T>> {
        self.conns.get_mut(&link).and_then(|cn| cn.sm_opt.take())
    }

    /// Handles HCI control events.
    fn handle_event(&mut self, evt: &mut hci::Event) -> Option<LeU> {
        use hci::{EventCode, EventType::*, SubeventCode};
        match evt.typ() {
            Le(SubeventCode::ConnectionComplete | SubeventCode::EnhancedConnectionComplete) => {
                return self.on_connect(&hci::LeConnectionComplete::from(evt));
            }
            Hci(EventCode::DisconnectionComplete) => {
                self.on_disconnect(hci::DisconnectionComplete::from(evt));
            }
            Hci(EventCode::NumberOfCompletedPackets) => {
                self.rm
                    .tx
                    .on_num_completed(&hci::NumberOfCompletedPackets::from(evt));
            }
            _ => unreachable!("Unhandled Channel Manager event: {}", evt.typ()),
        }
        None
    }

    /// Handles signaling channel communications.
    fn handle_signal(&mut self, cid: LeCid) {
        if cid.chan != Cid::LE_SIGNAL {
            return;
        }
        let _ = self;
        // TODO: Implement
        // TODO: This will miss signal channel errors
    }

    /// Handles the creation of a new LE-U logical link.
    fn on_connect(&mut self, evt: &hci::LeConnectionComplete) -> Option<LeU> {
        if !evt.status.is_ok() {
            return None;
        }
        let link = LeU::new(evt.handle);

        // [Vol 3] Part A, Section 4
        let sig = BasicChan::new(link.chan(Cid::LE_SIGNAL), &self.rm.tx, 23);
        // [Vol 3] Part G, Section 5.2
        let att = BasicChan::new(link.chan(Cid::ATT), &self.rm.tx, 23);
        // [Vol 3] Part H, Section 3.2
        // TODO: MTU is 23 when LE Secure Connections is not supported
        let sm = BasicChan::new(link.chan(Cid::SM), &self.rm.tx, 65);

        // [Vol 3] Part A, Section 2.2
        self.rm.tx.register_link(LeU::new(evt.handle));
        self.rm.rx.register_chan(&att.raw);
        self.rm.rx.register_chan(&sig.raw);
        self.rm.rx.register_chan(&sm.raw);
        let cn = Conn {
            sig,
            att: Arc::clone(&att.raw),
            att_opt: Some(att),
            sm: Arc::clone(&sm.raw),
            sm_opt: Some(sm),
        };
        assert!(self.conns.insert(link, cn).is_none());
        Some(link)
    }

    fn on_disconnect(&mut self, evt: hci::DisconnectionComplete) {
        if !evt.status.is_ok() {
            return;
        }
        let Some(mut cn) = self.conns.remove(&LeU::new(evt.handle)) else { return };
        self.rm.rx.remove_chan(cn.sig.raw.cid);
        self.rm.rx.remove_chan(cn.att.cid);
        self.rm.rx.remove_chan(cn.sm.cid);
        self.rm.tx.on_disconnect(evt);
        cn.set_closed();
    }
}

/// Resource manager responsible for routing Service/Protocol Data Units between
/// logical channels and host transport.
#[derive(Debug)]
struct ResManager<T: host::Transport> {
    rx: rx::State<T>,
    tx: Arc<tx::State<T>>,
}

impl<T: host::Transport + 'static> ResManager<T> {
    /// Creates a new resource manager after configuring the ACL data packet
    /// parameters ([Vol 3] Part A, Section 1.1).
    #[allow(clippy::similar_names)]
    async fn new(host: &hci::Host<T>) -> Result<Self> {
        // [Vol 4] Part E, Section 4.1 and [Vol 4] Part E, Section 7.8.2
        let mut cbuf = host.le_read_buffer_size().await?;
        let acl_num_pkts = if cbuf.acl_data_len == 0 || cbuf.acl_num_pkts == 0 {
            let shared = host.read_buffer_size().await?; // TODO: Handle invalid params
            cbuf.acl_data_len = shared.acl_data_len;
            shared.acl_num_pkts
        } else {
            u16::from(cbuf.acl_num_pkts)
        };
        debug!("Controller buffers: {:?}", cbuf);

        // [Vol 4] Part E, Section 4.2 and [Vol 4] Part E, Section 7.3.39
        // TODO: Check supported features first
        // TODO: Enable controller to host flow control?
        let hbuf = hci::BufferSize {
            acl_data_len: cbuf.acl_data_len,
            acl_num_pkts: 1,
        };
        host.host_buffer_size(hbuf).await?;

        Ok(Self {
            rx: rx::State::new(host.transport().clone(), hbuf.acl_data_len),
            tx: tx::State::new(host.transport().clone(), acl_num_pkts, cbuf.acl_data_len),
        })
    }
}

/// Established connection over an LE-U logical link.
#[derive(Debug)]
struct Conn<T: host::Transport> {
    /// LE Signaling fiexed channel.
    sig: BasicChan<T>,
    /// Attribute Protocol fixed channel.
    att: Arc<RawChan<T>>,
    att_opt: Option<BasicChan<T>>,
    /// Security Manager fixed channel.
    sm: Arc<RawChan<T>>,
    sm_opt: Option<BasicChan<T>>,
}

impl<T: host::Transport> Conn<T> {
    /// Marks all channels as closed.
    fn set_closed(&mut self) {
        self.sig.raw.set_closed();
        self.att.set_closed();
        self.sm.set_closed();
    }
}

/// ACL data transfer allocator.
#[derive(Debug)]
struct Alloc<T: host::Transport> {
    /// Host transport.
    transport: T,
    /// Transfers that can be reused.
    free: parking_lot::Mutex<Vec<T::Transfer>>,
    /// Maximum size of a PDU fragment in an ACL data packet.
    acl_data_len: usize,
    /// Transfer direction.
    dir: host::Direction,
}

impl<T: host::Transport> Alloc<T> {
    /// Creates a new transfer allocator.
    #[inline]
    #[must_use]
    fn new(transport: T, dir: host::Direction, acl_data_len: u16) -> Arc<Self> {
        let acl_data_len = usize::from(acl_data_len);
        assert!(acl_data_len >= hci::ACL_LE_MIN_DATA_LEN);
        Arc::new(Self {
            transport,
            free: parking_lot::Mutex::new(Vec::with_capacity(8)), // TODO: Tune
            acl_data_len,
            dir,
        })
    }

    /// Allocates an empty ACL data transfer.
    #[must_use]
    fn xfer(self: &Arc<Self>) -> AclTransfer<T> {
        let xfer = self.free.lock().pop().map_or_else(
            || self.transport.acl(self.dir, self.acl_data_len),
            |mut xfer| {
                xfer.reset();
                xfer
            },
        );
        AclTransfer::new(xfer, Arc::clone(self))
    }

    /// Allocates an empty outbound buffer.
    #[inline]
    #[must_use]
    fn buf(self: &Arc<Self>, max_frame_len: usize) -> RawBuf<T> {
        if max_frame_len <= self.acl_data_len {
            return RawBuf::Transfer(self.xfer());
        }
        // TODO: Reuse buffers
        // TODO: This must include the transport header for non-USB transports
        RawBuf::Buf(LimitedBuf::new(ACL_HDR + max_frame_len))
    }
}

/// ACL data transfer containing a PDU fragment.
#[derive(Debug)]
struct AclTransfer<T: host::Transport>(ManuallyDrop<(T::Transfer, Arc<Alloc<T>>)>);

impl<T: host::Transport> AclTransfer<T> {
    /// Wraps an ACL transfer obtained from `alloc`.
    #[inline]
    fn new(xfer: T::Transfer, alloc: Arc<Alloc<T>>) -> Self {
        Self(ManuallyDrop::new((xfer, alloc)))
    }

    /// Submits the transfer for execution.
    #[inline]
    async fn submit(mut self) -> host::Result<Self> {
        // SAFETY: self.0 is not used again
        let (xfer, alloc) = unsafe { ManuallyDrop::take(&mut self.0) };
        mem::forget(self);
        Ok(Self::new(xfer.submit()?.await, alloc))
    }
}

impl<T: host::Transport> Drop for AclTransfer<T> {
    fn drop(&mut self) {
        // SAFETY: self.0 is not used again
        let (xfer, alloc) = unsafe { ManuallyDrop::take(&mut self.0) };
        let mut free = alloc.free.lock();
        if free.len() < free.capacity() {
            free.push(xfer);
        }
    }
}

impl<T: host::Transport> Deref for AclTransfer<T> {
    type Target = T::Transfer;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0 .0
    }
}

impl<T: host::Transport> DerefMut for AclTransfer<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0 .0
    }
}

/// Inbound or outbound service data unit.
#[derive(Debug)]
#[must_use]
pub(crate) struct Sdu<T: host::Transport> {
    raw: RawBuf<T>,
    off: usize,
}

impl<T: host::Transport> Sdu<T> {
    /// Returns the SDU buffer, which always starts with ACL and L2CAP packet
    /// headers that must not be modified.
    #[inline]
    pub fn buf_mut(&mut self) -> &mut LimitedBuf {
        self.raw.buf_mut()
    }
}

impl<T: host::Transport> AsRef<[u8]> for Sdu<T> {
    /// Returns SDU bytes after the ACL and L2CAP packet headers.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        &self.raw.as_ref()[self.off..]
    }
}

/// PDU/SDU buffer optimized to avoid data copies when the data fits within a
/// single ACL data packet.
#[derive(Debug)]
enum RawBuf<T: host::Transport> {
    Transfer(AclTransfer<T>),
    Buf(LimitedBuf),
}

impl<T: host::Transport> RawBuf<T> {
    /// Creates a PDU from a single ACL data packet.
    #[inline]
    #[must_use]
    fn complete(xfer: AclTransfer<T>) -> Self {
        debug_assert!(xfer.as_ref().len() >= ACL_HDR + L2CAP_HDR);
        Self::Transfer(xfer)
    }

    /// Allocates a PDU recombination buffer and appends the first fragment,
    /// which must start with the ACL data packet header.
    #[inline]
    #[must_use]
    fn first(pdu_len: usize, frag: &[u8]) -> Self {
        debug_assert!(frag.len() >= ACL_HDR + L2CAP_HDR);
        let mut buf = LimitedBuf::with_capacity(ACL_HDR + L2CAP_HDR + pdu_len);
        buf.put_at(0, frag);
        Self::Buf(buf)
    }

    /// Returns whether the buffer has been allocated.
    #[inline]
    const fn is_some(&self) -> bool {
        match *self {
            Self::Transfer(_) => true,
            Self::Buf(ref buf) => buf.lim() != 0,
        }
    }

    /// Returns whether the PDU is fully recombined.
    #[inline]
    #[must_use]
    fn is_complete(&self) -> bool {
        self.rem_len() == 0
    }

    /// Returns the number of bytes needed to complete the PDU.
    #[inline]
    #[must_use]
    fn rem_len(&self) -> usize {
        match *self {
            Self::Transfer(_) => 0,
            Self::Buf(ref buf) => {
                // SAFETY: buf starts with ACL and basic L2CAP headers
                let pdu_len = u16::from_le_bytes(unsafe { *buf.as_ptr().add(ACL_HDR).cast() });
                ACL_HDR + L2CAP_HDR + usize::from(pdu_len) - buf.len()
            }
        }
    }

    /// Returns the PDU buffer, which starts with an ACL data packet header.
    #[inline]
    fn buf_mut(&mut self) -> &mut LimitedBuf {
        match *self {
            Self::Transfer(ref mut xfer) => xfer.buf_mut(),
            Self::Buf(ref mut buf) => buf,
        }
    }

    /// Returns the transfer containing a complete PDU or `None` if the PDU is
    /// fragmented.
    #[inline]
    fn take_xfer(&mut self) -> Option<AclTransfer<T>> {
        match mem::take(self) {
            Self::Transfer(xfer) => Some(xfer),
            Self::Buf(buf) => {
                *self = Self::Buf(buf);
                None
            }
        }
    }
}

impl<T: host::Transport> Default for RawBuf<T> {
    #[inline]
    fn default() -> Self {
        Self::Buf(LimitedBuf::default())
    }
}

impl<T: host::Transport> AsRef<[u8]> for RawBuf<T> {
    /// Returns PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        let b = match *self {
            Self::Transfer(ref xfer) => xfer.as_ref(),
            Self::Buf(ref buf) => buf.as_ref(),
        };
        // SAFETY: b always starts with an ACL data packet header
        unsafe { b.get_unchecked(ACL_HDR..) }
    }
}
