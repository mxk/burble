//! Logical Link Control and Adaptation Protocol ([Vol 3] Part A).

use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::mem;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use structbuf::{Pack, Packer, StructBuf};
use tracing::debug;

pub(crate) use chan::*;
pub use {consts::*, handle::*};

use crate::hci::{Role, ACL_HDR};
use crate::host::Transfer;
use crate::le::Addr;
use crate::{att, hci, host, smp};

mod chan;
mod consts;
mod handle;
mod rx;
mod tx;

// TODO: Remove this assumption? Currently, it is required to index into buffers
// that may contain 2^16 bytes of payload with additional headers. Probably
// split off header and payload lengths in methods that require knowing both.
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
    local_addr: Addr,
}

impl<T: host::Transport + 'static> ChanManager<T> {
    /// Creates a new Channel Manager. `local_addr` is the address used to
    /// establish new connections, which is then used by the Security Manager.
    pub async fn new(host: &hci::Host<T>, local_addr: Addr) -> Result<Self> {
        let ctl = host.register(hci::EventFilter::ChanManager)?;
        let rm = ResManager::new(host).await?;
        // TODO: Figure out how to get the local address on a per-connection
        // basis.
        Ok(Self {
            ctl,
            conns: HashMap::new(),
            rm,
            local_addr,
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
    pub fn att_chan(&mut self, link: LeU) -> Option<att::Bearer<T>> {
        (self.conns.get_mut(&link)).and_then(|cn| cn.att_opt.take().map(att::Bearer::new))
    }

    /// Returns the Security Manager (SM) fixed channel for the specified LE-U
    /// logical link.
    pub fn sm_chan(&mut self, link: LeU) -> Option<smp::Peripheral<T>> {
        (self.conns.get_mut(&link).and_then(|cn| cn.sm_opt.take())).map(smp::Peripheral::new)
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
                (self.rm.tx).on_num_completed(&hci::NumberOfCompletedPackets::from(evt));
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
        debug!("Signal on {cid}");
        // TODO: Implement
        // TODO: This will miss signal channel errors
    }

    /// Handles the creation of a new LE-U logical link.
    fn on_connect(&mut self, evt: &hci::LeConnectionComplete) -> Option<LeU> {
        if !evt.status.is_ok() {
            return None;
        }
        let link = LeU::new(evt.handle);
        let ci = Arc::new(ConnInfo {
            role: evt.role,
            local_addr: self.local_addr,
            peer_addr: evt.peer_addr, // TODO: Handle peer_rpa
        });

        // [Vol 3] Part A, Section 4
        let sig = BasicChan::new(link.chan(Cid::LE_SIGNAL), &ci, &self.rm.tx, 23);
        // [Vol 3] Part G, Section 5.2
        let att = BasicChan::new(link.chan(Cid::ATT), &ci, &self.rm.tx, 23);
        // [Vol 3] Part H, Section 3.2
        // TODO: MTU is 23 when LE Secure Connections is not supported
        let sm = BasicChan::new(link.chan(Cid::SM), &ci, &self.rm.tx, 65);

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

/// Information about an established LE-U connection.
#[derive(Debug)]
pub(crate) struct ConnInfo {
    /// Local role.
    pub role: Role,
    /// Local public or random address.
    pub local_addr: Addr,
    /// Remote public or random address.
    pub peer_addr: Addr,
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

/// Inbound or outbound service data unit (SDU) containing at most MTU bytes of
/// information payload ([Vol 3] Part A, Section 3).
#[derive(Debug)]
#[must_use]
pub(crate) struct Sdu<T: host::Transport> {
    i: usize,
    f: Frame<T>,
}

impl<T: host::Transport> Sdu<T> {
    /// Creates an SDU from a frame with information payload at index `i`.
    #[inline]
    fn new(f: Frame<T>, i: usize) -> Self {
        debug_assert!(f.as_ref().len() >= i);
        Self { i, f }
    }
}

impl<T: host::Transport> AsRef<[u8]> for Sdu<T> {
    /// Returns the SDU information payload.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        // SAFETY: SDU always starts with i bytes of L2CAP header data
        unsafe { self.f.as_ref().get_unchecked(self.i..) }
    }
}

impl<T: host::Transport> Pack for Sdu<T> {
    #[inline]
    fn append(&mut self) -> Packer {
        self.f.append()
    }

    #[inline]
    fn at(&mut self, i: usize) -> Packer {
        self.f.at(self.i + i)
    }
}

/// PDU/SDU buffer optimized to avoid data copies when the data fits within one
/// ACL data packet. Each frame starts with a basic L2CAP header at index 0.
#[derive(Debug)]
#[must_use]
enum Frame<T: host::Transport> {
    /// Complete PDU or SDU, starting with the ACL data packet header.
    Transfer(AclTransfer<T>),
    /// Possibly incomplete PDU or SDU, starting with the basic L2CAP header.
    Buf(StructBuf),
}

impl<T: host::Transport> Frame<T> {
    /// Creates a frame from a single ACL data packet.
    #[inline]
    fn complete(xfer: AclTransfer<T>) -> Self {
        debug_assert!(ACL_HDR + L2CAP_HDR <= xfer.as_ref().len());
        Self::Transfer(xfer)
    }

    /// Allocates a PDU recombination buffer and appends the first fragment.
    #[inline]
    fn first(xfer: &AclTransfer<T>, frame_len: usize) -> StructBuf {
        let frag = xfer.as_ref();
        debug_assert!(ACL_HDR + L2CAP_HDR <= frag.len() && frag.len() < ACL_HDR + frame_len);
        let mut buf = StructBuf::with_capacity(frame_len);
        // SAFETY: frag starts with an ACL data packet header
        buf.put_at(0, unsafe { frag.get_unchecked(ACL_HDR..) });
        buf
    }

    /// Returns the transfer containing a complete PDU or `None` if the PDU is
    /// fragmented.
    #[inline]
    fn take_xfer(&mut self) -> Option<AclTransfer<T>> {
        if matches!(*self, Self::Buf(_)) {
            return None;
        }
        match mem::take(self) {
            Self::Transfer(xfer) => Some(xfer),
            Self::Buf(_) => unreachable!(),
        }
    }
}

impl<T: host::Transport> Default for Frame<T> {
    #[inline]
    fn default() -> Self {
        Self::Buf(StructBuf::default())
    }
}

impl<T: host::Transport> AsRef<[u8]> for Frame<T> {
    /// Returns PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        match *self {
            // SAFETY: xfer always starts with an ACL data packet header
            Self::Transfer(ref xfer) => unsafe { xfer.as_ref().get_unchecked(ACL_HDR..) },
            Self::Buf(ref buf) => buf.as_ref(),
        }
    }
}

impl<T: host::Transport> Pack for Frame<T> {
    #[inline]
    fn append(&mut self) -> Packer {
        match *self {
            Self::Transfer(ref mut xfer) => xfer.append(),
            Self::Buf(ref mut buf) => buf.append(),
        }
    }

    #[inline]
    fn at(&mut self, i: usize) -> Packer {
        match *self {
            Self::Transfer(ref mut xfer) => xfer.at(ACL_HDR + i),
            Self::Buf(ref mut buf) => buf.at(i),
        }
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
    acl_data_len: u16,
    /// Transfer direction.
    dir: host::Direction,
}

impl<T: host::Transport> Alloc<T> {
    /// Creates a new transfer allocator.
    #[inline]
    #[must_use]
    fn new(transport: T, dir: host::Direction, acl_data_len: u16) -> Arc<Self> {
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

    /// Allocates an outbound frame with a zero-filled basic L2CAP header.
    fn frame(self: &Arc<Self>, max_frame_len: usize) -> Frame<T> {
        if max_frame_len <= self.acl_data_len as usize {
            // TODO: Reuse transfers. This change was made because remaining()
            // has to report `max_frame_len` rather than `acl_data_len`.
            //let mut xfer = self.xfer();
            #[allow(clippy::cast_possible_truncation)]
            let mut xfer = self.transport.acl(self.dir, max_frame_len as u16);
            xfer.at(ACL_HDR + L2CAP_HDR).put([]);
            Frame::Transfer(AclTransfer::new(xfer, Arc::clone(self)))
        } else {
            // TODO: Reuse buffers?
            let mut buf = StructBuf::new(max_frame_len);
            buf.at(L2CAP_HDR).put([]);
            Frame::Buf(buf)
        }
    }
}

/// ACL data transfer containing a PDU fragment. The transfer starts with an ACL
/// data packet header at index 0.
#[derive(Debug)]
struct AclTransfer<T: host::Transport>(ManuallyDrop<(T::Transfer, Arc<Alloc<T>>)>);

impl<T: host::Transport> AclTransfer<T> {
    /// Wraps an ACL transfer obtained from `alloc`.
    #[inline]
    fn new(xfer: T::Transfer, alloc: Arc<Alloc<T>>) -> Self {
        Self(ManuallyDrop::new((xfer, alloc)))
    }

    /// Returns the transfer direction.
    #[inline]
    fn dir(&self) -> host::Direction {
        self.0 .1.dir
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
        let (mut xfer, alloc) = unsafe { ManuallyDrop::take(&mut self.0) };
        if xfer.at(ACL_HDR).remaining() != alloc.acl_data_len as usize {
            return; // TODO: Remove workaround
        }
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
