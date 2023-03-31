//! Logical Link Control and Adaptation Protocol ([Vol 3] Part A).

use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fmt::{Debug, Formatter};
use std::mem;
use std::ops::Deref;
use std::sync::Arc;

use structbuf::{Pack, Packer, StructBuf};
use tracing::error;

pub(crate) use chan::*;
pub use handle::*;
use {consts::*, rx::Receiver, tx::Sender};

use crate::hci::ACL_HDR;
use crate::l2cap::sig::SigChan;
use crate::{att, hci, host, smp, SyncMutex};

mod chan;
mod consts;
mod handle;
mod rx;
mod sig;
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
pub struct ChanManager {
    rx: tokio::sync::mpsc::Receiver<Conn>,
    join: Option<tokio::task::JoinHandle<Result<()>>>,
}

impl ChanManager {
    /// Creates a new Channel Manager.
    #[inline]
    pub async fn new(host: &hci::Host) -> Result<Self> {
        let (tx, rx) = tokio::sync::mpsc::channel(4);
        let task = ChanManagerTask::new(host.clone(), tx).await?;
        Ok(Self {
            rx,
            join: Some(tokio::spawn(task.run())),
        })
    }

    /// Returns the next LE-U connection. This method is cancel safe.
    #[inline]
    pub async fn next(&mut self) -> Result<Conn> {
        if let Some(cn) = self.rx.recv().await {
            return Ok(cn);
        }
        let join = self.join.take().expect("event loop already joined");
        join.await.expect("event loop panic")?;
        unreachable!()
    }

    /// Closes all connections.
    #[inline]
    pub async fn close_all(mut self) -> Result<()> {
        self.rx.close();
        if let Some(join) = self.join.take() {
            join.await.expect("event loop panic")?;
        }
        Ok(())
    }
}

/// Established connection over an LE-U logical link.
pub struct Conn {
    raw: Arc<RawConn>,
    att: Option<Chan>,
    smp: Option<Chan>,
}

impl Conn {
    /// Creates fixed channel state for a newly established connection.
    #[must_use]
    fn new(host: &hci::Host, link: LeU, rm: &mut ResManager) -> (Self, SigChan) {
        let cn = host.conn(link.into()).expect("invalid link");
        // [Vol 3] Part A, Section 4
        let sig = Chan::new(link.chan(Cid::SIG), &cn, &rm.tx, 23);
        // [Vol 3] Part G, Section 5.2
        let att = Chan::new(link.chan(Cid::ATT), &cn, &rm.tx, 23);
        // [Vol 3] Part H, Section 3.2
        let smp = Chan::new(link.chan(Cid::SMP), &cn, &rm.tx, 65);
        let cn = Self {
            raw: Arc::new(RawConn {
                sig: Arc::clone(&sig.raw),
                att: Arc::clone(&att.raw),
                smp: Arc::clone(&smp.raw),
            }),
            att: Some(att),
            smp: Some(smp),
        };
        // [Vol 3] Part A, Section 2.2
        rm.tx.register_link(link);
        rm.rx.register_chan(&cn.raw.sig);
        rm.rx.register_chan(&cn.raw.att);
        rm.rx.register_chan(&cn.raw.smp);
        (cn, SigChan::new(sig))
    }

    /// Returns the LE-U logical link handle.
    #[inline(always)]
    #[must_use]
    pub fn link(&self) -> LeU {
        self.raw.sig.cid.link
    }

    /// Returns the Attribute Protocol (ATT) fixed channel bearer or [`None`] if
    /// the channel was already consumed.
    #[inline]
    pub fn att_bearer(&mut self) -> Option<att::Bearer> {
        self.att.take().map(att::Bearer::new)
    }

    /// Returns the Security Manager Protocol (SMP) fixed channel for the
    /// Peripheral role or [`None`] if the channel was already consumed.
    #[inline]
    pub fn smp_peripheral(&mut self) -> Option<smp::Peripheral> {
        self.smp.take().map(smp::Peripheral::new)
    }
}

impl Debug for Conn {
    #[inline(always)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.raw, f)
    }
}

/// Internal connection state.
#[derive(Debug)]
struct RawConn {
    /// LE Signaling fixed channel.
    sig: Arc<RawChan>,
    /// Attribute Protocol fixed channel.
    att: Arc<RawChan>,
    /// Security Manager fixed channel.
    smp: Arc<RawChan>,
}

/// Drop guard that closes all connection channels when dropped.
#[derive(Debug)]
#[repr(transparent)]
struct ConnGuard(Arc<RawConn>);

impl Drop for ConnGuard {
    #[inline(always)]
    fn drop(&mut self) {
        self.smp.set_closed();
        self.att.set_closed();
        self.sig.set_closed();
    }
}

impl Deref for ConnGuard {
    type Target = RawConn;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Channel manager task state.
#[derive(Debug)]
pub struct ChanManagerTask {
    host: hci::Host,
    ctl: hci::EventStream,
    tx: tokio::sync::mpsc::Sender<Conn>,
    rm: ResManager,
    conns: BTreeMap<LeU, ConnGuard>,
}

impl ChanManagerTask {
    /// Creates a new channel manager task state.
    #[inline]
    async fn new(host: hci::Host, tx: tokio::sync::mpsc::Sender<Conn>) -> Result<Self> {
        let ctl = host.events();
        let rm = ResManager::new(&host).await?;
        Ok(Self {
            host,
            ctl,
            tx,
            rm,
            conns: BTreeMap::new(),
        })
    }

    /// Handles HCI events and ACL data packets.
    async fn run(mut self) -> Result<()> {
        loop {
            tokio::select! {
                evt = self.ctl.next() => self.handle_event(&evt?),
                r = self.rm.rx.recv() => r?,
            }
        }
    }

    /// Handles HCI control events.
    fn handle_event(&mut self, evt: &hci::Event) {
        use hci::EventCode::*;
        match evt.code() {
            DisconnectionComplete => self.handle_disconnect(evt.get()),
            NumberOfCompletedPackets => self.rm.tx.handle_num_completed(&evt.get()),
            LeConnectionComplete | LeEnhancedConnectionComplete => self.handle_connect(&evt.get()),
            _ => {}
        }
    }

    /// Handles the creation of a new LE-U logical link.
    fn handle_connect(&mut self, evt: &hci::LeConnectionComplete) {
        if !evt.status.is_ok() {
            return;
        }
        let (cn, sig) = Conn::new(&self.host, LeU::new(evt.handle), &mut self.rm);
        let link = cn.link();
        let guard = ConnGuard(Arc::clone(&cn.raw));
        self.tx.try_send(cn).expect("connection channel is full");
        tokio::task::spawn(sig.serve()); // TODO: Store handle?
        assert!(self.conns.insert(link, guard).is_none());
    }

    /// Handles LE-U logical link disconnection.
    fn handle_disconnect(&mut self, evt: hci::DisconnectionComplete) {
        if !evt.status.is_ok() {
            return;
        }
        let Some(cn) = self.conns.remove(&LeU::new(evt.handle)) else { return };
        self.rm.rx.remove_chan(cn.sig.cid);
        self.rm.rx.remove_chan(cn.att.cid);
        self.rm.rx.remove_chan(cn.smp.cid);
        self.rm.tx.handle_disconnect(evt);
    }
}

/// Resource manager responsible for routing Service/Protocol Data Units between
/// logical channels and host transport.
#[derive(Debug)]
struct ResManager {
    rx: Receiver,
    tx: Arc<Sender>,
}

impl ResManager {
    /// Creates a new resource manager after configuring the ACL data packet
    /// parameters ([Vol 3] Part A, Section 1.1).
    async fn new(host: &hci::Host) -> Result<Self> {
        let cbuf = host.info().buffer_size();
        // [Vol 4] Part E, Section 4.2 and [Vol 4] Part E, Section 7.3.39
        // TODO: Enable controller to host flow control?
        let hbuf = hci::BufferSize {
            acl_data_len: cbuf.acl_data_len,
            acl_num_pkts: 1,
        };
        host.host_buffer_size(hbuf).await?;
        Ok(Self {
            rx: Receiver::new(host.transport(), hbuf.acl_data_len),
            tx: Sender::new(host.transport(), cbuf.acl_num_pkts, cbuf.acl_data_len),
        })
    }
}

/// Inbound or outbound service data unit (SDU) containing at most MTU bytes of
/// information payload ([Vol 3] Part A, Section 3).
#[derive(Debug)]
#[must_use]
pub(crate) struct Payload {
    f: Frame,
    i: usize,
}

impl Payload {
    /// Creates an SDU from a frame with information payload at index `i`.
    #[inline]
    fn new(f: Frame, i: usize) -> Self {
        debug_assert!(f.as_ref().len() >= i);
        Self { f, i }
    }
}

impl AsRef<[u8]> for Payload {
    /// Returns the SDU information payload.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        // SAFETY: SDU always starts with i bytes of L2CAP header data
        unsafe { self.f.as_ref().get_unchecked(self.i..) }
    }
}

impl Pack for Payload {
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
enum Frame {
    /// Complete PDU or SDU, starting with the ACL data packet header.
    Transfer(Box<dyn host::Transfer>),
    /// Possibly incomplete PDU or SDU, starting with the basic L2CAP header.
    Buf(StructBuf),
}

impl Frame {
    /// Creates a frame from a single ACL data packet.
    #[inline]
    fn complete(xfer: Box<dyn host::Transfer>) -> Self {
        debug_assert!(ACL_HDR + L2CAP_HDR <= (*xfer).as_ref().len());
        Self::Transfer(xfer)
    }

    /// Allocates a PDU recombination buffer and appends the first fragment.
    #[inline]
    fn first(xfer: &dyn host::Transfer, frame_len: usize) -> StructBuf {
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
    fn take_xfer(&mut self) -> Option<Box<dyn host::Transfer>> {
        if matches!(*self, Self::Buf(_)) {
            return None;
        }
        match mem::take(self) {
            Self::Transfer(xfer) => Some(xfer),
            Self::Buf(_) => unreachable!(),
        }
    }
}

impl Default for Frame {
    #[inline]
    fn default() -> Self {
        Self::Buf(StructBuf::default())
    }
}

impl AsRef<[u8]> for Frame {
    /// Returns PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        match *self {
            // SAFETY: xfer always starts with an ACL data packet header
            Self::Transfer(ref xfer) => unsafe { (**xfer).as_ref().get_unchecked(ACL_HDR..) },
            Self::Buf(ref buf) => buf.as_ref(),
        }
    }
}

impl Pack for Frame {
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
struct Alloc {
    /// Host transport.
    transport: Arc<dyn host::Transport>,
    /// Transfer direction.
    dir: host::Direction,
    /// Maximum size of a PDU fragment in an ACL data packet.
    acl_data_len: u16,
}

impl Alloc {
    /// Creates a new transfer allocator.
    #[inline]
    #[must_use]
    fn new(t: &Arc<dyn host::Transport>, dir: host::Direction, acl_data_len: u16) -> Self {
        assert!(acl_data_len >= hci::ACL_LE_MIN_DATA_LEN);
        Self {
            transport: Arc::clone(t),
            dir,
            acl_data_len,
        }
    }

    /// Allocates a new ACL data transfer.
    #[must_use]
    fn xfer(&self) -> Box<dyn host::Transfer> {
        self.transport.acl(self.dir, self.acl_data_len)
    }

    /// Allocates an outbound frame with a zero-filled basic L2CAP header.
    fn frame(&self, max_frame_len: usize) -> Frame {
        if max_frame_len <= self.acl_data_len as usize {
            #[allow(clippy::cast_possible_truncation)]
            let mut xfer = self.transport.acl(self.dir, max_frame_len as u16);
            xfer.at(ACL_HDR + L2CAP_HDR).put([]);
            Frame::Transfer(xfer)
        } else {
            let mut buf = StructBuf::new(max_frame_len);
            buf.at(L2CAP_HDR).put([]);
            Frame::Buf(buf)
        }
    }
}
