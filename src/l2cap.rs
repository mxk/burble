use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::mem::align_of;
use std::sync::Arc;

use bytes::{BufMut, BytesMut};
use tokio::sync::futures::Notified;

pub use consts::*;
use {rx::*, tx::*};

use crate::hci::{
    ConnHandle, EventCode, EventGuard, EventType, NumberOfCompletedPackets, ACL_HDR,
    ACL_LE_MIN_DATA,
};
use crate::host::Transfer;
use crate::{hci, host};

mod consts;
mod rx;
mod tx;

/// Error type returned by the L2CAP layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    // TODO: Review error encapsulation, maybe combine error types.
    #[error(transparent)]
    Hci(#[from] hci::Error),
    #[error("invalid {0:?}")]
    InvalidConn(ConnHandle),
}

impl From<host::Error> for Error {
    #[inline]
    fn from(e: host::Error) -> Self {
        Self::Hci(e.into())
    }
}

/// Common L2CAP result type.
pub type Result<T> = std::result::Result<T, Error>;

/// Channel identifier.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Cid(u16);

impl Cid {
    /// Invalid CID.
    pub const INVALID: Self = Self(0x0000);
    /// BR/EDR signaling channel.
    pub const SIGNAL: Self = Self(0x0001);
    /// Attribute protocol channel.
    pub const ATT: Self = Self(0x0004);
    /// LE signaling channel.
    pub const LE_SIGNAL: Self = Self(0x0005);
    /// Security Manager protocol channel.
    pub const SM: Self = Self(0x0006);
    /// Maximum LE CID.
    pub const LE_MAX: Self = Self(0x007F);

    /// Wraps a raw CID.
    #[inline]
    #[must_use]
    const fn from_raw(h: u16) -> Self {
        Self(h)
    }

    /// Returns whether the CID is valid.
    #[inline]
    #[must_use]
    pub const fn is_valid(self) -> bool {
        self.0 != 0x0000 // [Vol 3] Part A, Section 2.1
    }
}

impl From<Cid> for u16 {
    #[inline]
    fn from(h: Cid) -> Self {
        h.0
    }
}

/// Channel identifier associated with a specific connection.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct ConnCid {
    hdl: ConnHandle,
    cid: Cid,
}

impl ConnCid {
    /// Combines connection handle with a channel ID.
    #[inline]
    #[must_use]
    const fn new(hdl: ConnHandle, cid: Cid) -> Self {
        Self { hdl, cid }
    }

    /// Returns whether the channel is valid.
    #[inline]
    #[must_use]
    const fn is_valid(self) -> bool {
        self.hdl.is_valid() && self.cid.is_valid()
    }
}

/// Basic L2CAP header size ([Vol 3] Part A, Section 3).
const L2CAP_HDR: usize = 4;

/// Channel manager.
#[derive(Debug)]
#[repr(transparent)]
pub struct ChanManager<T: host::Transport> {
    chans: HashMap<Cid, Arc<RawChan<T>>>,
}

/// L2CAP channel.
#[derive(Debug)]
struct RawChan<T: host::Transport> {
    rm: Arc<ResourceManager<T>>,
    cc: ConnCid,
    pdu_cap: usize,
}

impl<T: host::Transport> RawChan<T> {
    /// Allocates a new outbound PDU.
    #[inline]
    #[must_use]
    pub fn new_pdu(&self) -> TxPdu<T> {
        TxPdu::new(Arc::clone(&self.rm), self.cc, self.pdu_cap)
    }
}

#[derive(Debug)]
struct ResourceManager<T: host::Transport> {
    transport: T,
    ctl: hci::EventWaiterGuard<T>,
    tx: parking_lot::Mutex<TxScheduler<T>>,
    tx_event: tokio::sync::Notify,
    rx: parking_lot::Mutex<RxQueue<T>>,
    rx_event: tokio::sync::Notify,
}

impl<T: host::Transport> ResourceManager<T> {
    async fn tx_wait<R>(&self, mut f: impl FnMut(&mut TxScheduler<T>) -> Option<R>) -> Result<R> {
        loop {
            let notify = {
                let mut sched = self.tx.lock();
                if let Some(r) = f(&mut sched) {
                    return Ok(r);
                }
                self.tx_event.notified()
            };
            self.event(notify).await?;
        }
    }

    /// Blocks the task until notify is signaled.
    async fn event(&self, notify: Notified<'_>) -> Result<()> {
        tokio::pin!(notify);
        loop {
            let evt: EventGuard<T> = tokio::select! {
                _ = &mut notify => return Ok(()),
                evt = self.ctl.next() => evt?,
            };
            // TODO: Connection closed, etc.
            if matches!(
                evt.typ(),
                EventType::Hci(EventCode::NumberOfCompletedPackets)
            ) {
                self.tx
                    .lock()
                    .update(&NumberOfCompletedPackets::from(&mut evt.get()));
                self.tx_event.notify_waiters();
            }
        }
    }
}

/// Raw L2CAP PDU buffer optimized to avoid data copies when the PDU fits within
/// a single ACL data packet.
#[derive(Debug, Default)]
struct RawPdu<T: host::Transport> {
    xfer: Option<T::Transfer>,
    buf: BytesMut,
}

impl<T: host::Transport> RawPdu<T> {
    /// Creates a PDU from a single ACL data packet.
    #[inline]
    #[must_use]
    fn complete(xfer: T::Transfer) -> Self {
        Self {
            xfer: Some(xfer),
            buf: BytesMut::new(),
        }
    }

    /// Allocates a PDU buffer, which can store `data_cap` bytes, excluding the
    /// ACL data packet header.
    #[inline]
    #[must_use]
    fn alloc(data_cap: usize) -> Self {
        let buf = BytesMut::with_capacity(ACL_HDR + data_cap);
        debug_assert_eq!(buf.as_ptr() as usize % align_of::<u16>(), 0);
        Self { xfer: None, buf }
    }

    /// Allocates a PDU recombination buffer and appends the first fragment.
    #[inline]
    #[must_use]
    fn first(pdu_len: usize, data: &[u8]) -> Self {
        let mut pdu = Self::alloc(L2CAP_HDR + pdu_len);
        pdu.buf.extend_from_slice(data);
        pdu
    }

    /// Returns whether the PDU is fully recombined.
    #[inline]
    #[must_use]
    fn is_complete(&self) -> bool {
        self.rem_len() == 0
    }

    /// Returns the number of bytes remaining to complete the PDU.
    #[inline]
    #[must_use]
    fn rem_len(&self) -> usize {
        if self.xfer.is_some() {
            return 0;
        }
        // SAFETY: self.buf starts with aligned ACL data packet and basic L2CAP
        // headers.
        let pdu_len = u16::from_le(unsafe { *self.buf.get_unchecked(ACL_HDR..).as_ptr().cast() });
        ACL_HDR + L2CAP_HDR + usize::from(pdu_len) - self.buf.len()
    }

    /// Returns the PDU buffer, which starts with an ACL data packet header.
    #[inline]
    #[must_use]
    fn buf_mut(&mut self) -> &mut BytesMut {
        (self.xfer.as_mut()).map_or(&mut self.buf, |xfer| xfer.buf_mut())
    }
}

impl<T: host::Transport> AsRef<[u8]> for RawPdu<T> {
    /// Returns the PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        // SAFETY: Buffer always starts with an ACL data packet header
        unsafe {
            (self.xfer.as_ref())
                .map_or(self.buf.as_ref(), |xfer| xfer.as_ref())
                .get_unchecked(ACL_HDR..)
        }
    }
}

/// ACL data transfer allocator.
#[derive(Debug)]
struct Alloc<T: host::Transport> {
    /// Transfers that can be reused.
    free: Vec<T::Transfer>,
    /// Maximum size of a PDU fragment.
    max_frag_len: usize,
    /// Transfer direction.
    out: bool,
}

impl<T: host::Transport> Alloc<T> {
    /// Returns a new outbound transfer allocator.
    #[inline]
    #[must_use]
    fn tx(max_frag_len: usize) -> Self {
        Self::new(max_frag_len, true)
    }

    /// Returns a new inbound transfer allocator.
    #[inline]
    #[must_use]
    fn rx(max_frag_len: usize) -> Self {
        Self::new(max_frag_len, false)
    }

    /// Returns a new inbound transfer allocator.
    #[inline]
    #[must_use]
    fn new(max_frag_len: usize, out: bool) -> Self {
        assert!(max_frag_len >= ACL_LE_MIN_DATA);
        Self {
            free: Vec::with_capacity(8), // TODO: Tune
            max_frag_len,
            out,
        }
    }

    /// Allocates a new ACL data transfer.
    #[inline]
    fn xfer(&mut self, transport: &T) -> T::Transfer {
        if let Some(mut xfer) = self.free.pop() {
            xfer.reset();
            return xfer;
        }
        if self.out {
            transport.acl_in(self.max_frag_len)
        } else {
            transport.acl_out(self.max_frag_len)
        }
    }

    /// Releases an ACL data transfer for reuse.
    #[inline]
    fn free_xfer(&mut self, xfer: T::Transfer) {
        if self.free.len() < self.free.capacity() {
            // TODO: Ensure that xfer was returned by alloc()?
            self.free.push(xfer);
        }
    }

    /// Releases multiple ACL data transfers for reuse.
    #[inline]
    fn free_xfers(&mut self, it: impl Iterator<Item = T::Transfer>) {
        self.free
            .extend(it.take(self.free.capacity() - self.free.len()))
    }

    /// Allocates a new PDU buffer.
    #[inline]
    fn pdu(&mut self, transport: &T, data_cap: usize) -> RawPdu<T> {
        if data_cap <= self.max_frag_len {
            RawPdu::complete(self.xfer(transport))
        } else {
            RawPdu::alloc(data_cap)
        }
    }

    /// Releases PDU buffer for reuse.
    #[inline]
    fn free_pdu(&mut self, mut pdu: RawPdu<T>) {
        pdu.xfer.take().map(|xfer| self.free_xfer(xfer));
    }
}
