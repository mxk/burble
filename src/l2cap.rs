#![allow(dead_code)] // TODO: Remove

use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::mem::{take, ManuallyDrop};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use bytes::{BufMut, BytesMut};
use tokio::sync::futures::Notified;

pub use consts::*;

use crate::host::Transfer;
use crate::{hci, hci::*, host};

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
    /// Combines connection handle with a channel ID. Both must be valid.
    #[inline]
    #[must_use]
    const fn new(hdl: ConnHandle, cid: Cid) -> Self {
        assert!(hdl.is_valid());
        assert!(cid.is_valid());
        Self { hdl, cid }
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
    pub fn new_pdu(&self) -> tx::Pdu<T> {
        tx::Pdu::new(Arc::clone(&self.rm), self.cc, self.pdu_cap)
    }
}

#[derive(Debug)]
struct ResourceManager<T: host::Transport> {
    ctl: EventWaiterGuard<T>,
    rx: rx::State<T>,
    tx: tx::State<T>,
}

impl<T: host::Transport> ResourceManager<T> {
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
                    .update(&NumberOfCompletedPackets::from(&mut evt.get()));
            }
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
    /// Maximum size of a PDU fragment.
    max_frag_len: usize,
    /// Transfer direction.
    dir: host::Direction,
}

impl<T: host::Transport> Alloc<T> {
    /// Returns a new transfer allocator.
    #[inline]
    #[must_use]
    fn new(transport: T, dir: host::Direction, max_frag_len: usize) -> Arc<Self> {
        assert!(max_frag_len >= ACL_LE_MIN_DATA);
        Arc::new(Self {
            transport,
            free: parking_lot::Mutex::new(Vec::with_capacity(8)), // TODO: Tune
            max_frag_len,
            dir,
        })
    }

    /// Allocates a new ACL data transfer.
    #[inline]
    fn xfer(self: &Arc<Self>) -> AclTransfer<T> {
        let xfer = self.free.lock().pop().map_or_else(
            || self.transport.acl(self.dir, self.max_frag_len),
            |mut xfer| {
                xfer.reset();
                xfer
            },
        );
        AclTransfer::new(xfer, Arc::clone(self))
    }

    /// Allocates a new PDU buffer.
    #[inline]
    fn pdu(self: &Arc<Self>, data_cap: usize) -> RawPdu<T> {
        RawPdu::new(self, data_cap)
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

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0 .0
    }
}

impl<T: host::Transport> DerefMut for AclTransfer<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0 .0
    }
}

/// PDU buffer optimized to avoid data copies when the PDU fits within a single
/// ACL data packet.
#[derive(Debug)]
enum RawPdu<T: host::Transport> {
    Xfer(AclTransfer<T>),
    Buf(BytesMut),
}

impl<T: host::Transport> RawPdu<T> {
    /// Allocates a new PDU.
    #[inline]
    #[must_use]
    fn new(alloc: &Arc<Alloc<T>>, data_cap: usize) -> Self {
        let mut this = if data_cap <= alloc.max_frag_len {
            debug_assert!(data_cap >= L2CAP_HDR);
            Self::Xfer(alloc.xfer())
        } else {
            Self::Buf(BytesMut::with_capacity(ACL_HDR + data_cap))
        };
        this.buf_mut().put_bytes(0, ACL_HDR + L2CAP_HDR);
        this
    }

    /// Creates a PDU from a single ACL data packet.
    #[inline]
    #[must_use]
    fn complete(xfer: AclTransfer<T>) -> Self {
        debug_assert!(xfer.as_ref().len() >= ACL_HDR + L2CAP_HDR);
        Self::Xfer(xfer)
    }

    /// Allocates a PDU recombination buffer and appends the first fragment,
    /// which must start with the ACL data packet header.
    #[inline]
    #[must_use]
    fn first(pdu_len: usize, frag: &[u8]) -> Self {
        debug_assert!(frag.len() >= ACL_HDR + L2CAP_HDR);
        let mut buf = BytesMut::with_capacity(ACL_HDR + L2CAP_HDR + pdu_len);
        buf.extend_from_slice(frag);
        Self::Buf(buf)
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
            Self::Xfer(_) => 0,
            Self::Buf(ref buf) => {
                // SAFETY: buf starts with ACL and basic L2CAP headers
                let pdu_len = u16::from_le_bytes(unsafe { *buf.as_ptr().add(ACL_HDR).cast() });
                ACL_HDR + L2CAP_HDR + usize::from(pdu_len) - buf.len()
            }
        }
    }

    /// Returns the PDU buffer, which starts with an ACL data packet header.
    #[inline]
    #[must_use]
    fn buf_mut(&mut self) -> &mut BytesMut {
        match *self {
            Self::Xfer(ref mut xfer) => xfer.buf_mut(),
            Self::Buf(ref mut buf) => buf,
        }
    }

    /// Returns the transfer containing a complete PDU or `None` if the PDU is
    /// fragmented.
    #[inline]
    fn take_xfer(&mut self) -> Option<AclTransfer<T>> {
        match take(self) {
            Self::Xfer(xfer) => Some(xfer),
            Self::Buf(buf) => {
                *self = Self::Buf(buf);
                None
            }
        }
    }
}

impl<T: host::Transport> Default for RawPdu<T> {
    #[inline]
    fn default() -> Self {
        Self::Buf(BytesMut::new())
    }
}

impl<T: host::Transport> AsRef<[u8]> for RawPdu<T> {
    /// Returns PDU bytes, starting with the basic L2CAP header.
    #[inline]
    fn as_ref(&self) -> &[u8] {
        let b = match *self {
            Self::Xfer(ref xfer) => xfer.as_ref(),
            Self::Buf(ref buf) => buf.as_ref(),
        };
        // SAFETY: b always starts with an ACL data packet header
        unsafe { b.get_unchecked(ACL_HDR..) }
    }
}
