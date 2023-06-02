use std::fmt::Formatter;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};

use pin_project::{pin_project, pinned_drop};
use structbuf::{Pack, Packer, StructBuf};
use tokio_util::sync::WaitForCancellationFutureOwned;
use tracing::debug;

use burble_const::Uuid;

use crate::name_of;

use super::*;

// TODO: Add support for async callbacks?

/// I/O callback result type.
pub type IoResult = std::result::Result<(), ErrorCode>;

/// Characteristic or descriptor I/O callback.
#[derive(Clone)]
#[repr(transparent)]
pub struct Io(Arc<dyn for<'a> Fn(IoReq<'a>) -> IoResult + Send + Sync>);

impl Io {
    pub const NONE: () = ();

    /// Returns an I/O callback for a method of `T`.
    #[inline(always)]
    pub fn with<T: Send + Sync + 'static>(
        this: &Arc<T>,
        f: impl Fn(&T, IoReq) -> IoResult + Send + Sync + 'static,
    ) -> Self {
        let this = Arc::clone(this);
        Self(Arc::new(move |req: IoReq| f(&this, req)))
    }
}

impl Debug for Io {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (f.debug_tuple(name_of!(Io)).field(&Arc::as_ptr(&self.0))).finish()
    }
}

impl<T: Fn(IoReq) -> IoResult + Send + Sync + 'static> From<T> for Io {
    #[inline(always)]
    fn from(f: T) -> Self {
        Self(Arc::new(f))
    }
}

#[doc(hidden)]
impl From<()> for Io {
    fn from(_: ()) -> Self {
        lazy_static::lazy_static! {
            static ref IO: Io = Io(Arc::new(|_: IoReq| unreachable!()));
        }
        Self(Arc::clone(&IO.0))
    }
}

/// Map of handles to their I/O callbacks.
#[derive(Debug, Default)]
pub(super) struct IoMap(pub(super) BTreeMap<Handle, Io>);

impl IoMap {
    /// Executes a read request.
    #[inline(always)]
    pub fn read(&self, r: &mut ReadReq) -> IoResult {
        self.exec(r.hdl, IoReq::Read(r))
    }

    /// Executes a write request.
    #[inline(always)]
    pub fn write(&self, w: &WriteReq) -> IoResult {
        self.exec(w.hdl, IoReq::Write(w))
    }

    /// Executes a notify request.
    #[inline(always)]
    pub fn notify(&self, n: NotifyReq) -> IoResult {
        self.exec(n.hdl, IoReq::Notify(n))
    }

    /// Executes the specified request.
    #[inline]
    fn exec(&self, hdl: Handle, req: IoReq) -> IoResult {
        (self.0.get(&hdl).ok_or(ErrorCode::UnlikelyError)).and_then(|io| io.0(req))
    }
}

/// Characteristic or descriptor I/O request.
#[derive(Debug)]
#[non_exhaustive]
pub enum IoReq<'a> {
    Read(&'a mut ReadReq),
    Write(&'a WriteReq<'a>),
    Notify(NotifyReq),
}

/// Server characteristic or descriptor read request.
#[derive(Debug)]
pub struct ReadReq {
    pub(super) op: Opcode,
    pub(super) hdl: Handle,
    pub(super) uuid: Option<Uuid>,
    pub(super) off: u16,
    pub(super) buf: StructBuf,
}

impl ReadReq {
    /// Creates a new read request.
    #[inline(always)]
    pub(super) const fn new(op: Opcode, mtu: u16) -> Self {
        Self {
            op,
            hdl: Handle::MAX,
            uuid: None,
            off: 0,
            buf: StructBuf::new(mtu as _),
        }
    }

    /// Sets request parameters.
    #[inline(always)]
    pub(super) fn with(&mut self, hdl: Handle, uuid: Uuid, off: u16) -> &mut Self {
        self.hdl = hdl;
        self.uuid = Some(uuid);
        self.off = off;
        self.buf.clear();
        self
    }

    /// Returns the attribute handle.
    #[inline(always)]
    #[must_use]
    pub const fn handle(&self) -> Handle {
        self.hdl
    }

    /// Returns the attribute UUID.
    #[inline(always)]
    #[must_use]
    pub fn uuid(&self) -> Uuid {
        self.uuid.expect("request parameters not set")
    }

    /// Returns the value offset.
    #[inline(always)]
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.off as _
    }

    /// Provides the complete attribute value with automatic offset and MTU
    /// handling.
    #[inline]
    pub fn complete(&mut self, v: impl AsRef<[u8]>) -> IoResult {
        self.partial((v.as_ref().get(self.offset()..)).ok_or(ErrorCode::InvalidOffset)?)
    }

    /// Provides the attribute value starting at the requested offset. The value
    /// may be truncated to fit within the MTU.
    #[inline]
    pub fn partial(&mut self, v: impl AsRef<[u8]>) -> IoResult {
        let v = v.as_ref();
        self.buf.clear();
        // SAFETY: Truncating the value if `buf` is smaller
        (self.buf).put_at(0, unsafe { v.get_unchecked(..v.len().min(self.buf.lim())) });
        Ok(())
    }
}

/// Server characteristic or descriptor write request.
#[derive(Debug)]
pub struct WriteReq<'a> {
    pub(super) op: Opcode,
    pub(super) hdl: Handle,
    pub(super) uuid: Uuid,
    pub(super) off: u16,
    pub(super) val: &'a [u8],
}

impl<'a> WriteReq<'a> {
    /// Returns the attribute handle.
    #[inline(always)]
    #[must_use]
    pub const fn handle(&self) -> Handle {
        self.hdl
    }

    /// Returns the attribute UUID.
    #[inline(always)]
    #[must_use]
    pub const fn uuid(&self) -> Uuid {
        self.uuid
    }

    /// Returns the value offset.
    #[inline(always)]
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.off as _
    }

    /// Returns the value to be written at the specified offset.
    #[inline(always)]
    #[must_use]
    pub const fn value(&self) -> &'a [u8] {
        self.val
    }

    /// Updates `dst` with the written value. Returns either `InvalidOffset` or
    /// `InvalidAttributeValueLength` if the written value is not a subslice of
    /// `dst`.
    #[inline]
    pub fn update(&self, mut dst: impl AsMut<[u8]>) -> IoResult {
        let Some(dst) = dst.as_mut().get_mut(self.off as usize..) else {
            return Err(ErrorCode::InvalidOffset);
        };
        let Some(dst) = dst.get_mut(..self.val.len()) else {
            return Err(ErrorCode::InvalidAttributeValueLength);
        };
        dst.copy_from_slice(self.val);
        Ok(())
    }
}

impl<'a> AsRef<[u8]> for WriteReq<'a> {
    #[inline(always)]
    fn as_ref(&self) -> &'a [u8] {
        self.val
    }
}

/// Server characteristic notification or indication request. The receiver is
/// expected to keep this request and use it to notify the client of
/// characteristic value changes.
#[derive(Debug)]
pub struct NotifyReq {
    // TODO: Provide ConnHandle or just peer address?
    pub(super) hdl: Handle,
    pub(super) uuid: Uuid,
    pub(super) mtu: u16,
    pub(super) ind: bool,
    pub(super) tx: tokio::sync::mpsc::Sender<NotifyVal>,
    pub(super) ct: tokio_util::sync::CancellationToken,
}

impl NotifyReq {
    /// Returns the characteristic value handle.
    #[inline(always)]
    #[must_use]
    pub const fn handle(&self) -> Handle {
        self.hdl
    }

    /// Returns the characteristic value UUID.
    #[inline(always)]
    #[must_use]
    pub const fn uuid(&self) -> Uuid {
        self.uuid
    }

    /// Returns whether the client requested indications (notifications with
    /// confirmation).
    #[inline(always)]
    #[must_use]
    pub const fn is_indicate(&self) -> bool {
        self.ind
    }

    // TODO: Add a robust version of notifications that places a barrier in the
    // send queue and doesn't return until the PDU is acknowledged with
    // NumberOfCompletedPackets? We don't want to do this unconditionally
    // because it would prevent us from submitting multiple notifications to the
    // controller at the same time, potentially harming throughput.

    /// Calls `f` to provide the updated characteristic value and returns a
    /// future that resolves when the operation is completed. For notifications,
    /// the future resolves once the payload is transferred to the controller's
    /// buffer. For indications, the future resolves after confirmation is
    /// received.
    pub fn notify(&self, f: impl FnOnce(&mut Packer)) -> Notify {
        let mut val = StructBuf::new(usize::from(self.mtu) - 3);
        f(&mut val.append());
        let (hdl, ind) = (self.hdl, self.ind);
        let ct = self.ct.clone().cancelled_owned();
        let (tx, rx) = tokio::sync::oneshot::channel();
        Notify {
            val: Some(NotifyVal { hdl, val, ind, tx }),
            // TODO: We don't want Notify to have a lifetime, but this allocates
            // a ReusableBoxFuture. That's ok for now since we also allocate
            // a oneshot channel.
            tx: tokio_util::sync::PollSender::new(self.tx.clone()),
            rx,
            ct,
        }
    }

    /// Returns when the notification session is closed. This method is cancel
    /// safe.
    #[inline(always)]
    pub async fn closed(&self) {
        self.ct.cancelled().await;
    }

    /// Returns whether the notification session is closed.
    #[inline(always)]
    #[must_use]
    pub fn is_closed(&self) -> bool {
        self.ct.is_cancelled()
    }
}

impl Drop for NotifyReq {
    fn drop(&mut self) {
        if !self.ct.is_cancelled() {
            debug!(
                "Service cancelled notify request for {} {}",
                self.uuid.typ(),
                self.hdl
            );
            self.ct.cancel();
        }
    }
}

/// Characteristic notification or confirmation future.
#[pin_project(PinnedDrop)]
#[derive(Debug)]
pub struct Notify {
    val: Option<NotifyVal>,
    tx: tokio_util::sync::PollSender<NotifyVal>,
    #[pin]
    rx: tokio::sync::oneshot::Receiver<Result<()>>,
    #[pin]
    ct: WaitForCancellationFutureOwned,
}

impl Future for Notify {
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        const CLOSED: Poll<Result<()>> = Poll::Ready(Err(Error::NotifyClosed));
        let this = self.project();
        if this.ct.poll(cx).is_ready() {
            return CLOSED;
        }
        if this.val.is_some() {
            if ready!(this.tx.poll_reserve(cx)).is_err() {
                return CLOSED;
            }
            // SAFETY: `self.val` is Some
            let val = unsafe { this.val.take().unwrap_unchecked() };
            if this.tx.send_item(val).is_err() {
                return CLOSED;
            }
            this.tx.close();
        }
        match this.rx.poll(cx) {
            Poll::Ready(Ok(r)) => Poll::Ready(r),
            Poll::Ready(Err(_)) => CLOSED,
            Poll::Pending => Poll::Pending,
        }
    }
}

#[pinned_drop]
impl PinnedDrop for Notify {
    #[inline(always)]
    fn drop(self: Pin<&mut Self>) {
        self.project().tx.abort_send();
    }
}

/// Characteristic notification or indication value.
#[derive(Debug)]
pub(super) struct NotifyVal {
    hdl: Handle,
    val: StructBuf,
    ind: bool,
    tx: tokio::sync::oneshot::Sender<Result<()>>, // TODO: Avoid allocation
}

impl NotifyVal {
    /// Executes notification or indication procedure.
    #[inline]
    pub async fn exec(self, br: &mut Bearer) {
        let val = self.val.as_ref();
        let r = if self.ind {
            br.handle_value_ind(self.hdl, val).await
        } else {
            br.handle_value_ntf(self.hdl, val).await
        };
        self.result(r);
    }

    /// Reports the notification/indication result to the source.
    #[inline(always)]
    pub fn result(self, r: Result<()>) {
        let _ = self.tx.send(r);
    }
}

impl AsRef<[u8]> for NotifyVal {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        self.val.as_ref()
    }
}
