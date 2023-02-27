use std::fmt::Formatter;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{ready, Context, Poll};

use pin_project::{pin_project, pinned_drop};
use structbuf::{Pack, Packer, StructBuf};
use tokio_util::sync::WaitForCancellationFutureOwned;

use burble_const::Uuid;

use crate::name_of;

use super::*;

/// I/O callback result type.
pub type IoResult = std::result::Result<(), ErrorCode>;

/// Characteristic or descriptor I/O callback.
#[derive(Clone)]
#[repr(transparent)]
pub struct Io(Arc<dyn for<'a> Fn(IoReq<'a>) -> IoResult + Send + Sync>);

impl Io {
    pub const NONE: () = ();
}

impl Debug for Io {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(name_of!(Io))
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
#[allow(clippy::exhaustive_enums)]
#[derive(Debug)]
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
    pub(super) uuid: Uuid,
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
            uuid: Uuid::MAX,
            off: 0,
            buf: StructBuf::new(mtu as _),
        }
    }

    /// Sets read request parameters.
    #[inline(always)]
    pub(super) fn with(&mut self, hdl: Handle, uuid: Uuid, off: u16) -> &mut Self {
        self.hdl = hdl;
        self.uuid = uuid;
        self.off = off;
        self.buf.clear();
        self
    }

    /// Returns request opcode.
    #[inline(always)]
    pub(super) const fn opcode(&self) -> Opcode {
        self.op
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
    pub const fn uuid(&self) -> Uuid {
        self.uuid
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
        // SAFETY: Truncating the value if the MTU is smaller
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
    pub(super) mtu: u16,
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

    /// Returns the value to be written at the requested offset.
    #[inline(always)]
    #[must_use]
    pub const fn value(&self) -> &'a [u8] {
        self.val
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
    pub(super) op: Opcode,
    pub(super) hdl: Handle,
    pub(super) uuid: Uuid,
    pub(super) mtu: u16,
    pub(super) ind: bool,
    pub(super) tx: tokio_util::sync::PollSender<NotifyVal>,
    pub(super) ct: tokio_util::sync::CancellationToken,
}

// TODO: Reset CCCD when NotifyReq is dropped?

impl NotifyReq {
    /// Returns the request handle.
    #[inline(always)]
    #[must_use]
    pub const fn handle(&self) -> Handle {
        self.hdl
    }

    /// Returns a future that sends an updated characteristic value to the
    /// client. For indications, the future resolves once confirmation is
    /// received.
    pub fn notify(&mut self, f: impl FnOnce(&mut Packer)) -> Notify {
        let mut val = StructBuf::new(usize::from(self.mtu) - 3);
        f(&mut val.append());
        let (hdl, ind) = (self.hdl, self.ind);
        let ct = self.ct.clone().cancelled_owned();
        let (tx, rx) = tokio::sync::oneshot::channel();
        Notify {
            req: self,
            val: Some(NotifyVal { hdl, val, ind, tx }),
            rx,
            ct,
        }
    }

    /// Resolves when the notification session is closed.
    #[inline(always)]
    async fn closed(&self) {
        self.ct.cancelled().await;
    }

    /// Returns whether the notification session is closed.
    #[inline(always)]
    #[must_use]
    fn is_closed(&self) -> bool {
        self.ct.is_cancelled()
    }
}

/// Characteristic notification or confirmation future.
#[pin_project(PinnedDrop)]
#[derive(Debug)]
pub struct Notify<'a> {
    req: &'a mut NotifyReq,
    val: Option<NotifyVal>,
    #[pin]
    rx: tokio::sync::oneshot::Receiver<Result<()>>,
    #[pin]
    ct: WaitForCancellationFutureOwned,
}

impl Future for Notify<'_> {
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        const CLOSED: Poll<Result<()>> = Poll::Ready(Err(Error::NotifyClosed));
        let this = self.project();
        if this.ct.poll(cx).is_ready() {
            return CLOSED;
        }
        if this.val.is_some() {
            if ready!(this.req.tx.poll_reserve(cx)).is_err() {
                return CLOSED;
            }
            // SAFETY: `self.val` is Some
            let val = unsafe { this.val.take().unwrap_unchecked() };
            if this.req.tx.send_item(val).is_err() {
                return CLOSED;
            }
            this.req.tx.close();
        }
        match this.rx.poll(cx) {
            Poll::Ready(Ok(r)) => Poll::Ready(r),
            Poll::Ready(Err(_)) => CLOSED,
            Poll::Pending => Poll::Pending,
        }
    }
}

#[pinned_drop]
impl PinnedDrop for Notify<'_> {
    #[inline(always)]
    fn drop(self: Pin<&mut Self>) {
        self.project().req.tx.abort_send();
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
        let _ = self.tx.send(if self.ind {
            br.handle_value_ind(self.hdl, self.val.as_ref()).await
        } else {
            br.handle_value_ntf(self.hdl, self.val.as_ref()).await
        });
    }
}
