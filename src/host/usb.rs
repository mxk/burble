use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use std::thread;
use std::time::Duration;

use rusb::UsbContext;
use structbuf::{Pack, Packer};
use tracing::{debug, error, info, trace, warn};

use crate::hci;
use crate::hci::{Direction, TransferType};

use super::*;

type Device = rusb::Device<rusb::Context>;
type DeviceHandle = rusb::DeviceHandle<rusb::Context>;

/// Provides access to USB Bluetooth controllers. Dropping this value stops the
/// event processing thread, which stops all transfers.
#[derive(Debug)]
pub struct Usb {
    ctx: rusb::Context,
    run: Arc<AtomicBool>,
    thr: Option<thread::JoinHandle<()>>,
}

impl Usb {
    /// Creates an interface for accessing USB Bluetooth controllers.
    pub fn new() -> Result<Self> {
        let ctx = libusb::new_ctx(libusb::LIBUSB_LOG_LEVEL_INFO)?;
        let run = Arc::new(AtomicBool::new(true));
        let thr = {
            let (ctx, run) = (ctx.clone(), Arc::clone(&run));
            Some(thread::spawn(move || Self::event_thread(ctx, run)))
        };
        Ok(Self { ctx, run, thr })
    }

    /// Returns information about all available controllers.
    pub fn controllers(&self) -> Result<Vec<UsbControllerInfo>> {
        Ok((self.ctx.devices()?.iter())
            .filter_map(UsbControllerInfo::for_device)
            .collect())
    }

    /// Convenience function for opening the first device with the matching
    /// Vendor/Product ID.
    pub fn open_first(&self, vid: u16, pid: u16) -> Result<UsbController> {
        info!("Opening device ID {:04X}:{:04X}", vid, pid);
        let hdl = libusb::reset(
            (self.ctx.open_device_with_vid_pid(vid, pid)).ok_or(rusb::Error::NotFound)?,
        )?;
        let ep = Endpoints::discover(&hdl.device()).ok_or(rusb::Error::NotSupported)?;
        Ok(UsbController::new(hdl, ep))
    }

    /// Dedicated thread for async transfer and hotplug events.
    #[allow(clippy::needless_pass_by_value)]
    fn event_thread(ctx: rusb::Context, run: Arc<AtomicBool>) {
        debug!("Event thread started");
        while run.load(Ordering::Acquire) {
            if let Err(e) = ctx.handle_events(None) {
                // TODO: Stop all transfers
                error!("Event thread error: {e}");
                break;
            }
        }
        debug!("Event thread terminating");
    }
}

impl Drop for Usb {
    fn drop(&mut self) {
        self.run.store(false, Ordering::Release);
        self.ctx.interrupt_handle_events();
        self.thr.take().map(thread::JoinHandle::join);
    }
}

/// Information about a USB Bluetooth controller.
#[derive(Debug)]
pub struct UsbControllerInfo {
    dev: Device,
    ep: Endpoints,
}

impl UsbControllerInfo {
    /// Returns `Some(UsbControllerInfo)` if `dev` is a valid Bluetooth
    /// controller.
    fn for_device(dev: Device) -> Option<Self> {
        Endpoints::discover(&dev).map(|ep| Self { dev, ep })
    }

    /// Opens the controller for HCI communication.
    pub fn open(&self) -> Result<UsbController> {
        info!("Opening {:?}", self.dev);
        // BUG: This may hang for two minutes on Windows when UsbDk enters some
        // bad state.
        Ok(UsbController::new(
            libusb::reset(self.dev.open()?)?,
            self.ep,
        ))
    }
}

impl Display for UsbControllerInfo {
    #[inline(always)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.dev, f)
    }
}

/// Opened USB Bluetooth controller.
#[derive(Clone, Debug)]
pub struct UsbController {
    hdl: Arc<DeviceHandle>,
    ep: Endpoints,
}

impl UsbController {
    fn new(hdl: DeviceHandle, ep: Endpoints) -> Self {
        let hdl = Arc::new(hdl);
        Self { hdl, ep }
    }

    /// Configures the controller for HCI access.
    pub fn init(&mut self) -> Result<()> {
        let hdl = Arc::get_mut(&mut self.hdl).expect("device is shared");
        if rusb::supports_detach_kernel_driver() {
            // Not supported on Windows
            debug!("Enabling automatic kernel driver detachment");
            hdl.set_auto_detach_kernel_driver(true)?;
        }
        debug!("Claiming main interface");
        hdl.claim_interface(self.ep.main_iface)?;
        if let Some(isoch) = self.ep.isoch_iface {
            // Do not reserve any bandwidth for the isochronous interface
            debug!("Claiming isochronous interface");
            hdl.claim_interface(isoch)?;
            debug!("Setting isochronous interface alt setting to 0");
            hdl.set_alternate_setting(isoch, 0)?;
        }
        Ok(())
    }
}

impl Transport for UsbController {
    fn command(&self) -> Box<dyn Transfer> {
        use rusb::constants::{
            LIBUSB_ENDPOINT_OUT, LIBUSB_RECIPIENT_INTERFACE, LIBUSB_REQUEST_TYPE_CLASS,
        };
        const CMD_REQUEST_TYPE: u8 =
            LIBUSB_ENDPOINT_OUT | LIBUSB_RECIPIENT_INTERFACE | LIBUSB_REQUEST_TYPE_CLASS;
        let mut t = libusb::Transfer::new_control(&self.hdl, hci::CMD_BUF);
        // [Vol 4] Part B, Section 2.2.2
        t.control_setup(
            CMD_REQUEST_TYPE,              // bmRequestType
            0x00,                          // bRequest
            0x00,                          // wValue
            u16::from(self.ep.main_iface), // wIndex
        );
        // A one-second timer is recommended for command completion, so we use
        // the same for submission ([Vol 4] Part E, Section 4.4)
        t.set_timeout(Duration::from_secs(1));
        Box::new(UsbTransfer::Idle(t))
    }

    fn event(&self) -> Box<dyn Transfer> {
        Box::new(UsbTransfer::Idle(libusb::Transfer::new_interrupt(
            &self.hdl,
            self.ep.event,
            hci::EVT_BUF,
        )))
    }

    fn acl(&self, dir: Direction, max_data_len: u16) -> Box<dyn Transfer> {
        let endpoint = match dir {
            Direction::ToHost => self.ep.acl_in,
            Direction::FromHost => self.ep.acl_out,
        };
        Box::new(UsbTransfer::Idle(libusb::Transfer::new_bulk(
            &self.hdl,
            endpoint,
            hci::ACL_HDR + max_data_len as usize,
        )))
    }
}

/// Asynchronous USB transfer.
#[derive(Debug)]
enum UsbTransfer {
    Idle(Box<libusb::Transfer<rusb::Context>>),
    Future(libusb::TransferFuture<rusb::Context>),
}

impl Transfer for UsbTransfer {
    fn typ(&self) -> TransferType {
        use rusb::{Direction::*, TransferType::*};
        match *self {
            Self::Idle(ref t) => match t.typ() {
                Control => TransferType::Command,
                Isochronous => unreachable!(),
                Bulk => TransferType::Acl(match t.dir() {
                    In => Direction::ToHost,
                    Out => Direction::FromHost,
                }),
                Interrupt => TransferType::Event,
            },
            Self::Future(_) => unreachable!(),
        }
    }

    fn exec(mut self: Box<Self>) -> Exec {
        *self = match *self {
            Self::Idle(t) => match t.submit() {
                Ok(t) => Self::Future(t),
                Err(e) => return Exec::ready(Err(e.into())),
            },
            Self::Future(_) => unreachable!(),
        };
        let fut: Pin<Box<Self>> = Pin::new(self);
        Exec::pending(fut)
    }

    fn reset(&mut self) {
        match *self {
            Self::Idle(ref mut t) => t.reset(),
            Self::Future(_) => unreachable!(),
        }
    }
}

impl Future for UsbTransfer {
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.get_mut();
        *this = Self::Idle(match *this {
            Self::Idle(_) => unreachable!(),
            Self::Future(ref mut f) => ready!(Pin::new(f).poll(cx))?,
        });
        Poll::Ready(Ok(()))
    }
}

impl PendingTransfer for UsbTransfer {
    #[inline(always)]
    unsafe fn ready(self: Pin<Box<Self>>) -> Box<dyn Transfer> {
        let t: Box<Self> = Pin::into_inner(self);
        t
    }
}

impl AsRef<[u8]> for UsbTransfer {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        match *self {
            Self::Idle(ref t) => (**t).as_ref(),
            Self::Future(_) => unreachable!(),
        }
    }
}

impl Pack for UsbTransfer {
    #[inline]
    fn append(&mut self) -> Packer {
        match *self {
            Self::Idle(ref mut t) => (**t).append(),
            Self::Future(_) => unreachable!(),
        }
    }

    #[inline]
    fn at(&mut self, i: usize) -> Packer {
        match *self {
            Self::Idle(ref mut t) => (**t).at(i),
            Self::Future(_) => unreachable!(),
        }
    }
}

/// USB addresses for Bluetooth interfaces and endpoints ([Vol 4] Part B,
/// Section 2.1.1).
#[derive(Clone, Copy, Debug, Default)]
struct Endpoints {
    main_iface: u8,
    event: u8,
    acl_out: u8,
    acl_in: u8,
    isoch_iface: Option<u8>,
}

impl Endpoints {
    /// Discovers Bluetooth interfaces and endpoints from USB descriptors.
    /// Returns `None` if `dev` is not a single-function or composite Bluetooth
    /// device.
    fn discover(dev: &Device) -> Option<Self> {
        let cfg = (dev.active_config_descriptor())
            .map_err(|e| {
                warn!("Failed to get config descriptor for {dev:?} ({e})");
                e
            })
            .ok()?;
        if !cfg.interfaces().any(|ifc| Self::is_bluetooth(&ifc)) {
            return None;
        }

        debug!("Bluetooth device at {dev:?}");
        trace!("|__ {:?}", dev.device_descriptor());
        trace!("|__ Active {cfg:?}");

        let mut ep = Self::default();
        let mut all = cfg.interfaces().peekable();
        while let Some(ifc) = all.next() {
            let ifd = (ifc.descriptors().find(|id| id.setting_number() == 0)).unwrap();
            if !Self::is_bluetooth(&ifc) {
                trace!("    |__ [Non-BT] {ifd:?}");
                continue;
            }
            trace!("    |__ [BT] {ifd:?}");
            let cont = if all.peek().is_some() { '|' } else { ' ' };
            if ifd.num_endpoints() == 3 && ifc.descriptors().count() == 1 {
                ep.main_iface = ifd.interface_number();
                for epd in ifd.endpoint_descriptors() {
                    use rusb::{Direction::*, TransferType::*};
                    trace!("    {cont}   |__ {epd:?}");
                    match (epd.transfer_type(), epd.direction()) {
                        (Interrupt, In) => ep.event = epd.address(),
                        (Bulk, In) => ep.acl_in = epd.address(),
                        (Bulk, Out) => ep.acl_out = epd.address(),
                        _ => {
                            warn!("Unexpected endpoint: {epd:?}");
                            return None;
                        }
                    }
                }
            } else if ifd.num_endpoints() == 2 {
                ep.isoch_iface = Some(ifd.interface_number());
                for epd in ifd.endpoint_descriptors() {
                    trace!("    {cont}   |__ {epd:?}");
                }
                for alt in ifc.descriptors().filter(|id| id.setting_number() != 0) {
                    trace!("    {cont}   |__ [Alt] {alt:?}");
                }
            }
        }
        Some(ep)
    }

    fn is_bluetooth(ifc: &rusb::Interface) -> bool {
        // [Vol 4] Part B, Section 6.2
        let d = ifc.descriptors().next().unwrap();
        d.class_code() == 0xE0 && d.sub_class_code() == 0x01 && d.protocol_code() == 0x01
    }
}

impl From<rusb::Error> for Error {
    fn from(e: rusb::Error) -> Self {
        use rusb::Error::*;
        match e {
            Io => Self::Other("I/O error"),
            InvalidParam => Self::Other("invalid parameter"),
            Access => Self::Access,
            NoDevice => Self::Broken,
            NotFound => Self::NotFound,
            Busy => Self::Other("resource busy"),
            Timeout => Self::Timeout,
            Overflow => Self::Other("overflow"),
            Pipe => Self::Other("pipe error"),
            Interrupted => Self::Other("system call interrupted"),
            NoMem => Self::Other("insufficient memory"),
            NotSupported => Self::Other("operation not supported"),
            BadDescriptor => Self::Other("malformed descriptor"),
            Other => Self::Other("other error"),
        }
    }
}

mod libusb {
    use std::ffi::{c_char, c_int, c_uint, c_void, CStr};
    use std::fmt::Debug;
    use std::future::Future;
    use std::mem::{align_of, replace};
    use std::pin::Pin;
    use std::ptr::{null_mut, NonNull};
    use std::sync::{Arc, Once};
    use std::task::{Poll, Waker};
    use std::time::Duration;

    pub use rusb::constants::{
        LIBUSB_LOG_LEVEL_DEBUG, LIBUSB_LOG_LEVEL_ERROR, LIBUSB_LOG_LEVEL_INFO,
        LIBUSB_LOG_LEVEL_NONE, LIBUSB_LOG_LEVEL_WARNING,
    };
    use rusb::{constants::*, ffi::*, *};
    use structbuf::{Pack, Packer, StructBuf};
    use tracing::{debug, error, info, trace, warn};

    use crate::{SyncMutex, SyncMutexGuard};

    macro_rules! check {
        ($x:expr) => {
            // SAFETY: C API call
            match unsafe { $x } {
                LIBUSB_SUCCESS => Ok(()),
                e => Err(api_error(e)),
            }
        };
    }

    /// Asynchronous transfer.
    #[derive(Debug)]
    pub(super) struct Transfer<T: UsbContext> {
        inner: NonNull<libusb_transfer>,
        buf: StructBuf,

        // The waker mutex is used to synchronize TransferFuture with the event
        // thread.
        waker: SyncMutex<Option<Waker>>,
        result: Option<Result<()>>,

        // The DeviceHandle must stay open while the transfer is in flight to
        // avoid a crash if libusb_cancel_transfer is called:
        // https://github.com/libusb/libusb/issues/1206
        //
        // It also keeps the context alive. The context must remain referenced
        // after the first call to libusb_submit_transfer, which stores a
        // permanent device reference, which is used to get the context in
        // <os>_destroy_device when libusb_free_transfer calls
        // libusb_unref_device. If libusb_exit is called before
        // libusb_free_transfer, the device contexts are set to NULL, leading to
        // a crash.
        hdl: Arc<DeviceHandle<T>>,
    }

    // SAFETY: libusb_transfer is not aliased
    unsafe impl<T: UsbContext> Send for Transfer<T> {}

    // SAFETY: libusb_transfer is not aliased
    unsafe impl<T: UsbContext> Sync for Transfer<T> {}

    impl<T: UsbContext> Transfer<T> {
        /// Creates a new control transfer.
        #[allow(clippy::unnecessary_box_returns)]
        pub fn new_control(hdl: &Arc<DeviceHandle<T>>, mut buf_cap: usize) -> Box<Self> {
            buf_cap += LIBUSB_CONTROL_SETUP_SIZE;
            let mut t = Self::new(hdl, LIBUSB_TRANSFER_TYPE_CONTROL, 0, buf_cap);
            t.buf.put_at(LIBUSB_CONTROL_SETUP_SIZE, []);
            t
        }

        /// Creates a new interrupt transfer.
        #[allow(clippy::unnecessary_box_returns)]
        pub fn new_interrupt(
            hdl: &Arc<DeviceHandle<T>>,
            endpoint: u8,
            buf_cap: usize,
        ) -> Box<Self> {
            assert_eq!(endpoint & LIBUSB_ENDPOINT_DIR_MASK, LIBUSB_ENDPOINT_IN);
            Self::new(hdl, LIBUSB_TRANSFER_TYPE_INTERRUPT, endpoint, buf_cap)
        }

        /// Creates a new bulk transfer.
        #[allow(clippy::unnecessary_box_returns)]
        pub fn new_bulk(hdl: &Arc<DeviceHandle<T>>, endpoint: u8, buf_cap: usize) -> Box<Self> {
            Self::new(hdl, LIBUSB_TRANSFER_TYPE_BULK, endpoint, buf_cap)
        }

        /// Creates new transfer state and buffer.
        #[allow(clippy::unnecessary_box_returns)]
        fn new(hdl: &Arc<DeviceHandle<T>>, typ: u8, endpoint: u8, buf_cap: usize) -> Box<Self> {
            // SAFETY: C API call
            let inner = NonNull::new(unsafe { libusb_alloc_transfer(0) })
                .expect("failed to allocate libusb_transfer struct");
            assert_eq!(inner.as_ptr() as usize % align_of::<libusb_transfer>(), 0);
            let is_out = endpoint & LIBUSB_ENDPOINT_DIR_MASK == LIBUSB_ENDPOINT_OUT;
            // TODO: Use DMA buffers on Linux?
            let mut t = Box::new(Self {
                inner,
                buf: if is_out {
                    StructBuf::new(buf_cap)
                } else {
                    StructBuf::with_capacity(buf_cap)
                },
                waker: SyncMutex::new(None),
                result: None,
                hdl: Arc::clone(hdl),
            });
            let mut inner = t.inner_mut();
            if is_out {
                inner.flags = LIBUSB_TRANSFER_SHORT_NOT_OK;
            }
            inner.endpoint = endpoint;
            inner.transfer_type = typ;
            inner.callback = Self::callback;
            t
        }

        /// Returns the length of the header that precedes the payload in the
        /// transfer buffer.
        #[inline]
        fn hdr_len(&self) -> usize {
            if self.inner().transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                LIBUSB_CONTROL_SETUP_SIZE
            } else {
                0
            }
        }

        /// Returns whether a callback is expected.
        #[inline]
        fn callback_pending(&self) -> bool {
            // This must be based on user_data to ensure that the callback will
            // panic even if it does get called.
            !self.inner().user_data.is_null()
        }

        /// Returns a shared reference to the `libusb_transfer` struct.
        #[inline]
        fn inner(&self) -> &libusb_transfer {
            // SAFETY: new() ensures that inner can be converted to a reference
            unsafe { self.inner.as_ref() }
        }

        /// Returns a mutable reference to the `libusb_transfer` struct.
        #[inline]
        fn inner_mut(&mut self) -> &mut libusb_transfer {
            // SAFETY: new() ensures that inner can be converted to a reference
            unsafe { self.inner.as_mut() }
        }

        /// Returns the transfer type.
        #[inline]
        #[must_use]
        pub fn typ(&self) -> TransferType {
            match self.inner().transfer_type {
                LIBUSB_TRANSFER_TYPE_CONTROL => TransferType::Control,
                LIBUSB_TRANSFER_TYPE_ISOCHRONOUS => TransferType::Isochronous,
                LIBUSB_TRANSFER_TYPE_BULK => TransferType::Bulk,
                LIBUSB_TRANSFER_TYPE_INTERRUPT => TransferType::Interrupt,
                _ => unreachable!(),
            }
        }

        /// Returns the transfer direction.
        #[inline]
        #[must_use]
        pub fn dir(&self) -> Direction {
            match self.inner().endpoint & LIBUSB_ENDPOINT_DIR_MASK {
                LIBUSB_ENDPOINT_OUT => Direction::Out,
                LIBUSB_ENDPOINT_IN => Direction::In,
                _ => unreachable!(),
            }
        }

        /// Sets control transfer parameters.
        #[inline]
        pub fn control_setup(&mut self, request_type: u8, request: u8, value: u16, index: u16) {
            debug_assert_eq!(self.inner().transfer_type, LIBUSB_TRANSFER_TYPE_CONTROL);
            debug_assert!(self.buf.len() >= LIBUSB_CONTROL_SETUP_SIZE);
            // SAFETY: buffer length is at least LIBUSB_CONTROL_SETUP_SIZE
            unsafe {
                libusb_fill_control_setup(
                    self.buf.as_mut_ptr(),
                    request_type,
                    request,
                    value,
                    index,
                    0, // Final wLength is set in submit()
                );
            }
        }

        /// Sets transfer timeout.
        #[inline]
        pub fn set_timeout(&mut self, timeout: Duration) {
            self.inner_mut().timeout = c_uint::try_from(timeout.as_millis()).unwrap();
        }

        /// Submits the transfer.
        pub fn submit(mut self: Box<Self>) -> Result<TransferFuture<T>> {
            assert_eq!(self.result, None, "tried to resubmit a finished transfer");

            let buf_ptr = self.buf.as_mut_ptr();
            let buf_len = match self.inner().endpoint & LIBUSB_ENDPOINT_DIR_MASK {
                LIBUSB_ENDPOINT_OUT => self.buf.len(),
                LIBUSB_ENDPOINT_IN => self.buf.capacity(),
                _ => unreachable!(),
            };
            let dev_handle = self.hdl.as_raw();
            let inner = self.inner_mut();
            if inner.transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                let n =
                    u16::try_from(buf_len.checked_sub(LIBUSB_CONTROL_SETUP_SIZE).unwrap()).unwrap();
                // SAFETY: buf_ptr is not null and buf_len is at least
                // LIBUSB_CONTROL_SETUP_SIZE
                unsafe { (*buf_ptr.cast::<libusb_control_setup>()).wLength = n.to_le() };
            }
            inner.dev_handle = dev_handle;
            inner.length = c_int::try_from(buf_len).unwrap();
            inner.buffer = buf_ptr;

            let inner = self.inner.as_ptr();
            let raw = Box::into_raw(self);
            // SAFETY: inner is a valid pointer
            unsafe { (*inner).user_data = raw.cast() };

            if let Err(e) = check!(libusb_submit_transfer(inner)) {
                // SAFETY: raw is a valid pointer
                unsafe { (*raw).result = Some(Err(e)) };
                Self::callback(inner);
                return Err(e);
            }
            Ok(TransferFuture(raw))
        }

        /// Resets the transfer to its pre-submit state.
        pub fn reset(&mut self) {
            let mut inner = self.inner_mut();
            inner.status = 0;
            inner.length = 0;
            inner.actual_length = 0;
            inner.buffer = null_mut();
            // SAFETY: buf capacity is at least hdr_len(), which is always
            // initialized.
            unsafe { self.buf.set_len(self.hdr_len()) };
            self.result = None;
        }

        /// Ensures mutual exclusion between the callback and
        /// [`TransferFuture`]. Returns [`None`] if `raw` is null.
        #[inline]
        fn lock<'a>(raw: *mut Self) -> Option<(SyncMutexGuard<'a, Option<Waker>>, &'a mut Self)> {
            // SAFETY: raw is either null or a valid shared reference
            let waker = unsafe { raw.as_ref() }?.waker.lock();
            // SAFETY: raw is a valid reference, and we have exclusive access
            Some((waker, unsafe { &mut *raw }))
        }

        /// Handles transfer completion callback.
        extern "system" fn callback(inner: *mut libusb_transfer) {
            let r = std::panic::catch_unwind(|| {
                // SAFETY: user_data was set in submit()
                let raw = unsafe { (*inner).user_data }.cast();
                let (mut waker, t) = Self::lock(raw).unwrap();

                // Update state while holding the waker lock
                let inner = t.inner_mut();
                inner.dev_handle = null_mut();
                inner.user_data = null_mut();
                let n = inner.actual_length.unsigned_abs() as usize;
                // SAFETY: buf contains hdr_len() + n valid bytes
                unsafe { t.buf.set_len(t.hdr_len() + n) };

                if t.result.is_some() {
                    // TransferFuture was dropped or submit failed
                    // SAFETY: We have the only pointer to the original Transfer
                    drop(unsafe { Box::from_raw(t as _) });
                } else if let Some(w) = waker.take() {
                    w.wake();
                }
            });
            if let Err(e) = r {
                eprintln!("libusb_transfer callback panic: {e:?}");
                std::process::abort();
            }
        }
    }

    impl<T: UsbContext> Drop for Transfer<T> {
        fn drop(&mut self) {
            // SAFETY: C API call, inner can be null
            unsafe { libusb_free_transfer(self.inner.as_ptr()) }
        }
    }

    impl<T: UsbContext> AsRef<[u8]> for Transfer<T> {
        #[inline]
        fn as_ref(&self) -> &[u8] {
            // SAFETY: buf contains at least hdr_len() bytes
            unsafe { self.buf.as_ref().get_unchecked(self.hdr_len()..) }
        }
    }

    impl<T: UsbContext> Pack for Transfer<T> {
        #[inline]
        fn append(&mut self) -> Packer {
            self.buf.append()
        }

        #[inline]
        fn at(&mut self, i: usize) -> Packer {
            self.buf.at(self.hdr_len() + i)
        }
    }

    /// An in-flight transfer that will resolve to the original transfer once
    /// finished.
    #[derive(Debug)]
    #[repr(transparent)]
    pub(super) struct TransferFuture<T: UsbContext>(*mut Transfer<T>);

    // SAFETY: Transfer pointer is Send
    unsafe impl<T: UsbContext> Send for TransferFuture<T> {}

    // SAFETY: Transfer pointer is Sync
    unsafe impl<T: UsbContext> Sync for TransferFuture<T> {}

    impl<T: UsbContext> TransferFuture<T> {
        /// Takes ownership of a finished transfer.
        #[allow(clippy::unnecessary_box_returns)]
        #[inline]
        unsafe fn take(&mut self) -> Box<Transfer<T>> {
            debug_assert_ne!(self.0, null_mut());
            Box::from_raw(replace(&mut self.0, null_mut()))
        }
    }

    impl<T: UsbContext> Future for TransferFuture<T> {
        type Output = Result<Box<Transfer<T>>>;

        fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
            let this = self.get_mut();
            let (mut waker, t) = Transfer::lock(this.0).expect("poll of a finished transfer");
            if t.callback_pending() {
                *waker = Some(cx.waker().clone());
                return Poll::Pending;
            }
            // SAFETY: We have the only valid pointer
            let mut t = unsafe { this.take() };
            let r = match t.inner().status {
                LIBUSB_TRANSFER_COMPLETED => Ok(()),
                LIBUSB_TRANSFER_ERROR => Err(Error::Io),
                LIBUSB_TRANSFER_TIMED_OUT => Err(Error::Timeout),
                LIBUSB_TRANSFER_CANCELLED => Err(Error::Interrupted),
                LIBUSB_TRANSFER_STALL => match t.inner().transfer_type {
                    LIBUSB_TRANSFER_TYPE_CONTROL => Err(Error::NotSupported),
                    _ => Err(Error::Pipe),
                },
                LIBUSB_TRANSFER_NO_DEVICE => Err(Error::NoDevice),
                LIBUSB_TRANSFER_OVERFLOW => Err(Error::Overflow),
                _ => Err(Error::Other),
            };
            t.result = Some(r);
            Poll::Ready(r.map(|_| t))
        }
    }

    impl<T: UsbContext> Drop for TransferFuture<T> {
        fn drop(&mut self) {
            // We need to hold the waker lock to ensure that dev_handle is
            // not cleared by the callback. See:
            // https://github.com/libusb/libusb/issues/1206
            let Some((mut waker, t)) = Transfer::lock(self.0) else { return };
            t.result = Some(if t.inner().dev_handle.is_null() {
                Err(Error::NotFound)
            } else {
                check!(libusb_cancel_transfer(t.inner.as_ptr()))
            });
            waker.take();
            if !t.callback_pending() {
                // SAFETY: We have the only valid pointer
                drop(unsafe { self.take() });
            }
        }
    }

    /// Creates a new libusb context.
    pub(super) fn new_ctx(max_log_level: c_int) -> Result<Context> {
        init_lib();
        let ctx = Context::new()?;
        if cfg!(windows) {
            match check!(libusb_set_option(ctx.as_raw(), LIBUSB_OPTION_USE_USBDK)) {
                Ok(()) => info!("Using UsbDk backend"),
                Err(Error::NotFound) => info!("Using WinUSB backend"),
                Err(e) => return Err(e),
            }
        }
        check!(libusb_set_option(
            ctx.as_raw(),
            LIBUSB_OPTION_LOG_LEVEL,
            max_log_level,
        ))?;
        Ok(ctx)
    }

    /// Resets the specified device handle.
    pub(super) fn reset<T: UsbContext>(mut hdl: DeviceHandle<T>) -> Result<DeviceHandle<T>> {
        let dev = hdl.device();
        let port = dev.port_numbers()?;
        // WinUSB API with libusbK driver requires interface 0 to be claimed in
        // order to perform an actual device reset:
        // https://github.com/libusb/libusb/issues/1261
        if let Err(e) = hdl.claim_interface(0) {
            warn!("Failed to claim interface 0 before reset: {e}");
        }
        info!("Resetting {dev:?}");
        let ctx = match hdl.reset() {
            Ok(_) => return Ok(hdl),
            Err(Error::NotFound) => {
                let ctx = hdl.context().clone();
                drop(hdl);
                ctx
            }
            Err(e) => return Err(e),
        };
        info!("Attempting to re-open device");
        let all = ctx.devices()?;
        for dev in all.iter() {
            match dev.port_numbers() {
                Ok(p) if p == port => return dev.open(),
                _ => {}
            }
        }
        error!("Failed to find device after reset");
        Err(Error::NoDevice)
    }

    /// Initializes libusb.
    fn init_lib() {
        static INIT: Once = Once::new();
        // SAFETY: C API calls
        INIT.call_once(|| unsafe {
            let v = version();
            info!(
                "libusb version: {}.{}.{}.{}{}",
                v.major(),
                v.minor(),
                v.micro(),
                v.nano(),
                v.rc().unwrap_or("")
            );
            debug!("- LIBUSB_CAP_HAS_CAPABILITY = {}", has_capability());
            debug!("- LIBUSB_CAP_HAS_HOTPLUG = {}", has_hotplug());
            debug!(
                "- LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER = {}",
                supports_detach_kernel_driver()
            );
            libusb_set_log_cb(null_mut(), Some(log_cb), LIBUSB_LOG_CB_GLOBAL);
            let rc = libusb_set_option(null_mut(), LIBUSB_OPTION_LOG_LEVEL, LIBUSB_LOG_LEVEL_DEBUG);
            if rc != LIBUSB_SUCCESS {
                warn!("Failed to enable libusb logging");
            }
        });
    }

    /// Converts libusb error code to [`Error`].
    const fn api_error(rc: c_int) -> Error {
        /// Compile-time detection of new error variants.
        const fn _error_variants(e: Error) {
            use Error::*;
            match e {
                Io | InvalidParam | Access | NoDevice | NotFound | Busy | Timeout | Overflow
                | Pipe | Interrupted | NoMem | NotSupported | BadDescriptor | Other => {}
            }
        }
        match rc {
            LIBUSB_ERROR_IO => Error::Io,
            LIBUSB_ERROR_INVALID_PARAM => Error::InvalidParam,
            LIBUSB_ERROR_ACCESS => Error::Access,
            LIBUSB_ERROR_NO_DEVICE => Error::NoDevice,
            LIBUSB_ERROR_NOT_FOUND => Error::NotFound,
            LIBUSB_ERROR_BUSY => Error::Busy,
            LIBUSB_ERROR_TIMEOUT => Error::Timeout,
            LIBUSB_ERROR_OVERFLOW => Error::Overflow,
            LIBUSB_ERROR_PIPE => Error::Pipe,
            LIBUSB_ERROR_INTERRUPTED => Error::Interrupted,
            LIBUSB_ERROR_NO_MEM => Error::NoMem,
            LIBUSB_ERROR_NOT_SUPPORTED => Error::NotSupported,
            _ => Error::Other,
        }
    }

    extern "system" fn log_cb(_: *mut libusb_context, lvl: c_int, msg: *mut c_void) {
        let r = std::panic::catch_unwind(|| {
            // SAFETY: msg is a valid C string
            let orig = unsafe { CStr::from_ptr(msg as *const c_char) }.to_string_lossy();
            let msg = match orig.as_ref().split_once("libusb: ") {
                Some((_, tail)) => tail.trim_end(),
                _ => return, // Debug header (see log_v() in libusb/core.c)
            };
            match lvl {
                LIBUSB_LOG_LEVEL_ERROR => error!("{}", msg.trim_start_matches("error ")),
                LIBUSB_LOG_LEVEL_WARNING => warn!("{}", msg.trim_start_matches("warning ")),
                LIBUSB_LOG_LEVEL_INFO => debug!("{}", msg.trim_start_matches("info ")),
                LIBUSB_LOG_LEVEL_DEBUG => trace!("{}", msg.trim_start_matches("debug ")),
                _ => trace!("{}", msg.trim_start_matches("unknown ")),
            }
        });
        if let Err(e) = r {
            eprintln!("libusb log callback panic: {e:?}");
            std::process::abort();
        }
    }
}
