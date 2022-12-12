use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use std::thread;
use std::time::Duration;

use rusb::UsbContext;
use tracing::{debug, error, trace, warn};

use crate::hci;

use super::*;

type Device = rusb::Device<rusb::Context>;
type DeviceHandle = rusb::DeviceHandle<rusb::Context>;

/// Provides access to USB Bluetooth controllers.
#[derive(Debug)]
pub struct Usb {
    ctx: rusb::Context,
    run: Arc<AtomicBool>,
    thr: Option<thread::JoinHandle<()>>,
}

impl Usb {
    /// Creates an interface for accessing USB Bluetooth controllers.
    pub fn new() -> Result<Self> {
        let ctx = libusb::new_ctx()?;
        let run = Arc::new(AtomicBool::new(true));
        let thr = {
            let (ctx, run) = (ctx.clone(), Arc::clone(&run));
            Some(thread::spawn(move || Self::event_thread(ctx, run)))
        };
        Ok(Self { ctx, run, thr })
    }

    /// Returns information about all available controllers.
    pub fn controllers(&self) -> Result<Vec<UsbControllerInfo>> {
        Ok(self
            .ctx
            .devices()?
            .iter()
            .filter_map(UsbControllerInfo::for_device)
            .collect())
    }

    /// Convenience function for opening the first device with matching
    /// Vendor/Product ID.
    #[allow(clippy::similar_names)]
    pub fn open_first(&self, vid: u16, pid: u16) -> Result<UsbController> {
        debug!("Opening ID {:04X}:{:04X}", vid, pid);
        let dev = self
            .ctx
            .open_device_with_vid_pid(vid, pid)
            .ok_or(rusb::Error::NotFound)?;
        let ep = Endpoints::discover(&dev.device()).ok_or(rusb::Error::NotSupported)?;
        Ok(UsbController::new(dev, ep))
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
        debug!("Opening {:?}", self.dev);
        // BUG: This may hang for two minutes on Windows when UsbDk enters some
        // bad state.
        Ok(UsbController::new(self.dev.open()?, self.ep))
    }
}

impl Display for UsbControllerInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.dev.fmt(f)
    }
}

/// Opened USB Bluetooth controller.
#[derive(Clone, Debug)]
pub struct UsbController {
    dev: Arc<DeviceHandle>,
    ep: Endpoints,
}

impl UsbController {
    fn new(dev: DeviceHandle, ep: Endpoints) -> Self {
        let dev = Arc::new(dev);
        Self { dev, ep }
    }

    /// Issues a USB reset command.
    pub fn reset(&mut self) -> Result<()> {
        // TODO: Take shared reference: https://github.com/a1ien/rusb/issues/148
        Ok(Arc::get_mut(&mut self.dev).unwrap().reset()?)
    }

    /// Configures the controller for HCI access.
    pub fn init(&mut self) -> Result<()> {
        let dev = Arc::get_mut(&mut self.dev).unwrap();
        if rusb::supports_detach_kernel_driver() {
            // Not supported on Windows
            debug!("Enabling automatic kernel driver detachment");
            dev.set_auto_detach_kernel_driver(true)?;
        }
        debug!("Claiming main interface");
        dev.claim_interface(self.ep.main_iface)?;
        if let Some(isoch) = self.ep.isoch_iface {
            // Do not reserve any bandwidth for the isochronous interface
            debug!("Claiming isochronous interface");
            dev.claim_interface(isoch)?;
            debug!("Setting isochronous interface alt setting to 0");
            dev.set_alternate_setting(isoch, 0)?;
        }
        Ok(())
    }
}

impl Transport for UsbController {
    type Transfer = UsbTransfer;

    fn command(&self) -> Self::Transfer {
        let mut t = libusb::Transfer::new_control(hci::CMD_BUF);
        // [Vol 4] Part B, Section 2.2.2
        t.control_setup(
            libusb::CMD_REQUEST_TYPE,      // bmRequestType
            0x00,                          // bRequest
            0x00,                          // wValue
            u16::from(self.ep.main_iface), // wIndex
        );
        // A one-second timer is recommended for command completion, so we use
        // the same for submission ([Vol 4] Part E, Section 4.4)
        t.set_timeout(Duration::from_secs(1));
        Self::Transfer {
            t,
            dev: Arc::clone(&self.dev),
        }
    }

    fn event(&self) -> Self::Transfer {
        Self::Transfer {
            t: libusb::Transfer::new_interrupt(self.ep.event, hci::EVT_BUF),
            dev: Arc::clone(&self.dev),
        }
    }

    fn acl(&self, dir: Direction, max_data_len: usize) -> Self::Transfer {
        let endpoint = match dir {
            Direction::In => self.ep.acl_in,
            Direction::Out => self.ep.acl_out,
        };
        Self::Transfer {
            t: libusb::Transfer::new_bulk(endpoint, hci::ACL_HDR + max_data_len),
            dev: Arc::clone(&self.dev),
        }
    }
}

/// Asynchronous USB transfer.
#[derive(Debug)]
pub struct UsbTransfer {
    t: Box<libusb::Transfer<rusb::Context>>,
    dev: Arc<DeviceHandle>,
}

impl Transfer for UsbTransfer {
    type Future = UsbTransferFuture;

    #[inline(always)]
    fn buf(&self) -> &LimitedBuf {
        self.t.buf()
    }

    #[inline(always)]
    fn buf_mut(&mut self) -> &mut LimitedBuf {
        self.t.buf_mut()
    }

    #[inline(always)]
    fn hdr_len(&self) -> usize {
        self.t.hdr_len()
    }

    #[inline]
    fn submit(self) -> Result<Self::Future> {
        let dev = Arc::clone(&self.dev);
        self.t
            .submit(Arc::clone(&dev))
            .map_or_else(|e| Err(Error::from(e)), |fut| Ok(Self::Future { fut, dev }))
    }

    #[inline]
    fn result(&self) -> Option<Result<()>> {
        self.t.result().map(|r| r.map_err(Error::from))
    }

    #[inline]
    fn reset(&mut self) {
        self.t.reset();
    }
}

impl AsRef<[u8]> for UsbTransfer {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        (*self.t).as_ref()
    }
}

/// Future that resolves to the finished transfer.
#[derive(Debug)]
pub struct UsbTransferFuture {
    fut: libusb::TransferFuture<rusb::Context>,
    dev: Arc<DeviceHandle>,
}

impl Future for UsbTransferFuture {
    type Output = UsbTransfer;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.fut.poll(cx).map(|t| Self::Output {
            t,
            dev: Arc::clone(&self.dev),
        })
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
        let cfg = dev
            .active_config_descriptor()
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
        for ifc in cfg.interfaces() {
            let ifd = ifc
                .descriptors()
                .find(|id| id.setting_number() == 0)
                .unwrap();
            if !Self::is_bluetooth(&ifc) {
                trace!("    |__ [Non-BT] {ifd:?}");
                continue;
            }
            trace!("    |__ [BT] {ifd:?}");

            if ifd.num_endpoints() == 3 && ifc.descriptors().count() == 1 {
                ep.main_iface = ifd.interface_number();
                for epd in ifd.endpoint_descriptors() {
                    use rusb::{Direction::*, TransferType::*};
                    trace!("        |__ {epd:?}");
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
                    trace!("        |__ {epd:?}");
                }
                for alt in ifc.descriptors().filter(|id| id.setting_number() != 0) {
                    trace!("        |__ [Alt] {alt:?}");
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

mod libusb {
    use std::ffi::{c_char, c_int, c_uint, c_void, CStr};
    use std::fmt::Debug;
    use std::mem::{align_of, replace};
    use std::ptr::{null_mut, NonNull};
    use std::sync::{Arc, Once};
    use std::task::{Poll, Waker};
    use std::time::Duration;

    use rusb::{constants::*, ffi::*, *};
    use tracing::{debug, error, info, trace, warn};

    use crate::util::LimitedBuf;

    macro_rules! check {
        ($x:expr) => {
            // SAFETY: C API call
            match unsafe { $x } {
                LIBUSB_SUCCESS => Ok(()),
                e => Err(api_error(e)),
            }
        };
    }

    pub(super) const CMD_REQUEST_TYPE: u8 =
        LIBUSB_ENDPOINT_OUT | LIBUSB_REQUEST_TYPE_CLASS | LIBUSB_RECIPIENT_INTERFACE;

    /// Asynchronous transfer.
    #[derive(Debug)]
    pub(super) struct Transfer<T: UsbContext> {
        inner: NonNull<libusb_transfer>,
        buf: LimitedBuf,

        // The waker mutex is used to synchronize TransferFuture with the event
        // thread.
        waker: parking_lot::Mutex<Option<Waker>>,
        result: Option<Result<()>>,

        // The context must remain referenced after the first call to
        // libusb_submit_transfer, which stores a permanent device reference,
        // which is used to get the context in <os>_destroy_device when
        // libusb_free_transfer calls libusb_unref_device. If libusb_exit is
        // called before libusb_free_transfer, the device contexts are set to
        // NULL, leading to a crash.
        ctx: Option<T>,

        // The DeviceHandle must stay open while the transfer is in flight to
        // avoid a crash if libusb_cancel_transfer is called:
        // https://github.com/libusb/libusb/issues/1206
        dev: Option<Arc<DeviceHandle<T>>>,
    }

    // SAFETY: libusb_transfer is not aliased
    unsafe impl<T: UsbContext> Send for Transfer<T> {}

    // SAFETY: libusb_transfer is not aliased
    unsafe impl<T: UsbContext> Sync for Transfer<T> {}

    impl<T: UsbContext> Transfer<T> {
        /// Creates a new control transfer.
        pub fn new_control(mut buf_cap: usize) -> Box<Self> {
            buf_cap += LIBUSB_CONTROL_SETUP_SIZE;
            let mut t = Self::new(LIBUSB_TRANSFER_TYPE_CONTROL, 0, buf_cap);
            t.buf.put_at(LIBUSB_CONTROL_SETUP_SIZE, &[]);
            t
        }

        /// Creates a new interrupt transfer.
        pub fn new_interrupt(endpoint: u8, buf_cap: usize) -> Box<Self> {
            assert_eq!(endpoint & LIBUSB_ENDPOINT_DIR_MASK, LIBUSB_ENDPOINT_IN);
            Self::new(LIBUSB_TRANSFER_TYPE_INTERRUPT, endpoint, buf_cap)
        }

        /// Creates a new bulk transfer.
        pub fn new_bulk(endpoint: u8, buf_cap: usize) -> Box<Self> {
            Self::new(LIBUSB_TRANSFER_TYPE_BULK, endpoint, buf_cap)
        }

        /// Creates new transfer state and buffer.
        fn new(typ: u8, endpoint: u8, buf_cap: usize) -> Box<Self> {
            // SAFETY: C API call
            let inner = NonNull::new(unsafe { libusb_alloc_transfer(0) })
                .expect("failed to allocate libusb_transfer struct");
            assert_eq!(inner.as_ptr() as usize % align_of::<libusb_transfer>(), 0);
            let is_out = endpoint & LIBUSB_ENDPOINT_DIR_MASK == LIBUSB_ENDPOINT_OUT;
            // TODO: Use DMA buffers on Linux?
            let mut t = Box::new(Self {
                inner,
                buf: if is_out {
                    LimitedBuf::new(buf_cap)
                } else {
                    LimitedBuf::with_capacity(buf_cap)
                },
                waker: parking_lot::Mutex::new(None),
                result: None,
                ctx: None,
                dev: None,
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

        /// Returns a reference to the transfer buffer.
        #[inline(always)]
        pub const fn buf(&self) -> &LimitedBuf {
            &self.buf
        }

        /// Returns a mutable reference to the transfer buffer.
        #[inline(always)]
        pub fn buf_mut(&mut self) -> &mut LimitedBuf {
            &mut self.buf
        }

        /// Returns the length of the header that precedes the payload in the
        /// transfer buffer.
        #[inline]
        pub fn hdr_len(&self) -> usize {
            if self.inner().transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                LIBUSB_CONTROL_SETUP_SIZE
            } else {
                0
            }
        }

        /// Returns the transfer result or [`None`] if the transfer was not yet
        /// submitted.
        #[inline]
        pub const fn result(&self) -> Option<Result<()>> {
            self.result
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

        /// Sets control transfer parameters.
        #[inline]
        pub fn control_setup(&mut self, request_type: u8, request: u8, value: u16, index: u16) {
            debug_assert_eq!(self.inner().transfer_type, LIBUSB_TRANSFER_TYPE_CONTROL);
            debug_assert!(self.buf.len() >= LIBUSB_CONTROL_SETUP_SIZE);
            // SAFETY: Buffer length is at least LIBUSB_CONTROL_SETUP_SIZE
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
        pub fn submit(mut self: Box<Self>, dev: Arc<DeviceHandle<T>>) -> Result<TransferFuture<T>> {
            assert_eq!(self.result, None, "tried to resubmit a finished transfer");

            let buf_ptr = self.buf.as_mut_ptr();
            let buf_len = match self.inner().endpoint & LIBUSB_ENDPOINT_DIR_MASK {
                LIBUSB_ENDPOINT_OUT => self.buf.len(),
                LIBUSB_ENDPOINT_IN => self.buf.cap(),
                _ => unreachable!(),
            };
            let inner = self.inner_mut();
            if inner.transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                let n =
                    u16::try_from(buf_len.checked_sub(LIBUSB_CONTROL_SETUP_SIZE).unwrap()).unwrap();
                // SAFETY: buf_ptr is not null and buf_len is at least
                // LIBUSB_CONTROL_SETUP_SIZE
                unsafe { (*buf_ptr.cast::<libusb_control_setup>()).wLength = n.to_le() };
            }
            inner.dev_handle = dev.as_raw();
            inner.length = c_int::try_from(buf_len).unwrap();
            inner.buffer = buf_ptr;
            self.ctx = Some(dev.context().clone());
            self.dev = Some(dev);

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
            // SAFETY: buf capacity is at least hdr_len, which is always
            // initialized.
            unsafe { self.buf.set_len(self.hdr_len()) };
            self.result = None;
        }

        /// Ensures mutual exclusion between the callback and
        /// [`TransferFuture`]. Returns [`None`] if `raw` is null.
        #[inline]
        fn lock<'a>(
            raw: *mut Self,
        ) -> Option<(parking_lot::MutexGuard<'a, Option<Waker>>, &'a mut Self)> {
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
                t.dev = None;
                let inner = t.inner_mut();
                inner.dev_handle = null_mut();
                inner.user_data = null_mut();
                let n = inner.actual_length.unsigned_abs() as usize;
                // SAFETY: buf contains hdr_len + n valid bytes
                unsafe { t.buf.set_len(t.hdr_len() + n) }

                if t.result.is_some() {
                    // TransferFuture was dropped or submit failed
                    // SAFETY: We have the only pointer to the original Transfer
                    drop(unsafe { Box::from_raw(raw) });
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

    impl<T: UsbContext> AsRef<[u8]> for Transfer<T> {
        #[inline]
        fn as_ref(&self) -> &[u8] {
            &self.buf
        }
    }

    impl<T: UsbContext> Drop for Transfer<T> {
        fn drop(&mut self) {
            // SAFETY: C API call, inner can be null
            unsafe { libusb_free_transfer(self.inner.as_ptr()) }
        }
    }

    /// An in-flight transfer that will resolve to the original transfer once
    /// finished.
    #[derive(Debug)]
    pub(super) struct TransferFuture<T: UsbContext>(*mut Transfer<T>);

    // SAFETY: Transfer pointer is Send
    unsafe impl<T: UsbContext> Send for TransferFuture<T> {}

    impl<T: UsbContext> TransferFuture<T> {
        /// `Future::poll()` implementation.
        pub fn poll(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Box<Transfer<T>>> {
            let (mut waker, t) = Transfer::lock(self.0).expect("poll of a finished transfer");
            if t.callback_pending() {
                *waker = Some(cx.waker().clone());
                return Poll::Pending;
            }
            // SAFETY: We have the only valid pointer
            let mut t = unsafe { self.take() };
            t.result = Some(match t.inner().status {
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
            });
            Poll::Ready(t)
        }

        /// Cancels a pending transfer. Cancellation is asynchronous and the
        /// future should continue to be polled to determine the final result.
        pub fn cancel(&self) -> Result<()> {
            if let Some((_waker, t)) = Transfer::lock(self.0) {
                // We need to hold the waker lock to ensure that dev_handle is
                // not cleared by the callback. See:
                // https://github.com/libusb/libusb/issues/1206
                if !t.inner().dev_handle.is_null() {
                    return check!(libusb_cancel_transfer(t.inner.as_ptr()));
                }
            }
            Err(Error::NotFound)
        }

        /// Takes ownership of a finished transfer.
        #[inline]
        unsafe fn take(&mut self) -> Box<Transfer<T>> {
            debug_assert_ne!(self.0, null_mut());
            Box::from_raw(replace(&mut self.0, null_mut()))
        }
    }

    impl<T: UsbContext> Drop for TransferFuture<T> {
        fn drop(&mut self) {
            let r = self.cancel();
            if let Some((mut waker, t)) = Transfer::lock(self.0) {
                waker.take();
                t.result = Some(r);
                if !t.callback_pending() {
                    // SAFETY: We have the only valid pointer
                    drop(unsafe { self.take() });
                }
            }
        }
    }

    /// Creates a new libusb context.
    pub(super) fn new_ctx() -> Result<Context> {
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
            LIBUSB_LOG_LEVEL_DEBUG,
        ))?;
        Ok(ctx)
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

    /// Converts libusb error code to [`rusb::Error`].
    const fn api_error(rc: c_int) -> Error {
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

    /// Compile-time detection of new error variants.
    const fn _error_variants(e: Error) {
        match e {
            Error::Io
            | Error::InvalidParam
            | Error::Access
            | Error::NoDevice
            | Error::NotFound
            | Error::Busy
            | Error::Timeout
            | Error::Overflow
            | Error::Pipe
            | Error::Interrupted
            | Error::NoMem
            | Error::NotSupported
            | Error::BadDescriptor
            | Error::Other => {}
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
