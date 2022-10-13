use std::fmt::{Debug, Display, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use std::thread;
use std::time::Duration;

use bytes::BytesMut;
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
    /// Returns an interface for accessing USB Bluetooth controllers.
    pub fn new() -> Result<Self> {
        let ctx = libusb::new_ctx()?;
        let run = Arc::new(AtomicBool::new(true));
        let thr = {
            let (ctx, run) = (ctx.clone(), run.clone());
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
    pub fn open_first(&self, vid: u16, pid: u16) -> Result<UsbController> {
        debug!("Opening ID {:04x}:{:04x}", vid, pid);
        let dev = self
            .ctx
            .open_device_with_vid_pid(vid, pid)
            .ok_or(rusb::Error::NotFound)?;
        let ep = Endpoints::discover(&dev.device()).ok_or(rusb::Error::NotSupported)?;
        Ok(UsbController::new(dev, ep))
    }

    /// Dedicated thread for async transfer and hotplug events.
    fn event_thread(ctx: rusb::Context, run: Arc<AtomicBool>) {
        debug!("Event thread started");
        while run.load(Ordering::Acquire) {
            if let Err(e) = ctx.handle_events(None) {
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
        self.thr.take().map(|h| h.join());
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
#[derive(Debug)]
pub struct UsbController {
    dev: Arc<DeviceHandle>,
    ep: Endpoints,
}

impl UsbController {
    fn new(dev: DeviceHandle, ep: Endpoints) -> Self {
        let dev = Arc::new(dev);
        UsbController { dev, ep }
    }

    /// Issues a USB reset command.
    pub fn reset(&self) -> Result<()> {
        let dev = unsafe { &mut *(Arc::as_ptr(&self.dev) as *mut DeviceHandle) };
        Ok(dev.reset()?)
    }

    /// Configures the controller for HCI access.
    pub fn init(&mut self) -> Result<()> {
        let dev = Arc::get_mut(&mut self.dev).unwrap();
        if cfg!(unix) {
            // Not supported on Windows
            debug!("Enabling automatic kernel driver detachment");
            dev.set_auto_detach_kernel_driver(true)?;
        }
        debug!("Claiming main interface");
        dev.claim_interface(self.ep.main_iface)?;
        if let Some(isoch) = self.ep.isoch_iface {
            // Do not reserve any bandwidth for the isochronous interface.
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

    fn cmd(&self, f: impl FnOnce(&mut BytesMut)) -> UsbTransfer {
        let mut t = libusb::Transfer::new_control(hci::CMD_BUF);
        // [Vol 4] Part B, Section 2.2.2
        t.control_setup(libusb::CMD_REQUEST_TYPE, 0, 0, self.ep.main_iface as _);
        // A one-second timer is recommended for command completion, so we use
        // the same for submission ([Vol 4] Part E, Section 4.4)
        t.set_timeout(Duration::from_secs(1));
        f(t.buf_mut());
        UsbTransfer::from_raw(t)
    }

    fn evt(&self) -> UsbTransfer {
        let t = libusb::Transfer::new_interrupt(self.ep.hci_evt, hci::EVT_BUF);
        UsbTransfer::from_raw(t)
    }

    fn submit(&self, t: &mut UsbTransfer) -> Result<()> {
        Ok(libusb::Transfer::submit(&mut t.0, self.dev.clone())?)
    }
}

// TODO: Transfer caching and reuse.

type ArcTransfer = Arc<parking_lot::Mutex<libusb::Transfer<rusb::Context>>>;

/// Asynchronous USB transfer.
#[derive(Debug)]
pub struct UsbTransfer(ArcTransfer);

impl UsbTransfer {
    #[inline]
    fn from_raw(t: libusb::Transfer<rusb::Context>) -> Self {
        Self(Arc::new(parking_lot::Mutex::new(t)))
    }
}

impl Transfer for UsbTransfer {
    type Future = UsbTransferResult;

    fn result(&mut self) -> UsbTransferResult {
        UsbTransferResult(self.0.clone())
    }

    fn buf_mut(&mut self) -> &mut BytesMut {
        Arc::get_mut(&mut self.0)
            .expect("transfer is busy")
            .get_mut()
            .buf_mut()
    }

    fn map<T>(&self, f: impl FnOnce(Result<()>, &[u8]) -> T) -> T {
        // Could use an RwLock for this, but the most common case is one caller
        let t = self.0.lock();
        f(
            t.result().expect("transfer not done").map_err(Error::from),
            t.buf(),
        )
    }
}

impl Drop for UsbTransfer {
    fn drop(&mut self) {
        let mut t = self.0.lock();
        if t.in_flight() {
            let _ = t.cancel();
        }
    }
}

/// Future that resolves to the transfer result.
pub struct UsbTransferResult(ArcTransfer);

impl Future for UsbTransferResult {
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        self.0.lock().poll(ctx).map_err(Error::from)
    }
}

/// USB addresses for Bluetooth interfaces and endpoints ([Vol 4] Part B,
/// Section 2.1.1).
#[derive(Clone, Copy, Debug, Default)]
struct Endpoints {
    main_iface: u8,
    hci_evt: u8,
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
                    trace!("        |__ {epd:?}");
                    use rusb::{Direction::*, TransferType::*};
                    match (epd.transfer_type(), epd.direction()) {
                        (Interrupt, In) => ep.hci_evt = epd.address(),
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
    use std::ffi::{c_char, c_int, c_void, CStr};
    use std::mem::align_of;
    use std::ptr::{null_mut, NonNull};
    use std::sync::{Arc, Once};
    use std::task::{Poll, Waker};
    use std::time::Duration;

    use bytes::{BufMut, BytesMut};
    use rusb::{constants::*, ffi::*, *};
    use tracing::{debug, error, info, trace, warn};

    macro_rules! check {
        ($x:expr) => {
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
        buf: BytesMut,
        waker: Option<Waker>,

        // The context must remain referenced after the first call to
        // libusb_submit_transfer, which stores a permanent device reference,
        // which is used to get the context when libusb_free_transfer calls
        // libusb_unref_device followed by <os>_destroy_device functions. If
        // libusb_exit is called before libusb_free_transfer, the device
        // contexts are set to NULL, leading to a crash.
        ctx: Option<T>,

        // The DeviceHandle must stay alive while the transfer is in flight to
        // avoid a crash on Windows: https://github.com/libusb/libusb/issues/1206
        dev: Option<Arc<DeviceHandle<T>>>,
    }

    unsafe impl<T: UsbContext> Send for Transfer<T> {}

    impl<T: UsbContext> Transfer<T> {
        /// Returns a new control transfer.
        pub fn new_control(buf_size: usize) -> Self {
            let mut this = Self::new(
                LIBUSB_TRANSFER_TYPE_CONTROL,
                0,
                LIBUSB_CONTROL_SETUP_SIZE + buf_size,
            );
            this.inner_mut().flags = LIBUSB_TRANSFER_SHORT_NOT_OK;
            this.buf.put_bytes(0, LIBUSB_CONTROL_SETUP_SIZE);
            this
        }

        /// Returns a new interrupt transfer.
        pub fn new_interrupt(endpoint: u8, buf_size: usize) -> Self {
            Self::new(LIBUSB_TRANSFER_TYPE_INTERRUPT, endpoint, buf_size)
        }

        /// Allocates new transfer state and buffer.
        fn new(typ: u8, endpoint: u8, buf_size: usize) -> Self {
            let mut inner = NonNull::new(unsafe { libusb_alloc_transfer(0) })
                .expect("failed to allocate libusb_transfer struct");
            assert_eq!(
                inner.as_ptr().align_offset(align_of::<libusb_transfer>()),
                0
            );
            let mut t = unsafe { inner.as_mut() };
            t.endpoint = endpoint;
            t.transfer_type = typ;
            t.callback = Self::callback;
            // TODO: Use DMA buffers on Linux?
            Self {
                inner,
                buf: BytesMut::with_capacity(buf_size),
                waker: None,
                ctx: None,
                dev: None,
            }
        }

        /// Returns a shared transfer buffer.
        #[inline]
        pub fn buf(&self) -> &[u8] {
            self.buf.as_ref()
        }

        /// Returns a mutable transfer buffer.
        #[inline]
        pub fn buf_mut(&mut self) -> &mut BytesMut {
            &mut self.buf
        }

        /// Returns a reference to the `libusb_transfer` struct.
        #[inline]
        fn inner(&self) -> &libusb_transfer {
            unsafe { self.inner.as_ref() }
        }

        /// Returns a mutable reference to the `libusb_transfer` struct.
        #[inline]
        fn inner_mut(&mut self) -> &mut libusb_transfer {
            unsafe { self.inner.as_mut() }
        }

        /// Returns whether the transfer is idle (not yet submitted).
        #[inline]
        fn is_idle(&self) -> bool {
            self.inner().buffer.is_null()
        }

        /// Returns whether the transfer is in flight.
        #[inline]
        pub(super) fn in_flight(&self) -> bool {
            !self.inner().user_data.is_null()
        }

        /// Sets control transfer parameters.
        #[inline]
        pub fn control_setup(&mut self, request_type: u8, request: u8, value: u16, index: u16) {
            assert!(self.is_idle());
            assert_eq!(self.inner().transfer_type, LIBUSB_TRANSFER_TYPE_CONTROL);
            unsafe {
                libusb_fill_control_setup(
                    self.buf.as_mut_ptr(),
                    request_type,
                    request,
                    value,
                    index,
                    0, // Final wLength is set in submit()
                )
            }
        }

        /// Sets transfer timeout.
        #[inline]
        pub fn set_timeout(&mut self, timeout: Duration) {
            assert!(self.is_idle());
            self.inner_mut().timeout = timeout.as_millis() as _;
        }

        /// Submits the transfer.
        pub fn submit(
            arc: &mut Arc<parking_lot::Mutex<Self>>,
            dev: Arc<DeviceHandle<T>>,
        ) -> Result<()> {
            let mut this = arc.lock();
            assert!(this.is_idle());

            let (buf_ptr, buf_len, buf_cap) =
                (this.buf.as_mut_ptr(), this.buf.len(), this.buf.capacity());
            let t = this.inner_mut();
            t.dev_handle = dev.as_raw();
            t.length = if t.endpoint & LIBUSB_ENDPOINT_DIR_MASK == LIBUSB_ENDPOINT_OUT {
                buf_len
            } else {
                buf_cap
            } as _;
            t.user_data = Arc::into_raw(arc.clone()) as _;
            t.buffer = buf_ptr;
            if t.transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                let n = buf_len - LIBUSB_CONTROL_SETUP_SIZE;
                assert_eq!(n as u16 as usize, n);
                unsafe { (*libusb_control_transfer_get_setup(t)).wLength = (n as u16).to_le() };
            }

            this.ctx = Some(dev.context().clone());
            this.dev = Some(dev);

            check!(libusb_submit_transfer(this.inner.as_ptr())).map_err(|e| {
                this.dev = None;
                let t = this.inner_mut();
                drop(unsafe { Arc::from_raw(t.user_data as *const parking_lot::Mutex<Self>) });
                t.user_data = null_mut();
                e
            })
        }

        /// Cancels a pending transfer. This operation is not immediate and the
        /// transfer should continue to be polled to determine the final result.
        pub fn cancel(&mut self) -> Result<()> {
            if self.inner().dev_handle.is_null() {
                // TODO: Remove when fixed: https://github.com/libusb/libusb/issues/1206
                return Err(Error::NotFound);
            }
            check!(libusb_cancel_transfer(self.inner.as_ptr()))
        }

        /// `Future::poll()` implementation.
        pub fn poll(&mut self, ctx: &mut std::task::Context<'_>) -> Poll<Result<()>> {
            if self.in_flight() {
                self.waker = Some(ctx.waker().clone());
                Poll::Pending
            } else {
                Poll::Ready(self.result().expect("poll of an idle transfer"))
            }
        }

        /// Returns the transfer result or [`None`] if the transfer is not
        /// finished.
        pub fn result(&self) -> Option<Result<()>> {
            if self.is_idle() || self.in_flight() {
                return None;
            }
            Some(match self.inner().status {
                LIBUSB_TRANSFER_COMPLETED => Ok(()),
                LIBUSB_TRANSFER_TIMED_OUT => Err(Error::Timeout),
                LIBUSB_TRANSFER_CANCELLED => Err(Error::Interrupted),
                LIBUSB_TRANSFER_STALL => match self.inner().transfer_type {
                    LIBUSB_TRANSFER_TYPE_CONTROL => Err(Error::NotSupported),
                    _ => Err(Error::Pipe),
                },
                LIBUSB_TRANSFER_NO_DEVICE => Err(Error::NoDevice),
                LIBUSB_TRANSFER_OVERFLOW => Err(Error::Overflow),
                _ => Err(Error::Io),
            })
        }

        /// Handles transfer completion callbacks.
        extern "system" fn callback(t: *mut libusb_transfer) {
            if t.is_null() || unsafe { (*t).user_data }.is_null() {
                warn!("Callback for an invalid transfer");
                unsafe { libusb_free_transfer(t) };
                return;
            }
            let arc = unsafe { Arc::from_raw((*t).user_data as *const parking_lot::Mutex<Self>) };
            let mut this = arc.lock();
            debug_assert_eq!(this.inner.as_ptr(), t);
            this.dev = None;
            let t = this.inner_mut();
            t.dev_handle = null_mut();
            t.user_data = null_mut();
            let n = if t.transfer_type == LIBUSB_TRANSFER_TYPE_CONTROL {
                LIBUSB_CONTROL_SETUP_SIZE
            } else {
                0
            } + t.actual_length as usize;
            unsafe { this.buf.set_len(n) }
            if let Some(w) = this.waker.take() {
                w.wake();
            }
            drop(this);
        }
    }

    impl<T: UsbContext> Drop for Transfer<T> {
        fn drop(&mut self) {
            assert!(!self.in_flight());
            assert!(self.waker.is_none());
            unsafe { libusb_free_transfer(self.inner.as_ptr()) }
            self.inner = NonNull::dangling();
        }
    }

    /// Returns a new libusb context.
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
    fn api_error(rc: c_int) -> Error {
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
    fn _error_variants(e: Error) {
        match e {
            Error::Io => {}
            Error::InvalidParam => {}
            Error::Access => {}
            Error::NoDevice => {}
            Error::NotFound => {}
            Error::Busy => {}
            Error::Timeout => {}
            Error::Overflow => {}
            Error::Pipe => {}
            Error::Interrupted => {}
            Error::NoMem => {}
            Error::NotSupported => {}
            Error::BadDescriptor => {}
            Error::Other => {}
        }
    }

    extern "system" fn log_cb(_: *mut libusb_context, lvl: c_int, msg: *mut c_void) {
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
    }
}
