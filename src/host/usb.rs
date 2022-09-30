use std::fmt::{Debug, Display, Formatter};
use std::time::Duration;

use rusb::UsbContext;
use tracing::{debug, trace, warn};

use super::*;

type Device = rusb::Device<rusb::Context>;
type DeviceHandle = rusb::DeviceHandle<rusb::Context>;

const TIMEOUT: Duration = Duration::from_millis(100);

/// Provides access to USB Bluetooth controllers.
#[derive(Debug)]
pub struct Usb {
    ctx: rusb::Context,
}

impl Usb {
    /// Returns a new `Usb` instance for accessing USB Bluetooth controllers.
    pub fn new() -> Result<Self> {
        Ok(Self {
            ctx: Self::new_ctx()?,
        })
    }

    #[cfg(windows)]
    fn new_ctx() -> rusb::Result<rusb::Context> {
        // UsbDk isn't required, but it's more feature-rich and simpler to use
        // than WinUSB or other alternatives
        rusb::Context::with_options(&[rusb::UsbOption::use_usbdk()])
    }

    #[cfg(unix)]
    fn new_ctx() -> rusb::Result<rusb::Context> {
        rusb::Context::new()
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
        UsbController::new(self.dev.open()?, self.ep)
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
    dev: DeviceHandle,
    ep: Endpoints,
}

impl UsbController {
    fn new(dev: DeviceHandle, ep: Endpoints) -> Result<Self> {
        Ok(UsbController { dev, ep })
    }

    /// Configures the controller for HCI access.
    pub fn init(&mut self) -> Result<()> {
        if cfg!(unix) {
            // Not supported on Windows
            debug!("Enabling automatic kernel driver detachment");
            self.dev.set_auto_detach_kernel_driver(true)?;
        }
        debug!("Claiming main interface");
        self.dev.claim_interface(self.ep.main_iface)?;
        if let Some(isoch) = self.ep.isoch_iface {
            // Do not reserve any bandwidth for the isochronous interface.
            debug!("Claiming isochronous interface");
            self.dev.claim_interface(isoch)?;
            debug!("Setting isochronous interface alt setting to 0");
            self.dev.set_alternate_setting(isoch, 0)?;
        }
        Ok(())
    }
}

impl Transport for UsbController {
    fn write_cmd(&self, b: &[u8]) -> Result<()> {
        // [Vol 4] Part B, Section 2.2.2
        let r = self.dev.write_control(
            rusb::request_type(
                rusb::Direction::Out,
                rusb::RequestType::Class,
                rusb::Recipient::Interface,
            ),
            0,
            0,
            self.ep.main_iface as _,
            b,
            TIMEOUT,
        );
        ensure_eq(r, b.len())
    }

    fn write_async_data(&self, b: &[u8]) -> Result<()> {
        let r = self.dev.write_bulk(self.ep.acl_out, b, TIMEOUT);
        ensure_eq(r, b.len())
    }

    fn read_event(&self, b: &mut [u8]) -> Result<usize> {
        Ok(self.dev.read_interrupt(self.ep.hci_evt, b, TIMEOUT)?)
    }
}

/// USB addresses for Bluetooth interfaces and endpoints ([Vol 4] Part B,
/// Section 2.1.1).
#[derive(Clone, Copy, Debug, Default)]
struct Endpoints {
    main_iface: u8,
    hci_cmd: u8,
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

#[inline]
fn ensure_eq(r: rusb::Result<usize>, want: usize) -> Result<()> {
    match r {
        Ok(n) if n == want => Ok(()),
        Ok(_) => Err(Error::from(rusb::Error::Interrupted)),
        Err(e) => Err(Error::from(e)),
    }
}
