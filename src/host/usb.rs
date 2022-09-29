use std::fmt::{Debug, Display, Formatter};
use std::time::Duration;

use bytes::BytesMut;
use rusb::UsbContext;
use tracing::{debug, trace, warn};

use super::*;

type Device = rusb::Device<rusb::Context>;
type DeviceHandle = rusb::DeviceHandle<rusb::Context>;

const TIMEOUT: Duration = Duration::from_millis(5);

/// Provides access to Bluetooth controllers.
#[derive(Debug)]
pub struct Usb {
    ctx: rusb::Context,
}

impl Usb {
    /// Returns a new `Host` instances that can enumerate available controllers.
    pub fn new() -> Result<Self> {
        let ctx = if cfg!(windows) {
            rusb::Context::with_options(&[rusb::UsbOption::use_usbdk()])
        } else {
            rusb::Context::new()
        }?;
        Ok(Self { ctx })
    }

    /// Returns information about all available controllers.
    pub fn controllers(&self) -> Result<Vec<ControllerInfo>> {
        Ok(self
            .ctx
            .devices()?
            .iter()
            .filter_map(ControllerInfo::for_device)
            .collect())
    }
}

/// Information about a Bluetooth controller.
#[derive(Debug)]
pub struct ControllerInfo {
    dev: Device,
    ep: Endpoints,
}

impl ControllerInfo {
    /// Returns `Some(ControllerInfo)` if `dev` is a valid Bluetooth controller.
    fn for_device(dev: Device) -> Option<Self> {
        Endpoints::discover(&dev).map(|ep| Self { dev, ep })
    }

    /// Opens the controller for HCI communication.
    pub fn open(&self) -> Result<Controller> {
        debug!("Opening {:?}", self.dev);
        Controller::new(self.dev.open()?, self.ep)
    }
}

impl Display for ControllerInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.dev.fmt(f)
    }
}

/// Opened Bluetooth controller.
#[derive(Debug)]
pub struct Controller {
    dev: DeviceHandle,
    ep: Endpoints,
}

impl Controller {
    fn new(dev: DeviceHandle, ep: Endpoints) -> Result<Self> {
        Ok(Controller { dev, ep })
    }

    pub fn init(&mut self) -> Result<()> {
        self.dev
            .set_auto_detach_kernel_driver(true)
            .or_else(|e| match e {
                rusb::Error::NotSupported => {
                    warn!("Automatic kernel driver detachment not supported");
                    Ok(())
                }
                _ => Err(e),
            })?;
        self.dev.claim_interface(self.ep.main_iface)?;
        if let Some(isoch) = self.ep.isoch_iface {
            // Do not reserve any bandwidth for the isochronous interface.
            self.dev.claim_interface(isoch)?;
            self.dev.set_alternate_setting(isoch, 0)?;
        }
        Ok(())
    }
}

impl Transport for Controller {
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

    fn read_event(&self) -> Result<Bytes> {
        let mut b = BytesMut::zeroed(2 + 255); // [Vol 4] Part E, Section 5.4.4
        let n = self
            .dev
            .read_interrupt(self.ep.hci_evt, b.as_mut(), TIMEOUT)?;
        b.truncate(n);
        Ok(b.freeze())
    }
}

/// USB addresses for BT interfaces and endpoints ([Vol 4] Part B).
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
