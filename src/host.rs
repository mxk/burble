use std::fmt::{Debug, Display, Formatter};

use rusb::UsbContext;
use tracing::{debug, trace, warn};

#[derive(Debug, thiserror::Error)]
#[error("libusb error")]
pub struct UsbError {
    #[from]
    source: rusb::Error,
}

pub type Result<T> = std::result::Result<T, UsbError>;
pub type Device = rusb::Device<rusb::Context>;
pub type DeviceHandle = rusb::DeviceHandle<rusb::Context>;

/// Provides access to Bluetooth controllers.
#[derive(Debug)]
pub struct Host {
    ctx: rusb::Context,
}

impl Host {
    pub fn new() -> Result<Self> {
        let ctx = if cfg!(windows) {
            rusb::Context::with_options(&[rusb::UsbOption::use_usbdk()])
        } else {
            rusb::Context::new()
        }?;
        Ok(Self { ctx })
    }

    pub fn controllers(&self) -> Result<Vec<ControllerInfo>> {
        Ok(self
            .ctx
            .devices()?
            .iter()
            .filter_map(ControllerInfo::for_device)
            .collect())
    }
}

#[derive(Debug)]
pub struct ControllerInfo {
    dev: Device,
    ep: Endpoints,
}

impl ControllerInfo {
    fn for_device(dev: Device) -> Option<Self> {
        Endpoints::discover(&dev).map(|ep| Self { dev, ep })
    }
}

impl Display for ControllerInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.dev.fmt(f)
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
