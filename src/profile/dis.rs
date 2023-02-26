//! Device Information Service ([DIS]).
//!
//! This service exposes manufacturer and/or vendor information about a device.
//!
//! [DIS]: https://www.bluetooth.com/specifications/specs/device-information-service-1-1/

use crate::att::Perms;
use crate::gatt::{Builder, Characteristic, Schema, Service, ServiceDef};

/// Device Information Service configuration.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct DeviceInfoService {
    pub manufacturer_name: Option<String>,
    pub model_num: Option<String>,
    pub serial_num: Option<String>,
    pub hardware_rev: Option<String>,
    pub firmware_rev: Option<String>,
    pub software_rev: Option<String>,
    pub system_id: Option<Eui64>,
    pub regulatory_data: Option<RegulatoryData>,
    pub pnp_id: Option<PnpId>,
}

/// Implements `with_<x>` methods for [`String`] characteristics.
macro_rules! with_str {
    ($($(#[$doc:meta])* $f:ident),*$(,)?) => {$(::paste::paste! {
        $(#[$doc])*
        #[inline(always)]
        #[must_use]
        pub fn [<with_ $f>](mut self, v: impl AsRef<str>) -> Self {
            self.$f = Some(v.as_ref().to_owned());
            self
        }
    })*}
}

impl DeviceInfoService {
    /// Creates an empty device information service.
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    with_str! {
        /// Sets device manufacturer name.
        manufacturer_name,
        /// Sets device model number.
        model_num,
        /// Sets device serial number.
        serial_num,
        /// Sets device hardware revision.
        hardware_rev,
        /// Sets device firmware revision.
        firmware_rev,
        /// Sets device software revision.
        software_rev,
    }

    /// Sets device system ID.
    #[inline(always)]
    #[must_use]
    pub const fn with_system_id(mut self, v: Eui64) -> Self {
        self.system_id = Some(v);
        self
    }

    /// Sets device IEEE 11073-20601 Regulatory Certification Data List.
    #[allow(clippy::missing_const_for_fn)]
    #[inline(always)]
    #[must_use]
    pub fn with_regulatory_data(mut self, v: RegulatoryData) -> Self {
        self.regulatory_data = Some(v);
        self
    }

    /// Sets device PNP ID.
    #[inline(always)]
    #[must_use]
    pub const fn with_pnp_id(mut self, v: PnpId) -> Self {
        self.pnp_id = Some(v);
        self
    }

    /// Defines the service schema.
    pub fn define(&self, b: &mut Builder<Schema>, perms: impl Into<Perms>) {
        fn chr(b: &mut Builder<ServiceDef>, c: Characteristic, p: Perms, v: impl AsRef<[u8]>) {
            b.ro_characteristic(c, p, v, |_| {});
        }
        let p = perms.into();
        b.primary_service(Service::DeviceInformation, [], |b| {
            use Characteristic::*;
            if let Some(v) = self.manufacturer_name.as_ref() {
                chr(b, ManufacturerNameString, p, v);
            }
            if let Some(v) = self.model_num.as_ref() {
                chr(b, ModelNumberString, p, v);
            }
            if let Some(v) = self.serial_num.as_ref() {
                chr(b, SerialNumberString, p, v);
            }
            if let Some(v) = self.hardware_rev.as_ref() {
                chr(b, HardwareRevisionString, p, v);
            }
            if let Some(v) = self.firmware_rev.as_ref() {
                chr(b, FirmwareRevisionString, p, v);
            }
            if let Some(v) = self.software_rev.as_ref() {
                chr(b, SoftwareRevisionString, p, v);
            }
            if let Some(v) = self.system_id.as_ref() {
                chr(b, SystemId, p, v.0.to_le_bytes());
            }
            if let Some(v) = self.regulatory_data.as_ref() {
                chr(b, IeeeRegulatoryCertificationDataList, p, &v.0);
            }
            if let Some(v) = self.pnp_id.as_ref() {
                chr(b, PnpId, p, v.to_bytes());
            }
        });
    }
}

/// Extended Organizationally Unique Identifier consisting of a 24-bit
/// IEEE-assigned OUI and a 40-bit manufacturer-defined identifier unique for
/// each individual instance of the product (\[GSS\] Section 3.210).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct Eui64(u64);

impl Eui64 {
    /// Creates a new EUI-64.
    #[inline(always)]
    #[must_use]
    pub const fn new(v: u64) -> Self {
        Self(v)
    }
}

/// IEEE 11073-20601 Regulatory Certification Data List.
///
/// See Section 2.2.5 in [Personal Health Devices Transcoding][PHD] Bluetooth
/// White Paper for format information.
///
/// [PHD]: https://www.bluetooth.com/wp-content/uploads/2019/03/PHD_Transcoding_WP_v16.pdf
#[derive(Clone, Debug)]
pub struct RegulatoryData(Vec<u8>);

impl From<Vec<u8>> for RegulatoryData {
    #[inline(always)]
    fn from(v: Vec<u8>) -> Self {
        Self(v)
    }
}

/// Plug and Play device ID.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PnpId {
    pub vid: VendorId,
    pub pid: u16,
    pub ver: u16,
}

impl PnpId {
    /// Creates PNP ID from vendor ID, product ID, and version. Returns [`None`]
    /// if the version cannot be encoded (`minor` and `patch` must be `<= 15`).
    #[allow(clippy::similar_names)]
    #[must_use]
    pub const fn new(vid: VendorId, pid: u16, ver: (u8, u8, u8)) -> Option<Self> {
        // TODO: Use `then_some` when const stable
        if ver.1 > 0xf || ver.2 > 0xf {
            return None;
        }
        let ver = (ver.0 as u16) << 8 | (ver.1 as u16) << 4 | ver.2 as u16;
        Some(Self { vid, pid, ver })
    }

    /// Converts PNP ID to byte representation.
    #[must_use]
    pub fn to_bytes(self) -> [u8; 7] {
        let mut v = [0; 7];
        // [DIS] Section 3.9.1.1
        let (src, vid) = match self.vid {
            VendorId::Bluetooth(vid) => (0x01, vid),
            VendorId::USB(vid) => (0x02, vid),
        };
        v[0] = src;
        v[1..3].copy_from_slice(&vid.to_le_bytes());
        v[3..5].copy_from_slice(&self.pid.to_le_bytes());
        v[5..7].copy_from_slice(&self.ver.to_le_bytes());
        v
    }
}

/// Namespaced device vendor ID.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum VendorId {
    Bluetooth(u16),
    USB(u16),
}
