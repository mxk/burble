//! Battery Service ([BAS]).
//!
//! The Battery Service exposes the battery level and other information for a
//! battery within a device.
//!
//! [BAS]: https://www.bluetooth.com/specifications/specs/battery-service/

use crate::att::{Handle, Perms};
use crate::gatt::{Builder, Characteristic, Schema, Service};

/// Battery service state.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct BatteryService;

impl BatteryService {
    /// Creates a battery service.
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Defines the service schema.
    pub fn define(&mut self, b: &mut Builder<Schema>, perms: impl Into<Perms>) -> Handle {
        let p = perms.into();
        let (hdl, _) = b.primary_service(Service::Battery, [], |b| {
            use Characteristic::*;
            b.ro_characteristic(BatteryLevel, &[100], p, |_| {});
        });
        hdl
    }
}
