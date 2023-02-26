//! Battery Service ([BAS]).
//!
//! The Battery Service exposes the battery level and other information for a
//! battery within a device.
//!
//! [BAS]: https://www.bluetooth.com/specifications/specs/battery-service/

use crate::att::{Handle, Perms};
use crate::gatt::{Builder, Characteristic, Db, Service};

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

    /// Defines the service db.
    pub fn define(&mut self, db: &mut Builder<Db>, perms: impl Into<Perms>) -> Handle {
        let p = perms.into();
        let (hdl, _) = db.primary_service(Service::Battery, [], |db| {
            use Characteristic::*;
            db.ro_characteristic(BatteryLevel, p, [100], |_| {});
        });
        hdl
    }
}
