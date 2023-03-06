//! Generic Access Profile ([Vol 3] Part C).

use burble_const::{Characteristic, Service};
pub use burble_const::{Uuid, Uuid16, UuidType, UuidVec};
pub use {consts::*, response_data::*};

use crate::att::Perms;
use crate::gatt::{Builder, Db};

mod consts;
mod response_data;

/// Generic Access Profile service.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct GapService {
    device_name: String,
    appearance: Appearance,
}

impl GapService {
    /// Creates a new GAP service.
    ///
    /// # Panics
    ///
    /// Panics if `device_name` is longer than 248 bytes.
    #[inline]
    pub fn new(device_name: impl Into<String>, appearance: Appearance) -> Self {
        let device_name = device_name.into();
        assert!(device_name.len() <= 248);
        Self {
            device_name,
            appearance,
        }
    }

    /// Defines the service structure.
    pub fn define(&self, db: &mut Builder<Db>, perms: impl Into<Perms>) {
        let p = perms.into();
        db.primary_service(Service::GenericAccess, [], |db| {
            use Characteristic::*;
            db.ro_characteristic(DeviceName, p, &self.device_name, |_| {});
            db.ro_characteristic(
                Appearance,
                p,
                (self.appearance as u16).to_le_bytes(),
                |_| {},
            );
        });
    }
}
