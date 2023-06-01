//! Generic Access Service ([Vol 3] Part C, Section 12).

use burble_const::{Characteristic, Service};

use crate::att::{Access, Bearer, HandleRange, Result};
use crate::gap::Appearance;
use crate::gatt::{Builder, Db};

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

    /// Reads GAP service characteristics of the remote host.
    pub async fn read(br: &mut Bearer) -> Result<Self> {
        let req = br.read_by_type_req(HandleRange::ALL, Characteristic::DeviceName);
        let rsp = br.exec(req).await?;
        let device_name = if let Some((_, v)) = rsp.read_by_type_rsp()?.next() {
            String::from_utf8_lossy(v).to_string()
        } else {
            String::new()
        };
        Ok(Self {
            device_name,
            appearance: Appearance::GenericUnknown,
        })
    }

    /// Defines the service structure.
    pub fn define(&self, db: &mut Builder<Db>) {
        db.primary_service(Service::GenericAccess, [], |db| {
            use Characteristic::*;
            db.ro_characteristic(DeviceName, Access::READ, &self.device_name, |_| {});
            db.ro_characteristic(
                Appearance,
                Access::READ,
                (self.appearance as u16).to_le_bytes(),
                |_| {},
            );
        });
    }

    /// Returns the device name.
    #[inline(always)]
    #[must_use]
    pub fn device_name(&self) -> &str {
        self.device_name.as_str()
    }

    /// Returns the device appearance.
    #[inline(always)]
    #[must_use]
    pub const fn appearance(&self) -> Appearance {
        self.appearance
    }
}
