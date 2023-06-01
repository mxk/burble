//! Generic Attribute Profile ([Vol 3] Part G).

use std::collections::BTreeMap;
use std::fmt::Debug;
use std::time::Duration;

use tracing::{info, warn};

pub use {consts::*, db::*, io::*, server::*};

use crate::att::*;
use crate::le;
use crate::smp::BondId;

mod consts;
#[path = "db/db.rs"]
mod db;
mod io;
mod server;

/// Independent GATT service implementations.
pub mod service {
    pub mod bas;
    pub mod dis;
    pub mod gaps;
    #[cfg(feature = "hid")]
    pub mod hids;
}

/// Interface to persistent GATT cache storage.
pub type CacheStore = dyn crate::PeerStore<Value = Cache>;

/// Per-device cache ([Vol 3] Part G, Section 2.5.2). For bonded devices
/// (`bond_id` is [`Some`]), the cache persists across connections.
#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct Cache {
    bond_id: Option<BondId>,
    #[serde(with = "burble_crypto::u128ser")]
    db_hash: u128,
    service_changed: Option<ServiceChanged>,
    client_features: ClientFeature,
    cccd: BTreeMap<Handle, Cccd>,
}

impl Cache {
    /// Returns whether the client supports Robust Caching feature
    /// ([Vol 3] Part G, Section 2.5.2.1).
    #[inline(always)]
    #[must_use]
    const fn is_robust(&self) -> bool {
        self.client_features.contains(ClientFeature::ROBUST_CACHING)
    }

    /// Returns whether the client is change-aware or doesn't support Robust
    /// Caching.
    #[inline(always)]
    #[must_use]
    const fn is_change_aware(&self, db_hash: u128) -> bool {
        !self.is_robust() || self.db_hash == db_hash
    }
}

/// Service Changed characteristic state.
#[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
struct ServiceChanged {
    /// Hash that reflects the current GATT service structure. It is similar to
    /// db_hash, but includes all handles and a subset of GATT service values.
    #[serde(with = "burble_crypto::u128ser")]
    hash: u128,
    /// Characteristic value handle.
    handle: Handle,
    /// Client Characteristic Configuration descriptor value.
    cccd: Cccd,
}

impl ServiceChanged {
    /// Creates Service Changed parameters ([Vol 3] Part G, Section 2.5.2).
    /// Returns [`None`] if `db` does not contain Service Changed
    /// characteristic.
    #[must_use]
    fn new(db: &Db, features: ServerFeature) -> Option<Self> {
        let mut vhdl = None;
        let mut m = burble_crypto::AesCmac::db_hash();
        m.update([features.bits()]);
        for (hdl, uuid, _) in db.iter() {
            if uuid == Characteristic::ServiceChanged {
                assert_eq!(vhdl, None, "found multiple Service Changed characteristics");
                vhdl = Some(hdl);
            }
            m.update(u16::from(hdl).to_le_bytes());
            m.update(u128::from(uuid).to_le_bytes());
        }
        let vhdl = vhdl?;
        if u16::from(vhdl) != 3 {
            warn!("GATT service isn't first, which may result in compatibility problems");
        }
        Some(Self {
            hash: m.finalize(),
            handle: vhdl,
            cccd: Cccd::empty(),
        })
    }

    /// Returns whether the client enabled Service Changed indications
    /// ([Vol 3] Part G, Section 2.5.2).
    #[inline(always)]
    #[must_use]
    const fn is_enabled(&self) -> bool {
        self.cccd.contains(Cccd::INDICATE)
    }

    /// Sends a Service Changed indication to the client, if enabled,
    /// invalidating all handles. Returns whether the client confirmed the
    /// indication.
    async fn indicate_all(&self, br: &mut Bearer, peer: le::Addr) -> bool {
        const ALL_HANDLES: [u8; 4] = [0x01, 0x00, 0xFF, 0xFF];
        if !self.is_enabled() {
            info!("Service Changed indications not enabled for {peer}");
            return false;
        }
        info!("Sending Service Changed indication to {peer}");
        // This is a best-effort operation. We use a shorter timeout because the
        // indication may be sent when the cache is invalid, and it's not clear
        // whether the client is actually expecting it or will confirm it.
        // TODO: The server should ignore commands sent before the confirmation
        let indicate = tokio::time::timeout(
            Duration::from_secs(3),
            br.handle_value_ind(self.handle, ALL_HANDLES.as_ref()),
        );
        match indicate.await {
            Ok(Ok(_)) => {
                info!("Service Changed confirmed");
                return true;
            }
            Ok(Err(e)) => warn!("Service Changed indication error: {e}"),
            Err(_) => warn!("Service Changed indication timeout"),
        }
        false
    }
}

impl Default for ServiceChanged {
    #[inline(always)]
    fn default() -> Self {
        Self {
            hash: 0,
            handle: Handle::MAX,
            cccd: Cccd::empty(),
        }
    }
}
