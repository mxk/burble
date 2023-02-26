//! Generic Attribute Profile ([Vol 3] Part G).

#![allow(dead_code)] // TODO: Remove

use std::collections::BTreeMap;
use std::fmt::Debug;

pub use {consts::*, db::*, io::*, server::*};

use crate::att::*;

mod consts;
#[path = "db/db.rs"]
mod db;
mod io;
mod server;

/// Interface to persistent GATT cache storage.
type CacheStore = dyn crate::PeerStore<Value = Cache>;

// TODO: Remove blake3

/// Per-device cache ([Vol 3] Part G, Section 2.5.2). For bonded devices, the
/// cache persists across connections.
#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct Cache {
    db_hash: u128,
    vals: BTreeMap<Handle, Vec<u8>>,
}

impl Cache {
    /// Calculates a cryptographic hash of cache contents that can be used to
    /// detect changes.
    fn hash(&self) -> blake3::Hash {
        let mut h = blake3::Hasher::new();
        h.update(&self.db_hash.to_le_bytes());
        for (&hdl, v) in &self.vals {
            h.update(&u16::from(hdl).to_le_bytes());
            h.update(&v.len().to_le_bytes());
            h.update(v);
        }
        h.finalize()
    }
}
