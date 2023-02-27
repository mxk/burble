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

/// Per-device cache ([Vol 3] Part G, Section 2.5.2). For bonded devices, the
/// cache persists across connections.
#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct Cache {
    db_hash: u128,
    vals: BTreeMap<Handle, Vec<u8>>,
}
