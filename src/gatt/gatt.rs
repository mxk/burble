//! Generic Attribute Profile ([Vol 3] Part G).

#![allow(dead_code)] // TODO: Remove

use std::collections::BTreeMap;
use std::fmt::Debug;

pub use {consts::*, db::*, schema::*, server::*};

use crate::att::*;

mod consts;
mod db;
#[path = "schema/schema.rs"]
mod schema;
mod server;

/// Interface to persistent GATT server storage.
type ServerStore = dyn crate::PeerStore<Value = BondedClient>;

/// Bonded client state that persists across connections.
#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct BondedClient {
    db_hash: u128,
    values: BTreeMap<Handle, Vec<u8>>,
}
