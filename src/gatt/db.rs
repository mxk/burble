use std::collections::HashMap;

use structbuf::StructBuf;

use crate::att::*;
use crate::gap::{Uuid, Uuid16};

/// GATT server database.
#[derive(Clone, Debug, Default)]
#[must_use]
struct Db {
    attrs: Vec<Entry>,
    typs: HashMap<Handle, Uuid>,
}

/// A database entry. To optimize database size, all 128-bit UUIDs are stored in
/// a separate map.
#[derive(Clone, Debug)]
#[must_use]
struct Entry {
    hdl: Handle,
    typ: Option<Uuid16>,
    perms: Perms,
    val: StructBuf,
}

impl Db {
    /// Creates an empty database.
    #[inline]
    pub fn new() -> Self {
        Self {
            attrs: Vec::new(),
            typs: HashMap::new(),
        }
    }
}
