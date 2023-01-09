use std::collections::HashMap;
use std::ops::Deref;

use super::*;

/// GATT server database.
#[derive(Debug, Default)]
#[must_use]
pub struct Db {
    schema: Schema,
    vals: parking_lot::Mutex<HashMap<Handle, Vec<u8>>>,
}

impl Db {
    /// Creates an empty database.
    #[inline]
    pub fn new(schema: Schema) -> Self {
        Self {
            schema,
            vals: parking_lot::Mutex::new(HashMap::new()),
        }
    }
}

impl Deref for Db {
    type Target = Schema;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.schema
    }
}
