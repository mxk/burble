use std::collections::HashMap;

use structbuf::StructBuf;

use super::*;

/// GATT server database.
#[derive(Debug, Default)]
#[must_use]
pub struct Db {
    schema: Schema,
    vals: parking_lot::Mutex<HashMap<Handle, StructBuf>>,
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
