use std::collections::HashMap;
use std::ops::Deref;

use super::*;

type Map = HashMap<Handle, Vec<u8>>;

/// GATT server database.
#[derive(Debug, Default)]
#[must_use]
pub struct Db {
    schema: Schema,
    // An RwLock isn't needed since the common case is handling one request from
    // a single client, for which a regular Mutex is faster.
    vals: parking_lot::Mutex<Map>,
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

    /// Returns a database reader.
    #[inline]
    pub fn read(&self) -> DbReader {
        DbReader {
            schema: &self.schema,
            vals: self.vals.lock(),
        }
    }

    /// Returns a database writer.
    #[inline(always)]
    pub fn write(&self) -> DbWriter {
        DbWriter {
            schema: &self.schema,
            vals: self.vals.lock(),
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

/// Database reader guard.
#[derive(Debug)]
pub struct DbReader<'a> {
    schema: &'a Schema,
    // TODO: Defer locking?
    vals: parking_lot::MutexGuard<'a, Map>,
}

impl DbReader<'_> {
    /// Returns the handle value or [`None`] if the value is not in the
    /// database.
    #[inline]
    pub fn value(&self, hdl: Handle) -> Option<&[u8]> {
        let (_, v) = self.schema.get(hdl)?;
        if v.is_empty() {
            self.vals.get(&hdl).map(Vec::as_slice)
        } else {
            Some(v)
        }
    }
}

/// Database writer guard.
#[derive(Debug)]
pub struct DbWriter<'a> {
    schema: &'a Schema,
    vals: parking_lot::MutexGuard<'a, Map>,
}

impl DbWriter<'_> {
    /// Returns a [`Vec`] for the specified handle or [`None`] if the value is
    /// undefined or read-only.
    #[inline]
    #[must_use]
    pub fn value(&mut self, hdl: Handle) -> Option<&mut Vec<u8>> {
        self.vals.get_mut(&hdl)
    }

    /// Inserts a new value into the database.
    #[inline]
    pub fn insert(&mut self, hdl: Handle, v: Vec<u8>) -> Option<Vec<u8>> {
        let (_, val) = self.schema.get(hdl).expect("invalid handle");
        assert!(val.is_empty(), "read-only attribute {hdl}");
        self.vals.insert(hdl, v)
    }
}
