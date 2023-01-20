use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::{Deref, Index};

use super::*;

type Map = HashMap<Handle, Vec<u8>>;

/// GATT server database.
#[derive(Debug, Default)]
#[must_use]
pub struct Db {
    schema: Schema,
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

    /// Locks the database for exclusive access.
    #[inline(always)]
    pub fn lock(&self) -> DbGuard {
        DbGuard {
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

#[derive(Debug)]
pub struct DbGuard<'a> {
    schema: &'a Schema,
    vals: parking_lot::MutexGuard<'a, Map>,
}

impl DbGuard<'_> {
    #[inline]
    #[must_use]
    pub fn get(&self, hdl: Handle) -> Option<&Vec<u8>> {
        self.vals.get(&hdl)
    }

    #[inline]
    #[must_use]
    pub fn get_mut(&mut self, hdl: Handle) -> Option<&mut Vec<u8>> {
        self.vals.get_mut(&hdl)
    }

    #[inline]
    pub fn insert(&mut self, hdl: Handle, v: Vec<u8>) -> Option<Vec<u8>> {
        self.vals.insert(hdl, v)
    }

    #[inline]
    pub fn map<R>(&mut self, hdl: Handle, f: impl FnOnce(&mut Vec<u8>) -> R) -> R {
        f(self.vals.entry(hdl).or_default())
    }

    #[inline]
    fn try_map<R>(&mut self, hdl: Handle, f: impl FnOnce(&mut Vec<u8>) -> R) -> Option<R> {
        match self.vals.entry(hdl) {
            Entry::Occupied(e) => Some(f(e.into_mut())),
            Entry::Vacant(_) => None,
        }
    }
}

impl Index<Handle> for DbGuard<'_> {
    type Output = Vec<u8>;

    #[inline]
    fn index(&self, hdl: Handle) -> &Self::Output {
        self.vals.get(&hdl).expect("invalid handle")
    }
}
