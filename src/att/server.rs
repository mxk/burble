use std::collections::btree_map::Entry;
use std::collections::BTreeMap;

use super::*;

type Result<T> = std::result::Result<T, ErrorCode>;

// TODO: Should Server understand groups? If so, can make some Pdu and Bearer
// methods private.

/// ATT server responsible for assigning attribute handles and storing
/// associated attribute types and permissions.
#[derive(Debug)]
pub(crate) struct Server {
    attrs: BTreeMap<Handle, (Uuid, Perms)>,
}

impl Server {
    /// Creates an empty ATT server.
    #[inline]
    pub const fn new() -> Self {
        Self {
            attrs: BTreeMap::new(),
        }
    }

    /// Allocates a handle for a new attribute.
    pub fn append(&mut self, typ: Uuid, perms: Perms) -> Option<Handle> {
        let h = match self.attrs.last_key_value() {
            None => Handle::MIN,
            Some((&h, _)) => h.next()?,
        };
        self.attrs.insert(h, (typ, perms));
        Some(h)
    }

    /// Inserts a new attribute with a pre-defined handle. Returns `false` if
    /// the handle is already taken.
    pub fn insert(&mut self, h: Handle, typ: Uuid, p: Perms) -> bool {
        let Entry::Vacant(e) = self.attrs.entry(h) else { return false };
        e.insert((typ, p));
        true
    }
}
