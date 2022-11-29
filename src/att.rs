//! Attribute Protocol ([Vol 3] Part F).

#![allow(dead_code)] // TODO: Remove

use std::collections::HashMap;
use std::fmt::Debug;

pub(crate) use access::*;
pub use {consts::*, handle::*};

use crate::gap::Uuid;
use crate::l2cap::BasicChan;
use crate::{host, l2cap};

mod access;
mod consts;
mod handle;

#[derive(Debug)]
pub struct Server<T: host::Transport> {
    att: HashMap<Handle, Attr>,
    bearer: HashMap<l2cap::LeCid, Bearer<T>>,
    next_handle: Handle,
}

#[derive(Debug)]
pub struct Attr {
    typ: Uuid,
    hdl: Handle,
    access: AccessRules,
}

/// ATT Bearer ([Vol 3] Part F, Section 3.2.11).
#[derive(Debug)]
pub struct Bearer<T: host::Transport> {
    ch: BasicChan<T>,
    //sm: Arc<SecurityManager>,
    //authz: bool,
}

impl<T: host::Transport> Bearer<T> {
    #[inline]
    #[must_use]
    pub(super) const fn new(ch: BasicChan<T>) -> Self {
        Self { ch }
    }
}
