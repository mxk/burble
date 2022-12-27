//! Generic Attribute Profile ([Vol 3] Part G).

#![allow(dead_code)] // TODO: Remove

pub use {consts::*, server::*};

use crate::att::*;
use crate::gap::{Uuid, Uuid16};

mod char;
mod consts;
mod db;
mod server;

/// Service declaration ([Vol 3] Part G, Section 3.1).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ServiceDecl {
    primary: bool,
    uuid: Uuid,
}

/// Include declaration ([Vol 3] Part G, Section 3.2).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct IncludeDecl {
    service_hdl: Handle,
    end_group_hdl: Handle,
    service_uuid: Option<Uuid16>,
}

/// Characteristic declaration ([Vol 3] Part G, Section 3.3.1).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CharDecl {
    props: CharProps,
    value_hdl: Handle,
    uuid: Uuid,
}

/// Characteristic Presentation Format descriptor value
/// ([Vol 3] Part G, Section 3.3.3.5).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CharFmt {
    fmt: FmtType,
    exp: i8,
    unit: Unit,
    desc_ns: u8,
    desc: u16,
}
