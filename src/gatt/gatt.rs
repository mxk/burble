//! Generic Attribute Profile ([Vol 3] Part G).

pub use {consts::*, db::*, schema::*, server::*};

use crate::att::*;

mod consts;
mod db;
#[path = "schema/schema.rs"]
mod schema;
mod server;
