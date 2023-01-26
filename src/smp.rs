//! Security Manager Protocol ([Vol 3] Part H).

#![allow(dead_code)] // TODO: Remove

pub(crate) use cmd::*;
pub use {consts::*, peripheral::*};

use crate::l2cap;

mod cmd;
mod consts;
mod peripheral;

/// Error type returned by the SMP layer.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    L2cap(#[from] l2cap::Error),
    #[error(transparent)]
    Smp(#[from] Reason),
    #[error("timeout while waiting for {0}")]
    Timeout(Code),
}

/// Common SMP result type.
pub type Result<T> = std::result::Result<T, Error>;
