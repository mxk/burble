//! Security Manager Protocol ([Vol 3] Part H).

use std::fmt::Debug;
use std::io;

use futures_core::future::BoxFuture;

pub use burble_crypto::NumCompare;
pub(self) use cmd::*;
pub use {consts::*, peripheral::*, secdb::*};

use crate::l2cap;

mod cmd;
mod consts;
mod peripheral;
mod secdb;

/// Error type returned by the SMP layer.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    L2cap(#[from] l2cap::Error),
    #[error("local failure: {0}")]
    Local(Reason),
    #[error("remote failure: {0}")]
    Remote(Reason),
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("pairing timeout")]
    Timeout,
}

/// Common SMP result type.
pub type Result<T> = std::result::Result<T, Error>;

/// SMP interface to the local device.
#[derive(Debug)]
#[must_use]
pub struct Device {
    display: Option<Box<dyn Display>>,
    confirm: Option<Box<dyn Confirm>>,
}

impl Device {
    /// Creates a new device with no I/O capabilities.
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            display: None,
            confirm: None,
        }
    }

    /// Provides a display device.
    #[inline(always)]
    pub fn with_display(mut self, d: Box<dyn Display>) -> Self {
        self.display = Some(d);
        self
    }

    /// Provides a yes/no input device.
    #[inline(always)]
    pub fn with_confirm(mut self, c: Box<dyn Confirm>) -> Self {
        self.confirm = Some(c);
        self
    }

    /// Returns IO capabilities based on the device configuration.
    const fn io_cap(&self) -> IoCap {
        let inp = match self.confirm {
            Some(_) => InputCap::YesNo,
            _ => InputCap::None,
        };
        let out = match self.display {
            Some(_) => OutputCap::Numeric,
            None => OutputCap::None,
        };
        IoCap::new(inp, out)
    }
}

impl Default for Device {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

/// Device display capable of showing a 6-digit number to the user.
pub trait Display: Debug + Send + Sync {
    /// Show a 6-digit number to the user, returning `true` when the number is
    /// visible or `false` on error.
    fn show(&mut self, n: NumCompare) -> BoxFuture<bool>;
}

/// Input mechanism for the user to indicate either 'yes' or 'no'.
pub trait Confirm: Debug + Send + Sync {
    /// Get yes/no (`true`/`false`) confirmation from the user.
    fn confirm(&mut self) -> BoxFuture<bool>;
}

// TODO: Keyboard
