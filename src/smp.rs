//! Security Manager Protocol ([Vol 3] Part H).

#![allow(dead_code)] // TODO: Remove

use std::fmt::Debug;

use futures_core::future::BoxFuture;

use burble_crypto::NumCompare;
pub(self) use cmd::*;
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
    #[error("local failure: {0}")]
    Local(Reason),
    #[error("remote failure: {0}")]
    Remote(Reason),
    #[error("pairing timeout")]
    Timeout,
}

/// Common SMP result type.
pub type Result<T> = std::result::Result<T, Error>;

/// SMP interface to the local device.
#[derive(Debug)]
pub struct Device {
    display: Option<Box<dyn Display>>,
    confirm: Option<Box<dyn Confirm>>,
}

impl Device {
    /// Creates a new device with no I/O capabilities.
    #[inline(always)]
    fn new() -> Self {
        Self {
            display: None,
            confirm: None,
        }
    }

    /// Provides a display device.
    #[inline(always)]
    fn with_display(mut self, d: Box<dyn Display>) -> Self {
        self.display = Some(d);
        self
    }

    /// Provides a yes/no input device.
    #[inline(always)]
    fn with_confirm(&mut self, c: Box<dyn Confirm>) -> &mut Self {
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
