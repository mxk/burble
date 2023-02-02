//! Blackrock User-Mode Bluetooth LE library.
//!
//! A user-mode BLE stack implementation starting from the USB transport layer
//! via [libusb].
//!
//! [libusb]: https://github.com/libusb/libusb

#![cfg_attr(not(test), warn(unused_crate_dependencies))]

pub mod att;
pub mod gap;
pub mod gatt;
pub mod hci;
pub mod host;
pub mod l2cap;
pub mod le;
pub mod smp;
mod util;

/// Service Discovery Protocol constants ([Vol 3] Part B).
pub mod sdp {
    pub use burble_const::ServiceClass;
}
