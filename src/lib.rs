//! Blackrock User-Mode Bluetooth LE library.
//!
//! A user-mode BLE stack implementation starting from the USB transport layer
//! via [libusb].
//!
//! [libusb]: https://github.com/libusb/libusb

#![cfg_attr(not(test), warn(unused_crate_dependencies))]

pub use profile::*;

#[path = "att/att.rs"]
pub mod att;
#[path = "gap/gap.rs"]
pub mod gap;
#[path = "gatt/gatt.rs"]
pub mod gatt;
#[path = "hci/hci.rs"]
pub mod hci;
#[path = "host/host.rs"]
pub mod host;
#[path = "l2cap/l2cap.rs"]
pub mod l2cap;
pub mod le;
#[path = "profile/profile.rs"]
mod profile;
#[path = "smp/smp.rs"]
pub mod smp;
#[path = "util/util.rs"]
mod util;

/// Service Discovery Protocol constants ([Vol 3] Part B).
pub mod sdp {
    pub use burble_const::ServiceClass;
}
