//! Blackrock User-Mode Bluetooth LE library.
//!
//! A user-mode BLE stack implementation starting from the USB transport layer
//! via [libusb].
//!
//! [libusb]: https://github.com/libusb/libusb

#![warn(clippy::cargo)]
#![warn(clippy::nursery)]
#![allow(clippy::redundant_pub_crate)]
#![warn(clippy::pedantic)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::wildcard_imports)]
// #![warn(clippy::restriction)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::deref_by_slicing)]
#![warn(clippy::print_stdout)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unseparated_literal_suffix)]

pub mod hci;
pub mod host;
