//! Blackrock User-Mode Bluetooth LE library.
//!
//! A user-mode BLE stack implementation starting from the USB transport layer
//! via [libusb].
//!
//! [libusb]: https://github.com/libusb/libusb

#![warn(missing_debug_implementations)]
#![warn(non_ascii_idents)]
#![warn(single_use_lifetimes)]
#![cfg_attr(not(test), warn(unused_crate_dependencies))]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(clippy::cargo)]
#![warn(clippy::nursery)]
#![allow(clippy::redundant_pub_crate)]
#![warn(clippy::pedantic)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::inline_always)]
#![allow(clippy::match_wild_err_arm)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::wildcard_imports)]
// #![warn(clippy::restriction)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::deref_by_slicing)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::fn_to_numeric_cast_any)]
#![warn(clippy::if_then_some_else_none)]
#![warn(clippy::mod_module_files)]
#![warn(clippy::pattern_type_mismatch)]
#![warn(clippy::print_stdout)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
#![warn(clippy::str_to_string)]
#![warn(clippy::string_to_string)]
#![warn(clippy::todo)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unseparated_literal_suffix)]

pub mod att;
pub mod gap;
pub mod gatt;
pub mod hci;
pub mod host;
pub mod l2cap;
pub mod le;
mod util;
