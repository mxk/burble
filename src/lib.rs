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
#![allow(clippy::missing_panics_doc)] // TODO: Remove
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::wildcard_imports)]
// #![warn(clippy::restriction)]
#![warn(clippy::assertions_on_result_states)]
#![warn(clippy::clone_on_ref_ptr)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::decimal_literal_representation)]
#![warn(clippy::default_union_representation)]
#![warn(clippy::deref_by_slicing)]
#![warn(clippy::empty_drop)]
#![warn(clippy::empty_structs_with_brackets)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exit)]
#![warn(clippy::fn_to_numeric_cast_any)]
#![warn(clippy::format_push_string)]
#![warn(clippy::get_unwrap)]
#![warn(clippy::if_then_some_else_none)]
#![warn(clippy::lossy_float_literal)]
#![warn(clippy::missing_enforced_import_renames)]
#![warn(clippy::mixed_read_write_in_expression)]
#![warn(clippy::mod_module_files)]
#![warn(clippy::mutex_atomic)]
#![warn(clippy::pattern_type_mismatch)]
#![warn(clippy::print_stdout)]
#![warn(clippy::rc_buffer)]
#![warn(clippy::rc_mutex)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
//#![warn(clippy::semicolon_outside_block)]
#![warn(clippy::str_to_string)]
#![warn(clippy::string_add)]
#![warn(clippy::string_to_string)]
//#![warn(clippy::suspicious_xor_used_as_pow)]
#![warn(clippy::todo)]
#![warn(clippy::try_err)]
#![warn(clippy::undocumented_unsafe_blocks)]
//#![warn(clippy::unnecessary_safety_comment)]
//#![warn(clippy::unnecessary_safety_doc)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::unneeded_field_pattern)]
#![warn(clippy::unseparated_literal_suffix)]

pub mod att;
pub mod gap;
pub mod gatt;
pub mod hci;
pub mod host;
pub mod l2cap;
pub mod le;
pub mod sdp;
mod util;
