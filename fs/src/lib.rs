//! Bluetooth LE file system storage backend.

#![warn(missing_debug_implementations)]
#![warn(non_ascii_idents)]
#![warn(single_use_lifetimes)]
#![warn(unused_crate_dependencies)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(clippy::cargo)]
#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::inline_always)]
#![allow(clippy::module_name_repetitions)]
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
#![warn(clippy::str_to_string)]
#![warn(clippy::string_add)]
#![warn(clippy::string_to_string)]
#![warn(clippy::suspicious_xor_used_as_pow)]
#![warn(clippy::todo)]
#![warn(clippy::try_err)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unnecessary_safety_comment)]
#![warn(clippy::unnecessary_safety_doc)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::unneeded_field_pattern)]
#![warn(clippy::unseparated_literal_suffix)]

use std::fs::File;
use std::io;
use std::io::{BufRead, Cursor, Read, Write};
use std::path::{Path, PathBuf};

use tracing::warn;

use burble::le::Addr;
use burble::smp;
use burble::smp::Keys;
use burble_crypto::LTK;

/// Security database stored in a file system directory.
#[derive(Clone, Debug)]
pub struct SecDb(PathBuf);

impl SecDb {
    /// Creates a database store in the current user's local data directory.
    ///
    /// # Panics
    ///
    /// Panics if it cannot determine the user directory.
    #[must_use]
    pub fn new() -> Self {
        let dir = dirs::data_local_dir()
            .expect("user directory not available")
            .join("burble/secdb");
        Self(dir)
    }

    /// Returns the key file path for the specified peer address.
    fn path(&self, peer: Addr) -> PathBuf {
        let (raw, typ) = match peer {
            Addr::Public(ref raw) => (raw.as_le_bytes(), 'P'),
            Addr::Random(ref raw) => (raw.as_le_bytes(), 'R'),
        };
        let mut buf = Cursor::new([0_u8; "P001122334455".len()]);
        write!(
            buf,
            "{typ}{:02X}{:02X}{:02X}{:02X}{:02X}{:02X}",
            raw[5], raw[4], raw[3], raw[2], raw[1], raw[0]
        )
        .expect("key file name overflow");
        // SAFETY: `buf` contains a valid UTF-8 string
        (self.0).join(unsafe { std::str::from_utf8_unchecked(buf.get_ref()) })
    }
}

impl Default for SecDb {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl smp::Store for SecDb {
    fn save(&self, peer: Addr, keys: &Keys) -> io::Result<()> {
        let mut buf = [0_u8; 256];
        let mut cur = Cursor::new(buf.as_mut());
        writeln!(cur, "LTK {:032x}", u128::from(&keys.ltk)).expect("key file buffer overflow");
        #[allow(clippy::cast_possible_truncation)]
        let n = cur.position() as usize;
        if let Err(e) = std::fs::create_dir_all(&self.0) {
            warn!(
                "Failed to create security database directory: {} ({e})",
                self.0.display()
            );
        }
        let mut f = File::create(self.path(peer))?;
        f.write_all(&buf[..n])?;
        f.sync_data()
    }

    fn load(&self, peer: Addr) -> io::Result<Keys> {
        let mut buf = [0_u8; 256];
        let path = self.path(peer);
        let mut f = File::open(&path)?;
        let n = usize::try_from(f.metadata()?.len()).unwrap_or(usize::MAX);
        if buf.len() < n {
            return Err(corrupt(&path, "file too large"));
        }
        f.read_exact(&mut buf[..n])?;
        drop(f);
        let mut ltk = None;
        for ln in buf[..n].lines() {
            let ln = ln.map_err(|e| corrupt(&path, e))?;
            let mut tok = ln.split_ascii_whitespace().fuse();
            match (tok.next(), tok.next()) {
                (Some("LTK"), Some(v)) => ltk = u128::from_str_radix(v, 16).ok().map(LTK::new),
                _ => return Err(corrupt(&path, "invalid format")),
            }
        }
        ltk.map_or_else(
            || Err(corrupt(&path, "missing LTK")),
            |ltk| Ok(Keys { ltk }),
        )
    }
}

/// Returns an `InvalidData` error due to a corrupt key file.
#[inline]
fn corrupt(p: impl AsRef<Path>, e: impl Into<Box<dyn std::error::Error>>) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("corrupt key file: {} ({})", p.as_ref().display(), e.into()),
    )
}

#[cfg(test)]
mod tests {
    use tempfile::Builder;

    use burble::le::RawAddr;
    use burble::smp::Store;

    use super::*;

    #[test]
    fn save_load() {
        const PEER: Addr =
            Addr::Public(RawAddr::from_le_bytes([0x55, 0x44, 0x33, 0x22, 0x11, 0x00]));
        const KEYS: Keys = Keys {
            ltk: LTK::new(u128::MAX),
        };
        let tmp = (Builder::new().prefix(concat!("burble-test-")).tempdir()).unwrap();
        let db = SecDb(tmp.path().to_path_buf());
        db.save(PEER, &KEYS).unwrap();
        assert!(tmp.path().join("P001122334455").exists());
        assert_eq!(db.load(PEER).unwrap(), KEYS);
    }
}
