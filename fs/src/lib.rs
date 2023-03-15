//! Bluetooth LE file system storage backend.

use std::io::{Cursor, Write};
use std::path::{Path, PathBuf};
use std::{fs, io};

use tracing::{debug, error, warn};

use burble::le::Addr;
use burble::{gatt, smp};

/// Security database stored in a file system directory.
#[derive(Clone, Debug)]
pub struct KeyStore(Dir);

impl KeyStore {
    const NAME: &'static str = "keys";

    /// Creates or opens a security database store in the specified root
    /// directory.
    #[inline(always)]
    #[must_use]
    pub fn open(root: impl AsRef<Path>) -> Self {
        Self(Dir::open(root, Self::NAME))
    }

    /// Creates or opens a security database store in the current user's local
    /// data directory.
    ///
    /// # Panics
    ///
    /// Panics if it cannot determine the user directory.
    #[inline(always)]
    #[must_use]
    pub fn per_user(app: impl AsRef<Path>) -> Self {
        Self(Dir::per_user(app, Self::NAME))
    }
}

impl burble::PeerStore for KeyStore {
    type Value = smp::Keys;

    #[inline(always)]
    fn save(&self, peer: Addr, v: &Self::Value) -> bool {
        self.0.save(peer, v)
    }

    #[inline(always)]
    fn load(&self, peer: Addr) -> Option<Self::Value> {
        self.0.load(peer)
    }

    #[inline(always)]
    fn remove(&self, peer: Addr) {
        self.0.remove(peer);
    }

    #[inline(always)]
    fn clear(&self) {
        self.0.clear();
    }
}

/// GATT server database stored in a file system directory.
#[derive(Clone, Debug)]
pub struct GattServerStore(Dir);

impl GattServerStore {
    const NAME: &'static str = "gatts";

    /// Creates or opens a GATT server database store in the specified root
    /// directory.
    #[inline(always)]
    #[must_use]
    pub fn open(root: impl AsRef<Path>) -> Self {
        Self(Dir::open(root, Self::NAME))
    }

    /// Creates or opens a GATT server database store in the current user's
    /// local data directory.
    ///
    /// # Panics
    ///
    /// Panics if it cannot determine the user directory.
    #[inline(always)]
    #[must_use]
    pub fn per_user(app: impl AsRef<Path>) -> Self {
        Self(Dir::per_user(app, Self::NAME))
    }
}

impl burble::PeerStore for GattServerStore {
    type Value = gatt::Cache;

    #[inline(always)]
    fn save(&self, peer: Addr, v: &Self::Value) -> bool {
        self.0.save(peer, v)
    }

    #[inline(always)]
    fn load(&self, peer: Addr) -> Option<Self::Value> {
        self.0.load(peer)
    }

    #[inline(always)]
    fn remove(&self, peer: Addr) {
        self.0.remove(peer);
    }

    #[inline(always)]
    fn clear(&self) {
        self.0.clear();
    }
}

/// Database in a file system directory.
#[derive(Clone, Debug)]
#[repr(transparent)]
struct Dir(PathBuf);

impl Dir {
    const FILE_NAME_FMT: &'static str = "P-001122334455";

    /// Creates or opens a database store in the specified root directory.
    #[inline(always)]
    #[must_use]
    fn open(root: impl AsRef<Path>, name: impl AsRef<Path>) -> Self {
        Self(root.as_ref().join(name))
    }

    /// Creates or opens a database store in the current user's local data
    /// directory.
    ///
    /// # Panics
    ///
    /// Panics if it cannot determine the user directory.
    #[must_use]
    fn per_user(app: impl AsRef<Path>, name: impl AsRef<Path>) -> Self {
        let dir = dirs::data_local_dir()
            .expect("user directory not available")
            .join(app.as_ref())
            .join(name);
        Self(dir)
    }

    /// Saves peer data to the file system.
    fn save(&self, peer: Addr, v: &impl serde::ser::Serialize) -> bool {
        let s = serde_json::to_string_pretty(v).expect("failed to serialize peer data");
        if let Err(e) = fs::create_dir_all(&self.0) {
            warn!(
                "Failed to create database directory: {} ({e})",
                self.0.display()
            );
        }
        let path = self.path(peer);
        // TODO: Make atomic?
        match fs::File::create(&path)
            .and_then(|mut f| f.write_all(s.as_bytes()).and_then(|_| f.sync_data()))
        {
            Ok(_) => {
                debug!("Wrote: {}", path.display());
                true
            }
            Err(e) => {
                error!("Failed to write: {} ({e})", path.display());
                false
            }
        }
    }

    /// Loads peer data from the file system.
    fn load<T: serde::de::DeserializeOwned>(&self, peer: Addr) -> Option<T> {
        let path = self.path(peer);
        let s = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => return None,
            Err(e) => {
                error!("Failed to read: {} ({e})", path.display());
                return None;
            }
        };
        serde_json::from_str(&s)
            .map_err(|e| {
                error!("Invalid file contents: {} ({e})", path.display());
                Err::<T, ()>(())
            })
            .ok()
    }

    /// Removes peer data from the file system.
    fn remove(&self, peer: Addr) {
        let path = self.path(peer);
        match fs::remove_file(&path) {
            Ok(_) => {}
            Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => {}
            Err(e) => error!("Failed to remove: {} ({e})", path.display()),
        }
    }

    /// Removes all peer data from the file system.
    fn clear(&self) {
        match fs::remove_dir_all(&self.0) {
            Ok(_) => {}
            Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => {}
            Err(e) => error!("Failed to remove: {} ({e})", self.0.display()),
        }
    }

    /// Returns the key file path for the specified peer address.
    fn path(&self, peer: Addr) -> PathBuf {
        let (raw, typ) = match peer {
            Addr::Public(ref raw) => (raw.as_le_bytes(), 'P'),
            Addr::Random(ref raw) => (raw.as_le_bytes(), 'R'),
        };
        let mut buf = Cursor::new([0_u8; Self::FILE_NAME_FMT.len()]);
        write!(
            buf,
            "{typ}-{:02X}{:02X}{:02X}{:02X}{:02X}{:02X}",
            raw[5], raw[4], raw[3], raw[2], raw[1], raw[0]
        )
        .expect("key file name overflow");
        // SAFETY: `buf` contains a valid UTF-8 string
        (self.0).join(unsafe { std::str::from_utf8_unchecked(buf.get_ref()) })
    }
}

#[cfg(test)]
mod tests {
    use tempfile::Builder;

    use burble::le::RawAddr;
    use burble::PeerStore;

    use super::*;

    #[test]
    fn save_load() {
        const PEER: Addr =
            Addr::Public(RawAddr::from_le_bytes([0x55, 0x44, 0x33, 0x22, 0x11, 0x00]));
        let tmp = (Builder::new().prefix(concat!("burble-test-")).tempdir()).unwrap();
        let db = KeyStore(Dir(tmp.path().to_path_buf()));
        let keys = smp::Keys::test();
        assert!(db.save(PEER, &keys));
        assert!(tmp.path().join(Dir::FILE_NAME_FMT).exists());
        assert_eq!(db.load(PEER).unwrap(), keys);
    }
}
