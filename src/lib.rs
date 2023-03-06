//! Blackrock User-Mode Bluetooth LE library.
//!
//! A user-mode BLE stack implementation starting from the USB transport layer
//! via [libusb].
//!
//! [libusb]: https://github.com/libusb/libusb

#![cfg_attr(test, allow(unused_crate_dependencies))]

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

/// Service Discovery Protocol constants ([Vol 3] Part B).
pub mod sdp {
    pub use burble_const::ServiceClass;
}

type SyncMutex<T> = parking_lot::Mutex<T>;
type SyncMutexGuard<'a, T> = parking_lot::MutexGuard<'a, T>;
type AsyncMutex<T> = tokio::sync::Mutex<T>;
type AsyncRwLock<T> = tokio::sync::RwLock<T>;

/// Interface to persistent peer data storage.
pub trait PeerStore: std::fmt::Debug + Send + Sync {
    /// Type of stored data.
    type Value;

    /// Saves peer data and returns `true` if the operation was successful.
    fn save(&self, peer: le::Addr, v: &Self::Value) -> bool;

    /// Loads peer data.
    #[must_use]
    fn load(&self, peer: le::Addr) -> Option<Self::Value>;

    /// Removes peer data.
    fn remove(&self, peer: le::Addr);
}

/// Forwards [`core::fmt::Display`] implementation to [`core::fmt::Debug`].
macro_rules! impl_display_via_debug {
    ($($t:ty),*$(,)?) => {$(
        impl ::core::fmt::Display for $t {
            #[inline(always)]
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::fmt::Debug::fmt(self, f)
            }
        }
    )*};
}
pub(crate) use impl_display_via_debug;

/// Returns a string representation of the specified type.
macro_rules! name_of {
    ($t:ty) => {{
        // TODO: Switch to `core::any::type_name` when stabilized
        type _T = $t; // Allows $t to be recognized as a type for refactoring
        stringify!($t)
    }};
}
pub(crate) use name_of;
