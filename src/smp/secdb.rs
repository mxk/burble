use std::collections::BTreeMap;
use std::fmt::Debug;
use std::mem;
use std::num::NonZeroU128;
use std::sync::Arc;

use tracing::{debug, error, warn};

use burble_crypto::LTK;

use crate::hci;

/// Interface to persistent security database storage.
pub(crate) type KeyStore = dyn crate::PeerStore<Value = Keys>;

/// Security keys for a peer device.
#[derive(Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[must_use]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub struct Keys {
    pub(super) id: Option<BondId>,
    pub(super) sec: hci::ConnSec,
    pub(super) ltk: LTK,
}

impl Keys {
    /// Creates a new key set.
    #[doc(hidden)]
    #[inline(always)]
    pub(super) fn new(sec: hci::ConnSec, ltk: LTK) -> Self {
        let id = (sec.contains(hci::ConnSec::BOND)).then(|| BondId::new(sec, &ltk));
        Self { id, sec, ltk }
    }

    /// Returns unit test keys.
    #[doc(hidden)]
    pub fn test() -> Self {
        Self::new(
            hci::ConnSec::key_len(128).union(hci::ConnSec::AUTHN),
            LTK::new(u128::MAX),
        )
    }

    /// Returns whether the keys are valid by comparing bond ID.
    #[inline(always)]
    #[must_use]
    fn is_valid(&self) -> bool {
        (self.id).map_or(true, |id| id == BondId::new(self.sec, &self.ltk))
    }
}

/// Bond ID derived from the connection security properties and the LTK.
#[allow(clippy::unsafe_derive_deserialize)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[must_use]
#[repr(transparent)]
#[serde(transparent)]
pub(crate) struct BondId(#[serde(with = "burble_crypto::nz_u128_hex")] NonZeroU128);

impl BondId {
    /// Generates the bond ID for the specified keys.
    pub fn new(sec: hci::ConnSec, ltk: &LTK) -> Self {
        // TODO: Add a store-specific input to easily invalidate all keys
        let mut h = blake3::Hasher::new_derive_key("secdb v1");
        h.update(&[sec.bits()]);
        h.update(u128::from(ltk).to_ne_bytes().as_ref());
        for b in h.finalize().as_bytes().chunks_exact(mem::size_of::<u128>()) {
            // SAFETY: b is &[u8; 16]
            let id = u128::from_ne_bytes(unsafe { *b.as_ptr().cast() });
            if let Some(nz) = NonZeroU128::new(id) {
                return Self(nz);
            }
        }
        unreachable!();
    }
}

/// Security database that stores encryption (LTK), identity (IRK), and signing
/// (CSRK) keys.
#[derive(Debug)]
pub struct SecDb {
    ctl: hci::EventStream,
    host: hci::Host,
    store: Arc<KeyStore>,
    sec: BTreeMap<hci::ConnHandle, hci::ConnSec>,
}

impl SecDb {
    /// Creates a security database that will handle HCI host key requests.
    #[inline(always)]
    pub fn new(host: hci::Host, store: Arc<KeyStore>) -> Self {
        Self {
            ctl: host.events(),
            host,
            store,
            sec: BTreeMap::new(),
        }
    }

    /// Handles security database events until an error is encountered. This
    /// method is not cancel safe.
    pub async fn event_loop(&mut self) -> hci::Result<()> {
        use hci::EventCode::*;
        loop {
            let req = loop {
                let evt = self.ctl.next().await?;
                match evt.code() {
                    LeConnectionComplete | LeEnhancedConnectionComplete => {
                        if evt.status().is_ok() {
                            self.load_keys(evt.conn_handle().unwrap());
                        }
                    }
                    DisconnectionComplete => {
                        // TODO: Remove temporary keys from store
                        if evt.status().is_ok() {
                            self.sec.remove(&evt.conn_handle().unwrap());
                        }
                    }
                    LeLongTermKeyRequest => break evt.get(),
                    EncryptionChangeV1 | EncryptionChangeV2 => {
                        self.handle_encryption_change(evt.get());
                    }
                    _ => {}
                }
            };
            self.handle_ltk_request(req).await?;
        }
    }

    /// Loads the keys for the specified connection handle and updates the
    /// connection bond ID.
    fn load_keys(&mut self, hdl: hci::ConnHandle) -> Option<Keys> {
        let Some(peer) = self.host.conn(hdl).map(|cn| cn.borrow().peer_addr) else {
            error!("Unknown {hdl}");
            return None;
        };
        match self.store.load(peer) {
            Some(k) if k.is_valid() => {
                debug!("Found keys for {peer} {hdl}");
                if k.id.is_some() {
                    self.host.update_conn(hdl, |cn| cn.bond_id = k.id);
                } else {
                    debug!("Removing temporary keys for {peer} {hdl}");
                    self.store.remove(peer);
                }
                return Some(k);
            }
            Some(_) => {
                warn!("Invalidating keys for {peer} {hdl}");
                self.store.remove(peer);
                self.host.update_conn(hdl, |cn| cn.bond_id = None);
            }
            None => debug!("No saved keys for {peer} {hdl}"),
        }
        None
    }

    /// Handles [`hci::LeLongTermKeyRequest`] event.
    async fn handle_ltk_request(&mut self, req: hci::LeLongTermKeyRequest) -> hci::Result<()> {
        // We want to refresh the connection bond ID no matter what
        let keys = self.load_keys(req.handle);
        let ltk = if req.rand == 0 && req.ediv == 0 {
            keys.as_ref().map(|k| {
                self.sec.insert(req.handle, k.sec);
                &k.ltk
            })
        } else {
            // [Vol 3] Part H, Section 2.4.4
            error!("Non-zero Rand or EDIV in LTK request for {}", req.handle);
            None
        };
        (self.host.le_long_term_key_request_reply(req.handle, ltk)).await
    }

    /// Handles [`hci::EncryptionChange`] event.
    fn handle_encryption_change(&mut self, e: hci::EncryptionChange) {
        let Some(peer) = self.host.conn(e.handle).map(|cn| cn.borrow().peer_addr) else { return };
        if !e.status.is_ok() {
            warn!("Encryption change for {peer} failed: {}", e.status);
            return;
        }
        self.host.update_conn(e.handle, |cn| {
            cn.sec = if let (true, Some(sec)) = (e.enabled, self.sec.remove(&e.handle)) {
                debug!("Encryption enabled for {peer}: {sec}");
                // TODO: Should AUTHZ bit ever be kept?
                sec.difference(hci::ConnSec::AUTHZ)
            } else {
                debug!("Security reset for {peer}");
                hci::ConnSec::empty()
            };
        });
    }
}
