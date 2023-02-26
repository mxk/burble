use std::collections::BTreeMap;
use std::fmt::Debug;
use std::sync::Arc;

use tracing::{debug, error, warn};

use burble_crypto::LTK;

use crate::hci;

/// Security keys for a peer device.
#[derive(Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub struct Keys {
    pub(super) sec: hci::ConnSec,
    pub(super) ltk: LTK,
}

impl Keys {
    /// Returns unit test keys.
    #[doc(hidden)]
    #[must_use]
    pub const fn test() -> Self {
        Self {
            sec: hci::ConnSec::key_len(128).union(hci::ConnSec::AUTHN),
            ltk: LTK::new(u128::MAX),
        }
    }
}

/// Interface to persistent security database storage.
pub(crate) type KeyStore = dyn crate::PeerStore<Value = Keys>;

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
                    LeLongTermKeyRequest => break evt.get(),
                    EncryptionChangeV1 | EncryptionChangeV2 => {
                        self.handle_encryption_change(evt.get());
                    }
                    DisconnectionComplete => {
                        self.sec.remove(&evt.conn_handle().unwrap());
                    }
                    _ => {}
                }
            };
            self.handle_ltk_req(req).await?;
        }
    }

    /// Handles [`hci::LeLongTermKeyRequest`] event.
    async fn handle_ltk_req(&mut self, req: hci::LeLongTermKeyRequest) -> hci::Result<()> {
        let ltk = if req.rand == 0 && req.ediv == 0 {
            self.load_keys(req.handle).map(|k| {
                self.sec.insert(req.handle, k.sec);
                k.ltk
            })
        } else {
            // [Vol 3] Part H, Section 2.4.4
            error!(
                "Non-zero Rand or EDIV value in LTK request for {}",
                req.handle
            );
            None
        };
        (self.host)
            .le_long_term_key_request_reply(req.handle, ltk.as_ref())
            .await
    }

    /// Handles [`hci::EncryptionChange`] event.
    fn handle_encryption_change(&mut self, e: hci::EncryptionChange) {
        let Some(mut cn) = self.host.conn(e.handle) else { return };
        if !e.status.is_ok() {
            warn!(
                "Encryption change for {} failed: {}",
                cn.peer_addr, e.status
            );
            return;
        }
        cn.sec = if let (true, Some(mut sec)) = (e.enabled, self.sec.remove(&e.handle)) {
            // TODO: Should AUTHZ bit ever be kept?
            sec.remove(hci::ConnSec::AUTHZ);
            debug!("Encryption enabled for {}: {sec}", cn.peer_addr);
            sec
        } else {
            debug!("Security reset for {}", cn.peer_addr);
            hci::ConnSec::empty()
        };
    }

    /// Tries to load the keys for the specified connection handle.
    fn load_keys(&self, hdl: hci::ConnHandle) -> Option<Keys> {
        let Some(cn) = self.host.conn(hdl) else {
            error!("LTK request for an unknown {hdl}");
            return None;
        };
        self.store.load(cn.peer_addr)
    }
}
