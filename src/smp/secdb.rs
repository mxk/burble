use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

use tracing::{error, warn};

use burble_crypto::LTK;

use crate::hci::EventCode;
use crate::{hci, le};

/// Security keys for a peer device.
#[derive(Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub struct Keys {
    pub ltk: LTK,
}

/// Interface to persistent security database storage.
pub(crate) type KeyStore = dyn crate::PeerStore<Value = Keys>;

/// Security database that stores encryption (LTK), identity (IRK), and CSRKs.
#[derive(Debug)]
pub struct SecDb {
    ctl: hci::EventReceiver,
    host: hci::Host,
    store: Arc<KeyStore>,
    peer: HashMap<hci::ConnHandle, le::Addr>,
}

impl SecDb {
    /// Creates a security database that will handle HCI host key requests.
    #[inline]
    pub fn new(host: hci::Host, store: Arc<KeyStore>) -> hci::Result<Self> {
        Ok(Self {
            ctl: host.recv()?,
            host,
            store,
            peer: HashMap::new(),
        })
    }

    /// Handles security database events until an error is encountered.
    pub async fn event_loop(&mut self) -> hci::Result<()> {
        loop {
            let req = {
                let evt = self.ctl.next().await?;
                match self.handle_event(&mut evt.get()) {
                    Some(req) => req,
                    None => continue,
                }
            };
            let ltk = if req.rand == 0 && req.ediv == 0 {
                self.load_ltk(req.conn_handle)
            } else {
                // [Vol 3] Part H, Section 2.4.4
                error!(
                    "Non-zero Rand or EDIV value in LTK request for {}",
                    req.conn_handle
                );
                None
            };
            self.host
                .le_long_term_key_request_reply(req.conn_handle, ltk.as_ref())
                .await?;
        }
    }

    /// Handles HCI control events.
    fn handle_event(&mut self, e: &mut hci::Event<'_>) -> Option<hci::LeLongTermKeyRequest> {
        use hci::{EventType::*, SubeventCode::*};
        match e.typ() {
            Hci(EventCode::DisconnectionComplete) => {
                self.peer.remove(&e.conn_handle().unwrap());
            }
            Le(ConnectionComplete | EnhancedConnectionComplete) => {
                let e = hci::LeConnectionComplete::from(e);
                self.peer.insert(e.handle, e.peer_addr);
            }
            Le(LongTermKeyRequest) => {
                return Some(hci::LeLongTermKeyRequest::from(e));
            }
            _ => warn!("Unexpected {}", e.typ()),
        }
        None
    }

    /// Tries to load the LTK for the specified connection handle.
    fn load_ltk(&self, hdl: hci::ConnHandle) -> Option<LTK> {
        let Some(&peer) = self.peer.get(&hdl) else {
            error!("LTK request for an unknown {hdl}");
            return None;
        };
        self.store.load(peer).map(|k| k.ltk)
    }
}
