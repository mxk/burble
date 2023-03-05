use std::collections::btree_map::Entry;
use std::sync::{Arc, Weak};
use std::vec;

use structbuf::Unpack;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info, warn};

use ErrorCode::*;

use crate::gap::{Uuid, UuidType};
use crate::{hci, le, SyncMutex, SyncMutexGuard};

use super::*;

/// GATT server. The server connects services with individual ATT bearers and
/// maintains shared client state.
#[derive(Debug)]
pub struct Server {
    db: Db,
    io: IoMap,
    sc: Option<ServiceChanged>,
    features: ServerFeature,
    store: Arc<CacheStore>,
    clients: SyncMutex<BTreeMap<le::Addr, Weak<SyncMutex<ClientCtx>>>>,
}

impl Server {
    /// Defines the Generic Attribute service ([Vol 3] Part G, Section 7). This
    /// service should be first to maintain consistent handles for control-point
    /// characteristics. In particular, the Service Changed characteristic
    /// handle must not change while the server has a trusted relationship
    /// (bond) with any client.
    pub fn define_service(db: &mut Builder<Db>) {
        db.primary_service(Service::GenericAttribute, [], |db| {
            db.characteristic(
                Characteristic::ServiceChanged,
                Prop::INDICATE,
                Access::NONE,
                Io::NONE, // I/O is handled directly
                |db| db.cccd(Access::READ_WRITE),
            );
            db.characteristic(
                Characteristic::ClientSupportedFeatures,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                Io::NONE,
                |_| {},
            );
            db.characteristic(
                Characteristic::DatabaseHash,
                Prop::READ,
                Access::READ,
                Io::NONE,
                |_| {},
            );
            db.ro_characteristic(
                Characteristic::ServerSupportedFeatures,
                Access::READ,
                [],
                |_| {},
            );
        });
    }

    /// Creates a new GATT server. The first service in the database should be
    /// Generic Attribute, which can be defined with [`Self::define_service`].
    #[inline]
    #[must_use]
    pub fn new(db: Builder<Db>, store: Arc<CacheStore>) -> Arc<Self> {
        let (db, io) = db.freeze();
        let features = ServerFeature::empty();
        let sc = ServiceChanged::new(&db, features);
        // TODO: Move this logic to Builder
        debug_assert!(
            db.iter()
                .filter(|&(_, uuid, _)| uuid == Characteristic::DatabaseHash)
                .count()
                <= 1
        );
        Arc::new(Self {
            db,
            io,
            sc,
            features,
            store,
            clients: SyncMutex::new(BTreeMap::new()),
        })
    }

    /// Returns the server database.
    #[inline(always)]
    #[must_use]
    pub const fn db(&self) -> &Db {
        &self.db
    }

    /// Creates a [`ServerCtx`] for the specified ATT bearer. The first bearer
    /// for a new connection, which should be the fixed ATT channel, becomes the
    /// sender of all notifications and indications.
    #[inline]
    pub fn attach(self: &Arc<Self>, br: &Bearer) -> ServerCtx {
        let peer = br.conn().borrow().peer_addr;
        let cc = self.get_client_ctx(peer);
        let notify = cc.lock().notify_rx();
        ServerCtx {
            peer,
            srv: Arc::clone(self),
            cc,
            notify,
            db_oos_sent: false,
        }
    }

    /// Returns the shared [`ClientCtx`] for the specified peer address.
    fn get_client_ctx(&self, peer: le::Addr) -> ArcClientCtx {
        let mut clients = self.clients.lock();
        clients.retain(|_, cc| cc.strong_count() != 0);
        match clients.entry(peer) {
            Entry::Vacant(e) => {
                let cc = ClientCtx::new();
                e.insert(Arc::downgrade(&cc));
                cc
            }
            Entry::Occupied(mut e) => e.get().upgrade().unwrap_or_else(|| {
                // `upgrade()` shouldn't fail because we just removed all Arcs
                // without strong references, but there is a race with the last
                // strong reference being dropped in a multithreaded runtime.
                let cc = ClientCtx::new();
                e.insert(Arc::downgrade(&cc));
                cc
            }),
        }
    }
}

/// Server context used by an ATT bearer to handle client requests and service
/// notifications/indications.
#[derive(Debug)]
#[must_use]
pub struct ServerCtx {
    peer: le::Addr,
    srv: Arc<Server>,
    cc: ArcClientCtx,
    notify: Option<tokio::sync::mpsc::Receiver<NotifyVal>>,
    db_oos_sent: bool,
}

impl ServerCtx {
    // TODO: The same ATT bearer can act both as a server and a client. We
    // provide a simple way of running a server-only event loop via serve(), but
    // we may want to expose some of the helper methods for mixed uses.
    // Alternatively, EATT support would allow using a dedicated channel for
    // client functionality.

    /// Runs a server event loop for the specified bearer.
    pub async fn serve(mut self, mut br: Bearer) -> Result<()> {
        br.exchange_mtu().await?;
        let Some(mut notify) = self.notify.take() else {
            // Additional bearer for an existing client connection that will not
            // handle notifications, indications, or connection events.
            // TODO: This should block until the cache is initialized
            loop {
                let pdu = br.recv().await?;
                self.handle(&mut br, &pdu).await?;
            }
        };
        self.cc.lock().notify_mtu = br.mtu();
        if let Some(sc) = self.restore_bond(&mut br) {
            self.indicate_service_changed(&mut br, sc).await;
        }
        let mut conn = br.conn().clone();
        let sec = conn.borrow_and_update().sec;
        self.configure_notify(sec);
        loop {
            tokio::select! {
                pdu = br.recv() => self.handle(&mut br, &pdu?).await?,
                // TODO: Allow processing requests while waiting for indication
                // confirmation?
                ntf = notify.recv() => ntf.unwrap().exec(&mut br).await,
                _ = conn.changed(), if conn.has_changed().is_ok() => {
                    let sec = conn.borrow().sec;
                    self.configure_notify(sec);
                }
            }
        }
    }

    /// Restores client cache if it's still valid and returns [`Some`] if a
    /// Service Changed indication should be sent
    /// ([Vol 3] Part G, Section 2.5.2 and 7.1).
    fn restore_bond(&mut self, br: &mut Bearer) -> Option<ServiceChanged> {
        self.notify.as_ref()?;
        // TODO: This assumes that SecDb has set bond_id while we were calling
        // exchange_mtu(). It would be better to have a guarantee.
        let (peer, bond_id) = (self.peer, br.conn().borrow().bond_id);
        let mut cc = self.cc.lock();
        match self.srv.store.load(peer) {
            Some(mut cache) => {
                cache.cccd.retain(|_, cccd| !cccd.is_empty());
                cc.cache = cache;
            }
            None => cc.cache.bond_id = None,
        }

        // Handle the absence or loss of a trusted relationship
        if bond_id.is_none() || cc.cache.bond_id != bond_id {
            match (bond_id.is_some(), cc.cache.bond_id.is_some()) {
                (false, false) => info!("No bond or GATT cache for {peer}"),
                (false, true) => warn!("Invalidating GATT cache for {peer}"),
                (true, false) => warn!("Missing GATT cache for {peer}"),
                (true, true) => warn!("Unexpected LTK and GATT cache mismatch for {peer}"),
            }
            // Try to send a Service Changed indication if the client had it
            // enabled previously.
            let sc = cc.cache.service_changed;
            // "The initial state of a client without a trusted relationship is
            // change-aware" ([Vol 3] Part G, Section 2.5.2.1).
            cc.cache = Cache {
                bond_id,
                db_hash: self.srv.db.hash(),
                service_changed: self.srv.sc,
                ..Cache::default()
            };
            if bond_id.is_none() {
                self.srv.store.remove(peer);
            } else {
                self.persist(&cc.cache);
            }
            return sc;
        }

        // Check for any changes that may invalidate the cache
        let db_match = cc.cache.db_hash == self.srv.db.hash();
        let sc_match = match (cc.cache.service_changed, self.srv.sc) {
            (Some(client), Some(server)) => client.hash == server.hash,
            (None, None) => true,
            _ => false,
        };
        match (db_match, sc_match) {
            (false, false) => warn!("Database and Service Changed hash mismatch for {peer}"),
            (false, true) => warn!("Database hash mismatch for {peer}"),
            (true, false) => warn!("Service Changed hash mismatch for {peer}"),
            (true, true) => {
                info!("Restored bond with {peer}");
                return None;
            }
        }

        // Let `indicate_service_changed()` finish clean-up, even if the actual
        // Service Changed indication is disabled.
        if cc.cache.service_changed.is_none() {
            cc.cache.service_changed = self.srv.sc;
        }
        let sc = cc.cache.service_changed.unwrap_or_default();

        // Only a database hash mismatch invalidates the cache since it captures
        // the core service schema. A change in just the Service Changed hash
        // represents backward-compatible modifications.
        if !db_match {
            // "The initial state of a client with a trusted relationship is
            // unchanged from the previous connection unless the database has
            // been updated since the last connection, in which case the initial
            // state is change-unaware" ([Vol 3] Part G, Section 2.5.2.1).
            cc.cache = Cache {
                bond_id,
                db_hash: cc.cache.db_hash,
                service_changed: self.srv.sc,
                ..Cache::default()
            };
            self.persist(&cc.cache);
        }
        Some(sc)
    }

    /// Sends a Service Changed indication to the client, if enabled, and
    /// updates change-awareness state if the client supports Robust Caching
    /// ([Vol 3] Part G, Section 2.5.2 and 7.1).
    async fn indicate_service_changed(&self, br: &mut Bearer, sc: ServiceChanged) {
        // The spec says that "The Service Changed characteristic Attribute
        // Handle on the server shall not change if the server has a trusted
        // relationship with any client" ([Vol 3] Part G, Section 7.1), but we
        // can't enforce that, so the confirmation is considered valid only if
        // the characteristic is still the same. Otherwise, we're just hoping
        // that the client will interpret it correctly and invalidate its cache,
        // but we can't be sure.
        let peer = self.peer;
        let confirmed = sc.indicate_all(br, peer).await
            && self.srv.sc.map_or(false, |srv| srv.handle == sc.handle);
        let db_hash = self.srv.db.hash();
        let mut cc = self.cc.lock();
        if cc.cache.db_hash == db_hash {
            info!("{} is change-aware", self.peer);
            return;
        }
        // "A change-unaware connected client using exactly one ATT bearer
        // becomes change-aware when... [it] confirms a Handle Value Indication
        // for the Service Changed characteristic"
        // ([Vol 3] Part G, Section 2.5.2.1).
        // TODO: Verify exactly one bearer
        if !confirmed {
            if cc.cache.is_robust() {
                info!("{} is change-unaware", self.peer);
                return;
            }
            // This is bad because the client has no way of knowing that its
            // cache is invalid.
            // TODO: Wipe LTK and disconnect the client?
            warn!("No Service Changed confirmation or Robust Caching for {peer}");
        }
        info!("{} is change-aware", self.peer);
        cc.cache.db_hash = db_hash;
        self.persist(&cc.cache);
    }

    /// Synchronizes service notification/indication state with the current
    /// connection parameters. The spec says "When a client reconnects to a
    /// server and expects to receive indications or notifications for which
    /// security is required, the client shall enable encryption with the
    /// server" ([Vol 3] Part C, Section 10.3.1.1). This resumes all
    /// notifications/indications that are enabled by the client and meet the
    /// connection security parameters, and disables all others.
    fn configure_notify(&mut self, sec: hci::ConnSec) {
        if self.notify.is_none() {
            return;
        }
        let req = Opcode::WriteCmd.request(sec);
        let mut cc = self.cc.lock();
        // "Except for a Handle Value Indication for the Service Changed
        // characteristic, the server shall not send notifications and
        // indications to such a client until it becomes change-aware"
        // ([Vol 3] Part G, Section 2.5.2.1).
        if !cc.cache.is_change_aware(self.srv.db.hash()) {
            self.disable_notify(&mut cc);
            return;
        }
        cc.notify_cancel.retain(|&hdl, ct| {
            let keep = !ct.is_cancelled() && self.srv.db.try_access(req, hdl).is_ok();
            if !keep {
                ct.cancel();
            }
            keep
        });
        let enable: Vec<(Handle, Cccd)> = (cc.cache.cccd.iter())
            .filter_map(|(&hdl, &cccd)| {
                (!cc.notify_cancel.contains_key(&hdl) && self.srv.db.try_access(req, hdl).is_ok())
                    .then_some((hdl, cccd))
            })
            .collect();
        if enable.is_empty() {
            return;
        }
        for (hdl, cccd) in enable {
            let _ = self.cccd_apply(&mut cc, hdl, cccd, true);
        }
        self.persist(&cc.cache);
    }

    /// Cancels all notification/indication requests.
    #[inline]
    fn disable_notify(&self, cc: &mut ClientCtx) {
        debug!("Disabling all notifications for {}", self.peer);
        for ct in cc.notify_cancel.values() {
            ct.cancel();
        }
        cc.notify_cancel.clear();
    }

    // Handles Robust Caching logic when a new request is received
    // ([Vol 3] Part G, Section 2.5.2.1).
    fn handle_robust_caching(&mut self, br: &Bearer, op: Opcode) -> Option<Result<()>> {
        let mut cc = self.cc.lock();
        if cc.cache.is_change_aware(self.srv.db.hash()) {
            return None;
        }
        match op.typ() {
            // "If a change-unaware client sends an ATT command, the server
            // shall ignore it."
            PduType::Cmd => {
                debug!("Ignoring {op} for change-unaware client {}", self.peer);
                Some(Ok(()))
            }
            // "A change-unaware connected client becomes change-aware when it
            // reads the Database Hash characteristic and then the server
            // receives another ATT request from the client... A change-unaware
            // connected client using exactly one ATT bearer becomes
            // change-aware when... The server sends the client a response with
            // the Error Code parameter set to Database Out Of Sync (0x12) and
            // then the server receives another ATT request from the client."
            PduType::Req if cc.db_hash_read || self.db_oos_sent => {
                // TODO: Only if one bearer for db_oos_sent, and it is only
                // reset when "the client disconnects or the database changes
                // again before the client becomes change-aware."
                info!("{} is change-aware", self.peer);
                cc.cache.db_hash = self.srv.db.hash();
                self.persist(&cc.cache);
                drop(cc);
                let sec = br.conn().borrow().sec;
                self.configure_notify(sec);
                None
            }
            _ => None,
        }
    }

    /// Returns a [`DatabaseOutOfSync`] error if a Robust Caching client is
    /// change-unaware ([Vol 3] Part G, Section 2.5.2.1).
    fn require_db_sync(&mut self, op: Opcode) -> RspResult<()> {
        let cc = self.cc.lock();
        // TODO: Use atomics for fast path?
        if cc.cache.is_change_aware(self.srv.db.hash()) {
            return Ok(());
        }
        if self.db_oos_sent {
            Ok(())
        } else {
            self.db_oos_sent = true;
            op.err(DatabaseOutOfSync)
        }
    }

    /// Executes a read operation.
    fn do_read<'a>(&self, r: &'a mut ReadReq) -> RspResult<&'a [u8]> {
        use {Characteristic::*, Descriptor::*};
        #[inline(always)]
        fn trim(v: &[u8]) -> &[u8] {
            v.iter().rposition(|&b| b != 0).map_or(&[], |i| &v[..=i])
        }
        // TODO: Cache awareness check
        match r.uuid().typ() {
            UuidType::Characteristic(ClientSupportedFeatures) => {
                let v = self.cc.lock().cache.client_features;
                r.complete(trim(v.bits().to_le_bytes().as_ref()))
            }
            UuidType::Characteristic(DatabaseHash) => {
                // [Vol 3] Part G, Section 7.3
                if matches!(r.op, Opcode::ReadByTypeReq) {
                    self.cc.lock().db_hash_read = true;
                    r.complete(self.srv.db.hash().to_le_bytes())
                } else {
                    error!("Client attempted to read database hash using {}", r.op);
                    Err(RequestNotSupported)
                }
            }
            UuidType::Characteristic(ServerSupportedFeatures) => {
                r.complete(trim(self.srv.features.bits().to_le_bytes().as_ref()))
            }
            UuidType::Descriptor(ClientCharacteristicConfiguration) => {
                let cccd = (self.cc.lock().cache.cccd.get(&r.hdl)).map_or(Cccd::empty(), |&v| v);
                r.complete(cccd.bits().to_le_bytes())
            }
            _ => match self.srv.db.get(r.hdl) {
                Some((_, val)) if !val.is_empty() => r.complete(val),
                _ => self.srv.io.read(r),
            },
        }
        .map_or_else(|e| r.op.hdl_err(e, r.hdl), |_| Ok(r.buf.as_ref()))
    }

    /// Executes a write operation.
    fn do_write(&self, w: &WriteReq) -> RspResult<()> {
        use {Characteristic::*, Descriptor::*};
        // TODO: Cache awareness check
        match w.uuid.typ() {
            UuidType::Characteristic(ClientSupportedFeatures) => self.csf_write(w),
            UuidType::Descriptor(ClientCharacteristicConfiguration) => self.cccd_write(w),
            _ => self.srv.io.write(w),
        }
        .map_or_else(|e| w.op.hdl_err(e, w.hdl), Ok)
    }

    /// Executes a Client Supported Features write
    /// ([Vol 3] Part G, Section 7.2).
    #[inline]
    fn csf_write(&self, w: &WriteReq) -> IoResult {
        let mut v = ClientFeature::empty().bits().to_le_bytes();
        w.update(&mut v)?;
        // SAFETY: We allow the client to set unknown feature bits
        let new = unsafe { ClientFeature::from_bits_unchecked(v[0]) };
        let mut cc = self.cc.lock();
        if !new.contains(cc.cache.client_features) {
            // The client is not allowed to clear any bits
            return Err(ValueNotAllowed);
        }
        info!("Client Supported Features for {}: {new:?}", self.peer);
        cc.cache.client_features = new;
        Ok(())
    }

    /// Executes a Client Characteristic Configuration descriptor write
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    fn cccd_write(&self, w: &WriteReq) -> IoResult {
        let mut v = Cccd::empty().bits().to_le_bytes();
        w.update(&mut v)?;
        let Some(new) = Cccd::from_bits(u16::from_le_bytes(v)) else {
            return Err(ValueNotAllowed);
        };
        let mut cc = self.cc.lock();
        // TODO: Should the write be allowed for a change-unaware client?
        let is_change_aware = cc.cache.is_change_aware(self.srv.db.hash());
        let r = self.cccd_apply(&mut cc, w.hdl, new, is_change_aware);
        self.persist(&cc.cache);
        r
    }

    /// Applies CCCD configuration.
    fn cccd_apply(
        &self,
        cc: &mut SyncMutexGuard<ClientCtx>,
        hdl: Handle,
        new: Cccd,
        is_change_aware: bool,
    ) -> IoResult {
        let ch = (self.srv.db.get_characteristic(hdl)).expect("invalid CCCD handle");
        let (vhdl, vtyp) = (ch.vhdl, ch.uuid.typ());
        if !ch.props.cccd_mask().contains(new) {
            warn!("Invalid CCCD value for {vtyp} {vhdl}: {new:?}");
            return Err(CccdImproperlyConfigured);
        }
        if self.srv.sc.map_or(false, |srv| srv.handle == vhdl) {
            debug!("Service Changed CCCD: {new:?}");
            cc.cache.service_changed.as_mut().unwrap().cccd = new;
            return Ok(());
        }
        // A CCCD write always resets any existing notification session
        if let Some(ct) = cc.notify_cancel.remove(&hdl) {
            debug!("Disabling notifications for {vtyp} {vhdl}");
            ct.cancel();
        }
        if new.is_empty() {
            cc.cache.cccd.remove(&hdl);
            return Ok(());
        }
        if !is_change_aware {
            // [Vol 3] Part G, Section 2.5.2.1
            cc.cache.cccd.insert(hdl, new);
            return Ok(());
        }
        let ct = CancellationToken::new();
        // TODO: Drop cc during this call?
        debug_assert_ne!(cc.notify_mtu, 0);
        debug!("Enabling notifications for {vtyp} {vhdl}");
        let r = self.srv.io.notify(NotifyReq {
            hdl: vhdl,
            uuid: ch.uuid,
            mtu: cc.notify_mtu,
            ind: new.contains(Cccd::INDICATE),
            tx: cc.tx.clone(),
            ct: ct.clone(),
        });
        if let Err(e) = r {
            ct.cancel();
            warn!("Failed to enable notifications for {vtyp} {vhdl}: {e}");
        } else {
            cc.cache.cccd.insert(hdl, new);
            cc.notify_cancel.insert(hdl, ct);
        }
        r
    }

    /// Saves client cache to persistent storage.
    #[inline(always)]
    fn persist(&self, c: &Cache) {
        if c.bond_id.is_some() {
            self.srv.store.save(self.peer, c);
        }
    }

    /// Returns the UUID of the specified handle.
    #[inline]
    fn uuid(&self, hdl: Handle) -> Uuid {
        self.srv.db.get(hdl).expect("invalid handle").0
    }
}

/// GATT server procedures ([Vol 3] Part G, Section 4).
impl ServerCtx {
    /// Handles received client request.
    async fn handle(&mut self, br: &mut Bearer, pdu: &Pdu) -> Result<()> {
        use Opcode::*;
        let op = pdu.opcode();
        if let Some(r) = self.handle_robust_caching(br, op) {
            return r;
        }
        #[allow(clippy::match_same_arms)]
        let r = match op {
            ExchangeMtuReq => return br.handle_exchange_mtu_req(pdu).await,
            FindInformationReq => self.discover_characteristic_descriptors(br, pdu),
            FindByTypeValueReq => self.discover_primary_service_by_uuid(br, pdu),
            ReadByTypeReq => self.handle_read_by_type_req(br, pdu),
            ReadReq => self.read(br, pdu),
            ReadBlobReq => self.read_blob(br, pdu),
            ReadMultipleReq => self.read_multiple(br, pdu),
            ReadByGroupTypeReq => self.discover_primary_services(br, pdu),
            WriteReq | WriteCmd => match self.write(br, pdu) {
                Ok(Some(rsp)) => Ok(rsp),
                Ok(None) => return Ok(()),
                Err(e) => Err(e),
            },
            PrepareWriteReq => self.prepare_write(br, pdu),
            ExecuteWriteReq => self.execute_write(br, pdu),
            ReadMultipleVariableReq => self.read_multiple_variable(br, pdu),
            //SignedWriteCmd => unimplemented!(),
            _ => {
                if !matches!(pdu.opcode().typ(), PduType::Req) {
                    warn!("Ignoring unexpected {op}");
                    return Ok(());
                }
                pdu.err(RequestNotSupported)
            }
        };
        br.send_rsp(r).await
    }

    /// Handles `ATT_READ_BY_TYPE_REQ` PDUs.
    fn handle_read_by_type_req(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let (hdls, typ) = pdu.read_by_type_req()?;
        match typ.typ() {
            UuidType::Declaration(Declaration::Include) => self.find_included_services(br, hdls),
            UuidType::Declaration(Declaration::Characteristic) => {
                self.discover_service_characteristics(br, hdls)
            }
            UuidType::Characteristic(_) | UuidType::NonSig => {
                // DatabaseOutOfSync error requires "Attribute Type other than
                // Include or Characteristic and an Attribute Handle range other
                // than 0x0001 to 0xFFFF" ([Vol 3] Part G, Section 2.5.2.1).
                // This implies that a database hash read will always succeed,
                // which is shown in Figure 2.7.
                if hdls != HandleRange::ALL {
                    self.require_db_sync(pdu.opcode())?;
                }
                self.read_by_type(
                    br,
                    hdls.start(),
                    (self.srv.db).try_range_access(br.access_req(pdu), hdls, typ)?,
                )
            }
            _ => pdu.err(RequestNotSupported),
        }
    }

    /// Handles "Discover All Primary Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.1).
    fn discover_primary_services(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let (hdls, typ) = pdu.read_by_group_type_req()?;
        if typ != Declaration::PrimaryService {
            return pdu.err(UnsupportedGroupType);
        }
        if hdls.end() != Handle::MAX {
            return pdu.hdl_err(AttributeNotFound, hdls.start());
        }
        let mut it = (self.srv.db)
            .primary_services(hdls.start(), None)
            .map(|v| (v.handle_range(), v.value()));
        br.read_by_group_type_rsp(hdls.start(), ValueFn::new(|| it.next()))
    }

    /// Handles "Discover Primary Service by Service UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.2).
    fn discover_primary_service_by_uuid(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let (hdls, typ, uuid) = pdu.find_by_type_value_req()?;
        if typ != Declaration::PrimaryService {
            return pdu.err(UnsupportedGroupType);
        }
        let (Ok(uuid), true) = (Uuid::try_from(uuid), hdls.end() == Handle::MAX) else {
            return pdu.hdl_err(AttributeNotFound, hdls.start());
        };
        let it = (self.srv.db.primary_services(hdls.start(), Some(uuid)))
            .map(|v| (v.handle(), Some(v.handle_range().end())));
        br.find_by_type_value_rsp(it)
    }

    /// Handles "Find Included Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.5.1).
    fn find_included_services(&self, br: &mut Bearer, hdls: HandleRange) -> RspResult<Rsp> {
        let mut it = self.srv.db.includes(hdls).map(|v| (v.handle(), v.value()));
        br.read_by_type_rsp(hdls.start(), ValueFn::new(|| it.next()))
    }

    /// Handles "Discover All Characteristics of a Service" and "Discover
    /// Characteristics by UUID" sub-procedures
    /// ([Vol 3] Part G, Section 4.6.1 and 4.6.2).
    fn discover_service_characteristics(
        &self,
        br: &mut Bearer,
        hdls: HandleRange,
    ) -> RspResult<Rsp> {
        let mut it = (self.srv.db.characteristics(hdls)).map(|v| (v.handle(), v.value()));
        br.read_by_type_rsp(hdls.start(), ValueFn::new(|| it.next()))
    }

    /// Handles "Discover All Characteristic Descriptors" sub-procedure
    /// ([Vol 3] Part G, Section 4.7.1).
    fn discover_characteristic_descriptors(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let hdls = pdu.find_information_req()?;
        let it = (self.srv.db.descriptors(hdls)).map(|at| (at.handle(), at.uuid()));
        br.find_information_rsp(hdls.start(), it)
    }

    /// Handles "Read Characteristic Value" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.1).
    fn read(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        self.require_db_sync(pdu.opcode())?;
        let hdl = (self.srv.db).try_access(br.access_req(pdu), pdu.read_req()?)?;
        let mut r = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_rsp(self.do_read(r.with(hdl, self.uuid(hdl), 0))?)
    }

    /// Handles "Read Using Characteristic UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.2).
    fn read_by_type(&self, br: &mut Bearer, start: Handle, hdls: Vec<Handle>) -> RspResult<Rsp> {
        let req = ReadReq::new(Opcode::ReadByTypeReq, br.mtu());
        br.read_by_type_rsp(start, Reader::new(self, req, hdls))
    }

    /// Handles "Read Long Characteristic Values" and "Read Long Characteristic
    /// Descriptors" sub-procedures ([Vol 3] Part G, Section 4.8.3 and 4.12.2).
    fn read_blob(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        self.require_db_sync(pdu.opcode())?;
        let (hdl, off) = pdu.read_blob_req()?;
        let hdl = self.srv.db.try_access(br.access_req(pdu), hdl)?;
        let mut r = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_blob_rsp(self.do_read(r.with(hdl, self.uuid(hdl), off))?)
    }

    /// Handles "Read Multiple Characteristic Values" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.4).
    fn read_multiple(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        self.require_db_sync(pdu.opcode())?;
        let hdls = (self.srv.db).try_multi_access(br.access_req(pdu), pdu.read_multiple_req()?)?;
        let req = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_multiple_rsp(Reader::new(self, req, hdls))
    }

    /// Handles "Read Multiple Variable Length Characteristic Values"
    /// sub-procedure ([Vol 3] Part G, Section 4.8.5).
    fn read_multiple_variable(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        self.require_db_sync(pdu.opcode())?;
        let hdls = (self.srv.db)
            .try_multi_access(br.access_req(pdu), pdu.read_multiple_variable_req()?)?;
        let req = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_multiple_variable_rsp(Reader::new(self, req, hdls))
    }

    /// Handles "Write Without Response" and "Write Characteristic Value"
    /// sub-procedures ([Vol 3] Part G, Section 4.9.1 and 4.9.3).
    fn write(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Option<Rsp>> {
        self.require_db_sync(pdu.opcode())?;
        let (hdl, val) = pdu.write_req()?;
        let hdl = self.srv.db.try_access(br.access_req(pdu), hdl)?;
        if val.len() > MAX_VAL_LEN {
            return pdu.hdl_err(InvalidAttributeValueLength, hdl);
        }
        let w = WriteReq {
            op: pdu.opcode(),
            hdl,
            uuid: self.uuid(hdl),
            off: 0,
            mtu: br.mtu(),
            val,
        };
        self.do_write(&w)?;
        Ok(if Opcode::is_cmd(w.op as _) {
            None
        } else {
            Some(br.write_rsp()?)
        })
    }

    /// Handles the first phase of "Write Long Characteristic Values" and
    /// "Reliable Writes" sub-procedures
    /// ([Vol 3] Part G, Section 4.9.4 and 4.9.5).
    fn prepare_write(&mut self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        self.require_db_sync(pdu.opcode())?;
        let (hdl, off, v) = pdu.prepare_write_req()?;
        let hdl = self.srv.db.try_access(br.access_req(pdu), hdl)?;
        if off as usize + v.len() > MAX_VAL_LEN {
            return pdu.hdl_err(InvalidAttributeValueLength, hdl);
        }
        if !self.cc.lock().write_queue.add(hdl, off, v) {
            return pdu.hdl_err(PrepareQueueFull, hdl);
        }
        br.prepare_write_rsp(hdl, off, v)
    }

    /// Handles the second phase of "Write Long Characteristic Values" and
    /// "Reliable Writes" sub-procedures
    /// ([Vol 3] Part G, Section 4.9.4 and 4.9.5).
    fn execute_write(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let commit = pdu.execute_write_req()?;
        let mut cc = self.cc.lock();
        if !commit {
            cc.write_queue.clear();
            return br.execute_write_rsp();
        }
        let mut w = WriteReq {
            op: pdu.opcode(),
            hdl: Handle::MAX,
            uuid: Uuid::MAX,
            off: 0,
            mtu: br.mtu(),
            val: &[],
        };
        for (hdl, off, val) in cc.write_queue.iter() {
            (w.hdl, w.uuid, w.off, w.val) = (hdl, self.uuid(hdl), off, val);
            self.do_write(&w)?;
        }
        br.execute_write_rsp()
    }
}

impl Drop for ServerCtx {
    fn drop(&mut self) {
        if self.notify.is_some() {
            self.disable_notify(&mut self.cc.lock());
        }
    }
}

struct Reader<'a> {
    srv: &'a ServerCtx,
    req: ReadReq,
    hdls: vec::IntoIter<Handle>,
}

impl<'a> Reader<'a> {
    /// Creates a new read iterator.
    #[inline(always)]
    fn new(srv: &'a ServerCtx, req: ReadReq, hdls: Vec<Handle>) -> Self {
        Self {
            srv,
            req,
            hdls: hdls.into_iter(),
        }
    }
}

impl ValueIter<Handle> for Reader<'_> {
    fn more(&mut self) -> RspResult<bool> {
        let Some(hdl) = self.hdls.next() else { return Ok(false); };
        if let Err(e) = self.srv.do_read(self.req.with(hdl, self.srv.uuid(hdl), 0)) {
            self.hdls.nth(self.hdls.len());
            Err(e)
        } else {
            Ok(true)
        }
    }

    #[inline(always)]
    fn handle(&self) -> Handle {
        self.req.hdl
    }

    #[inline(always)]
    fn value(&self) -> &[u8] {
        &self.req.buf
    }
}

/// Shared client context.
type ArcClientCtx = Arc<SyncMutex<ClientCtx>>;

/// State shared by all ATT bearers for the same client.
#[derive(Debug)]
#[must_use]
struct ClientCtx {
    cache: Cache,
    db_hash_read: bool,
    write_queue: WriteQueue,
    notify_mtu: u16,
    notify_cancel: BTreeMap<Handle, CancellationToken>,
    tx: tokio::sync::mpsc::Sender<NotifyVal>,
    rx: Option<tokio::sync::mpsc::Receiver<NotifyVal>>,
}

impl ClientCtx {
    /// Creates a new client context.
    #[must_use]
    fn new() -> ArcClientCtx {
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        Arc::new(SyncMutex::new(Self {
            cache: Cache::default(),
            db_hash_read: false,
            write_queue: WriteQueue::default(),
            notify_mtu: 0,
            notify_cancel: BTreeMap::new(),
            tx,
            rx: Some(rx),
        }))
    }

    /// Returns the notification/indication channel receiver. Returns [`None`]
    /// if another bearer is responsible for sending notifications.
    #[must_use]
    fn notify_rx(&mut self) -> Option<tokio::sync::mpsc::Receiver<NotifyVal>> {
        self.rx.take().or_else(|| {
            // This normally shouldn't happen, but if a previous receiver was
            // dropped before the client connection was closed, then we want
            // another ATT bearer to take over the role. The previous receiver
            // can't be passed back in ServerCtx::drop() because the MTU of the
            // new bearer may be different, and we don't know if another bearer
            // is even available, so services may try to send notifications
            // without any receiver.
            self.tx.is_closed().then(|| {
                let (tx, rx) = tokio::sync::mpsc::channel(1);
                self.tx = tx;
                rx
            })
        })
    }
}

/// Prepared write queue ([Vol 3] Part F, Section 3.4.6).
#[derive(Clone, Debug, Default)]
#[must_use]
struct WriteQueue {
    seq: Vec<(Handle, u16, u16)>,
    buf: Vec<u8>,
    clear: bool,
}

impl WriteQueue {
    const OP_LIMIT: usize = 1024;
    const BUF_LIMIT: usize = 1024 * 1024;

    /// Adds a prepared write to the queue.
    #[inline]
    fn add(&mut self, hdl: Handle, off: u16, v: &[u8]) -> bool {
        if self.clear {
            self.clear();
        }
        if self.seq.len() + 1 > Self::OP_LIMIT || self.buf.len() + v.len() > Self::BUF_LIMIT {
            return false;
        }
        let n = u16::try_from(v.len()).expect("invalid value length");
        self.buf.extend_from_slice(v);
        self.seq.push((hdl, off, n));
        true
    }

    /// Clears the queue.
    #[inline]
    fn clear(&mut self) {
        self.clear = false;
        self.seq.clear();
        self.buf.clear();
    }

    /// Returns an iterator over all prepared writes. After this is called, the
    /// next `add()` will automatically clear the queue.
    #[inline]
    fn iter(&mut self) -> impl Iterator<Item = (Handle, u16, &[u8])> {
        self.clear = true;
        let mut v = self.buf.unpack();
        self.seq.iter().map(move |&(hdl, off, n)| {
            // SAFETY: `buf` contains `n` bytes for each `seq` entry
            (hdl, off, unsafe {
                v.skip(n as _).unwrap_unchecked().into_inner()
            })
        })
    }
}
