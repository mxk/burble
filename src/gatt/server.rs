use std::collections::btree_map::Entry;
use std::sync::{Arc, Weak};
use std::time::Duration;
use std::vec;

use structbuf::{Unpack, Unpacker};
use tokio_util::sync::CancellationToken;
use tracing::{debug, warn};

use ErrorCode::*;

use crate::gap::{Uuid, UuidType};
use crate::le::Addr;
use crate::{SyncMutex, SyncMutexGuard};

use super::*;

/// GATT server. The server connects services with individual ATT bearers and
/// maintains shared client state.
#[derive(Debug)]
pub struct Server {
    db: Db,
    io: IoMap,
    store: Arc<CacheStore>,
    clients: SyncMutex<BTreeMap<Addr, Weak<SyncMutex<ClientCtx>>>>,
}

impl Server {
    /// Creates a new GATT server.
    #[inline]
    #[must_use]
    pub fn new(mut db: Builder<Db>, store: Arc<CacheStore>) -> Arc<Self> {
        let srv_io = Io::from(|_: IoReq| unreachable!());
        // [Vol 3] Part G, Section 7
        db.primary_service(Service::GenericAttribute, [], |db| {
            // TODO: Implement
            /*db.characteristic(
                Characteristic::ServiceChanged,
                Prop::INDICATE,
                Access::NONE,
                srv_io.clone(),
                |b| b.client_cfg(Access::READ_WRITE),
            );*/
            /*db.characteristic(
                Characteristic::ClientSupportedFeatures,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                srv_io.clone(),
                |_| {},
            );*/
            db.characteristic(
                Characteristic::DatabaseHash,
                Prop::READ,
                Access::READ,
                srv_io.clone(),
                |_| {},
            );
        });
        let (db, io) = db.freeze();
        Arc::new(Self {
            db,
            io,
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
    pub fn attach(self: &Arc<Self>, br: &Bearer) -> ServerCtx {
        let srv = Arc::clone(self);
        let cc = self.get_client_ctx(br.conn().peer_addr);
        let ntf = cc.lock().notify_rx();
        ServerCtx { srv, cc, ntf }
    }

    /// Returns shared [`ClientCtx`] for the specified peer address.
    fn get_client_ctx(&self, peer: Addr) -> ArcClientCtx {
        let mut clients = self.clients.lock();
        clients.retain(|_, cc| cc.strong_count() != 0);
        match clients.entry(peer) {
            Entry::Vacant(e) => {
                let cc = self.new_client_ctx();
                e.insert(Arc::downgrade(&cc));
                cc
            }
            Entry::Occupied(mut e) => e.get().upgrade().unwrap_or_else(|| {
                // `upgrade()` shouldn't fail because we just removed all Arcs
                // without strong references, but there is a race with the last
                // strong reference being dropped in a multithreaded runtime.
                let cc = self.new_client_ctx();
                e.insert(Arc::downgrade(&cc));
                cc
            }),
        }
    }

    /// Creates a new [`ClientCtx`].
    fn new_client_ctx(&self) -> ArcClientCtx {
        let cache = Cache {
            // Guarantee hash mismatch until the first request is received and
            // bond status can be determined.
            db_hash: !self.db.hash(),
            vals: BTreeMap::new(),
        };
        let (tx, rx) = tokio::sync::mpsc::channel(1);
        Arc::new(SyncMutex::new(ClientCtx {
            bond: None,
            cache,
            write_queue: WriteQueue::default(),
            notify_cancel: BTreeMap::new(),
            tx,
            rx: Some(rx),
        }))
    }

    /// Saves client cache to persistent storage.
    fn persist(&self, cc: &mut ClientCtx) {
        let Some(peer) = cc.bond else { return };
        cc.cache.vals.retain(|&hdl, v| {
            v != &ClientCfg::INIT
                || self.db.get(hdl).map_or(true, |(uuid, _)| {
                    uuid != Descriptor::ClientCharacteristicConfiguration
                })
        });
        self.store.save(peer, &cc.cache);
    }
}

/// Server context used by an ATT bearer to handle client requests and service
/// notifications/indications.
#[derive(Debug)]
#[must_use]
pub struct ServerCtx {
    srv: Arc<Server>,
    cc: ArcClientCtx,
    ntf: Option<tokio::sync::mpsc::Receiver<NotifyVal>>,
}

impl ServerCtx {
    /// Runs a server event loop for the specified bearer.
    pub async fn serve(mut self, mut br: Bearer) -> Result<()> {
        // Configuration phase for exchanging the MTU before enabling
        // notifications ([Vol 3] Part G, Section 4.3).
        // TODO: Fixed ATT bearer only
        match tokio::time::timeout(Duration::from_secs(3), br.recv()).await {
            Ok(Ok(pdu)) => {
                if pdu.opcode() == Opcode::ExchangeMtuReq {
                    br.handle_exchange_mtu_req(&pdu).await?;
                } else {
                    self.handle(&mut br, &pdu).await?;
                }
            }
            Ok(Err(e)) => return Err(e),
            Err(_timeout) => {}
        }
        if self.enable_notify(&mut br) {
            loop {
                // TODO: Allow processing requests while waiting for indication
                // confirmation?
                tokio::select! {
                    pdu = br.recv() => self.handle(&mut br, &pdu?).await?,
                    nv = self.notify() => nv.exec(&mut br).await,
                }
            }
        } else {
            loop {
                let pdu = br.recv().await?;
                self.handle(&mut br, &pdu).await?;
            }
        }
    }

    /// Enables service notifications/indications. This resumes notification
    /// sessions for bonded clients and should be done after the MTU exchange.
    /// Returns false if this bearer will not be used for sending notifications,
    /// in which case [`Self::notify`] will never resolve.
    fn enable_notify(&mut self, br: &mut Bearer) -> bool {
        if self.ntf.is_none() {
            return false;
        }
        // TODO: The client can re-enable encryption at any time, so there
        // should be some signal mechanism for when that happens. Right now, we
        // assume that if a bond exists, then the client will enable encryption
        // immediately.
        if self.restore_bond(br) {
            self.resume_notify(br);
        }
        true
    }

    /// Restores cached state for bonded clients. Returns whether the in-memory
    /// cache was updated.
    fn restore_bond(&mut self, br: &mut Bearer) -> bool {
        let peer = {
            let cn = br.conn();
            if !cn.bond() {
                drop(cn);
                // The initial state of a client without a trusted relationship
                // is change-aware ([Vol 3] Part G, Section 2.5.2.1).
                let mut cc = self.cc.lock();
                cc.bond = None;
                cc.cache.db_hash = self.srv.db.hash();
                return false;
            }
            cn.peer_addr
        };
        let Some(cache) = self.srv.store.load(peer) else { return false };
        let cur_hash = self.srv.db.hash();
        if cache.db_hash != cur_hash {
            // TODO: Service changed notification
            debug!("Invalidating cache for {peer}");
            self.srv.store.remove(peer);
            return false;
        }
        let mut cc = self.cc.lock();
        if cc.cache.db_hash == cur_hash {
            // Client became change-aware before re-enabling encryption? This
            // shouldn't happen. We don't know whether to use the saved or
            // in-memory cache, so we keep the latter.
            warn!("Ignoring saved cache for {peer}");
            return false;
        }
        cc.bond = Some(br.conn().peer_addr);
        cc.cache = cache;
        debug!("Restored bond for {peer}");
        true
    }

    /// Resumes notification sessions after the bond is restored.
    fn resume_notify(&mut self, br: &mut Bearer) {
        let mut cc = self.cc.lock();
        if cc.bond.is_none() {
            return;
        }
        let v: Vec<(Handle, ClientCfg)> = (cc.cache.vals.iter())
            .filter_map(|(&hdl, val)| {
                (val.unpack().map(Unpacker::u16))
                    .and_then(ClientCfg::from_bits)
                    .and_then(|cfg| {
                        (cfg.intersects(ClientCfg::NOTIFY_MASK)
                            && self.uuid(hdl) == Descriptor::ClientCharacteristicConfiguration)
                            .then_some((hdl, cfg))
                    })
            })
            .collect();
        let mtu = br.mtu();
        for (hdl, cfg) in v {
            let _ = self.cccd_apply(&mut cc, Opcode::ExecuteWriteReq, hdl, mtu, cfg);
        }
    }

    /// Receives the next service notification or indication.
    async fn notify(&mut self) -> NotifyVal {
        let Some(rx) = self.ntf.as_mut() else {
            return std::future::pending().await;
        };
        // ClientCtx is holding a sender, so this will never return None
        rx.recv().await.unwrap()
    }

    /// Handles received client request.
    async fn handle(&mut self, br: &mut Bearer, pdu: &Pdu) -> Result<()> {
        use Opcode::*;
        let op = pdu.opcode();
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
    fn handle_read_by_type_req(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let (hdls, typ) = pdu.read_by_type_req()?;
        match typ.typ() {
            UuidType::Declaration(Declaration::Include) => self.find_included_services(br, hdls),
            UuidType::Declaration(Declaration::Characteristic) => {
                self.discover_service_characteristics(br, hdls)
            }
            // TODO: Remove Characteristic-only restriction?
            UuidType::Characteristic(_) | UuidType::NonSig => self.read_by_type(
                br,
                hdls.start(),
                (self.srv.db).try_range_access(br.access_req(pdu), hdls, typ)?,
            ),
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
    /// Characteristics by UUID" sub-procedures ([Vol 3] Part G, Section 4.6.1).
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
    fn read(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
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
    fn read_blob(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let (hdl, off) = pdu.read_blob_req()?;
        let hdl = self.srv.db.try_access(br.access_req(pdu), hdl)?;
        let mut r = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_blob_rsp(self.do_read(r.with(hdl, self.uuid(hdl), off))?)
    }

    /// Handles "Read Multiple Characteristic Values" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.4).
    fn read_multiple(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let hdls = (self.srv.db).try_multi_access(br.access_req(pdu), pdu.read_multiple_req()?)?;
        let req = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_multiple_rsp(Reader::new(self, req, hdls))
    }

    /// Handles "Read Multiple Variable Length Characteristic Values"
    /// sub-procedure ([Vol 3] Part G, Section 4.8.5).
    fn read_multiple_variable(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
        let hdls = (self.srv.db)
            .try_multi_access(br.access_req(pdu), pdu.read_multiple_variable_req()?)?;
        let req = ReadReq::new(pdu.opcode(), br.mtu());
        br.read_multiple_variable_rsp(Reader::new(self, req, hdls))
    }

    /// Handles "Write Without Response" and "Write Characteristic Value"
    /// sub-procedures ([Vol 3] Part G, Section 4.9.1 and 4.9.3).
    fn write(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Option<Rsp>> {
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
    fn prepare_write(&self, br: &mut Bearer, pdu: &Pdu) -> RspResult<Rsp> {
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
            (w.hdl, w.off, w.val) = (hdl, off, val);
            self.do_write(&w)?;
        }
        br.execute_write_rsp()
    }

    /// Executes a read operation.
    fn do_read<'a>(&self, r: &'a mut ReadReq) -> RspResult<&'a [u8]> {
        use Descriptor::*;
        // TODO: Cache awareness check
        match r.uuid().typ() {
            // TODO: DatabaseHash read results in change-aware state
            UuidType::Descriptor(ClientCharacteristicConfiguration) => self.cache_read(r),
            _ => self.srv.io.read(r),
        }
    }

    /// Executes a write operation.
    fn do_write(&self, w: &WriteReq) -> RspResult<()> {
        use Descriptor::*;
        // TODO: Cache awareness check
        match w.uuid.typ() {
            UuidType::Descriptor(ClientCharacteristicConfiguration) => self.cccd_write(w),
            _ => self.srv.io.write(w),
        }
    }

    /// Executes a CCCD write operation.
    #[inline]
    fn cccd_write(&self, w: &WriteReq) -> RspResult<()> {
        if w.off != 0 {
            return w.op.hdl_err(InvalidOffset, w.hdl);
        }
        let Some(cfg) = w.val.unpack().map(Unpacker::u16).and_then(ClientCfg::from_bits) else {
            return w.op.hdl_err(ValueNotAllowed, w.hdl);
        };
        if cfg.contains(ClientCfg::NOTIFY) && cfg.contains(ClientCfg::INDICATE) {
            // TODO: Is this combination valid?
            return w.op.hdl_err(ValueNotAllowed, w.hdl);
        }
        self.cccd_apply(&mut self.cc.lock(), w.op, w.hdl, w.mtu, cfg)
    }

    /// Applies CCCD configuration.
    #[allow(clippy::similar_names)]
    fn cccd_apply(
        &self,
        cc: &mut SyncMutexGuard<ClientCtx>,
        op: Opcode,
        hdl: Handle,
        mtu: u16,
        cfg: ClientCfg,
    ) -> RspResult<()> {
        let char = (self.srv.db.get_characteristic(hdl)).expect("invalid CCCD handle");
        let vhdl = char.value_handle();
        let uuid = self.uuid(vhdl);
        let val = cfg.bits().to_le_bytes();
        let cached = (cc.cache.vals.entry(hdl)).or_insert_with(|| ClientCfg::INIT.to_vec());
        if cached == &val {
            return Ok(());
        }
        (cc.cache.vals.entry(hdl))
            .and_modify(|v| v.copy_from_slice(&val))
            .or_insert_with(|| val.to_vec());
        if cfg.union(ClientCfg::NOTIFY_MASK).is_empty() {
            if let Some(ct) = cc.notify_cancel.remove(&hdl) {
                ct.cancel();
            }
            self.srv.persist(cc);
            debug!("Disabled notifications for {} ({vhdl})", uuid.typ());
            return Ok(());
        }
        let ct = CancellationToken::new();
        let r = self.srv.io.notify(NotifyReq {
            op,
            hdl: vhdl,
            uuid,
            mtu,
            ind: cfg.contains(ClientCfg::INDICATE),
            tx: tokio_util::sync::PollSender::new(cc.tx.clone()),
            ct: ct.clone(),
        });
        match r {
            Ok(_) => {
                debug!("Enabled notifications for {} ({vhdl})", uuid.typ());
                cc.notify_cancel.insert(hdl, ct);
            }
            Err(e) => {
                warn!(
                    "Failed to enable notifications for {} ({vhdl}): {}",
                    uuid.typ(),
                    e.err()
                );
                ct.cancel();
                if let Some(v) = cc.cache.vals.get_mut(&hdl) {
                    v.copy_from_slice(&cfg.difference(ClientCfg::NOTIFY_MASK).bits().to_le_bytes());
                }
            }
        }
        self.srv.persist(cc);
        r
    }

    /// Executes a read operation from the client's cache.
    #[inline]
    fn cache_read<'a>(&self, r: &'a mut ReadReq) -> RspResult<&'a [u8]> {
        let cc = self.cc.lock();
        match r.complete(&cc.cache.vals[&r.handle()]) {
            Ok(_) => Ok(r.buf.as_ref()),
            Err(e) => r.opcode().hdl_err(e, r.handle()),
        }
    }

    /// Returns the UUID of the specified handle.
    #[inline]
    fn uuid(&self, hdl: Handle) -> Uuid {
        self.srv.db.get(hdl).expect("invalid handle").0
    }
}

impl Drop for ServerCtx {
    fn drop(&mut self) {
        if self.ntf.is_some() {
            let mut cc = self.cc.lock();
            for ct in cc.notify_cancel.values() {
                ct.cancel();
            }
            cc.notify_cancel.clear();
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
        let Some(hdl) = self.hdls.next() else { return Ok(false) };
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
    bond: Option<Addr>,
    cache: Cache,
    write_queue: WriteQueue,
    notify_cancel: BTreeMap<Handle, CancellationToken>,
    tx: tokio::sync::mpsc::Sender<NotifyVal>,
    rx: Option<tokio::sync::mpsc::Receiver<NotifyVal>>,
}

impl ClientCtx {
    /// Returns the notification/indication channel receiver. Returns [`None`]
    /// if another bearer is responsible for sending notifications.
    fn notify_rx(&mut self) -> Option<tokio::sync::mpsc::Receiver<NotifyVal>> {
        self.rx.take().or_else(|| {
            // This normally shouldn't happen, but if a previous notification
            // receiver was dropped before the client connection was closed,
            // then we want another ATT bearer to take over the role. The
            // previous receiver can't be passed back in `ServerCtx::drop()`
            // because we don't know if another bearer is available, so services
            // may try to send notifications without any receiver, and the MTU
            // of the new bearer may be different.
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
