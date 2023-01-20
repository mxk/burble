use std::sync::Arc;

use ErrorCode::*;

use crate::gap::Uuid;
use crate::gatt::db::Db;
use crate::host;

use super::*;

/// GATT server providing services to a single client.
#[derive(Debug)]
pub struct Server<T: host::Transport> {
    br: Bearer<T>,
    db: Arc<Db>,
}

impl<T: host::Transport> Server<T> {
    /// Creates a new GATT server.
    #[inline]
    pub fn new(br: Bearer<T>, db: Arc<Db>) -> Self {
        Self { br, db }
    }

    /// Handles server configuration procedures ([Vol 3] Part G, Section 4.3).
    pub async fn configure(&mut self) -> Result<()> {
        let pdu = self.br.recv().await?; // TODO: Timeout?
        if pdu.opcode() == Opcode::ExchangeMtuReq {
            self.br.handle_exchange_mtu_req(pdu).await
        } else {
            self.handle_req(&pdu).await
        }
    }

    /// Runs main server loop.
    pub async fn serve(&self) -> Result<()> {
        loop {
            self.handle_req(&self.br.recv().await?).await?;
        }
    }

    /// Handles one client request.
    async fn handle_req(&self, pdu: &Pdu<T>) -> Result<()> {
        use Opcode::*;
        #[allow(clippy::match_same_arms)]
        let r = match pdu.opcode() {
            FindInformationReq => self.discover_characteristic_descriptors(pdu),
            FindByTypeValueReq => self.discover_primary_service_by_uuid(pdu),
            ReadByTypeReq => self.handle_read_by_type_req(pdu),
            ReadReq => unimplemented!(),
            ReadBlobReq => unimplemented!(),
            ReadMultipleReq => unimplemented!(),
            ReadByGroupTypeReq => self.discover_primary_services(pdu),
            WriteReq => unimplemented!(),
            WriteCmd => unimplemented!(),
            PrepareWriteReq => unimplemented!(),
            ExecuteWriteReq => unimplemented!(),
            ReadMultipleVariableReq => unimplemented!(),
            SignedWriteCmd => unimplemented!(),
            _ => pdu.err(RequestNotSupported),
        };
        self.br.send_rsp(r).await
    }

    /// Handles `ATT_READ_BY_TYPE_REQ` PDUs.
    fn handle_read_by_type_req(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        const INCLUDE: Uuid = Declaration::Include.uuid();
        const CHARACTERISTIC: Uuid = Declaration::Characteristic.uuid();
        let (hdls, typ) = pdu.read_by_type_req()?;
        match typ {
            INCLUDE => self.find_included_services(hdls),
            CHARACTERISTIC => self.discover_service_characteristics(hdls),
            _ => pdu.err(RequestNotSupported),
        }
    }

    fn access_check(&self, pdu: &Pdu<T>, hdl: Handle) -> RspResult<Handle> {
        let Some(req) = pdu.opcode().access_type() else {
            return pdu.hdl_err(RequestNotSupported, hdl);
        };
        self.db.access_check(pdu.opcode(), req, hdl)
    }
}

/// Primary service discovery ([Vol 3] Part G, Section 4.4).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Primary Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.1).
    fn discover_primary_services(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let (hdls, typ) = pdu.read_by_group_type_req()?;
        if typ != Declaration::PrimaryService {
            return pdu.err(UnsupportedGroupType);
        }
        if hdls.end() != Handle::MAX {
            return pdu.hdl_err(AttributeNotFound, hdls.start());
        }
        let it = (self.db)
            .primary_services(hdls.start(), None)
            .map(|v| (v.handle_range(), v.value()));
        self.br.read_by_group_type_rsp(hdls.start(), it)
    }

    /// Handles "Discover Primary Service by Service UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.2).
    fn discover_primary_service_by_uuid(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let (hdls, typ, uuid) = pdu.find_by_type_value_req()?;
        if typ != Declaration::PrimaryService {
            return pdu.err(UnsupportedGroupType);
        }
        let (Ok(uuid), true) = (Uuid::try_from(uuid), hdls.end() == Handle::MAX) else {
            return pdu.hdl_err(AttributeNotFound, hdls.start());
        };
        let it = (self.db.primary_services(hdls.start(), Some(uuid)))
            .map(|v| (v.handle(), Some(v.handle_range().end())));
        self.br.find_by_type_value_rsp(it)
    }
}

/// Relationship discovery ([Vol 3] Part G, Section 4.5).
impl<T: host::Transport> Server<T> {
    /// Handles "Find Included Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.5.1).
    fn find_included_services(&self, hdls: HandleRange) -> RspResult<Rsp<T>> {
        let it = self.db.includes(hdls).map(|v| (v.handle(), v.value()));
        self.br.read_by_type_rsp(hdls.start(), it)
    }
}

/// Characteristic discovery ([Vol 3] Part G, Section 4.6).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Characteristics of a Service" and "Discover
    /// Characteristics by UUID" sub-procedures ([Vol 3] Part G, Section 4.6.1).
    fn discover_service_characteristics(&self, hdls: HandleRange) -> RspResult<Rsp<T>> {
        let it = (self.db.characteristics(hdls)).map(|v| (v.handle(), v.value()));
        self.br.read_by_type_rsp(hdls.start(), it)
    }
}

/// Characteristic descriptor discovery ([Vol 3] Part G, Section 4.7).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Characteristic Descriptors"
    /// ([Vol 3] Part G, Section 4.7.1).
    fn discover_characteristic_descriptors(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdls = pdu.find_information_req()?;
        let it = self.db.descriptors(hdls).map(|at| (at.handle(), at.uuid()));
        self.br.find_information_rsp(hdls.start(), it)
    }
}

/// Characteristic value read ([Vol 3] Part G, Section 4.8).
impl<T: host::Transport> Server<T> {
    /// Handles "Read Characteristic Value" ([Vol 3] Part G, Section 4.8.1).
    fn read_characteristic_value(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdl = self.access_check(pdu, pdu.read_req()?)?;
        (self.br).read_rsp(self.db.lock().get(hdl).map_or(&[], |v| v.as_ref()))
    }
}
