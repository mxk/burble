use std::sync::Arc;

use ErrorCode::*;

use crate::gap::{Uuid, Uuid16, UuidType};
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
            ReadReq => self.read(pdu),
            ReadBlobReq => self.read_blob(pdu),
            ReadMultipleReq => self.read_multiple(pdu),
            ReadByGroupTypeReq => self.discover_primary_services(pdu),
            WriteReq => unimplemented!(),
            WriteCmd => unimplemented!(),
            PrepareWriteReq => unimplemented!(),
            ExecuteWriteReq => unimplemented!(),
            ReadMultipleVariableReq => self.read_multiple_variable(pdu),
            SignedWriteCmd => unimplemented!(),
            _ => pdu.err(RequestNotSupported),
        };
        self.br.send_rsp(r).await
    }

    /// Handles `ATT_READ_BY_TYPE_REQ` PDUs.
    fn handle_read_by_type_req(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let (hdls, typ) = pdu.read_by_type_req()?;
        if let Some(t) = typ.as_uuid16().map(Uuid16::typ) {
            match t {
                UuidType::Declaration(Declaration::Include) => {
                    return self.find_included_services(hdls);
                }
                UuidType::Declaration(Declaration::Characteristic) => {
                    return self.discover_service_characteristics(hdls);
                }
                // TODO: Remove this restriction?
                UuidType::Characteristic(_) => {}
                _ => return pdu.err(RequestNotSupported),
            }
        }
        // 16-bit characteristic or any 128-bit attribute
        self.read_by_type(
            hdls.start(),
            (self.db).try_range_access(self.br.access_req(pdu), hdls, typ)?,
        )
    }

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

    /// Handles "Find Included Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.5.1).
    fn find_included_services(&self, hdls: HandleRange) -> RspResult<Rsp<T>> {
        let it = self.db.includes(hdls).map(|v| (v.handle(), v.value()));
        self.br.read_by_type_rsp(hdls.start(), it)
    }

    /// Handles "Discover All Characteristics of a Service" and "Discover
    /// Characteristics by UUID" sub-procedures ([Vol 3] Part G, Section 4.6.1).
    fn discover_service_characteristics(&self, hdls: HandleRange) -> RspResult<Rsp<T>> {
        let it = (self.db.characteristics(hdls)).map(|v| (v.handle(), v.value()));
        self.br.read_by_type_rsp(hdls.start(), it)
    }

    /// Handles "Discover All Characteristic Descriptors" sub-procedure
    /// ([Vol 3] Part G, Section 4.7.1).
    fn discover_characteristic_descriptors(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdls = pdu.find_information_req()?;
        let it = self.db.descriptors(hdls).map(|at| (at.handle(), at.uuid()));
        self.br.find_information_rsp(hdls.start(), it)
    }

    /// Handles "Read Characteristic Value" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.1).
    fn read(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdl = (self.db).try_access(self.br.access_req(pdu), pdu.read_req()?)?;
        let db = self.db.lock();
        self.br.read_rsp(db.get(hdl).unwrap_or_default())
    }

    /// Handles "Read Using Characteristic UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.2).
    fn read_by_type(&self, start: Handle, hdls: Vec<Handle>) -> RspResult<Rsp<T>> {
        let db = self.db.lock();
        let it = (hdls.into_iter()).map(|hdl| (hdl, db.get(hdl).unwrap_or_default()));
        self.br.read_by_type_rsp(start, it)
    }

    /// Handles "Read Long Characteristic Values" and "Read Long Characteristic
    /// Descriptors" sub-procedures ([Vol 3] Part G, Section 4.8.3 and 4.12.2).
    fn read_blob(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let (hdl, off) = pdu.read_blob_req()?;
        let hdl = self.db.try_access(self.br.access_req(pdu), hdl)?;
        (self.db.lock().get(hdl).unwrap_or_default())
            .get(usize::from(off)..)
            .map_or_else(
                || pdu.hdl_err(InvalidOffset, hdl),
                |v| self.br.read_blob_rsp(v),
            )
    }

    /// Handles "Read Multiple Characteristic Values" sub-procedure
    /// ([Vol 3] Part G, Section 4.8.4).
    fn read_multiple(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdls = (self.db).try_multi_access(self.br.access_req(pdu), pdu.read_multiple_req()?)?;
        let db = self.db.lock();
        let it = hdls.into_iter().map(|hdl| db.get(hdl).unwrap_or_default());
        self.br.read_multiple_rsp(it)
    }

    /// Handles "Read Multiple Variable Length Characteristic Values"
    /// sub-procedure ([Vol 3] Part G, Section 4.8.5).
    fn read_multiple_variable(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdls = (self.db)
            .try_multi_access(self.br.access_req(pdu), pdu.read_multiple_variable_req()?)?;
        let db = self.db.lock();
        let it = hdls.into_iter().map(|hdl| db.get(hdl).unwrap_or_default());
        self.br.read_multiple_variable_rsp(it)
    }
}
