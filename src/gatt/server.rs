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
            CHARACTERISTIC => self.discover_service_characteristics(pdu, hdls),
            _ => pdu.err(RequestNotSupported),
        }
    }
}

/// Primary service discovery ([Vol 3] Part G, Section 4.4).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Primary Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.1).
    fn discover_primary_services(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        pdu.read_by_group_type_req().and_then(|(hdls, typ)| {
            if typ != Declaration::PrimaryService {
                return pdu.err(UnsupportedGroupType);
            }
            let it = (self.db)
                .primary_services(hdls, None)
                .map(|s| (s.handle_range(), s.decl_value()));
            self.br.read_by_group_type_rsp(hdls.start(), it)
        })
    }

    /// Handles "Discover Primary Service by Service UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.2).
    fn discover_primary_service_by_uuid(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let (hdls, typ, uuid) = pdu.find_by_type_value_req()?;
        if typ != Declaration::PrimaryService {
            return pdu.err(UnsupportedGroupType);
        }
        let Ok(uuid) = Uuid::try_from(uuid) else {
            return pdu.hdl_err(AttributeNotFound, hdls.start());
        };
        self.br.find_by_type_value_rsp(
            (self.db.primary_services(hdls, Some(uuid)))
                .map(|s| (s.decl_handle(), Some(s.end_group_handle()))),
        )
    }
}

/// Relationship discovery ([Vol 3] Part G, Section 4.5).
impl<T: host::Transport> Server<T> {
    /// Handles "Find Included Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.5.1).
    fn find_included_services(&self, hdls: HandleRange) -> RspResult<Rsp<T>> {
        (self.br).read_by_type_rsp(hdls.start(), self.db.includes(hdls))
    }
}

/// Characteristic discovery ([Vol 3] Part G, Section 4.6).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Characteristics of a Service" and "Discover
    /// Characteristics by UUID" sub-procedures ([Vol 3] Part G, Section 4.6.1).
    fn discover_service_characteristics(
        &self,
        pdu: &Pdu<T>,
        hdls: HandleRange,
    ) -> RspResult<Rsp<T>> {
        self.db.service(hdls).map_or_else(
            || pdu.hdl_err(AttributeNotFound, hdls.start()),
            |s| {
                self.br.read_by_type_rsp(
                    hdls.start(),
                    (s.characteristics()).map(|s| (s.decl_handle(), s.decl_value())),
                )
            },
        )
    }
}

/// Characteristic descriptor discovery ([Vol 3] Part G, Section 4.7).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Characteristic Descriptors"
    /// ([Vol 3] Part G, Section 4.7.1).
    fn discover_characteristic_descriptors(&self, pdu: &Pdu<T>) -> RspResult<Rsp<T>> {
        let hdls = pdu.find_information_req()?;
        (self.br).find_information_rsp(hdls.start(), self.db.descriptors(hdls))
    }
}
