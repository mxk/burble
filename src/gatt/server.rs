use std::sync::Arc;

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
            self.handle_req(pdu).await
        }
    }

    /// Runs main server loop.
    pub async fn serve(&self) -> Result<()> {
        loop {
            self.handle_req(self.br.recv().await?).await?;
        }
    }

    /// Handles one client request.
    async fn handle_req(&self, pdu: Pdu<T>) -> Result<()> {
        use Opcode::*;
        #[allow(clippy::match_same_arms)]
        match pdu.opcode() {
            FindInformationReq => unimplemented!(),
            FindByTypeValueReq => self.discover_primary_service_by_uuid(pdu).await,
            ReadByTypeReq => self.handle_read_by_type_req(pdu).await,
            ReadReq => unimplemented!(),
            ReadBlobReq => unimplemented!(),
            ReadMultipleReq => unimplemented!(),
            ReadByGroupTypeReq => self.discover_all_primary_services(pdu).await,
            WriteReq => unimplemented!(),
            WriteCmd => unimplemented!(),
            PrepareWriteReq => unimplemented!(),
            ExecuteWriteReq => unimplemented!(),
            ReadMultipleVariableReq => unimplemented!(),
            SignedWriteCmd => unimplemented!(),
            op => (self.br.error_rsp(op, None, ErrorCode::RequestNotSupported)).await,
        }
    }

    /// Handles `ATT_READ_BY_TYPE_REQ` PDUs.
    async fn handle_read_by_type_req(&self, pdu: Pdu<T>) -> Result<()> {
        const INCLUDE: Uuid = Declaration::Include.uuid();
        const CHARACTERISTIC: Uuid = Declaration::Characteristic.uuid();
        let (hdls, typ) = self.br.check(pdu.read_by_type_req(), Ok).await?;
        match typ {
            INCLUDE => self.find_included_services(pdu, hdls).await,
            CHARACTERISTIC => self.discover_service_characteristics(pdu, hdls).await,
            _ => (self.br.error_rsp(pdu, None, ErrorCode::RequestNotSupported)).await,
        }
    }
}

/// Primary service discovery ([Vol 3] Part G, Section 4.4).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Primary Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.1).
    async fn discover_all_primary_services(&self, pdu: Pdu<T>) -> Result<()> {
        let v = self.br.check(pdu.read_by_group_type_req(), |(hdls, typ)| {
            if typ != Declaration::PrimaryService {
                return Err(pdu.err(ErrorCode::UnsupportedGroupType));
            }
            Ok((
                hdls.start(),
                (self.db.primary_services(hdls, None))
                    .map(|s| (s.decl_handle(), s.end_group_handle(), s.decl_value())),
            ))
        });
        let (start, it) = v.await?;
        self.br.read_by_group_type_rsp(start, it).await
    }

    /// Handles "Discover Primary Service by Service UUID" sub-procedure
    /// ([Vol 3] Part G, Section 4.4.2).
    async fn discover_primary_service_by_uuid(&self, pdu: Pdu<T>) -> Result<()> {
        let v = (self.br).check(pdu.find_by_type_value_req(), |(hdls, typ, uuid)| {
            if typ != Declaration::PrimaryService {
                return Err(pdu.err(ErrorCode::UnsupportedGroupType));
            }
            let uuid = Uuid::try_from(uuid)
                .map_err(|_| pdu.hdl_err(hdls.start(), ErrorCode::AttributeNotFound))?;
            Ok((self.db.primary_services(hdls, Some(uuid)))
                .map(|s| (s.decl_handle(), Some(s.end_group_handle()))))
        });
        self.br.find_by_type_value_rsp(v.await?).await
    }
}

/// Relationship discovery ([Vol 3] Part G, Section 4.5).
impl<T: host::Transport> Server<T> {
    /// Handles "Find Included Services" sub-procedure
    /// ([Vol 3] Part G, Section 4.5.1).
    async fn find_included_services(&self, pdu: Pdu<T>, hdls: HandleRange) -> Result<()> {
        let Some(s) = self.db.service(hdls) else {
            return self.br.error_rsp(pdu, hdls.start(), ErrorCode::InvalidHandle).await;
        };
        self.br.read_by_type_rsp(hdls.start(), s.includes()).await
    }
}

/// Characteristic discovery ([Vol 3] Part G, Section 4.6).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Characteristics of a Service" and "Discover
    /// Characteristics by UUID" sub-procedures ([Vol 3] Part G, Section 4.6.1).
    async fn discover_service_characteristics(&self, pdu: Pdu<T>, hdls: HandleRange) -> Result<()> {
        let Some(s) = self.db.service(hdls) else {
            return self.br.error_rsp(pdu, hdls.start(), ErrorCode::InvalidHandle).await;
        };
        let it = (s.characteristics()).map(|s| (s.decl_handle(), s.decl_value()));
        self.br.read_by_type_rsp(hdls.start(), it).await
    }
}
