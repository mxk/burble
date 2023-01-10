use std::sync::Arc;

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
            ReadByTypeReq => unimplemented!(),
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
}

/// Primary service discovery ([Vol 3] Part G, Section 4.4).
impl<T: host::Transport> Server<T> {
    /// Handles "Discover All Primary Services" procedure
    /// ([Vol 3] Part G, Section 4.4.1).
    async fn discover_all_primary_services(&self, pdu: Pdu<T>) -> Result<()> {
        let v = self.br.check(pdu.read_by_group_type_req(), |(hdls, typ)| {
            if typ != Declaration::PrimaryService {
                return Err(pdu.err(ErrorCode::UnsupportedGroupType));
            }
            (self.db.primary_services(hdls, &[]))
                .ok_or_else(|| pdu.hdl_err(hdls.start(), ErrorCode::AttributeNotFound))
        });
        self.br.read_by_group_type_rsp(v.await?).await
    }

    /// Handles "Discover Primary Service by Service UUID" procedure
    /// ([Vol 3] Part G, Section 4.4.2).
    async fn discover_primary_service_by_uuid(&self, pdu: Pdu<T>) -> Result<()> {
        let v = (self.br).check(pdu.find_by_type_value_req(), |(hdls, typ, uuid)| {
            if typ != Declaration::PrimaryService {
                return Err(pdu.err(ErrorCode::UnsupportedGroupType));
            }
            (self.db.primary_services(hdls, uuid))
                .ok_or_else(|| pdu.hdl_err(hdls.start(), ErrorCode::AttributeNotFound))
                .map(move |it| it.map(|(start, end, _)| (start, Some(end))))
        });
        self.br.find_by_type_value_rsp(v.await?).await
    }
}
