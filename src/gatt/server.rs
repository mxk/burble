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
    pub async fn config(&mut self) -> Result<()> {
        let pdu = self.br.recv().await?;
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
        #[allow(clippy::match_same_arms)]
        match pdu.opcode() {
            Opcode::FindInformationReq => {}
            Opcode::FindByTypeValueReq => {}
            Opcode::ReadByTypeReq => {}
            Opcode::ReadReq => {}
            Opcode::ReadBlobReq => {}
            Opcode::ReadMultipleReq => {}
            Opcode::ReadByGroupTypeReq => {}
            Opcode::WriteReq => {}
            Opcode::WriteCmd => {}
            Opcode::PrepareWriteReq => {}
            Opcode::ExecuteWriteReq => {}
            Opcode::ReadMultipleVariableReq => {}
            Opcode::SignedWriteCmd => {}
            op => (self.br.error_rsp(op, None, ErrorCode::RequestNotSupported)).await?,
        }
        Ok(())
    }
}
