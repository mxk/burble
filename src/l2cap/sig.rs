//! Signaling channel manager ([Vol 3] Part A, Section 4).

use structbuf::Unpack;
use tracing::warn;

use super::*;

/// Signaling channel manager ([Vol 3] Part A, Section 4).
#[derive(Debug)]
pub(super) struct SigChan {
    ch: Chan,
}

impl SigChan {
    /// Creates a new signaling channel manager.
    #[inline(always)]
    #[must_use]
    pub const fn new(ch: Chan) -> Self {
        Self { ch }
    }

    /// Handles signaling channel communications.
    pub async fn serve(mut self) -> Result<()> {
        loop {
            self.recv().await?;
        }
    }

    /// Receives and handles the next request.
    async fn recv(&mut self) -> Result<()> {
        let pdu = self.ch.recv().await?;
        let mut p = pdu.unpack();
        let (code, ident) = (p.u8(), p.u8());
        if let Ok(code) = SigCode::try_from(code) {
            warn!("Rejecting {code} on {}", self.ch.cid());
            if !code.is_req() {
                return Ok(());
            }
        } else {
            warn!("Rejecting unknown code {code:#04X} on {}", self.ch.cid());
        }
        let mut rsp = self.ch.alloc();
        rsp.append()
            .u8(SigCode::CommandRejectRsp)
            .u8(ident)
            .u16(2_u16)
            .u16(Reason::CommandNotUnderstood);
        self.ch.send(rsp).await
    }
}
