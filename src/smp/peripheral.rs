use std::time::Duration;

use tracing::error;

use burble_crypto::{Nonce, PublicKeyX, SecretKey, LTK};

use crate::hci::Role;
use crate::l2cap::Chan;
use crate::{hci, le};

use super::*;

/// Peripheral role security manager implementing LE security mode 1 level 4
/// "Authenticated LE Secure Connections pairing" in "Secure Connections Only"
/// mode ([Vol 3] Part C, Section 10.2.1 and 10.2.4).
#[derive(Debug)]
pub struct Peripheral {
    ch: Chan,
}

impl Peripheral {
    /// Creates a new peripheral security manager.
    #[inline(always)]
    pub(crate) fn new(ch: Chan) -> Self {
        assert_eq!(ch.conn().borrow().role, Role::Peripheral);
        Self { ch }
    }

    /// Handles responder pairing role. This method is not cancel safe.
    pub async fn respond(&mut self, dev: &mut Device, store: &KeyStore) -> Result<()> {
        // TODO: Return a cancellable task?
        let init = match Command::try_from(self.ch.recv().await?) {
            Ok(Command::PairingRequest(init)) => init,
            Ok(Command::PairingFailed(r)) => {
                error!("Remote error instead of PairingRequest: {r}");
                return Err(Error::Remote(r));
            }
            Ok(cmd) => {
                error!("Unexpected command instead of PairingRequest: {cmd:?}");
                return self.fail(Reason::InvalidParameters).await;
            }
            Err(reason) => return self.fail(reason).await,
        };
        let Phase1 { a, b, method, sec } = self.phase1(dev, init).await?;
        let (peer, ltk) = self.phase2(dev, method, a.into(), b.into()).await?;
        let mut keys = Keys::new(sec, ltk);
        // TODO: Save LTK before phase 3 to ensure that SecDb finds it
        (self.phase3(b.initiator_keys, b.responder_keys, &mut keys)).await?;
        if !store.save(peer, &keys) {
            return Err(Error::Io(io::Error::new(
                io::ErrorKind::Other,
                "failed to save peer keys",
            )));
        }
        Ok(())
    }

    /// Performs Pairing Feature Exchange phase
    /// ([Vol 3] Part H, Section 2.3.5.1 and C.1).
    async fn phase1(&mut self, dev: &Device, a: PairingParams) -> Result<Phase1> {
        if !a.auth_req.contains(AuthReq::SC) {
            // [Vol 3] Part H, Section 2.3 and C.5.1
            error!("Peer does not support LE Secure Connections");
            return self.fail(Reason::PairingNotSupported).await;
        }
        if a.max_key_len < PairingParams::MIN_KEY_LEN {
            // [Vol 3] Part H, Section C.5.3
            error!(
                "Peer does not support {}-bit encryption (maximum is {})",
                PairingParams::MIN_KEY_LEN * 8,
                a.max_key_len * 8
            );
            return self.fail(Reason::EncryptionKeySize).await;
        }
        let mut b = PairingParams {
            io_cap: dev.io_cap(),
            ..PairingParams::default()
        };
        (b.auth_req).set(AuthReq::BONDING, a.auth_req.contains(AuthReq::BONDING));
        (b.auth_req).set(AuthReq::MITM, !matches!(b.io_cap, IoCap::NoInputNoOutput));
        // [Vol 3] Part H, Section 2.3.5.1, Table 2.7
        if a.oob_data | b.oob_data {
            error!("OOB pairing method not implemented"); // TODO: Implement
            return self.fail(Reason::OobNotAvailable).await;
        }
        // [Vol 3] Part H, Section 2.3.5.1, Table 2.8
        let method = if a.auth_req.union(b.auth_req).contains(AuthReq::MITM) {
            KeyGenMethod::resolve(a.io_cap, b.io_cap)
        } else {
            KeyGenMethod::JustWorks
        };
        // TODO: Out-of-band may be unauthenticated
        // ([Vol 3] Part H, Section 2.3.5.1)
        let authn = !matches!(method, KeyGenMethod::JustWorks);
        // TODO: Compile-time option to allow Just Works?
        if b.auth_req.contains(AuthReq::MITM) && !authn {
            error!("Just Works association model selected, but MITM protection is required");
            return self.fail(Reason::AuthenticationRequirements).await;
        }
        self.send(Command::PairingResponse(b)).await?;
        // [Vol 3] Part H, Section 2.3.4
        let mut sec = hci::ConnSec::key_len(a.max_key_len.min(b.max_key_len) * 8);
        if authn {
            sec.insert(hci::ConnSec::AUTHN);
        }
        if b.auth_req.contains(AuthReq::BONDING) {
            sec.insert(hci::ConnSec::BOND);
        }
        Ok(Phase1 { a, b, method, sec })
    }

    /// Performs LE Secure Connections Long Term Key (LTK) Generation phase
    /// ([Vol 3] Part H, Section 2.3.5.6).
    async fn phase2(
        &mut self,
        dev: &mut Device,
        method: KeyGenMethod,
        ioa: burble_crypto::IoCap,
        iob: burble_crypto::IoCap,
    ) -> Result<(le::Addr, LTK)> {
        // Public key exchange ([Vol 3] Part H, Section 2.3.5.6.1 and C.2.2.1)
        let skb = SecretKey::new();
        let pkb = skb.public_key();
        let Command::PairingPublicKey(pka) = self.recv().await? else {
            return self.expecting(Code::PairingPublicKey).await;
        };

        // Send the local public key before validating the remote key to allow
        // parallel computation of DHKey. No security risk in doing so.
        self.send(Command::PairingPublicKey(pkb)).await?;
        let Some(dh_key) = skb.dh_key(pka) else {
            return self.fail(Reason::InvalidParameters).await; // Invalid or debug key
        };

        // Authentication stage 1 ([Vol 3] Part H, Section C.2.2.2)
        let Authn1 { na, nb, ra, rb } = match method {
            KeyGenMethod::JustWorks | KeyGenMethod::NumCompare => {
                self.authn1_num_compare(dev, method, pka.x(), pkb.x())
                    .await?
            }
            KeyGenMethod::PasskeyEntry => unimplemented!("Passkey Entry protocol"), // TODO
        };

        // Authentication stage 2 and long term key calculation
        // ([Vol 3] Part H, Section 2.3.5.6.5 and C.2.2.4).
        let (peer, a, b) = {
            let (peer_addr, local_addr) = {
                let cn = self.ch.conn().borrow();
                (cn.peer_addr, cn.local_addr)
            };
            if local_addr.is_zero() {
                error!("Pairing failed because local address is unknown");
                return self.fail(Reason::UnspecifiedReason).await;
            }
            (peer_addr, peer_addr.into(), local_addr.into())
        };
        let (mac_key, ltk) = dh_key.f5(na, nb, a, b);
        let eb = mac_key.f6(nb, na, ra, iob, b, a);
        let Command::PairingDhKeyCheck(ea) = self.recv().await? else {
            return self.expecting(Code::PairingDhKeyCheck).await;
        };
        if ea != mac_key.f6(na, nb, rb, ioa, a, b) {
            return self.fail(Reason::DhKeyCheckFailed).await;
        }
        self.send(Command::PairingDhKeyCheck(eb)).await?;
        Ok((peer, ltk))
    }

    /// Implements Authentication stage 1 – Just Works or Numeric Comparison
    /// ([Vol 3] Part H, Section 2.3.5.6.2 and C.2.2.2.1).
    async fn authn1_num_compare(
        &mut self,
        dev: &mut Device,
        method: KeyGenMethod,
        pka: &PublicKeyX,
        pkb: &PublicKeyX,
    ) -> Result<Authn1> {
        let nb = Nonce::new();
        let (rb, ra) = (0, 0);
        let cb = nb.f4(pkb, pka, 0);
        // SUBTLE: The order of these send/recv ops is important. See last
        // paragraph of Section 2.3.5.6.2.
        self.send(Command::PairingConfirm(cb)).await?;
        let Command::PairingRandom(na) = self.recv().await? else {
            return self.expecting(Code::PairingRandom).await;
        };
        self.send(Command::PairingRandom(nb)).await?;
        if matches!(method, KeyGenMethod::JustWorks) {
            return Ok(Authn1 { na, nb, ra, rb });
        }
        let vb = na.g2(pka, pkb, &nb);
        let display = dev.display.as_mut().expect("display not available");
        let yes_no = dev.confirm.as_mut().expect("input not available");
        // TODO: Abort if PairingFailed is received while waiting for the user
        if !display.show(vb).await || !yes_no.confirm().await {
            // [Vol 3] Part H, Section C.2.2.2.4
            return self.fail(Reason::NumericComparisonFailed).await;
        }
        Ok(Authn1 { na, nb, ra, rb })
    }

    /// Performs Transport Specific Key Distribution phase
    /// ([Vol 3] Part H, Section 3.6.1 and C.3).
    async fn phase3(&mut self, a: KeyDist, b: KeyDist, _k: &mut Keys) -> Result<()> {
        if !b.is_empty() {
            // TODO: IRK
            // TODO: BD_ADDR
            // TODO: CSRK
            return self.fail(Reason::UnspecifiedReason).await;
        }
        if !a.is_empty() {
            // TODO: IRK
            // TODO: BD_ADDR
            // TODO: CSRK
            return self.fail(Reason::KeyRejected).await;
        }
        Ok(())
    }

    /// Returns the next command.
    async fn recv(&mut self) -> Result<Command> {
        // [Vol 3] Part H, Section 3.4
        let pdu = match tokio::time::timeout(Duration::from_secs(30), self.ch.recv()).await {
            Ok(Ok(pdu)) => pdu,
            Ok(Err(e)) => return Err(e.into()),
            Err(_) => return Err(Error::Timeout), // TODO: Mark channel as unusable
        };
        match Command::try_from(pdu) {
            Ok(Command::PairingFailed(r)) => {
                error!("Remote error: {r}");
                Err(Error::Remote(r))
            }
            Ok(cmd) => Ok(cmd),
            Err(r) => self.fail(r).await,
        }
    }

    /// Sends a `PairingFailed(InvalidParameters)` response when the received
    /// command didn't match code `c`.
    async fn expecting<R>(&mut self, c: Code) -> Result<R> {
        error!("Command mismatch while waiting for {c}");
        self.fail(Reason::InvalidParameters).await
    }

    /// Sends a `PairingFailed` command over the channel to indicate a local
    /// failure.
    async fn fail<R>(&mut self, r: Reason) -> Result<R> {
        if let Err(e) = self.send(Command::PairingFailed(r)).await {
            error!("Error sending PairingFailed response: {e}");
        }
        Err(Error::Local(r))
    }

    /// Sends a command over the channel.
    async fn send(&mut self, cmd: Command) -> Result<()> {
        let mut pdu = self.ch.alloc();
        cmd.pack(&mut pdu);
        Ok(self.ch.send(pdu).await?)
    }
}

/// Output of phase 1.
#[derive(Clone, Copy)]
#[must_use]
struct Phase1 {
    a: PairingParams,
    b: PairingParams,
    method: KeyGenMethod,
    sec: hci::ConnSec,
}

/// Output of phase 2, authentication stage 1.
#[must_use]
struct Authn1 {
    na: Nonce,
    nb: Nonce,
    ra: u128,
    rb: u128,
}
