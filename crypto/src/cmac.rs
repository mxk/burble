use std::fmt::Debug;
use std::mem;

use aes::{cipher, Aes128};
use cmac::{digest, Cmac};
use subtle::{Choice, ConstantTimeEq};
use zeroize::{Zeroize, ZeroizeOnDrop};

use crate::{debug_secret, Addr, Confirm, IoCap, PublicKeyX};

/// 128-bit CMAC key.
#[allow(clippy::redundant_pub_crate)]
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub(crate) struct Key(cipher::Key<Aes128>);

impl Key {
    /// Creates a new key from a `u128`.
    #[inline(always)]
    fn new(k: u128) -> Self {
        Self(k.to_be_bytes().into())
    }

    /// Returns the salt key for [`DHKey::f5`] ([Vol 3] Part H, Section 2.2.7).
    #[inline(always)]
    pub fn salt() -> Self {
        Self::new(0x6C88_8391_AAF5_A538_6037_0BDB_5A60_83BE)
    }

    /// Returns new AES-CMAC state.
    #[inline(always)]
    #[must_use]
    pub fn aes_cmac(&self) -> AesCmac {
        AesCmac(digest::KeyInit::new(&self.0))
    }
}

impl From<Mac> for Key {
    #[inline(always)]
    fn from(m: Mac) -> Self {
        Self::new(m.0)
    }
}

debug_secret!(Key);

/// 128-bit key used to compute LE Secure Connections check value.
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct MacKey(pub(crate) Key);

debug_secret!(MacKey);

impl MacKey {
    /// Generates LE Secure Connections check value
    /// ([Vol 3] Part H, Section 2.2.8).
    pub fn f6(&self, n1: Nonce, n2: Nonce, r: u128, io_cap: IoCap, a1: Addr, a2: Addr) -> Mac {
        let mut m = self.0.aes_cmac();
        m.update(&n1.to_be_bytes())
            .update(&n2.to_be_bytes())
            .update(&r.to_be_bytes())
            .update(&io_cap.0)
            .update(&a1.0)
            .update(&a2.0);
        m.finalize()
    }
}

/// 128-bit random nonce value.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[must_use]
#[repr(transparent)]
pub struct Nonce(u128);

impl Nonce {
    /// Obtains a new random nonce value from the OS CSPRNG.
    #[inline]
    pub fn new() -> Self {
        use rand_core::{OsRng, RngCore};
        let mut b = [0; mem::size_of::<u128>()];
        OsRng.fill_bytes(b.as_mut_slice());
        Self(u128::from_ne_bytes(b))
    }

    /// Generates LE Secure Connections confirm value
    /// ([Vol 3] Part H, Section 2.2.6).
    pub fn f4(&self, u: &PublicKeyX, v: &PublicKeyX, z: u8) -> Confirm {
        let mut m = Key::new(self.0).aes_cmac();
        m.update(u.as_be_bytes())
            .update(v.as_be_bytes())
            .update(&[z]);
        Confirm::from(m.finalize().0)
    }

    /// Returns the nonce value in big-endian format.
    #[inline(always)]
    #[must_use]
    pub(crate) const fn to_be_bytes(self) -> [u8; mem::size_of::<Self>()] {
        self.0.to_be_bytes()
    }
}

impl From<u128> for Nonce {
    #[inline(always)]
    fn from(v: u128) -> Self {
        Self(v)
    }
}

impl From<Nonce> for u128 {
    #[inline(always)]
    fn from(v: Nonce) -> Self {
        v.0
    }
}

/// 128-bit message authentication code with constant-time comparison.
#[derive(Clone, Copy, Debug, Eq)]
#[must_use]
#[repr(transparent)]
pub struct Mac(u128);

impl Mac {
    /// Returns the MAC as a little-endian array.
    #[inline(always)]
    #[must_use]
    pub const fn to_le_bytes(self) -> [u8; 16] {
        self.0.to_le_bytes()
    }
}

impl ConstantTimeEq for Mac {
    #[inline(always)]
    fn ct_eq(&self, other: &Self) -> Choice {
        self.0.ct_eq(&other.0)
    }
}

impl PartialEq for Mac {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        bool::from(self.ct_eq(other))
    }
}

impl From<Mac> for u128 {
    #[inline(always)]
    fn from(v: Mac) -> Self {
        v.0
    }
}

/// RFC-4493 AES-CMAC ([Vol 3] Part H, Section 2.2.5).
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct AesCmac(Cmac<Aes128>);

impl AesCmac {
    /// Returns AES-CMAC state for GAP database hash calculation
    /// ([Vol 3] Part G, Section 7.3.1).
    #[inline(always)]
    #[must_use]
    pub fn db_hash() -> Self {
        Key::new(0).aes_cmac()
    }

    /// Updates CMAC state.
    #[inline(always)]
    pub fn update(&mut self, b: &[u8]) -> &mut Self {
        digest::Update::update(&mut self.0, b);
        self
    }

    /// Computes the final MAC value.
    #[inline(always)]
    pub fn finalize(self) -> Mac {
        Mac(u128::from_be_bytes(
            *digest::FixedOutput::finalize_fixed(self.0).as_ref(),
        ))
    }

    /// Computes the final MAC value and resets the state.
    #[inline(always)]
    pub(crate) fn finalize_reset(&mut self) -> Mac {
        Mac(u128::from_be_bytes(
            *digest::FixedOutputReset::finalize_fixed_reset(&mut self.0).as_ref(),
        ))
    }
}

#[allow(clippy::unusual_byte_groupings)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sizes() {
        assert_eq!(mem::size_of::<Key>(), 16);
    }

    #[test]
    fn nonce() {
        // No fair dice rolls for us!
        assert_ne!(Nonce::new().0, Nonce::new().0);
    }

    /// [Vol 3] Part H, Section D.1.
    #[test]
    fn aes_cmac_d1() {
        const fn b(v: u128) -> [u8; 16] {
            v.to_be_bytes()
        }
        let mut m = Key::new(0x2b7e1516_28aed2a6_abf71588_09cf4f3c).aes_cmac();
        assert_eq!(m.finalize_reset().0, 0xbb1d6929_e9593728_7fa37d12_9b756746);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a).as_slice());
        assert_eq!(m.finalize_reset().0, 0x070a16b4_6b4d4144_f79bdd9d_d04a287c);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a).as_slice());
        m.update(b(0xae2d8a57_1e03ac9c_9eb76fac_45af8e51).as_slice());
        m.update(0x30c81c46_a35ce411_u64.to_be_bytes().as_slice());
        assert_eq!(m.finalize_reset().0, 0xdfa66747_de9ae630_30ca3261_1497c827);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a).as_slice());
        m.update(b(0xae2d8a57_1e03ac9c_9eb76fac_45af8e51).as_slice());
        m.update(b(0x30c81c46_a35ce411_e5fbc119_1a0a52ef).as_slice());
        m.update(b(0xf69f2445_df4f9b17_ad2b417b_e66c3710).as_slice());
        assert_eq!(m.finalize_reset().0, 0x51f0bebf_7e3b9d92_fc497417_79363cfe);
    }

    /// [Vol 3] Part H, Section D.2.
    #[test]
    fn f4_d2() {
        fn u256(hi: u128, lo: u128) -> [u8; 32] {
            let mut v = [0; 32];
            v[..16].copy_from_slice(hi.to_be_bytes().as_slice());
            v[16..].copy_from_slice(lo.to_be_bytes().as_slice());
            v
        }
        let u = PublicKeyX::from_be_bytes(u256(
            0x20b003d2_f297be2c_5e2c83a7_e9f9a5b9,
            0xeff49111_acf4fddb_cc030148_0e359de6,
        ));
        let v = PublicKeyX::from_be_bytes(u256(
            0x55188b3d_32f6bb9a_900afcfb_eed4e72a,
            0x59cb9ac2_f19d7cfb_6b4fdd49_f47fc5fd,
        ));
        let z = 0x00;
        let m = Nonce(0xd5cb8454_d177733e_ffffb2ec_712baeab).f4(&u, &v, z);
        assert_eq!(m.0, 0xf2c916f1_07a9bd1c_f1eda1be_a974872d);
    }

    /// [Vol 3] Part H, Section D.4.
    #[test]
    fn f6_d4() {
        let k = MacKey(Key::new(0x2965f176_a1084a02_fd3f6a20_ce636e20));
        let n1 = Nonce(0xd5cb8454_d177733e_ffffb2ec_712baeab);
        let n2 = Nonce(0xa6e8e7cc_25a75f6e_216583f7_ff3dc4cf);
        let r = 0x12a3343b_b453bb54_08da42d2_0c2d0fc8;
        let io_cap = IoCap([0x01, 0x01, 0x02]);
        let a1 = Addr([0x00, 0x56, 0x12, 0x37, 0x37, 0xbf, 0xce]);
        let a2 = Addr([0x00, 0xa7, 0x13, 0x70, 0x2d, 0xcf, 0xc1]);
        let m = k.f6(n1, n2, r, io_cap, a1, a2);
        assert_eq!(m.0, 0xe3c47398_9cd0e8c5_d26c0b09_da958f61);
    }
}
