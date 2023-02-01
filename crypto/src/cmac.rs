use std::fmt::Debug;

use cmac::digest;
use zeroize::{Zeroize, ZeroizeOnDrop};

/// Provides a [`Debug`] implementation for a type containing sensitive data.
macro_rules! debug_secret {
    ($T:ty) => {
        impl ::std::fmt::Debug for $T {
            #[inline]
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_tuple(stringify!($T)).field(&"<secret>").finish()
            }
        }
    };
}
pub(super) use debug_secret;

/// RFC-4493 AES-CMAC ([Vol 3] Part H, Section 2.2.5).
#[derive(Debug)]
#[repr(transparent)]
pub struct AesCmac(cmac::Cmac<aes::Aes128>);

impl AesCmac {
    /// Creates new AES-CMAC state using key `k`.
    #[inline(always)]
    #[must_use]
    pub(super) fn new(k: &Key) -> Self {
        Self(digest::KeyInit::new(&k.0))
    }

    /// Creates new AES-CMAC state using an all-zero key for GAP database hash
    /// calculation ([Vol 3] Part G, Section 7.3.1).
    #[inline(always)]
    #[must_use]
    pub fn db_hash() -> Self {
        Self::new(&Key::new(0))
    }

    /// Updates CMAC state.
    #[inline(always)]
    pub fn update(&mut self, b: impl AsRef<[u8]>) -> &mut Self {
        digest::Update::update(&mut self.0, b.as_ref());
        self
    }

    /// Computes the final MAC value.
    #[inline(always)]
    #[must_use]
    pub fn finalize(self) -> u128 {
        u128::from_be_bytes(*digest::FixedOutput::finalize_fixed(self.0).as_ref())
    }

    /// Computes the final MAC value for use as a future key and resets the
    /// state.
    #[inline(always)]
    pub(super) fn finalize_key(&mut self) -> Key {
        // Best effort to avoid leaving copies
        let mut k = Key::new(0);
        digest::FixedOutputReset::finalize_into_reset(&mut self.0, &mut k.0);
        k
    }
}

/// 128-bit AES-CMAC key ([Vol 3] Part H, Section 2.2.5).
#[allow(clippy::redundant_pub_crate)]
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub(super) struct Key(aes::cipher::Key<aes::Aes128>);

debug_secret!(Key);

impl Key {
    /// Creates a key from a `u128` value.
    #[inline(always)]
    pub fn new(k: u128) -> Self {
        Self(k.to_be_bytes().into())
    }
}

impl From<&Key> for u128 {
    #[inline(always)]
    fn from(k: &Key) -> Self {
        Self::from_be_bytes(k.0.into())
    }
}

#[allow(clippy::unusual_byte_groupings)]
#[cfg(test)]
mod tests {
    use super::*;

    /// AES-CMAC RFC-4493 test vectors ([Vol 3] Part H, Section D.1).
    #[test]
    fn aes_cmac() {
        const fn b(v: u128) -> [u8; 16] {
            v.to_be_bytes()
        }
        fn eq(m: &mut AesCmac, v: u128) {
            assert_eq!(u128::from(&m.finalize_key()), v);
        }
        let mut m = AesCmac::new(&Key::new(0x2b7e1516_28aed2a6_abf71588_09cf4f3c));
        eq(&mut m, 0xbb1d6929_e9593728_7fa37d12_9b756746);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a));
        eq(&mut m, 0x070a16b4_6b4d4144_f79bdd9d_d04a287c);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a));
        m.update(b(0xae2d8a57_1e03ac9c_9eb76fac_45af8e51));
        m.update(0x30c81c46_a35ce411_u64.to_be_bytes());
        eq(&mut m, 0xdfa66747_de9ae630_30ca3261_1497c827);

        m.update(b(0x6bc1bee2_2e409f96_e93d7e11_7393172a));
        m.update(b(0xae2d8a57_1e03ac9c_9eb76fac_45af8e51));
        m.update(b(0x30c81c46_a35ce411_e5fbc119_1a0a52ef));
        m.update(b(0xf69f2445_df4f9b17_ad2b417b_e66c3710));
        assert_eq!(m.finalize(), 0x51f0bebf_7e3b9d92_fc497417_79363cfe);
    }
}
