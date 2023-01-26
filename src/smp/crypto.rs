use aes::{cipher, Aes128};
use cmac::{digest, Cmac};
use subtle::{Choice, ConstantTimeEq};

/// 128-bit secret key.
#[derive(Clone, Default, zeroize::Zeroize, zeroize::ZeroizeOnDrop)]
#[repr(transparent)]
pub(crate) struct Key(cipher::Key<Aes128>);

impl Key {
    /// Creates new AES-CMAC state.
    #[inline(always)]
    pub fn cmac(&self) -> AesCmac {
        AesCmac(digest::KeyInit::new(&self.0))
    }

    /// Generates LE Secure Connections confirm value
    /// ([Vol 3] Part H, Section 2.2.6).
    #[inline]
    pub(super) fn f4(&self, u: impl AsRef<[u8; 32]>, v: impl AsRef<[u8; 32]>, z: u8) -> Mac {
        let mut m = self.cmac();
        m.update(u.as_ref().as_slice())
            .update(v.as_ref().as_slice())
            .update(&[z]);
        m.finalize()
    }
}

/// 128-bit message authentication code.
#[derive(Clone, Copy, Debug, Eq)]
#[repr(transparent)]
pub(crate) struct Mac(u128);

impl Mac {
    /// Returns the MAC as a little-endian array.
    #[inline(always)]
    pub const fn to_le_bytes(self) -> [u8; 16] {
        self.0.to_le_bytes()
    }
}

impl From<u128> for Mac {
    #[inline(always)]
    fn from(v: u128) -> Self {
        Self(v)
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

/// 128-bit random nonce value.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(transparent)]
pub(crate) struct Nonce(u128);

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

/// RFC-4493 AES-CMAC ([Vol 3] Part H, Section 2.2.5).
#[derive(Clone, Debug)]
#[repr(transparent)]
pub(crate) struct AesCmac(Cmac<Aes128>);

impl AesCmac {
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
}
