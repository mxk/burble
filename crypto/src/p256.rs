use std::fmt::Debug;
use std::mem;

use p256::ecdh;
use structbuf::{Packer, Unpacker};
use zeroize::{Zeroize, ZeroizeOnDrop};

use crate::{debug_secret, Addr, AesCmac, Codec, Key, MacKey, Nonce};

/// P-256 elliptic curve secret key.
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct SecretKey(p256::NonZeroScalar);

debug_secret!(SecretKey);

impl SecretKey {
    /// Generates a new random secret key.
    #[allow(clippy::new_without_default)]
    #[inline(always)]
    pub fn new() -> Self {
        Self(p256::NonZeroScalar::random(&mut rand_core::OsRng))
    }

    /// Computes the associated public key.
    pub fn public_key(&self) -> PublicKey {
        use p256::elliptic_curve::sec1::{Coordinates::Uncompressed, ToEncodedPoint};
        let p = p256::PublicKey::from_secret_scalar(&self.0).to_encoded_point(false);
        match p.coordinates() {
            Uncompressed { x, y } => PublicKey {
                x: PublicKeyX(Coord(*x.as_ref())),
                y: Coord(*y.as_ref()),
            },
            _ => unreachable!("invalid secret key"),
        }
    }

    /// Computes a shared secret from the local secret key and remote public
    /// key. Returns [`None`] if the public key is either invalid or derived
    /// from the same secret key ([Vol 3] Part H, Section 2.3.5.6.1).
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn dh_key(&self, pk: PublicKey) -> Option<DHKey> {
        use p256::elliptic_curve::sec1::FromEncodedPoint;
        if pk.is_debug() {
            return None; // TODO: Compile-time option for debug-only mode
        }
        let (x, y) = (&pk.x.0 .0.into(), &pk.y.0.into());
        let rep = p256::EncodedPoint::from_affine_coordinates(x, y, false);
        let lpk = p256::PublicKey::from_secret_scalar(&self.0);
        // Constant-time ops not required:
        // https://github.com/RustCrypto/traits/issues/1227
        let rpk = Option::from(p256::PublicKey::from_encoded_point(&rep)).unwrap_or(lpk);
        (rpk != lpk).then(|| DHKey(ecdh::diffie_hellman(&self.0, rpk.as_affine())))
    }
}

/// P-256 elliptic curve public key ([Vol 3] Part H, Section 3.5.6).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[must_use]
pub struct PublicKey {
    x: PublicKeyX,
    y: Coord,
}

impl PublicKey {
    /// Returns the public key X coordinate.
    #[inline(always)]
    pub const fn x(&self) -> &PublicKeyX {
        &self.x
    }

    /// Returns whether `self` is the debug public key
    /// ([Vol 3] Part H, Section 2.3.5.6.1).
    #[allow(clippy::unusual_byte_groupings)]
    fn is_debug(&self) -> bool {
        let (x, y) = (&self.x.0 .0, &self.y.0);
        x[..16] == u128::to_be_bytes(0x20b003d2_f297be2c_5e2c83a7_e9f9a5b9)
            && x[16..] == u128::to_be_bytes(0xeff49111_acf4fddb_cc030148_0e359de6)
            && y[..16] == u128::to_be_bytes(0xdc809c49_652aeb6d_63329abf_5a52155c)
            && y[16..] == u128::to_be_bytes(0x766345c2_8fed3024_741c8ed0_1589d28b)
    }
}

impl Codec for PublicKey {
    #[inline]
    fn pack(&self, p: &mut Packer) {
        let (mut x, mut y) = (self.x.0 .0, self.y.0);
        x.reverse();
        y.reverse();
        p.put(x).put(y);
    }

    #[inline]
    fn unpack(p: &mut Unpacker) -> Option<Self> {
        let (mut x, mut y) = (PublicKeyX(Coord(p.bytes())), Coord(p.bytes()));
        x.0 .0.reverse();
        y.0.reverse();
        Some(Self { x, y })
    }
}

/// 256-bit elliptic curve coordinate in big-endian byte order.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
struct Coord([u8; 256 / u8::BITS as usize]);

/// P-256 elliptic curve public key affine X coordinate.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[must_use]
#[repr(transparent)]
pub struct PublicKeyX(Coord);

impl PublicKeyX {
    /// Creates the coordinate from a big-endian encoded byte array.
    #[cfg(test)]
    #[inline]
    pub(super) const fn from_be_bytes(x: [u8; mem::size_of::<Self>()]) -> Self {
        Self(Coord(x))
    }

    /// Returns the coordinate in big-endian byte order.
    #[inline(always)]
    pub(super) const fn as_be_bytes(&self) -> &[u8; mem::size_of::<Self>()] {
        &self.0 .0
    }
}

/// P-256 elliptic curve shared secret ([Vol 3] Part H, Section 2.3.5.6.1).
#[derive(ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct DHKey(ecdh::SharedSecret);

debug_secret!(DHKey);

impl DHKey {
    /// Generates LE Secure Connections `MacKey` and `LTK`
    /// ([Vol 3] Part H, Section 2.2.7).
    #[inline]
    pub fn f5(&self, n1: Nonce, n2: Nonce, a1: Addr, a2: Addr) -> (MacKey, LTK) {
        let n1 = n1.0.to_be_bytes();
        let n2 = n2.0.to_be_bytes();
        let half = |m: &mut AesCmac, counter: u8| {
            m.update([counter])
                .update(b"btle")
                .update(n1)
                .update(n2)
                .update(a1.0)
                .update(a2.0)
                .update(256_u16.to_be_bytes())
                .finalize_key()
        };
        let mut m = AesCmac::new(&Key::new(0x6C88_8391_AAF5_A538_6037_0BDB_5A60_83BE));
        m.update(self.0.raw_secret_bytes());
        let mut m = AesCmac::new(&m.finalize_key());
        (MacKey(half(&mut m, 0)), LTK(half(&mut m, 1)))
    }
}

/// LE Secure Connections long-term key.
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct LTK(Key);

debug_secret!(LTK);

/// Combines `hi` and `lo` values into a big-endian byte array.
#[allow(clippy::redundant_pub_crate)]
#[cfg(test)]
pub(super) fn u256<T: From<[u8; 32]>>(hi: u128, lo: u128) -> T {
    let mut b = [0; 32];
    b[..16].copy_from_slice(&hi.to_be_bytes());
    b[16..].copy_from_slice(&lo.to_be_bytes());
    T::from(b)
}

#[allow(clippy::similar_names)]
#[allow(clippy::unusual_byte_groupings)]
#[cfg(test)]
mod tests {
    use structbuf::{Pack, StructBuf, Unpack};

    use super::*;

    #[test]
    fn sizes() {
        assert_eq!(mem::size_of::<Coord>(), 32);
        assert_eq!(mem::size_of::<PublicKey>(), 64);
        assert_eq!(mem::size_of::<SecretKey>(), 32);
        assert_eq!(mem::size_of::<DHKey>(), 32);
    }

    /// Debug mode key ([Vol 3] Part H, Section 2.3.5.6.1).
    #[test]
    fn debug_key() {
        let sk = secret_key(
            0x3f49f6d4_a3c55f38_74c9b3e3_d2103f50,
            0x4aff607b_eb40b799_5899b8a6_cd3c1abd,
        );
        let pk = PublicKey {
            x: PublicKeyX(Coord(u256(
                0x20b003d2_f297be2c_5e2c83a7_e9f9a5b9,
                0xeff49111_acf4fddb_cc030148_0e359de6,
            ))),
            y: Coord(u256(
                0xdc809c49_652aeb6d_63329abf_5a52155c,
                0x766345c2_8fed3024_741c8ed0_1589d28b,
            )),
        };
        assert_eq!(sk.public_key(), pk);
        assert!(pk.is_debug());

        let mut b = StructBuf::with_capacity(mem::size_of_val(&pk));
        pk.pack(&mut b.append());
        assert_eq!(b.unpack().u8(), 0xe6);
        assert_eq!(PublicKey::unpack(&mut b.unpack()).unwrap(), pk);
    }

    /// P-256 data set 1 ([Vol 2] Part G, Section 7.1.2.1).
    #[test]
    fn p256_1() {
        let (ska, skb) = (
            secret_key(
                0x3f49f6d4_a3c55f38_74c9b3e3_d2103f50,
                0x4aff607b_eb40b799_5899b8a6_cd3c1abd,
            ),
            secret_key(
                0x55188b3d_32f6bb9a_900afcfb_eed4e72a,
                0x59cb9ac2_f19d7cfb_6b4fdd49_f47fc5fd,
            ),
        );
        let (pka, pkb) = (
            PublicKey {
                x: PublicKeyX(Coord(u256(
                    0x20b003d2_f297be2c_5e2c83a7_e9f9a5b9,
                    0xeff49111_acf4fddb_cc030148_0e359de6,
                ))),
                y: Coord(u256(
                    0xdc809c49_652aeb6d_63329abf_5a52155c,
                    0x766345c2_8fed3024_741c8ed0_1589d28b,
                )),
            },
            PublicKey {
                x: PublicKeyX(Coord(u256(
                    0x1ea1f0f0_1faf1d96_09592284_f19e4c00,
                    0x47b58afd_8615a69f_559077b2_2faaa190,
                ))),
                y: Coord(u256(
                    0x4c55f33e_429dad37_7356703a_9ab85160,
                    0x472d1130_e28e3676_5f89aff9_15b1214a,
                )),
            },
        );
        let dh_key = shared_secret(
            0xec0234a3_57c8ad05_341010a6_0a397d9b,
            0x99796b13_b4f866f1_868d34f3_73bfa698,
        );
        assert_eq!(ska.public_key(), pka);
        assert_eq!(skb.public_key(), pkb);
        assert_eq!(
            ska.dh_key(pkb).unwrap().0.raw_secret_bytes(),
            dh_key.0.raw_secret_bytes()
        );

        assert!(!pkb.is_debug());
        assert!(skb.dh_key(pkb).is_none());
    }

    /// P-256 data set 2 ([Vol 2] Part G, Section 7.1.2.2).
    #[test]
    fn p256_2() {
        let (ska, skb) = (
            secret_key(
                0x06a51669_3c9aa31a_6084545d_0c5db641,
                0xb48572b9_7203ddff_b7ac73f7_d0457663,
            ),
            secret_key(
                0x529aa067_0d72cd64_97502ed4_73502b03,
                0x7e8803b5_c60829a5_a3caa219_505530ba,
            ),
        );
        let (pka, pkb) = (
            PublicKey {
                x: PublicKeyX(Coord(u256(
                    0x2c31a47b_5779809e_f44cb5ea_af5c3e43,
                    0xd5f8faad_4a8794cb_987e9b03_745c78dd,
                ))),
                y: Coord(u256(
                    0x91951218_3898dfbe_cd52e240_8e43871f,
                    0xd0211091_17bd3ed4_eaf84377_43715d4f,
                )),
            },
            PublicKey {
                x: PublicKeyX(Coord(u256(
                    0xf465e43f_f23d3f1b_9dc7dfc0_4da87581,
                    0x84dbc966_204796ec_cf0d6cf5_e16500cc,
                ))),
                y: Coord(u256(
                    0x0201d048_bcbbd899_eeefc424_164e33c2,
                    0x01c2b010_ca6b4d43_a8a155ca_d8ecb279,
                )),
            },
        );
        let dh_key = shared_secret(
            0xab85843a_2f6d883f_62e5684b_38e30733,
            0x5fe6e194_5ecd1960_4105c6f2_3221eb69,
        );
        assert_eq!(ska.public_key(), pka);
        assert_eq!(skb.public_key(), pkb);
        assert_eq!(
            ska.dh_key(pkb).unwrap().0.raw_secret_bytes(),
            dh_key.0.raw_secret_bytes()
        );
    }

    /// Key generation function ([Vol 3] Part H, Section D.3).
    #[test]
    fn dh_key_f5_d3() {
        let w = shared_secret(
            0xec0234a3_57c8ad05_341010a6_0a397d9b,
            0x99796b13_b4f866f1_868d34f3_73bfa698,
        );
        let n1 = Nonce(0xd5cb8454_d177733e_ffffb2ec_712baeab);
        let n2 = Nonce(0xa6e8e7cc_25a75f6e_216583f7_ff3dc4cf);
        let a1 = Addr([0x00, 0x56, 0x12, 0x37, 0x37, 0xbf, 0xce]);
        let a2 = Addr([0x00, 0xa7, 0x13, 0x70, 0x2d, 0xcf, 0xc1]);
        let (mk, ltk) = w.f5(n1, n2, a1, a2);
        assert_eq!(ltk.0.to_u128(), 0x69867911_69d7cd23_980522b5_94750a38);
        assert_eq!(mk.0.to_u128(), 0x2965f176_a1084a02_fd3f6a20_ce636e20);
    }

    #[inline]
    fn secret_key(hi: u128, lo: u128) -> SecretKey {
        SecretKey(p256::NonZeroScalar::from_repr(u256(hi, lo)).unwrap())
    }

    #[inline]
    fn shared_secret(hi: u128, lo: u128) -> DHKey {
        DHKey(ecdh::SharedSecret::from(u256::<p256::FieldBytes>(hi, lo)))
    }
}
