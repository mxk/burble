use std::fmt::{Debug, Formatter};
use std::mem;

use p256::ecdh;
use structbuf::{Packer, Unpacker};
use zeroize::{Zeroize, ZeroizeOnDrop};

/// P-256 elliptic curve secret key.
#[derive(Zeroize, ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct SecretKey(p256::NonZeroScalar);

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
    /// key. Returns [`None`] if the public key is invalid.
    #[must_use]
    pub fn dh_key(&self, pk: PublicKey) -> Option<DHKey> {
        use p256::elliptic_curve::sec1::FromEncodedPoint;
        let (x, y) = (&pk.x.0 .0.into(), &pk.y.0.into());
        let p = p256::EncodedPoint::from_affine_coordinates(x, y, false);
        Option::<p256::PublicKey>::from(p256::PublicKey::from_encoded_point(&p))
            .map(|pk| DHKey(ecdh::diffie_hellman(&self.0, pk.as_affine())))
    }
}

impl Debug for SecretKey {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SecretKey").field(&"<secret>").finish()
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
    pub(super) const fn as_be_bytes(&self) -> &[u8; 256 / u8::BITS as usize] {
        &self.0 .0
    }
}

/// P-256 elliptic curve public key.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[must_use]
pub struct PublicKey {
    x: PublicKeyX,
    y: Coord,
}

impl PublicKey {
    /// Writes the public key in little-endian format to `p`.
    /// ([Vol 3] Part H, Section 3.5.6).
    #[inline]
    pub fn pack(&self, p: &mut Packer) {
        let (mut x, mut y) = (self.x.0 .0, self.y.0);
        x.reverse();
        y.reverse();
        p.put(x).put(y);
    }

    /// Reads the little-endian encoded public key from `p`.
    /// ([Vol 3] Part H, Section 3.5.6).
    #[inline]
    pub fn unpack(p: &mut Unpacker) -> Option<Self> {
        p.skip(2 * mem::size_of::<Coord>()).map(|mut p| {
            let (mut x, mut y) = (PublicKeyX(Coord(p.bytes())), Coord(p.bytes()));
            x.0 .0.reverse();
            y.0.reverse();
            Self { x, y }
        })
    }

    /// Returns the public key X coordinate.
    #[inline(always)]
    pub const fn x(&self) -> &PublicKeyX {
        &self.x
    }
}

/// P-256 elliptic curve shared secret.
#[derive(ZeroizeOnDrop)]
#[must_use]
#[repr(transparent)]
pub struct DHKey(ecdh::SharedSecret);

impl Debug for DHKey {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DHKey").field(&"<secret>").finish()
    }
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

    #[test]
    fn public_key() {
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
        let mut b = StructBuf::new(64);
        pk.pack(&mut b.append());
        assert_eq!(b.unpack().u8(), 0xe6);
        assert_eq!(PublicKey::unpack(&mut b.unpack()), Some(pk));
    }

    /// [Vol 2] Part G, Section 7.1.2.1
    #[test]
    fn p256_1() {
        let (ska, skb) = (
            SecretKey(
                p256::NonZeroScalar::from_repr(u256(
                    0x3f49f6d4_a3c55f38_74c9b3e3_d2103f50,
                    0x4aff607b_eb40b799_5899b8a6_cd3c1abd,
                ))
                .unwrap(),
            ),
            SecretKey(
                p256::NonZeroScalar::from_repr(u256(
                    0x55188b3d_32f6bb9a_900afcfb_eed4e72a,
                    0x59cb9ac2_f19d7cfb_6b4fdd49_f47fc5fd,
                ))
                .unwrap(),
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
        let dh_key = DHKey(ecdh::SharedSecret::from(u256::<p256::FieldBytes>(
            0xec0234a3_57c8ad05_341010a6_0a397d9b,
            0x99796b13_b4f866f1_868d34f3_73bfa698,
        )));
        assert_eq!(ska.public_key(), pka);
        assert_eq!(skb.public_key(), pkb);
        assert_eq!(
            ska.dh_key(pkb).unwrap().0.raw_secret_bytes(),
            dh_key.0.raw_secret_bytes()
        );
    }

    /// [Vol 2] Part G, Section 7.1.2.2
    #[test]
    fn p256_2() {
        let (ska, skb) = (
            SecretKey(
                p256::NonZeroScalar::from_repr(u256(
                    0x06a51669_3c9aa31a_6084545d_0c5db641,
                    0xb48572b9_7203ddff_b7ac73f7_d0457663,
                ))
                .unwrap(),
            ),
            SecretKey(
                p256::NonZeroScalar::from_repr(u256(
                    0x529aa067_0d72cd64_97502ed4_73502b03,
                    0x7e8803b5_c60829a5_a3caa219_505530ba,
                ))
                .unwrap(),
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
        let dh_key = DHKey(ecdh::SharedSecret::from(u256::<p256::FieldBytes>(
            0xab85843a_2f6d883f_62e5684b_38e30733,
            0x5fe6e194_5ecd1960_4105c6f2_3221eb69,
        )));
        assert_eq!(ska.public_key(), pka);
        assert_eq!(skb.public_key(), pkb);
        assert_eq!(
            ska.dh_key(pkb).unwrap().0.raw_secret_bytes(),
            dh_key.0.raw_secret_bytes()
        );
    }

    fn u256<T: From<[u8; 32]>>(hi: u128, lo: u128) -> T {
        let mut b = [0; 32];
        b[..16].copy_from_slice(&hi.to_be_bytes());
        b[16..].copy_from_slice(&lo.to_be_bytes());
        T::from(b)
    }
}
