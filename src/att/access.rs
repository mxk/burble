#![allow(dead_code)] // TODO: Remove

use super::*;

type Result<T> = std::result::Result<T, ErrorCode>;

/// Access permission builder.
#[derive(Clone, Copy, Debug)]
#[must_use]
#[repr(transparent)]
pub(super) struct Access(Perm);

impl Access {
    /// Creates read access permissions.
    #[inline]
    const fn read() -> Self {
        Self(Perm::READ)
    }

    /// Creates write access permissions.
    #[inline]
    const fn write() -> Self {
        Self(Perm::WRITE)
    }

    /// Creates read/write access permissions.
    #[inline]
    const fn read_write() -> Self {
        Self(Perm::READ_WRITE)
    }

    /// Sets authentication flag.
    #[inline]
    const fn authn(self) -> Self {
        Self(self.0.union(Perm::AUTHN))
    }

    /// Sets authorization flag.
    #[inline]
    const fn authz(self) -> Self {
        Self(self.0.union(Perm::AUTHZ))
    }

    /// Sets encryption key length between 56 and 128. Use 0 to clear encryption
    /// bits.
    #[inline]
    const fn encrypt(self, key_len: u8) -> Self {
        assert!(
            key_len == 0 || 56 <= key_len && key_len <= 128 && key_len % 8 == 0,
            "Invalid encryption key length"
        );
        let n = key_len.saturating_sub(Perm::KEY_LEN_BASE) & Perm::KEY_LEN.bits;
        // SAFETY: All bits are valid
        Self(unsafe { Perm::from_bits_unchecked(self.0.difference(Perm::KEY_LEN).bits | n) })
    }

    /// Creates access permission requirements.
    #[inline]
    const fn rule(self) -> AccessRule {
        AccessRule(self.0)
    }

    /// Creates access permission request.
    #[inline]
    const fn request(self) -> AccessReq {
        AccessReq(self.0)
    }
}

/// Access permission requirements.
#[derive(Clone, Copy, Debug, Default)]
#[must_use]
#[repr(transparent)]
pub(crate) struct AccessRule(Perm);

impl AccessRule {
    /// Tests whether the access request meets security requirements.
    const fn test(self, req: AccessReq) -> Result<()> {
        use ErrorCode::*;
        // For read/write, the rule must be a superset of the request
        let rw = req.0.access();
        let fail = rw.intersection(self.0.access().symmetric_difference(rw));
        if !fail.is_empty() || rw.is_empty() {
            return Err(match fail {
                Perm::READ => ReadNotPermitted,
                Perm::WRITE => WriteNotPermitted,
                _ => RequestNotSupported,
            });
        }
        // For security, the request must be a superset of the rule
        let sec = self.0.security();
        let fail = sec.intersection(req.0.security().symmetric_difference(sec));
        if !fail.is_empty() {
            // Order matches ATT_READ_REQ ([Vol 3] Part F, Section 3.4.4.3)
            Err(if fail.contains(Perm::AUTHZ) {
                InsufficientAuthorization
            } else if fail.contains(Perm::AUTHN) {
                InsufficientAuthentication
            } else {
                InsufficientEncryption
            })
        } else if req.0.key_len() < self.0.key_len() {
            Err(EncryptionKeySizeTooShort)
        } else {
            Ok(())
        }
    }
}

/// Access permission request.
#[derive(Clone, Copy, Debug)]
#[must_use]
#[repr(transparent)]
pub(crate) struct AccessReq(Perm);

/// A set of access permission requirements.
#[derive(Clone, Copy, Debug, Default)]
#[must_use]
#[repr(transparent)]
pub(crate) struct AccessRules([AccessRule; 4]);

impl AccessRules {
    /// Creates a new rule set.
    #[inline]
    pub const fn new(r: AccessRule) -> Self {
        let mut rs = Self([AccessRule(Perm::empty()); 4]);
        rs.0[r.0.index()] = r;
        rs
    }

    /// Adds or modifies an access rule. One rule is allowed for each
    /// combination of read/write bits.
    #[inline]
    pub fn set(&mut self, r: AccessRule) {
        self.0[r.0.index()] = r;
    }

    /// Tests whether the access request meets security requirements.
    pub const fn test(self, req: AccessReq) -> Result<()> {
        const RW: usize = Perm::READ_WRITE.index();
        let op = req.0.index();
        let r = self.0[op].test(req);
        if r.is_ok() || op == RW || self.0[RW].0.access().is_empty() {
            return r;
        }
        self.0[RW].test(req)
    }
}

bitflags::bitflags! {
    /// Attribute permissions ([Vol 3] Part F, Section 3.2.5).
    #[derive(Default)]
    #[must_use]
    #[repr(transparent)]
    struct Perm: u8 {
        /// Read access.
        const READ = 1 << 0;
        /// Write access.
        const WRITE = 1 << 1;
        /// Read/write access.
        const READ_WRITE = Self::READ.bits | Self::WRITE.bits;

        /// Authentication.
        const AUTHN = 1 << 2;

        /// Encryption. The flag is only returned by security() for on/off
        /// testing. The actual permissions store a 4-bit key length
        /// pre-multiplied by 8 ([Vol 3] Part H, Section 2.3.4).
        const ENCRYPT = 1 << 3;
        /// Encryption key length.
        const KEY_LEN = 0xF << 3;

        /// Authorization.
        const AUTHZ = 1 << 7;
    }
}

impl Perm {
    const KEY_LEN_BASE: u8 = 48;

    /// Returns the index for the `AccessRules` array.
    #[inline]
    #[must_use]
    const fn index(self) -> usize {
        self.access().bits as usize
    }

    /// Returns the read/write access bits.
    #[inline]
    const fn access(self) -> Self {
        self.intersection(Self::READ_WRITE)
    }

    /// Returns the authentication, authorization, and encryption bits.
    #[inline]
    const fn security(self) -> Self {
        self.difference(Self::READ_WRITE.union(Self::KEY_LEN))
            .union(if self.intersects(Self::KEY_LEN) {
                Self::ENCRYPT
            } else {
                Self::empty()
            })
    }

    /// Returns the key length in bits or 0 if encryption is not specified.
    #[inline]
    #[must_use]
    const fn key_len(self) -> u8 {
        match self.intersection(Self::KEY_LEN).bits {
            0 => 0,
            n => n + Self::KEY_LEN_BASE,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn access() {
        use ErrorCode::*;
        fn test(rule: Access, req: Access, want: Result<()>) {
            assert_eq!(rule.rule().test(req.request()), want);
        }
        let (ro, wo, rw) = (Access::read(), Access::write(), Access::read_write());

        test(ro, ro, Ok(()));
        test(ro, wo, Err(WriteNotPermitted));
        test(ro, rw, Err(WriteNotPermitted));
        test(wo, ro, Err(ReadNotPermitted));
        test(wo, wo, Ok(()));
        test(wo, rw, Err(ReadNotPermitted));
        test(rw, ro, Ok(()));
        test(rw, wo, Ok(()));
        test(rw, rw, Ok(()));

        test(ro.authn(), ro.authn().authz(), Ok(()));
        test(wo.authn(), wo.authz(), Err(InsufficientAuthentication));
        test(
            rw.authn().authz(),
            ro.authn(),
            Err(InsufficientAuthorization),
        );
        test(
            rw.authn().encrypt(128),
            wo.authn().authz(),
            Err(InsufficientEncryption),
        );

        test(ro.encrypt(0), ro.encrypt(0), Ok(()));
        test(ro.encrypt(56), ro.authn().encrypt(128), Ok(()));
        test(
            rw.encrypt(80),
            rw.authn().authz().encrypt(56),
            Err(EncryptionKeySizeTooShort),
        );
    }
}
