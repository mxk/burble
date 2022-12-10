#![allow(dead_code)] // TODO: Remove

use super::*;

type Result<T> = std::result::Result<T, ErrorCode>;

/// Access permission/request builder.
#[derive(Clone, Copy, Debug)]
#[must_use]
#[repr(transparent)]
pub(crate) struct Access(Perm);

impl Access {
    /// Creates read access permission/request.
    #[inline]
    pub const fn read() -> Self {
        Self(Perm::READ)
    }

    /// Creates write access permission/request.
    #[inline]
    pub const fn write() -> Self {
        Self(Perm::WRITE)
    }

    /// Creates read/write access permission/request.
    #[inline]
    pub const fn read_write() -> Self {
        Self(Perm::READ_WRITE)
    }

    /// Sets the authentication flag.
    #[inline]
    pub const fn authn(self) -> Self {
        Self(self.0.union(Perm::AUTHN))
    }

    /// Sets the authorization flag.
    #[inline]
    pub const fn authz(self) -> Self {
        Self(self.0.union(Perm::AUTHZ))
    }

    /// Sets encryption key length between 56 and 128 bits in 8 bit increments.
    /// A key length of 0 clears encryption requirement/status.
    #[inline]
    pub const fn key_len(self, n: u8) -> Self {
        assert!(
            Perm::KEY_MIN <= n && n <= Perm::KEY_MAX && n % 8 == 0 || n == 0,
            "Invalid encryption key length"
        );
        let n = n.saturating_sub(Perm::KEY_OFF) & Perm::KEY_LEN.bits;
        // SAFETY: All bits are valid
        Self(unsafe { Perm::from_bits_unchecked(self.0.difference(Perm::KEY_LEN).bits | n) })
    }
}

/// A set of attribute permissions. Contains separate permissions for read-only,
/// write-only, and read/write access.
#[derive(Clone, Copy, Debug, Default)]
#[must_use]
#[repr(transparent)]
pub(crate) struct Perms([Perm; 4]);

impl Perms {
    /// Creates a new permission set.
    #[inline]
    pub const fn new(allow: Access) -> Self {
        let mut ps = Self([Perm::empty(); 4]);
        ps.0[allow.0.index()] = allow.0;
        ps
    }

    /// Allows read-only, write-only, or read/write access.
    #[inline]
    pub fn allow(&mut self, p: Access) {
        self.0[p.0.index()] = p.0;
    }

    /// Tests whether an access request should be allowed.
    pub const fn test(self, req: Access) -> Result<()> {
        const RW: usize = Perm::READ_WRITE.index();
        let mut op = req.0.index();
        if !self.0[op].is_set() {
            op = RW;
        }
        // Read-only and write-only requests first check for an exact match and
        // then fall back to read/write permissions.
        let exact = self.0[op].test(req.0);
        if exact.is_ok() || op == RW {
            return exact;
        }
        let rw = self.0[RW].test(req.0);
        match rw {
            // EncryptionKeySizeTooShort takes precedence as it's more useful
            Ok(_) | Err(ErrorCode::EncryptionKeySizeTooShort) => rw,
            _ => exact,
        }
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

        /// Authentication flag.
        const AUTHN = 1 << 2;

        /// Encryption flag. The flag is only returned by `security()` for
        /// on/off testing. The actual value stores a 4-bit key length
        /// pre-multiplied by 8 ([Vol 3] Part H, Section 2.3.4).
        const ENCRYPT = 1 << 3;
        /// Encryption key length mask.
        const KEY_LEN = 0xF << 3;

        /// Authorization flag.
        const AUTHZ = 1 << 7;
    }
}

impl Perm {
    /// Minimum and maximum key length in bits, and offset for the stored value
    /// ([Vol 3] Part H, Section 2.3.4).
    const KEY_MIN: u8 = 56;
    const KEY_MAX: u8 = 128;
    const KEY_OFF: u8 = Self::KEY_MIN - 8;

    /// Returns the permission array index.
    #[inline]
    #[must_use]
    const fn index(self) -> usize {
        self.access().bits as usize
    }

    /// Returns the read/write access type.
    #[inline]
    #[must_use]
    const fn is_set(self) -> bool {
        self.intersects(Self::READ_WRITE)
    }

    /// Returns the read/write access type.
    #[inline]
    const fn access(self) -> Self {
        self.intersection(Self::READ_WRITE)
    }

    /// Returns the authentication, authorization, and encryption flags.
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
            n => n + Self::KEY_OFF,
        }
    }

    /// Tests whether the access request should be allowed.
    const fn test(self, req: Self) -> Result<()> {
        use ErrorCode::*;
        // Read/write access must be a superset of the request
        let want = req.access();
        let fail = want.intersection(self.access().symmetric_difference(want));
        if !fail.is_empty() || want.is_empty() {
            return Err(match fail {
                Self::READ => ReadNotPermitted,
                Self::WRITE => WriteNotPermitted,
                _ => RequestNotSupported,
            });
        }
        // Security requirements must be a subset of the request
        let need = self.security();
        let fail = need.intersection(req.security().symmetric_difference(need));
        if !fail.is_empty() {
            // Order matches ATT_READ_REQ ([Vol 3] Part F, Section 3.4.4.3)
            Err(if fail.contains(Self::AUTHZ) {
                InsufficientAuthorization
            } else if fail.contains(Self::AUTHN) {
                InsufficientAuthentication
            } else {
                InsufficientEncryption
            })
        } else if req.key_len() < self.key_len() {
            Err(EncryptionKeySizeTooShort)
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn access() {
        use ErrorCode::*;
        fn test(perm: Access, req: Access, want: Result<()>) {
            assert_eq!(perm.0.test(req.0), want);
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
            rw.authn().key_len(128),
            wo.authn().authz(),
            Err(InsufficientEncryption),
        );

        test(ro.key_len(0), ro.key_len(0), Ok(()));
        test(ro.key_len(56), ro.authn().key_len(128), Ok(()));
        test(
            rw.key_len(80),
            rw.authn().authz().key_len(56),
            Err(EncryptionKeySizeTooShort),
        );
    }

    #[test]
    fn perms() {
        use ErrorCode::*;
        fn test(ps: Perms, req: Access, want: Result<()>) {
            assert_eq!(ps.test(req), want);
        }
        let (ro, wo, rw) = (Access::read(), Access::write(), Access::read_write());

        let mut ps = Perms::new(Access::read_write().authz().key_len(128));
        ps.allow(Access::read().authn());

        test(ps, Access(Perm::default()), Err(RequestNotSupported));

        test(ps, ro, Err(InsufficientAuthentication));
        test(ps, ro.authz(), Err(InsufficientAuthentication));
        test(ps, ro.authz().key_len(120), Err(EncryptionKeySizeTooShort));
        test(ps, ro.authn(), Ok(()));
        test(ps, ro.authz().key_len(128), Ok(()));

        test(ps, wo, Err(InsufficientAuthorization));
        test(ps, wo.authz(), Err(InsufficientEncryption));
        test(ps, wo.authz().key_len(128), Ok(()));

        test(ps, rw.key_len(128), Err(InsufficientAuthorization));
        test(ps, rw.authz().key_len(128), Ok(()));
    }
}