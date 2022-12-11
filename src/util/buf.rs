#![allow(dead_code)] // TODO: Remove

use smallvec::SmallVec;

use super::Unpkr;

/// Inline capacity sufficient to avoid allocation for most HCI commands and ACL
/// outbound data transfers, while staying within one cache line.
const INLINE_CAP: usize = 32;

/// A capacity-limited buffer. The buffer starts out with a small internal
/// capacity that does not require allocation. It performs at most one heap
/// allocation up to the capacity limit once the internal capacity is exceeded.
/// The relationship `len <= cap <= lim` always holds. It panics if a write
/// operation exceeds the limit.
#[derive(Clone, Debug)]
#[must_use]
pub(crate) struct LimitedBuf {
    lim: usize,
    v: SmallVec<[u8; INLINE_CAP]>,
}

impl LimitedBuf {
    /// Creates a new capacity-limited buffer without allocating from the heap.
    #[inline]
    pub const fn new(lim: usize) -> Self {
        Self {
            lim,
            v: SmallVec::new_const(),
        }
    }

    /// Creates a pre-allocated capacity-limited buffer. This buffer will never
    /// reallocate.
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            lim: cap,
            v: SmallVec::with_capacity(cap),
        }
    }

    /// Returns the number of bytes in the buffer.
    #[inline]
    pub fn len(&self) -> usize {
        self.v.len()
    }

    /// Returns the buffer capacity.
    #[inline]
    pub fn cap(&self) -> usize {
        self.v.capacity().min(self.lim)
    }

    /// Returns the buffer capacity limit.
    #[inline(always)]
    pub const fn lim(&self) -> usize {
        self.lim
    }

    /// Returns a packer that will write values to the end of the buffer.
    #[inline]
    pub fn pack(&mut self) -> Pkr {
        self.pack_at(self.v.len())
    }

    /// Returns a packer that will write values starting at index `i`.
    #[inline]
    pub fn pack_at(&mut self, i: usize) -> Pkr {
        Pkr { i, b: self }
    }

    /// Returns an unpacker that will read values from the buffer.
    #[inline]
    pub fn unpack(&self) -> Unpkr {
        Unpkr::new(self.v.as_ref())
    }

    /// Returns whether `n` bytes can be written to the buffer at index `i`.
    #[inline]
    pub fn can_put_at(&mut self, i: usize, n: usize) -> bool {
        i + n <= self.lim
    }

    /// Writes `v` at index `i`. Any existing data at `i` is overwritten. If
    /// `self.len() < i`, then the buffer is extended with `i - self.len()`
    /// zeros.
    pub fn put_at(&mut self, i: usize, v: &[u8]) {
        let n = i.checked_add(v.len()).expect("usize overflow");
        assert!(n <= self.lim, "buffer limit exceeded");
        if !self.v.spilled() && self.v.inline_size() < n {
            self.v.grow(self.lim);
        }
        let extend = i.saturating_sub(self.v.len());
        let dst = self.v.as_mut_ptr();
        if extend > 0 {
            // SAFETY: dst is valid for at least n bytes and
            // self.v.len() + extend <= n
            unsafe { dst.add(self.v.len()).write_bytes(0, extend) };
        }
        // SAFETY: dst is valid for at least n bytes and v can't be a reference
        // into the buffer.
        unsafe { dst.add(i).copy_from_nonoverlapping(v.as_ptr(), v.len()) };
        if n > self.v.len() {
            // SAFETY: self.v contains n initialized bytes
            unsafe { self.v.set_len(n) };
        }
    }
}

impl AsRef<[u8]> for LimitedBuf {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        &self.v
    }
}

impl AsMut<[u8]> for LimitedBuf {
    #[inline]
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.v
    }
}

/// Packer of POD values into a [`LimitedBuf`]. The packer maintains an internal
/// index where new values are written, which may be less than the current
/// buffer length. Little-endian encoding is assumed.
#[derive(Debug)]
#[must_use]
pub(crate) struct Pkr<'a> {
    i: usize,
    b: &'a mut LimitedBuf,
}

impl<'a> Pkr<'a> {
    /// Returns the underlying buffer.
    #[inline(always)]
    fn into_inner(self) -> &'a mut LimitedBuf {
        self.b
    }

    /// Returns whether `n` bytes can be written to the buffer at the current
    /// index.
    #[inline]
    pub fn can_put(&mut self, n: usize) -> bool {
        self.b.can_put_at(self.i, n)
    }

    /// Writes a `u8` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn u8<T: Into<u8>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_ne_bytes().as_slice())
    }

    /// Writes a `u16` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn u16<T: Into<u16>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes().as_slice())
    }

    /// Writes a `u32` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn u32<T: Into<u32>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes().as_slice())
    }

    /// Writes a `u64` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn u64<T: Into<u64>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes().as_slice())
    }

    /// Writes a `u128` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn u128<T: Into<u128>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes().as_slice())
    }

    /// Appends `v` at the current index.
    ///
    /// # Panics
    ///
    /// Panics if the buffer limit is exceeded.
    #[inline]
    pub fn put(&mut self, v: &[u8]) -> &mut Self {
        self.b.put_at(self.i, v);
        self.i += v.len();
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn buf() {
        let mut b = LimitedBuf::new(4);
        assert_eq!(b.len(), 0);
        assert_eq!(b.cap(), 4);
        assert_eq!(b.lim(), 4);

        b.pack().u8(1);
        assert_eq!(b.len(), 1);
        assert_eq!(b.as_ref(), &[1]);

        b.pack().u8(2).u16(0x0403_u16);
        assert_eq!(b.len(), 4);
        assert_eq!(b.as_ref(), &[1, 2, 3, 4]);
    }

    #[test]
    #[should_panic]
    fn buf_limit() {
        let mut b = LimitedBuf::new(4);
        b.pack().u8(1).u16(2_u16);
        b.pack().u16(3_u16);
    }

    #[test]
    fn buf_overwrite() {
        let mut b = LimitedBuf::new(4);
        b.pack().u8(1).u8(2).u8(3).u8(4);
        b.pack_at(1).u16(0x0203_u16);
        assert_eq!(b.as_ref(), &[1, 3, 2, 4]);
    }

    #[test]
    fn buf_extend() {
        let mut b = LimitedBuf::with_capacity(INLINE_CAP + 1);
        // SAFETY: b is valid for b.cap() bytes
        unsafe { b.as_mut().as_mut_ptr().write_bytes(0xFF, b.cap()) };
        b.pack_at(INLINE_CAP).u8(1);
        assert!(&b.as_ref()[..32].iter().all(|&v| v == 0));
        assert_eq!(b.as_ref()[32], 1);
    }
}
