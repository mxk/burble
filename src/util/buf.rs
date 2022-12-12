use std::ops::{Deref, DerefMut};
use std::{mem, slice};

use smallvec::SmallVec;

/// Inline capacity sufficient to avoid allocation for most HCI commands and
/// outbound ACL data transfers, while staying within one cache line.
const INLINE_CAP: usize = 32;

/// A capacity-limited buffer used for encoding and decoding data packets. The
/// buffer starts out with a small internal capacity that does not require
/// allocation. It performs at most one heap allocation up to the capacity limit
/// once the internal capacity is exceeded. The relationship `len <= cap <= lim`
/// always holds. It panics if a write operation exceeds the limit.
#[derive(Clone, Debug, Default)]
#[must_use]
pub struct LimitedBuf {
    lim: usize,
    b: SmallVec<[u8; INLINE_CAP]>,
}

impl LimitedBuf {
    /// Creates a new capacity-limited buffer without allocating from the heap.
    #[inline]
    pub const fn new(lim: usize) -> Self {
        Self {
            lim,
            b: SmallVec::new_const(),
        }
    }

    /// Creates a pre-allocated capacity-limited buffer. This buffer will never
    /// reallocate.
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            lim: cap,
            b: SmallVec::with_capacity(cap),
        }
    }

    /// Returns the number of bytes in the buffer.
    #[inline]
    pub fn len(&self) -> usize {
        self.b.len()
    }

    /// Returns the buffer capacity.
    #[inline]
    pub fn cap(&self) -> usize {
        self.b.capacity().min(self.lim)
    }

    /// Returns the buffer capacity limit.
    #[inline(always)]
    pub const fn lim(&self) -> usize {
        self.lim
    }

    /// Returns the number of additional bytes that can be written to the
    /// buffer.
    #[inline]
    pub fn remaining(&self) -> usize {
        self.lim - self.b.len()
    }

    /// Returns whether the buffer contains the maximum number of bytes.
    #[inline]
    pub fn is_full(&self) -> bool {
        self.remaining() == 0
    }

    /// Clears the buffer, resetting its length to 0.
    #[inline]
    pub fn clear(&mut self) -> &mut Self {
        self.truncate(0)
    }

    /// Removes all but the first `n` bytes from the buffer.
    #[inline]
    pub fn truncate(&mut self, n: usize) -> &mut Self {
        if n < self.b.len() {
            // SAFETY: n is a valid length and there is nothing to drop
            unsafe { self.b.set_len(n) }
        }
        self
    }

    /// Returns a packer for writing values to the end of the buffer.
    #[inline]
    #[must_use]
    pub fn pack(&mut self) -> Packer {
        self.pack_at(self.b.len())
    }

    /// Returns a packer for writing values starting at index `i`.
    #[inline]
    #[must_use]
    pub fn pack_at(&mut self, i: usize) -> Packer {
        Packer { i, b: self }
    }

    /// Returns an unpacker for reading values from the start of the buffer.
    #[inline]
    pub fn unpack(&self) -> Unpacker {
        Unpacker::new(self.b.as_ref())
    }

    /// Returns whether `n` bytes can be written to the buffer at index `i`.
    #[inline]
    pub fn can_put_at(&mut self, i: usize, n: usize) -> bool {
        i + n <= self.lim
    }

    /// Writes slice `v` at index `i`. Any existing data at `i` is overwritten.
    /// If `self.len() < i`, then the buffer is padded with `i - self.len()` 0s.
    pub fn put_at(&mut self, i: usize, v: &[u8]) {
        let n = i.checked_add(v.len()).expect("usize overflow");
        assert!(n <= self.lim, "buffer limit exceeded");
        if !self.b.spilled() && self.b.inline_size() < n {
            self.b.grow(self.lim);
        }
        let pad = i.saturating_sub(self.b.len());
        let dst = self.b.as_mut_ptr();
        if pad > 0 {
            // SAFETY: dst is valid for at least n bytes and
            // `self.v.len() + pad == i <= n`.
            unsafe { dst.add(self.b.len()).write_bytes(0, pad) };
        }
        // SAFETY: dst is valid for at least n bytes and v can't be a reference
        // into the buffer.
        unsafe { dst.add(i).copy_from_nonoverlapping(v.as_ptr(), v.len()) };
        if n > self.b.len() {
            // SAFETY: self.v contains n initialized bytes
            unsafe { self.b.set_len(n) };
        }
    }

    /// Sets the buffer length.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the buffer contains `n` initialized bytes.
    #[inline]
    pub unsafe fn set_len(&mut self, n: usize) {
        assert!(n <= self.cap(), "buffer capacity exceeded");
        self.b.set_len(n);
    }
}

impl AsRef<[u8]> for LimitedBuf {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        &self.b
    }
}

impl AsMut<[u8]> for LimitedBuf {
    #[inline]
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.b
    }
}

impl Deref for LimitedBuf {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.b
    }
}

impl DerefMut for LimitedBuf {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.b
    }
}

/// Packer of POD values into a [`LimitedBuf`]. The packer maintains an index
/// where new values are written, which may be less than the current buffer
/// length. Little-endian encoding is assumed. All write operations panic if the
/// buffer capacity limit is exceeded.
#[derive(Debug)]
pub struct Packer<'a> {
    i: usize,
    b: &'a mut LimitedBuf,
}

impl<'a> Packer<'a> {
    /// Returns the underlying buffer.
    #[inline(always)]
    pub fn into_inner(self) -> &'a mut LimitedBuf {
        self.b
    }

    /// Writes a `bool` at the current index.
    #[inline]
    pub fn bool<T: Into<bool>>(&mut self, v: T) -> &mut Self {
        self.put([u8::from(v.into())])
    }

    /// Writes a `u8` at the current index.
    #[inline]
    pub fn u8<T: Into<u8>>(&mut self, v: T) -> &mut Self {
        self.put([v.into()])
    }

    /// Writes a `u16` at the current index.
    #[inline]
    pub fn u16<T: Into<u16>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes a `u24` at the current index.
    #[inline]
    pub fn u24<T: Into<u32>>(&mut self, v: T) -> &mut Self {
        let v = v.into().to_le_bytes();
        assert_eq!(v[3], 0);
        self.put(&v[..3])
    }

    /// Writes a `u32` at the current index.
    #[inline]
    pub fn u32<T: Into<u32>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes a `u64` at the current index.
    #[inline]
    pub fn u64<T: Into<u64>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes a `u128` at the current index.
    #[inline]
    pub fn u128<T: Into<u128>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes an `i8` at the current index.
    #[inline]
    pub fn i8<T: Into<i8>>(&mut self, v: T) -> &mut Self {
        #[allow(clippy::cast_sign_loss)]
        self.put([v.into() as u8])
    }

    /// Writes an `i16` at the current index.
    #[inline]
    pub fn i16<T: Into<i16>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes an `i32` at the current index.
    #[inline]
    pub fn i32<T: Into<i32>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes an `i64` at the current index.
    #[inline]
    pub fn i64<T: Into<i64>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Writes an `i128` at the current index.
    #[inline]
    pub fn i128<T: Into<i128>>(&mut self, v: T) -> &mut Self {
        self.put(v.into().to_le_bytes())
    }

    /// Returns whether `n` bytes can be written to the buffer at the current
    /// index.
    #[inline]
    pub fn can_put(&mut self, n: usize) -> bool {
        self.b.can_put_at(self.i, n)
    }

    /// Writes `v` at the current index.
    #[inline]
    pub fn put<T: AsRef<[u8]>>(&mut self, v: T) -> &mut Self {
        let v = v.as_ref();
        self.b.put_at(self.i, v);
        self.i += v.len();
        self
    }
}

/// Unpacker of POD values from a byte slice. Any reads past the end of the
/// slice return default values rather than panicking. The caller must check the
/// error status at the end to determine whether all returned values were valid.
/// Little-endian encoding is assumed.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
#[must_use]
#[repr(transparent)]
pub struct Unpacker<'a>(&'a [u8]);

impl<'a> Unpacker<'a> {
    /// Creates a new unpacker.
    #[inline]
    pub const fn new(b: &'a [u8]) -> Self {
        Self(b)
    }

    /// Returns the remaining number of bytes.
    #[inline]
    #[must_use]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the remaining byte slice.
    #[inline(always)]
    #[must_use]
    pub const fn peek(&self) -> &'a [u8] {
        self.0
    }

    /// Returns whether the byte slice is empty.
    #[inline]
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns whether all reads were within the bounds of the original byte
    /// slice.
    #[inline]
    #[must_use]
    pub fn is_ok(&self) -> bool {
        self.0.as_ptr() != Self::err().as_ptr()
    }

    /// Returns the remaining byte slice or `None` if any reads went past the
    /// end of the original slice.
    #[inline]
    #[must_use]
    pub fn into_inner(self) -> Option<&'a [u8]> {
        self.is_ok().then_some(self.0)
    }

    /// Returns the remaining byte slice in a new unpacker, leaving the original
    /// empty. This is primarily useful in combination with `map()`.
    #[inline]
    pub fn take(&mut self) -> Self {
        // SAFETY: `len()..` range is always valid for get()
        let empty = unsafe { self.0.get_unchecked(self.0.len()..) };
        Self(mem::replace(&mut self.0, empty))
    }

    /// Splits the byte slice at `i`, and returns two new unpackers or `None` if
    /// there is an insufficient number of bytes remaining.
    #[inline]
    pub fn split_at(&self, i: usize) -> Option<(Self, Self)> {
        self.0.len().checked_sub(i).map(|rem| {
            // SAFETY: 0 <= i <= self.0.len() and i + rem == self.0.len()
            unsafe {
                let p = self.0.as_ptr();
                (
                    Self(slice::from_raw_parts(p, i)),
                    Self(slice::from_raw_parts(p.add(i), rem)),
                )
            }
        })
    }

    /// Returns the result of passing the unpacker to `f`, or `None` if `f`
    /// fails to consume the entire slice without reading past the end.
    #[inline]
    pub fn map<T>(mut self, f: impl FnOnce(&mut Self) -> T) -> Option<T> {
        let v = f(&mut self);
        (self.is_ok() && self.0.is_empty()).then_some(v)
    }

    /// Returns the result of passing the unpacker to `f`, or `default` if `f`
    /// fails to consume the entire slice without reading past the end.
    #[inline]
    #[must_use]
    pub fn map_or<T>(self, default: T, f: impl FnOnce(&mut Self) -> T) -> T {
        self.map(f).unwrap_or(default)
    }

    /// Advances `self` by `n` bytes, returning a new unpacker for the skipped
    /// bytes or `None` if there is an insufficient number of bytes remaining,
    /// in which case any remaining bytes are discarded.
    #[inline]
    pub fn skip(&mut self, n: usize) -> Option<Self> {
        let (a, b) = self
            .split_at(n)
            .map_or_else(|| (None, Self::err()), |(a, b)| (Some(a), b.0));
        self.0 = b;
        a
    }

    /// Returns the next `u8` as a `bool` where any non-zero value is converted
    /// to `true`.
    #[inline]
    #[must_use]
    pub fn bool(&mut self) -> bool {
        // SAFETY: All bit patterns are valid
        unsafe { self.read::<u8>() != 0 }
    }

    /// Returns the next `u8`.
    #[inline]
    #[must_use]
    pub fn u8(&mut self) -> u8 {
        // SAFETY: All bit patterns are valid
        unsafe { self.read() }
    }

    /// Returns the next `u16`.
    #[inline]
    #[must_use]
    pub fn u16(&mut self) -> u16 {
        // SAFETY: All bit patterns are valid
        u16::from_le(unsafe { self.read() })
    }

    /// Returns the next `u32`.
    #[inline]
    #[must_use]
    pub fn u32(&mut self) -> u32 {
        // SAFETY: All bit patterns are valid
        u32::from_le(unsafe { self.read() })
    }

    /// Returns the next `u64`.
    #[inline]
    #[must_use]
    pub fn u64(&mut self) -> u64 {
        // SAFETY: All bit patterns are valid
        u64::from_le(unsafe { self.read() })
    }

    /// Returns the next `u128`.
    #[inline]
    #[must_use]
    pub fn u128(&mut self) -> u128 {
        // SAFETY: All bit patterns are valid
        u128::from_le(unsafe { self.read() })
    }

    /// Returns the next `i8`.
    #[inline]
    #[must_use]
    pub fn i8(&mut self) -> i8 {
        // SAFETY: All bit patterns are valid
        unsafe { self.read() }
    }

    /// Returns the next `i16`.
    #[inline]
    #[must_use]
    pub fn i16(&mut self) -> i16 {
        // SAFETY: All bit patterns are valid
        i16::from_le(unsafe { self.read() })
    }

    /// Returns the next `i32`.
    #[inline]
    #[must_use]
    pub fn i32(&mut self) -> i32 {
        // SAFETY: All bit patterns are valid
        i32::from_le(unsafe { self.read() })
    }

    /// Returns the next `i64`.
    #[inline]
    #[must_use]
    pub fn i64(&mut self) -> i64 {
        // SAFETY: All bit patterns are valid
        i64::from_le(unsafe { self.read() })
    }

    /// Returns the next `i128`.
    #[inline]
    #[must_use]
    pub fn i128(&mut self) -> i128 {
        // SAFETY: All bit patterns are valid
        i128::from_le(unsafe { self.read() })
    }

    /// Returns the next value of type `T`, or `T::default()` if there is an
    /// insufficient number of bytes remaining, in which case any remaining
    /// bytes are discarded.
    ///
    /// # Safety
    ///
    /// `T` must be able to hold the resulting bit pattern.
    #[inline]
    pub unsafe fn read<T: Default>(&mut self) -> T {
        let n = mem::size_of::<T>();
        if n > self.0.len() {
            self.0 = Self::err();
            return T::default();
        }
        // SAFETY: 0 <= size_of::<T>() <= self.0.len()
        let p = self.0.as_ptr().cast::<T>();
        self.0 = slice::from_raw_parts(p.add(1).cast(), self.0.len() - n);
        p.read_unaligned()
    }

    /// Returns a sentinel byte slice indicating that the original slice was too
    /// short.
    #[inline(always)]
    #[must_use]
    const fn err() -> &'static [u8] {
        // Can't be a const: https://github.com/rust-lang/rust/issues/105536
        // SAFETY: A dangling pointer is valid for a zero-length slice
        unsafe { slice::from_raw_parts(std::ptr::NonNull::dangling().as_ptr(), 0) }
    }
}

impl<'a> AsRef<[u8]> for Unpacker<'a> {
    #[inline(always)]
    #[must_use]
    fn as_ref(&self) -> &'a [u8] {
        self.0
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
    fn buf_pad() {
        let mut b = LimitedBuf::with_capacity(INLINE_CAP + 1);
        // SAFETY: b is valid for b.cap() bytes
        unsafe { b.as_mut().as_mut_ptr().write_bytes(0xFF, b.cap()) };
        b.pack_at(INLINE_CAP).u8(1);
        assert!(&b.as_ref()[..INLINE_CAP].iter().all(|&v| v == 0));
        assert_eq!(b.as_ref()[INLINE_CAP], 1);

        b.clear();
        b.put_at(4, &[]);
        assert_eq!(b.as_ref(), &[0, 0, 0, 0]);
    }

    #[test]
    fn unpacker() {
        let mut u = Unpacker::new(&[1, 2, 3]);
        assert_eq!(u.u8(), 1);
        assert!(u.is_ok());
        assert_eq!(u.u16(), 0x0302);
        assert!(u.is_ok());
        assert_eq!(u.u8(), 0);
        assert!(!u.is_ok());

        let mut u = Unpacker::new(&[1]);
        assert_eq!(u.u16(), 0);
        assert!(!u.is_ok());
        assert_eq!(u.u32(), 0);
    }

    #[test]
    fn unpacker_take() {
        let mut u = Unpacker::new(&[1, 2, 3]);
        assert_eq!(u.u8(), 1);
        let mut v = u.take();
        assert!(u.is_ok());
        assert!(u.is_empty());

        assert_eq!((v.u8(), v.u8()), (2, 3));
        assert!(v.is_ok());

        assert_eq!(u.u64(), 0);
        assert!(!u.is_ok());
        let v = u.take();
        assert!(!v.is_ok());
    }
}
