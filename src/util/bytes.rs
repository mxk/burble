#![allow(dead_code)] // TODO: Remove

use std::{mem, slice};

/// Unpacker of POD values from a byte slice. Any reads past the end of the
/// slice return default values rather than panicking. The caller must check the
/// error status at the end to determine whether all returned values were valid.
/// Little-endian encoding is assumed.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[must_use]
#[repr(transparent)]
pub(crate) struct Unpkr<'a>(&'a [u8]);

impl<'a> Unpkr<'a> {
    /// Creates a new unpacker.
    #[inline]
    pub const fn new(b: &'a [u8]) -> Self {
        Self(b)
    }

    /// Returns the remaining byte slice.
    #[inline(always)]
    #[must_use]
    pub const fn peek(&self) -> &'a [u8] {
        self.0
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

    /// Skips `n` bytes.
    #[inline]
    pub fn skip(&mut self, n: usize) {
        self.0 = (self.0.get(n..)).unwrap_or(Self::err());
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
        unsafe {
            let p = self.0.as_ptr().cast::<T>();
            self.0 = slice::from_raw_parts(p.add(1).cast(), self.0.len() - n);
            p.read_unaligned()
        }
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

impl<'a> AsRef<[u8]> for Unpkr<'a> {
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
    fn unpkr() {
        let mut u = Unpkr::new(&[1, 2, 3]);
        assert_eq!(u.u8(), 1);
        assert!(u.is_ok());
        assert_eq!(u.u16(), 0x0302);
        assert!(u.is_ok());
        assert_eq!(u.u8(), 0);
        assert!(!u.is_ok());

        let mut u = Unpkr::new(&[1]);
        assert_eq!(u.u16(), 0);
        assert!(!u.is_ok());
        assert_eq!(u.u32(), 0);
    }

    #[test]
    fn unpkr_take() {
        let mut u = Unpkr::new(&[1, 2, 3]);
        assert_eq!(u.u8(), 1);
        let mut v = u.take();
        assert!(u.is_ok());
        assert!(u.peek().is_empty());

        assert_eq!((v.u8(), v.u8()), (2, 3));
        assert!(v.is_ok());

        assert_eq!(u.u64(), 0);
        assert!(!u.is_ok());
        let v = u.take();
        assert!(!v.is_ok());
    }
}
