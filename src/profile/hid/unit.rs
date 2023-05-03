use std::ops;

/// Unit definition (\[HID\] Section 6.2.2.6). A unit is a `u32` Report
/// Descriptor item consisting of 7 nibbles, where the low nibble defines the
/// unit system (1-4) and the remaining 6 nibbles contain the exponents in the
/// range `-8..=7` for each unit type.
///
/// # Example
///
/// ```
/// # use burble::hid::descriptor::Unit;
/// let acceleration = Unit::CENTIMETERS / Unit::SECONDS.pow(2);
/// ```
#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct Unit {
    /// 4-bit unit system mask where bit 0 (LSB) is SI Linear and bit 3 (MSB) is
    /// English Rotation. We use a mask to allow incremental unit combination.
    sys: u8,
    /// 4-bit exponent for each unit type where index 0 is Length and index 5 is
    /// Luminous Intensity.
    exp: [i8; Self::N],
}

impl Unit {
    /// SI linear length.
    pub const CENTIMETERS: Self = Self::new(1, 0b0001);
    /// SI rotation.
    pub const RADIANS: Self = Self::new(1, 0b0010);
    /// English linear length.
    pub const INCHES: Self = Self::new(1, 0b0100);
    /// English rotation.
    pub const DEGREES: Self = Self::new(1, 0b1000);
    /// SI mass.
    pub const GRAMS: Self = Self::new(2, 0b0011);
    // English mass.
    pub const SLUGS: Self = Self::new(2, 0b1100);
    // Time.
    pub const SECONDS: Self = Self::new(3, 0b1111);
    // SI temperature.
    pub const KELVIN: Self = Self::new(4, 0b0011);
    // English temperature.
    pub const FAHRENHEIT: Self = Self::new(4, 0b1100);
    // Current.
    pub const AMPERES: Self = Self::new(5, 0b1111);
    // Luminous intensity.
    pub const CANDELAS: Self = Self::new(6, 0b1111);

    /// Number of unit types.
    const N: usize = 6;

    /// Defines the unit at nibble `i`.
    const fn new(i: usize, sys: u8) -> Self {
        let mut exp = [0; Self::N];
        exp[i - 1] = 1;
        Self { sys, exp }
    }

    /// Raises all contained units to the specified exponent.
    ///
    /// # Panics
    ///
    /// Panics if any resulting exponent is not in the range `[-8,7]`.
    #[inline]
    pub fn pow(self, exp: i8) -> Self {
        self.map(|e| e * exp)
    }

    /// Applies `f` to each exponent.
    ///
    /// # Panics
    ///
    /// Panics if any resulting exponent is not in the range `[-8,7]`.
    fn map(mut self, f: impl Fn(i8) -> i8) -> Self {
        for i in 0..Self::N {
            let e = f(self.exp[i]);
            assert_eq!(e << 4 >> 4, e, "exponent must be in the range [-8,7]");
            self.exp[i] = e;
        }
        self
    }

    /// Returns the raw unit value for the report descriptor.
    #[inline]
    #[must_use]
    pub(super) const fn raw(self) -> u32 {
        let mut v = (self.sys != 0) as u32 * (self.sys.trailing_zeros() + 1);
        let (mut i, mut off) = (0, 4);
        #[allow(clippy::cast_sign_loss)]
        loop {
            v |= ((self.exp[i] & 0xF) as u32) << off;
            (i, off) = (i + 1, off + 4);
            if i == Self::N {
                return v;
            }
        }
    }
}

impl ops::Mul for Unit {
    type Output = Self;

    #[inline]
    fn mul(mut self, rhs: Self) -> Self {
        self.sys &= rhs.sys;
        assert_ne!(self.sys, 0, "incompatible unit system");
        for i in 0..Self::N {
            assert!(self.exp[i] == 0 || rhs.exp[i] == 0, "unit conflict");
            self.exp[i] |= rhs.exp[i];
        }
        self
    }
}

impl ops::Div for Unit {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self {
        ops::Mul::mul(self, rhs.map(|e| -e))
    }
}

#[allow(clippy::unreadable_literal)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn units() {
        use Unit as U;
        assert_eq!(U::RADIANS.raw(), 0x12);
        assert_eq!(U::INCHES.raw(), 0x13);
        assert_eq!(U::DEGREES.raw(), 0x14);
        assert_eq!((U::CANDELAS / U::DEGREES).raw(), 0x010000F4);
    }

    /// Examples from the table on page 39 of the HID spec. These also overlap
    /// with the HID Descriptor Tool.
    #[test]
    fn units_hid_p39() {
        use Unit as U;
        // Distance (cm)
        assert_eq!(U::CENTIMETERS.raw(), 0x11);
        // Mass (g)
        assert_eq!(U::GRAMS.raw(), 0x0101);
        // Time (s)
        assert_eq!(U::SECONDS.raw(), 0x1001);
        // Velocity (cm/s)
        assert_eq!((U::CENTIMETERS / U::SECONDS).raw(), 0xF011);
        // Momentum
        assert_eq!((U::CENTIMETERS * U::GRAMS / U::SECONDS).raw(), 0xF111);
        // Acceleration
        assert_eq!((U::CENTIMETERS * U::SECONDS.pow(-2)).raw(), 0xE011);
        // Force
        assert_eq!(
            (U::CENTIMETERS * U::GRAMS / U::SECONDS.pow(2)).raw(),
            0xE111
        );
        // Energy
        assert_eq!(
            (U::CENTIMETERS.pow(2) * U::GRAMS / U::SECONDS.pow(2)).raw(),
            0xE121
        );
        // Angular Acceleration
        assert_eq!((U::RADIANS / U::SECONDS.pow(2)).raw(), 0xE012);
        // Volts (Unit Exp = 7)
        assert_eq!(
            (U::CENTIMETERS.pow(2) * U::GRAMS / (U::SECONDS.pow(3) * U::AMPERES)).raw(),
            0xF0D121
        );
    }

    /// "Quick Unit" examples from the [HID Descriptor Tool][DT].
    ///
    /// [DT]: https://usb.org/document-library/hid-descriptor-tool
    #[test]
    fn units_hid_descriptor_tool() {
        use Unit as U;
        // Angular Position
        assert_eq!(U::RADIANS.raw(), 0x12);
        // Angular Velocity
        assert_eq!((U::RADIANS / U::SECONDS).raw(), 0xF012);
        // Amps
        assert_eq!(U::AMPERES.raw(), 0x100001);
        // Hertz
        assert_eq!(U::SECONDS.pow(-1).raw(), 0xF001);
        // Power, VA or W (Unit Exp = 7)
        assert_eq!(
            (U::CENTIMETERS.pow(2) * U::GRAMS / U::SECONDS.pow(3)).raw(),
            0xD121
        );
        // Temperature (K)
        assert_eq!(U::KELVIN.raw(), 0x010001);
        // Battery Capacity (AmpSec)
        assert_eq!((U::SECONDS * U::AMPERES).raw(), 0x101001);
    }

    #[should_panic(expected = "incompatible unit system")]
    #[test]
    fn unit_system() {
        let _ = Unit::KELVIN * Unit::FAHRENHEIT;
    }

    #[should_panic(expected = "unit conflict")]
    #[test]
    fn unit_conflict() {
        let _ = Unit::CENTIMETERS * Unit::CENTIMETERS;
    }

    #[should_panic(expected = "exponent must be in the range [-8,7]")]
    #[test]
    fn unit_exponent() {
        let _ = Unit::CENTIMETERS.pow(2).pow(4);
    }
}
