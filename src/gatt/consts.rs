use std::fmt::Debug;
use std::num::NonZeroU8;

pub use burble_const::{Characteristic, Declaration, Descriptor, Service, Unit};

bitflags::bitflags! {
    /// Characteristic properties ([Vol 3] Part G, Section 3.3.1.1).
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    #[repr(transparent)]
    pub struct Prop: u8 {
        /// Permits broadcasts of the Characteristic Value using Server
        /// Characteristic Configuration Descriptor. If set, the Server
        /// Characteristic Configuration Descriptor shall exist.
        const BROADCAST = 0x01;
        /// Permits reads of the Characteristic Value.
        const READ = 0x02;
        /// Permit writes of the Characteristic Value without response.
        const WRITE_CMD = 0x04;
        /// Permits writes of the Characteristic Value with response.
        const WRITE = 0x08;
        /// Permits notifications of a Characteristic Value without
        /// acknowledgment. If set, the Client Characteristic Configuration
        /// Descriptor shall exist.
        const NOTIFY = 0x10;
        /// Permits indications of a Characteristic Value with acknowledgment.
        /// If set, the Client Characteristic Configuration Descriptor shall
        /// exist.
        const INDICATE = 0x20;
        /// Permits signed writes to the Characteristic Value.
        const SIGNED_WRITE_CMD = 0x40;
        /// Additional characteristic properties are defined in the
        /// Characteristic Extended Properties Descriptor. If set, the
        /// Characteristic Extended Properties Descriptor shall exist.
        const EXT_PROPS = 0x80;
    }
}

impl Prop {
    /// Returns a mask of valid CCCD bits.
    #[inline(always)]
    pub(super) const fn cccd_mask(self) -> Cccd {
        Cccd::from_bits_retain((self.bits() >> 4) as u16 & Cccd::NOTIFY_MASK.bits())
    }
}

bitflags::bitflags! {
    /// Characteristic extended properties ([Vol 3] Part G, Section 3.3.3.1).
    #[derive(Clone, Copy, Debug, Default)]
    #[repr(transparent)]
    pub struct ExtProp: u16 {
        /// Permits reliable writes of the Characteristic Value.
        const RELIABLE_WRITE = 1 << 0;
        /// Permits writes to the Characteristic User Description descriptor.
        const WRITABLE_AUX = 1 << 1;
    }
}

bitflags::bitflags! {
    /// Client Characteristic Configuration descriptor value
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[derive(Clone, Copy, Debug, Default, serde::Deserialize, serde::Serialize)]
    #[repr(transparent)]
    #[serde(transparent)]
    pub struct Cccd: u16 {
        /// The Characteristic Value shall be notified. This value can only be
        /// set if the characteristic's properties have the `NOTIFY` bit set.
        const NOTIFY = 1 << 0;
        /// The Characteristic Value shall be indicated. This value can only be
        /// set if the characteristic's properties have the `INDICATE` bit set.
        const INDICATE = 1 << 1;
        /// Notify or indicate bit mask.
        const NOTIFY_MASK = Self::NOTIFY.bits() | Self::INDICATE.bits();
    }
}

bitflags::bitflags! {
    /// Client Supported Features characteristic value
    /// ([Vol 3] Part G, Section 7.2).
    #[derive(Clone, Copy, Debug, Default, serde::Deserialize, serde::Serialize)]
    #[repr(transparent)]
    #[serde(transparent)]
    pub(super) struct ClientFeature: u8 {
        /// The client supports robust caching.
        const ROBUST_CACHING = 1 << 0;
        /// The client supports Enhanced ATT bearer.
        const ENHANCED_ATT = 1 << 1;
        /// The client supports receiving `ATT_MULTIPLE_HANDLE_VALUE_NTF` PDUs.
        const MULTI_NTF = 1 << 2;
    }
}

bitflags::bitflags! {
    /// Server Supported Features characteristic value
    /// ([Vol 3] Part G, Section 7.4).
    #[derive(Clone, Copy, Debug, Default)]
    #[repr(transparent)]
    pub(super) struct ServerFeature: u8 {
        /// The server supports Enhanced ATT bearer.
        const EATT = 1 << 0;
    }
}

/// Characteristic presentation format types ([Assigned Numbers] Section 2.4.1).
#[derive(
    Clone, Copy, Debug, Eq, PartialEq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum Format {
    /// Unsigned 1-bit (0 = false; 1 = true).
    Bool = 0x01,
    /// Unsigned 2-bit integer.
    U2 = 0x02,
    /// Unsigned 4-bit integer.
    U4 = 0x03,
    /// Unsigned 8-bit integer.
    U8 = 0x04,
    /// Unsigned 12-bit integer.
    U12 = 0x05,
    /// Unsigned 16-bit integer.
    U16 = 0x06,
    /// Unsigned 24-bit integer.
    U24 = 0x07,
    /// Unsigned 32-bit integer.
    U32 = 0x08,
    /// Unsigned 48-bit integer.
    U48 = 0x09,
    /// Unsigned 64-bit integer.
    U64 = 0x0A,
    /// Unsigned 128-bit integer.
    U128 = 0x0B,
    /// Signed 8-bit integer.
    I8 = 0x0C,
    /// Signed 12-bit integer.
    I12 = 0x0D,
    /// Signed 16-bit integer.
    I16 = 0x0E,
    /// Signed 24-bit integer.
    I24 = 0x0F,
    /// Signed 32-bit integer.
    I32 = 0x10,
    /// Signed 48-bit integer.
    I48 = 0x11,
    /// Signed 64-bit integer.
    I64 = 0x12,
    /// Signed 128-bit integer.
    I128 = 0x13,
    /// IEEE-754 32-bit floating point.
    F32 = 0x14,
    /// IEEE-754 64-bit floating point.
    F64 = 0x15,
    /// IEEE 11073-20601 16-bit SFLOAT ([[PHD]] Section 2.2.2).
    ///
    /// [PHD]: https://www.bluetooth.com/wp-content/uploads/2019/03/PHD_Transcoding_WP_v16.pdf
    MedF16 = 0x16,
    /// IEEE 11073-20601 32-bit FLOAT (\[PHD\] Section 2.2.1).
    MedF32 = 0x17,
    /// IEEE 11073-20601 nomenclature code (`[u16; 2]`).
    MedType = 0x18,
    /// UTF-8 string.
    Utf8 = 0x19,
    /// UTF-16 string.
    Utf16 = 0x1A,
    /// Opaque structure.
    Struct = 0x1B,
}

/// Characteristic presentation format description
/// ([Assigned Numbers] Section 2.4.2.1).
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Description {
    Unknown,
    Nth(NonZeroU8),
    Front,
    Back,
    Top,
    Bottom,
    Upper,
    Lower,
    Main,
    Backup,
    Auxiliary,
    Supplementary,
    Flash,
    Inside,
    Outside,
    Left,
    Right,
    Internal,
    External,
}

impl Description {
    /// Creates a description for the `N`th item, where `N > 0`.
    ///
    /// # Panics
    ///
    /// Panics if `n == 0`.
    #[inline]
    #[must_use]
    pub const fn nth(n: u8) -> Self {
        // TODO: Use expect when const stable
        match NonZeroU8::new(n) {
            Some(n) => Self::Nth(n),
            None => panic!("n cannot be zero"),
        }
    }

    /// Returns the raw description namespace.
    #[inline(always)]
    #[must_use]
    pub const fn ns(self) -> u8 {
        0x01 // [Assigned Numbers] Section 2.4.2
    }

    /// Returns the raw description ID.
    #[must_use]
    pub const fn raw(self) -> u16 {
        use Description::*;
        match self {
            Unknown => 0x0000,
            Nth(n) => n.get() as u16,
            Front => 0x0100,
            Back => 0x0101,
            Top => 0x0102,
            Bottom => 0x0103,
            Upper => 0x0104,
            Lower => 0x0105,
            Main => 0x0106,
            Backup => 0x0107,
            Auxiliary => 0x0108,
            Supplementary => 0x0109,
            Flash => 0x010A,
            Inside => 0x010B,
            Outside => 0x010C,
            Left => 0x010D,
            Right => 0x010E,
            Internal => 0x010F,
            External => 0x0110,
        }
    }
}

impl From<Description> for u16 {
    #[inline]
    fn from(d: Description) -> Self {
        d.raw()
    }
}
