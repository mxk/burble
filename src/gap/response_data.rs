//! Implementation of length-type-value response data format used in the
//! Extended Inquiry Response (EIR), Advertising Data (AD), Scan Response Data
//! (SRD), Additional Controller Advertising Data (ACAD), and OOB data blocks:
//!
//! * [Vol 3] Part C, Section 8
//! * [Vol 3] Part C, Section 11
//! * [Vol 6] Part B, Section 2.3.4.8
//! * [Core Specification Supplement] Part A, Section 1
//! * [Assigned Numbers] Section 2.3

use std::time::Duration;

use structbuf::{Pack, Packer, StructBuf};

use burble_const::Uuid;

use crate::gap::consts::ResponseDataType;
use crate::gap::{AdvFlag, Appearance};
use crate::hci::{ticks_1250us, ticks_625us};
use crate::le::TxPower;

/// Response data builder.
#[derive(Clone, Debug)]
pub struct ResponseDataMut(StructBuf);

impl ResponseDataMut {
    /// Creates a new response data buffer.
    #[inline]
    #[must_use]
    pub const fn new() -> Self {
        Self(StructBuf::new(254)) // [Vol 6] Part B, Section 2.3.4
    }

    /// Returns the final response data buffer.
    #[allow(clippy::missing_const_for_fn)]
    #[inline]
    pub fn get(self) -> StructBuf {
        self.0
    }

    /// Appends service class UUIDs (\[CSS\] Part A, Section 1.1). Each UUID is
    /// encoded in the optimal format.
    pub fn service_class<T: Copy + Into<Uuid>>(
        &mut self,
        complete: bool,
        uuids: &[T],
    ) -> &mut Self {
        let typ = u8::from(ResponseDataType::IncompleteServiceClass16) + u8::from(complete);
        self.maybe_put(complete, typ, |b| {
            (uuids.iter().filter_map(|&u| u.into().as_u16())).for_each(|v| {
                b.u16(v);
            });
        });
        self.maybe_put(complete, typ + 2, |b| {
            (uuids.iter().filter_map(|&u| u.into().as_u32())).for_each(|v| {
                b.u32(v);
            });
        });
        self.maybe_put(complete, typ + 4, |b| {
            (uuids.iter().filter_map(|&u| u.into().as_u128())).for_each(|v| {
                b.u128(v);
            });
        })
    }

    /// Appends either shortened or complete local device name
    /// (\[CSS\] Part A, Section 1.2).
    pub fn local_name<T: AsRef<str>>(&mut self, complete: bool, v: T) -> &mut Self {
        let typ = u8::from(ResponseDataType::ShortLocalName) + u8::from(complete);
        self.put(typ, |b| {
            b.put(v.as_ref().as_bytes());
        })
    }

    /// Appends advertising flags (\[CSS\] Part A, Section 1.3).
    pub fn flags(&mut self, v: AdvFlag) -> &mut Self {
        self.put(ResponseDataType::Flags, |b| {
            b.u8(v.bits());
        })
    }

    /// Appends manufacturer-specific data (\[CSS\] Part A, Section 1.4).
    pub fn manufacturer_data<T: AsRef<str>>(&mut self, company_id: u16, v: &[u8]) -> &mut Self {
        self.put(ResponseDataType::ManufacturerData, |b| {
            b.u16(company_id).put(v);
        })
    }

    /// Appends TX power level (\[CSS\] Part A, Section 1.5).
    pub fn tx_power(&mut self, v: TxPower) -> &mut Self {
        self.put(ResponseDataType::TxPower, |b| {
            b.i8(v);
        })
    }

    /// Appends peripheral connection interval range (\[CSS\] Part A, Section 1.9).
    ///
    /// # Panics
    ///
    /// Panics if either interval cannot be represented as a 16-bit count of
    /// 1.25ms.
    pub fn peripheral_connection_interval(
        &mut self,
        min: Option<Duration>,
        max: Option<Duration>,
    ) -> &mut Self {
        self.put(ResponseDataType::PeripheralConnectionIntervalRange, |b| {
            b.u16(min.map_or(u16::MAX, |v| ticks_1250us(v).unwrap()));
            b.u16(max.map_or(u16::MAX, |v| ticks_1250us(v).unwrap()));
        })
    }

    /// Appends device appearance (\[CSS\] Part A, Section 1.12).
    pub fn appearance(&mut self, v: Appearance) -> &mut Self {
        self.put(ResponseDataType::Appearance, |b| {
            b.u16(v);
        })
    }

    /// Appends advertising Interval (\[CSS\] Part A, Section 1.15).
    ///
    /// # Panics
    ///
    /// Panics if the interval cannot be represented as a 24-bit count of 625us.
    pub fn adv_interval(&mut self, v: Duration) -> &mut Self {
        let v: [u8; 4] = ticks_625us(v).unwrap().to_le_bytes();
        match v {
            [v @ .., 0, 0] => self.put(ResponseDataType::AdvInterval, |b| {
                b.put(v);
            }),
            [v @ .., 0] => self.put(ResponseDataType::AdvIntervalLong, |b| {
                b.put(v);
            }),
            v => self.put(ResponseDataType::AdvIntervalLong, |b| {
                b.put(v);
            }),
        }
    }

    /// Appends a length-type-data field to the buffer, calling `f` to provide
    /// the data.
    #[inline]
    fn put<T: Into<u8>>(&mut self, typ: T, f: impl Fn(&mut Packer)) -> &mut Self {
        self.maybe_put(true, typ, f)
    }

    /// Append a length-type-data field to the buffer, calling `f` to provide
    /// the data. If the data is empty and `keep_empty` is `false`, then nothing
    /// gets appended.
    fn maybe_put<T: Into<u8>>(
        &mut self,
        keep_empty: bool,
        typ: T,
        f: impl Fn(&mut Packer),
    ) -> &mut Self {
        let i = self.0.len();
        f(self.0.append().put([0, typ.into()]));
        let n = u8::try_from(self.0.len().wrapping_sub(i + 1)).expect("response data overflow");
        self.0[i] = n;
        if !keep_empty && n < 2 {
            self.0.truncate(i);
        }
        self
    }
}

impl Default for ResponseDataMut {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::sdp::ServiceClass;

    use super::*;

    #[test]
    fn css_example_2_1_1() {
        let mut eir = ResponseDataMut::new();
        eir.local_name(true, "Phone").service_class(
            true,
            &[ServiceClass::Panu, ServiceClass::HandsfreeAudioGateway],
        );
        let want = &[
            0x06, // Length of this Data
            0x09, // <Complete Local Name>
            0x50, // 'P'
            0x68, // 'h'
            0x6F, // 'o'
            0x6E, // 'n'
            0x65, // 'e'
            0x05, // Length of this Data
            0x03, // <Complete list of 16-bit Service UUIDs>
            0x15, // PANU service class UUID
            0x11, //
            0x1F, // Hands-free Audio Gateway service class UUID
            0x11, //
            0x01, // Length of this data
            0x05, // <Complete list of 32-bit Service UUIDs>
            0x01, // Length of this data
            0x07, // <Complete list of 128-bit Service UUIDs>
        ];
        assert_eq!(eir.get().as_ref(), want);
    }

    #[test]
    fn css_example_2_1_2() {
        let mut ad = ResponseDataMut::new();
        ad.flags(AdvFlag::LE_LIMITED).local_name(true, "Pedometer");
        let want = &[
            0x02, // Length of this Data
            0x01, // <Flags>
            0x01, // LE Limited Discoverable Flag set
            0x0A, // Length of this Data
            0x09, // <Complete local name>
            0x50, // 'P'
            0x65, // 'e'
            0x64, // 'd'
            0x6F, // 'o'
            0x6D, // 'm'
            0x65, // 'e'
            0x74, // 't'
            0x65, // 'e'
            0x72, // 'r'
        ];
        assert_eq!(ad.get().as_ref(), want);
    }
}
