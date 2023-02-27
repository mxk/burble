pub(crate) use Item::*;

#[repr(u8)]
enum ItemType {
    Main = 0 << 2,
    Global = 1 << 2,
    Local = 2 << 2,
}

/// HID report descriptor items.
#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(crate) enum Item {
    // Main
    MInput(Attr),
    MOutput(Attr),
    MFeature(Attr),

    // Collections
    MPhysical(Items),
    MApplication(Items),
    MLogical(Items),
    MReport(Items),
    MNamedArray(Items),
    MUsageSwitch(Items),
    MUsageModifier(Items),

    // Global
    GUsagePage(u16),
    GLogicalMin(i32),
    GLogicalMax(i32),
    GPhysicalMin(i32),
    GPhysicalMax(i32),
    GUnitExp(i8),
    GUnit(u32), // TODO: Provide more convenient API
    GReportSize(u8),
    GReportID(u8),
    GReportCount(u8),
    GPush,
    GPop,

    // Local
    LUsage(u32),
    LUsageMin(u32),
    LUsageMax(u32),
    LDesigIdx(u32),
    LDesigMin(u32),
    LDesigMax(u32),
    LStringIdx(u32),
    LStringMin(u32),
    LStringMax(u32),
    LDelim(Items),
}

bitflags::bitflags! {
    /// Input, output, and feature item attribute flags.
    pub(crate) struct Attr: u32 {
        const CONST        = 1 << 0; // Data / constant
        const VAR          = 1 << 1; // Array / variable
        const REL          = 1 << 2; // Absolute / relative
        const WRAP         = 1 << 3; // No wrap / wrap
        const NON_LINEAR   = 1 << 4; // Linear / non-linear
        const NO_PREFERRED = 1 << 5; // Preferred state / no preferred
        const NULL_STATE   = 1 << 6; // No null position / null state
        const VOLATILE     = 1 << 7; // Non volatile / volatile
        const BUF_BYTES    = 1 << 8; // Bit field / buffered bytes
    }
}

/// Collection of HID report descriptor items that can be converted to a byte
/// vector.
#[derive(Clone, Debug, Default)]
pub struct Items(Vec<Item>);

impl Items {
    /// Creates an empty report descriptor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Concatenates two report descriptors, consuming the other one.
    pub fn concat(&mut self, mut other: Self) -> &mut Self {
        self.0.append(&mut other.0);
        self
    }

    /// Converts contained items to a byte vector.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut b = Vec::with_capacity(128);
        self.append(&mut b);
        b
    }

    /// Appends contained items to an existing byte vector.
    fn append(&self, b: &mut Vec<u8>) {
        for v in &self.0 {
            v.append(b);
        }
    }
}

impl<const N: usize> From<[Item; N]> for Items {
    fn from(v: [Item; N]) -> Self {
        Self(Vec::from(v))
    }
}

impl Item {
    fn append(&self, b: &mut Vec<u8>) {
        match *self {
            MInput(v) => append_main(b, 0b1000, v.bits),
            MOutput(v) => append_main(b, 0b1001, v.bits),
            MFeature(v) => append_main(b, 0b1011, v.bits),

            MPhysical(ref v) => append_collection(b, 0x00, v),
            MApplication(ref v) => append_collection(b, 0x01, v),
            MLogical(ref v) => append_collection(b, 0x02, v),
            MReport(ref v) => append_collection(b, 0x03, v),
            MNamedArray(ref v) => append_collection(b, 0x04, v),
            MUsageSwitch(ref v) => append_collection(b, 0x05, v),
            MUsageModifier(ref v) => append_collection(b, 0x06, v),

            GUsagePage(v) => append_uglobal(b, 0b0000, u32::from(v)),
            GLogicalMin(v) => append_iglobal(b, 0b0001, v),
            GLogicalMax(v) => append_iglobal(b, 0b0010, v),
            GPhysicalMin(v) => append_iglobal(b, 0b0011, v),
            GPhysicalMax(v) => append_iglobal(b, 0b0100, v),
            GUnitExp(v) => append_iglobal(b, 0b0101, i32::from(v)),
            GUnit(v) => append_uglobal(b, 0b0110, v),
            GReportSize(v) => append_uglobal(b, 0b0111, u32::from(v)),
            GReportID(v) => {
                if v != 0 {
                    append_uglobal(b, 0b1000, u32::from(v));
                }
            }
            GReportCount(v) => append_uglobal(b, 0b1001, u32::from(v)),
            GPush => append_uglobal(b, 0b1010, 0),
            GPop => append_uglobal(b, 0b1011, 0),

            LUsage(v) => append_local(b, 0b0000, v),
            LUsageMin(v) => append_local(b, 0b0001, v),
            LUsageMax(v) => append_local(b, 0b0010, v),
            LDesigIdx(v) => append_local(b, 0b0011, v),
            LDesigMin(v) => append_local(b, 0b0100, v),
            LDesigMax(v) => append_local(b, 0b0101, v),
            LStringIdx(v) => append_local(b, 0b0111, v),
            LStringMin(v) => append_local(b, 0b1000, v),
            LStringMax(v) => append_local(b, 0b1001, v),
            LDelim(ref v) => {
                append_short(b, ItemType::Local, 0b1010, 1);
                v.append(b);
                b.push(0b1010 << 4 | ItemType::Local as u8);
            }
        }
    }
}

fn append_main(b: &mut Vec<u8>, tag: u8, v: u32) {
    append_short(b, ItemType::Main, tag, v);
}

fn append_collection(b: &mut Vec<u8>, typ: u8, v: &Items) {
    append_main(b, 0b1010, u32::from(typ));
    v.append(b);
    b.push(0b1100 << 4 | ItemType::Main as u8);
}

fn append_uglobal(b: &mut Vec<u8>, tag: u8, v: u32) {
    append_short(b, ItemType::Global, tag, v);
}

fn append_iglobal(b: &mut Vec<u8>, tag: u8, v: i32) {
    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::cast_sign_loss)]
    let v = if i32::from(v as i8) == v {
        v & 0xFF
    } else if i32::from(v as i16) == v {
        v & 0xFFFF
    } else {
        v
    } as u32;
    append_short(b, ItemType::Global, tag, v);
}

fn append_local(b: &mut Vec<u8>, tag: u8, v: u32) {
    append_short(b, ItemType::Local, tag, v);
}

fn append_short(b: &mut Vec<u8>, typ: ItemType, tag: u8, v: u32) {
    // No zero size case because Windows does not handle it correctly
    let (sz, n) = if v == v & 0xFF {
        (1, 1)
    } else if v == v & 0xFFFF {
        (2, 2)
    } else {
        (3, 4)
    };
    #[allow(clippy::cast_possible_truncation)]
    b.extend_from_slice(
        &[
            tag << 4 | typ as u8 | sz,
            v as u8,
            (v >> 8) as u8,
            (v >> 16) as u8,
            (v >> 24) as u8,
        ][..=n],
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn e10_mouse_report_descriptor() {
        // https://www.usb.org/sites/default/files/hid1_11.pdf E.10
        #[rustfmt::skip]
        assert_eq!(vec![
            0x05, 0x01,
            0x09, 0x02,
            0xA1, 0x01, // Collection (Application)

            0x09, 0x01,
            0xA1, 0x00, // Collection (Physical)

            0x05, 0x09,
            0x19, 0x01,
            0x29, 0x03,
            0x15, 0x00,
            0x25, 0x01,
            0x95, 0x03,
            0x75, 0x01,
            0x81, 0x02, // 3 button bits

            0x95, 0x01,
            0x75, 0x05,
            0x81, 0x01, // 5 bit padding

            0x05, 0x01,
            0x09, 0x30,
            0x09, 0x31,
            0x15, 0x81,
            0x25, 0x7F,
            0x75, 0x08,
            0x95, 0x02,
            0x81, 0x06, // 2 position bytes (X & Y)

            0xC0, // End Collection
            0xC0, // End Collection
        ], Items::from([
            GUsagePage(0x01),
            LUsage(0x02),
            MApplication([
                LUsage(0x01),
                MPhysical([
                    GUsagePage(0x09),
                    LUsageMin(1),
                    LUsageMax(3),
                    GLogicalMin(0),
                    GLogicalMax(1),
                    GReportCount(3),
                    GReportSize(1),
                    MInput(Attr::VAR),

                    GReportCount(1),
                    GReportSize(5),
                    MInput(Attr::CONST),

                    GUsagePage(0x01),
                    LUsage(0x30),
                    LUsage(0x31),
                    GLogicalMin(-127),
                    GLogicalMax(127),
                    GReportSize(8),
                    GReportCount(2),
                    MInput(Attr::VAR | Attr::REL),
                ].into()),
            ].into()),
        ]).to_bytes());
    }

    #[test]
    fn items() {
        let all = Items::from([
            // Main
            MInput(Attr::CONST),
            MOutput(Attr::VAR),
            MFeature(Attr::REL),
            // Collections
            MPhysical(Items::default()),
            MApplication(Items::default()),
            MLogical(Items::default()),
            MReport(Items::default()),
            MNamedArray(Items::default()),
            MUsageSwitch(Items::default()),
            MUsageModifier(Items::default()),
            // Global
            GUsagePage(u16::MIN),
            GLogicalMin(i32::MIN),
            GLogicalMax(i32::MAX),
            GPhysicalMin(-1),
            GPhysicalMax(0x7FFF),
            GUnitExp(1),
            GUnit(u32::MAX),
            GReportSize(u8::MIN),
            GReportID(u8::MAX),
            GReportCount(0),
            GPush,
            GPop,
            // Local
            LUsage(u32::MIN),
            LUsageMin(u32::MAX),
            LUsageMax(1),
            LDesigIdx(2),
            LDesigMin(3),
            LDesigMax(4),
            LStringIdx(5),
            LStringMin(6),
            LStringMax(0xFFFF),
            LDelim(Items::default()),
        ]);
        all.to_bytes();
    }
}
