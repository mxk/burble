pub use unit::*;

#[path = "unit.rs"]
mod unit;

/// Country code (`bCountryCode`) of the localized hardware
/// (\[HID\] Section 6.2.1).
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
#[repr(u8)]
pub enum Locale {
    #[default]
    None = 0,
    Arabic = 1,
    Belgian = 2,
    CanadianBilingual = 3,
    CanadianFrench = 4,
    CzechRepublic = 5,
    Danish = 6,
    Finnish = 7,
    French = 8,
    German = 9,
    Greek = 10,
    Hebrew = 11,
    Hungary = 12,
    International = 13,
    Italian = 14,
    JapanKatakana = 15,
    Korean = 16,
    LatinAmerican = 17,
    NetherlandsDutch = 18,
    Norwegian = 19,
    PersianFarsi = 20,
    Poland = 21,
    Portuguese = 22,
    Russia = 23,
    Slovakia = 24,
    Spanish = 25,
    Swedish = 26,
    SwissFrench = 27,
    SwissGerman = 28,
    Switzerland = 29,
    Taiwan = 30,
    TurkishQ = 31,
    Uk = 32,
    Us = 33,
    Yugoslavia = 34,
    TurkishF = 35,
}

/// An encoded HID report descriptor (\[HID\] Section 6.2.2).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReportDescriptor(Vec<u8>);

impl ReportDescriptor {
    /// Creates a new report descriptor.
    #[must_use]
    pub fn new(it: impl AsRef<[Item]>) -> Self {
        fn bytes(it: &[Item]) -> usize {
            let mut n = 0;
            for v in it {
                n += match *v {
                    Item::MCollection(_, ref v) | Item::LDelim(ref v) => 4 + bytes(v),
                    _ => 2,
                };
            }
            n
        }
        let it = it.as_ref();
        let mut rd = Self(Vec::with_capacity(bytes(it)));
        rd.extend(it);
        rd
    }

    /// Appends the contents of another report descriptor.
    #[inline]
    pub fn append(&mut self, other: &Self) {
        self.0.extend_from_slice(&other.0);
    }

    /// Appends all items in `it` to the descriptor.
    #[inline]
    pub fn extend(&mut self, it: impl AsRef<[Item]>) {
        for v in it.as_ref() {
            self.put(v);
        }
    }

    /// Appends item `v` to the descriptor.
    fn put(&mut self, v: &Item) {
        use {Item::*, Tag::*};
        match *v {
            MInput(v) => self.u32(Input, u32::from(v.bits())),
            MOutput(v) => self.u32(Output, u32::from(v.bits())),
            MFeature(v) => self.u32(Feature, u32::from(v.bits())),
            MCollection(typ, ref v) => {
                self.u32(Collection, typ as _);
                self.extend(v);
                self.0.push(EndCollection as _);
            }

            GUsagePage(v) => self.u32(UsagePage, u32::from(v)),
            GLogicalMin(v) => self.i32(LogicalMin, v),
            GLogicalMax(v) => self.i32(LogicalMax, v),
            GPhysicalMin(v) => self.i32(PhysicalMin, v),
            GPhysicalMax(v) => self.i32(PhysicalMax, v),
            GUnitExp(v) => self.i32(UnitExp, i32::from(v)),
            GUnit(v) => self.u32(Unit, v.raw()),
            GReportSize(v) => self.u32(ReportSize, u32::from(v)),
            GReportId(v) => {
                if v != 0 {
                    self.u32(ReportId, u32::from(v));
                }
            }
            GReportCount(v) => self.u32(ReportCount, u32::from(v)),
            GPush => self.0.push(Push as _),
            GPop => self.0.push(Pop as _),

            LUsage(v) => self.u32(Usage, v),
            LUsageMin(v) => self.u32(UsageMin, v),
            LUsageMax(v) => self.u32(UsageMax, v),
            LDesigIndex(v) => self.u32(DesigIndex, v),
            LDesigMin(v) => self.u32(DesigMin, v),
            LDesigMax(v) => self.u32(DesigMax, v),
            LStringIndex(v) => self.u32(StringIndex, v),
            LStringMin(v) => self.u32(StringMin, v),
            LStringMax(v) => self.u32(StringMax, v),
            LDelim(ref v) => {
                self.u32(Delim, 1);
                self.extend(v);
                self.u32(Delim, 0);
            }
        }
    }

    /// Appends a short `i32` item.
    fn i32(&mut self, t: Tag, v: i32) {
        #[allow(clippy::cast_possible_truncation)]
        let v = if i32::from(v as i16) == v {
            if i32::from(v as i8) == v {
                v & 0xFF
            } else {
                v & 0xFFFF
            }
        } else {
            v
        };
        #[allow(clippy::cast_sign_loss)]
        self.u32(t, v as _);
    }

    /// Appends a short `u32` item.
    fn u32(&mut self, t: Tag, v: u32) {
        // Windows does not handle some zero-size items correctly, so we always
        // output a 0 byte. This also matches the HID Descriptor Tool behavior.
        let n: usize = match v.leading_zeros() {
            z if z >= u32::BITS - u8::BITS => 1,
            z if z >= u32::BITS - u16::BITS => 2,
            _ => 4,
        };
        #[allow(clippy::cast_possible_truncation)]
        self.0.extend_from_slice(
            &[
                t as u8 | (n.trailing_zeros() as u8 + 1),
                v as u8,
                (v >> 8) as u8,
                (v >> 16) as u8,
                (v >> 24) as u8,
            ][..=n],
        );
    }
}

impl AsRef<[u8]> for ReportDescriptor {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

/// Item tag specifying the function of the item
/// (\[HID\] Section 6.2.2.3, 6.2.2.4, 6.2.2.7, 6.2.2.8).
#[allow(clippy::unusual_byte_groupings)]
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
enum Tag {
    // Main
    Input = 0b1000_00 << 2,
    Output = 0b1001_00 << 2,
    Feature = 0b1011_00 << 2,
    Collection = 0b1010_00 << 2,
    EndCollection = 0b1100_00 << 2,

    // Global
    UsagePage = 0b0000_01 << 2,
    LogicalMin = 0b0001_01 << 2,
    LogicalMax = 0b0010_01 << 2,
    PhysicalMin = 0b0011_01 << 2,
    PhysicalMax = 0b0100_01 << 2,
    UnitExp = 0b0101_01 << 2,
    Unit = 0b0110_01 << 2,
    ReportSize = 0b0111_01 << 2,
    ReportId = 0b1000_01 << 2,
    ReportCount = 0b1001_01 << 2,
    Push = 0b1010_01 << 2,
    Pop = 0b1011_01 << 2,

    // Local
    Usage = 0b0000_10 << 2,
    UsageMin = 0b0001_10 << 2,
    UsageMax = 0b0010_10 << 2,
    DesigIndex = 0b0011_10 << 2,
    DesigMin = 0b0100_10 << 2,
    DesigMax = 0b0101_10 << 2,
    StringIndex = 0b0111_10 << 2,
    StringMin = 0b1000_10 << 2,
    StringMax = 0b1001_10 << 2,
    Delim = 0b1010_10 << 2,

    // Long
    _Long = 0b1111_11 << 2,
}

bitflags::bitflags! {
    /// Input, output, and feature item data flags (\[HID\] Section 6.2.2.5).
    #[derive(Clone, Copy, Debug, Default)]
    pub struct Flag: u16 {
        /// Data / constant. Indicates whether the item is data or a constant
        /// value. Data indicates the item is defining report fields that
        /// contain modifiable device data. Constant indicates the item is a
        /// static read-only field in a report and cannot be modified (written)
        /// by the host.
        const CONST = 1 << 0;

        /// Array / variable. Indicates whether the item creates variable or
        /// array data fields in reports. In variable fields, each field
        /// represents data from a physical control. The number of bits reserved
        /// for each field is determined by preceding Report Size / Report Count
        /// items. For example, a bank of eight on/off switches could be
        /// reported in 1 byte declared by a variable Input item where each bit
        /// represents one switch, on (1) or off (0) (Report Size = 1, Report
        /// Count = 8). Alternatively, a variable Input item could add 1 report
        /// byte used to represent the state of four three-position buttons,
        /// where the state of each button is represented by two bits (Report
        /// Size = 2, Report Count = 4). Or 1 byte from a variable Input item
        /// could represent the X position of a joystick (Report Size = 8,
        /// Report Count = 1).
        ///
        /// An array provides an alternate means for describing the data
        /// returned from a group of buttons. Arrays are more efficient, if less
        /// flexible than variable items. Rather than returning a single bit for
        /// each button in the group, an array returns an index in each field
        /// that corresponds to the pressed button (like keyboard scan codes).
        /// An out-of range value in and array field is considered no controls
        /// asserted. Buttons or keys in an array that are simultaneously
        /// pressed need to be reported in multiple fields. Therefore, the
        /// number of fields in an array input item (Report Count) dictates the
        /// maximum number of simultaneous controls that can be reported. A
        /// keyboard could report up to three simultaneous keys using an array
        /// with three 8-bit fields (Report Size = 8, Report Count = 3). Logical
        /// Minimum specifies the lowest index value returned by the array and
        /// Logical Maximum specifies the largest. The number of elements in the
        /// array can be deduced by examining the difference between Logical
        /// Minimum and Logical Maximum (number of elements = Logical Maximum -
        /// Logical Minimum + 1).
        const VAR = 1 << 1;

        /// Absolute / relative. Indicates whether the data is absolute (based
        /// on a fixed origin) or relative (indicating the change in value from
        /// the last report). Mouse devices usually provide relative data, while
        /// tablets usually provide absolute data.
        const REL = 1 << 2;

        /// No wrap / wrap. Indicates whether the data "rolls over" when
        /// reaching either the extreme high or low value. For example, a dial
        /// that can spin freely 360 degrees might output values from 0 to 10.
        /// If wrap is indicated, the next value reported after passing the 10
        /// position in the increasing direction would be 0.
        const WRAP = 1 << 3;

        /// Linear / non-linear. Indicates whether the raw data from the device
        /// has been processed in some way, and no longer represents a linear
        /// relationship between what is measured and the data that is reported.
        /// Acceleration curves and joystick dead zones are examples of this
        /// kind of data. Sensitivity settings would affect the Units item, but
        /// the data would still be linear.
        const NON_LINEAR = 1 << 4;

        /// Preferred state / no preferred. Indicates whether the control has a
        /// preferred state to which it will return when the user is not
        /// physically interacting with the control. Push buttons (as opposed to
        /// toggle buttons) and self-centering joysticks are examples.
        const NO_PREF = 1 << 5;

        /// No null position / null state. Indicates whether the control has a
        /// state in which it is not sending meaningful data. One possible use
        /// of the null state is for controls that require the user to
        /// physically interact with the control in order for it to report
        /// useful data. For example, some joysticks have a multi-directional
        /// switch (a hat switch). When a hat switch is not being pressed it is
        /// in a null state. When in a null state, the control will report a
        /// value outside of the specified Logical Minimum and Logical Maximum
        /// (the most negative value, such as -128 for an 8-bit value).
        const NULL = 1 << 6;

        /// Non-volatile / volatile. Indicates whether the Feature or Output
        /// control's value should be changed by the host or not. Volatile
        /// output can change with or without host interaction. To avoid
        /// synchronization problems, volatile controls should be relative
        /// whenever possible.
        const VOLATILE = 1 << 7;

        /// Bit field / buffered bytes. Indicates that the control emits a
        /// fixed-size stream of bytes. The contents of the data field are
        /// determined by the application. The contents of the buffer are not
        /// interpreted as a single numeric quantity. Report data defined by a
        /// Buffered Bytes item must be aligned on an 8-bit boundary. The data
        /// from a bar code reader is an example.
        const BYTES = 1 << 8;
    }
}

/// Collection type (\[HID\] Section 6.2.2.6).
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[repr(u8)]
pub enum Collection {
    /// A physical collection used for a set of data items that represent data
    /// points collected at one geometric point. This is useful for sensing
    /// devices which may need to associate sets of measured or sensed data with
    /// a single point. It does not indicate that a set of data values comes
    /// from one device, such as a keyboard. In the case of device which reports
    /// the position of multiple sensors, physical collections are used to show
    /// which data comes from each separate sensor.
    Physical = 0x00,

    /// A group of Main items that might be familiar to applications. It could
    /// also be used to identify item groups serving different purposes in a
    /// single device. Common examples are a keyboard or mouse. A keyboard with
    /// an integrated pointing device could be defined as two different
    /// application collections. Data reports are usually (but not necessarily)
    /// associated with application collections (at least one report ID per
    /// application).
    Application = 0x01,

    /// A logical collection is used when a set of data items form a composite
    /// data structure. An example of this is the association between a data
    /// buffer and a byte count of the data. The collection establishes the link
    /// between the count and the buffer.
    Logical = 0x02,

    /// A logical collection that wraps all the fields in a report. A unique
    /// report ID will be contained in this collection. An application can
    /// easily determine whether a device supports a certain function. Note that
    /// any valid Report ID value can be declared for a Report collection.
    Report = 0x03,

    /// A logical collection that contains an array of selector usages. For a
    /// given function the set of selectors used by similar devices may vary.
    /// The naming of fields is common practice when documenting hardware
    /// registers. To determine whether a device supports a particular function
    /// like Status, an application might have to query for several known Status
    /// selector usages before it could determine whether the device supported
    /// Status. The Named Array usages allows the Array field that contains the
    /// selectors to be named, thus the application only needs to query for the
    /// Status usage to determine that a device supports status information.
    NamedArray = 0x04,

    /// A logical collection that modifies the meaning of the usages that it
    /// contains. This collection type indicates to an application that the
    /// usages found in this collection must be special cased. For instance,
    /// rather than declaring a usage on the LED page for every possible
    /// function, an Indicator usage can be applied to a Usage Switch collection
    /// and the standard usages defined in that collection can now be identified
    /// as indicators for a function rather than the function itself. Note that
    /// this collection type is not used for the labeling Ordinal collections, a
    /// Logical collection type is used for that.
    UsageSwitch = 0x05,

    /// Modifies the meaning of the usage attached to the encompassing
    /// collection. A usage typically defines a single operating mode for a
    /// control. The usage modifier allows the operating mode of a control to be
    /// extended. For instance, an LED is typically on or off. For particular
    /// states a device may want a generic method of blinking or choosing the
    /// color of a standard LED. Attaching the LED usage to a Usage Modifier
    /// collection will indicate to an application that the usage supports a new
    /// operating mode.
    UsageModifier = 0x06,
}

impl Collection {
    /// Defines a physical collection.
    #[inline(always)]
    #[must_use]
    pub fn physical(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Physical, it.as_ref().to_vec())
    }

    /// Defines an application collection.
    #[inline(always)]
    #[must_use]
    pub fn application(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Application, it.as_ref().to_vec())
    }

    /// Defines a logical collection.
    #[inline(always)]
    #[must_use]
    pub fn logical(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Logical, it.as_ref().to_vec())
    }

    /// Defines a report collection.
    #[inline(always)]
    #[must_use]
    pub fn report(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Report, it.as_ref().to_vec())
    }

    /// Defines a named array collection.
    #[inline(always)]
    #[must_use]
    pub fn named_array(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::NamedArray, it.as_ref().to_vec())
    }

    /// Defines a usage switch collection.
    #[inline(always)]
    #[must_use]
    pub fn usage_switch(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::UsageSwitch, it.as_ref().to_vec())
    }

    /// Defines a usage modifier collection.
    #[inline(always)]
    #[must_use]
    pub fn usage_modifier(it: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::UsageModifier, it.as_ref().to_vec())
    }
}

/// HID report descriptor items (\[HID\] Section 5.2).
///
/// Variants are prefixed with `M`, `G`, or `L` for Main, Global, or Local type,
/// respectively. Local items only describe the data fields defined by the next
/// Main item. Global items become the default attributes for all subsequent
/// data fields.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Item {
    /// Data from one or more similar controls on a device. For example,
    /// variable data such as reading the position of a single axis or a group
    /// of levers or array data such as one or more push buttons or switches.
    MInput(Flag),

    /// Data to one or more similar controls on a device such as setting the
    /// position of a single axis or a group of levers (variable data). Or, it
    /// can represent data to one or more LEDs (array data).
    MOutput(Flag),

    /// Device input and output not intended for consumption by the end user -
    /// for example, a software feature or Control Panel toggle.
    MFeature(Flag),

    /// A meaningful grouping of Input, Output, and Feature items - for example,
    /// mouse, keyboard, joystick, and pointer.
    MCollection(Collection, Vec<Item>),

    // Global
    GUsagePage(u16),
    GLogicalMin(i32),
    GLogicalMax(i32),
    GPhysicalMin(i32),
    GPhysicalMax(i32),
    GUnitExp(i8),
    GUnit(Unit),
    GReportSize(u8),
    GReportId(u8),
    GReportCount(u8),
    GPush,
    GPop,

    // Local
    LUsage(u32),
    LUsageMin(u32),
    LUsageMax(u32),
    LDesigIndex(u32),
    LDesigMin(u32),
    LDesigMax(u32),
    LStringIndex(u32),
    LStringMin(u32),
    LStringMax(u32),
    LDelim(Vec<Item>),
}

impl Item {
    /// Defines a delimited set of items.
    #[inline(always)]
    #[must_use]
    pub fn delim(it: impl AsRef<[Self]>) -> Self {
        Self::LDelim(it.as_ref().to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn e10_mouse_report_descriptor() {
        use Item::*;
        // https://www.usb.org/sites/default/files/hid1_11.pdf E.10
        assert_eq!(
            ReportDescriptor(vec![
                0x05, 0x01, //
                0x09, 0x02, //
                0xA1, 0x01, // Collection (Application)
                0x09, 0x01, //
                0xA1, 0x00, // Collection (Physical)
                0x05, 0x09, //
                0x19, 0x01, //
                0x29, 0x03, //
                0x15, 0x00, //
                0x25, 0x01, //
                0x95, 0x03, //
                0x75, 0x01, //
                0x81, 0x02, // 3 button bits
                0x95, 0x01, //
                0x75, 0x05, //
                0x81, 0x01, // 5 bit padding
                0x05, 0x01, //
                0x09, 0x30, //
                0x09, 0x31, //
                0x15, 0x81, //
                0x25, 0x7F, //
                0x75, 0x08, //
                0x95, 0x02, //
                0x81, 0x06, // 2 position bytes (X & Y)
                0xC0, // End Collection
                0xC0, // End Collection
            ]),
            ReportDescriptor::new([
                GUsagePage(0x01),
                LUsage(0x02),
                Collection::application([
                    LUsage(0x01),
                    Collection::physical([
                        GUsagePage(0x09),
                        LUsageMin(1),
                        LUsageMax(3),
                        GLogicalMin(0),
                        GLogicalMax(1),
                        GReportCount(3),
                        GReportSize(1),
                        MInput(Flag::VAR),
                        GReportCount(1),
                        GReportSize(5),
                        MInput(Flag::CONST),
                        GUsagePage(0x01),
                        LUsage(0x30),
                        LUsage(0x31),
                        GLogicalMin(-127),
                        GLogicalMax(127),
                        GReportSize(8),
                        GReportCount(2),
                        MInput(Flag::VAR | Flag::REL),
                    ]),
                ]),
            ])
        );
    }

    #[test]
    fn items() {
        use Item::*;
        let _ = ReportDescriptor::new([
            // Main
            MInput(Flag::CONST),
            MOutput(Flag::VAR),
            MFeature(Flag::REL),
            Collection::application([]),
            // Global
            GUsagePage(u16::MIN),
            GLogicalMin(i32::MIN),
            GLogicalMax(i32::MAX),
            GPhysicalMin(-1),
            GPhysicalMax(0x7FFF),
            GUnitExp(1),
            GUnit(Unit::CENTIMETERS),
            GReportSize(u8::MIN),
            GReportId(u8::MAX),
            GReportCount(0),
            GPush,
            GPop,
            // Local
            LUsage(u32::MIN),
            LUsageMin(u32::MAX),
            LUsageMax(1),
            LDesigIndex(2),
            LDesigMin(3),
            LDesigMax(4),
            LStringIndex(5),
            LStringMin(6),
            LStringMax(0xFFFF),
            Item::delim([]),
        ]);
    }
}
