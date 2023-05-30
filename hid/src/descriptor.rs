//! HID descriptor data types.

#![allow(clippy::unusual_byte_groupings)] // For num_enum::TryFromPrimitive

use alloc::vec::Vec;
use core::iter::FusedIterator;

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
    Czech = 5,
    Danish = 6,
    Finnish = 7,
    French = 8,
    German = 9,
    Greek = 10,
    Hebrew = 11,
    Hungarian = 12,
    International = 13,
    Italian = 14,
    JapaneseKatakana = 15,
    Korean = 16,
    LatinAmerican = 17,
    Dutch = 18,
    Norwegian = 19,
    PersianFarsi = 20,
    Polish = 21,
    Portuguese = 22,
    Russian = 23,
    Slovakian = 24,
    Spanish = 25,
    Swedish = 26,
    SwissFrench = 27,
    SwissGerman = 28,
    Swiss = 29,
    Taiwanese = 30,
    TurkishQ = 31,
    Uk = 32,
    Us = 33,
    Yugoslavian = 34,
    TurkishF = 35,
}

/// An encoded HID report descriptor (\[HID\] Section 5.2, 6.2.2).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReportDescriptor(Vec<u8>);

impl ReportDescriptor {
    /// Creates a new report descriptor.
    #[inline]
    pub fn new(items: impl AsRef<[Item]>) -> Self {
        fn bytes(items: &[Item]) -> usize {
            items.iter().fold(0, |n, v| match *v {
                Item::MCollection(_, ref v) | Item::LDelim(ref v) => n + 4 + bytes(v),
                _ => n + 3,
            })
        }
        let items = items.as_ref();
        let mut this = Self(Vec::with_capacity(bytes(items)));
        this.extend(items);
        this
    }

    /// Appends the contents of another report descriptor to this one.
    #[inline]
    pub fn append(&mut self, other: &Self) {
        self.0.extend_from_slice(&other.0);
    }

    /// Appends all `items` to the report descriptor.
    #[inline]
    pub fn extend(&mut self, items: impl AsRef<[Item]>) {
        for v in items.as_ref() {
            self.push(v);
        }
    }

    /// Returns an iterator over report descriptor items.
    #[inline(always)]
    #[must_use]
    pub fn iter(&self) -> Iter {
        Iter(&self.0)
    }

    /// Appends item `v` to the descriptor.
    fn push(&mut self, v: &Item) {
        use {Item::*, Tag::*};
        match *v {
            // Main
            MInput(v) => self.u32(Input, u32::from(v.bits())),
            MOutput(v) => self.u32(Output, u32::from(v.bits())),
            MFeature(v) => self.u32(Feature, u32::from(v.bits())),
            MCollection(typ, ref v) => {
                self.u32(Collection, typ as _);
                self.extend(v);
                self.0.push(EndCollection as _);
            }

            // Global
            GUsagePage(v) => self.u32(UsagePage, v as _),
            GLogicalMin(v) => self.i32(LogicalMin, v),
            GLogicalMax(v) => self.i32(LogicalMax, v),
            GPhysicalMin(v) => self.i32(PhysicalMin, v),
            GPhysicalMax(v) => self.i32(PhysicalMax, v),
            GUnitExp(v) => self.i32(UnitExp, i32::from(v)),
            GUnit(v) => self.u32(Unit, v.raw()),
            GReportSize(v) => self.u32(ReportSize, v),
            GReportId(v) => {
                if v != 0 {
                    self.u32(ReportId, u32::from(v));
                }
            }
            GReportCount(v) => self.u32(ReportCount, v),
            GPush => self.0.push(Push as _),
            GPop => self.0.push(Pop as _),

            // Local
            LUsage(v) => self.u32(Usage, v),
            LUsageMin(v) => self.u32(UsageMin, v),
            LUsageMax(v) => self.u32(UsageMax, v),
            LDesignatorIndex(v) => self.u32(DesignatorIndex, v),
            LDesignatorMin(v) => self.u32(DesignatorMin, v),
            LDesignatorMax(v) => self.u32(DesignatorMax, v),
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
        let n = (usize::from(i32::from(v as i16) != v) * 2)
            + (usize::from(i32::from(v as i8) != v) + 1);
        self.put(t, v.to_le_bytes(), n);
    }

    /// Appends a short `u32` item.
    fn u32(&mut self, t: Tag, v: u32) {
        #[allow(clippy::cast_possible_truncation)]
        let n = (usize::from(u32::from(v as u16) != v) * 2)
            + (usize::from(u32::from(v as u8) != v) + 1);
        self.put(t, v.to_le_bytes(), n);
    }

    /// Appends `n` bytes of a short value. Windows does not handle some
    /// zero-size items correctly, so `n` must be 1, 2, or 4, which also matches
    /// the HID Descriptor Tool behavior.
    #[inline]
    fn put(&mut self, t: Tag, v: [u8; 4], n: usize) {
        #[allow(clippy::cast_possible_truncation)]
        let hdr = t as u8 | (n.trailing_zeros() as u8 + 1);
        let item = [hdr, v[0], v[1], v[2], v[3]];
        // SAFETY: 1 + n <= item.len()
        unsafe { self.0.extend_from_slice(item.get_unchecked(..=n)) };
    }
}

impl AsRef<[u8]> for ReportDescriptor {
    #[inline(always)]
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl<'a> IntoIterator for &'a ReportDescriptor {
    type Item = <Iter<'a> as Iterator>::Item;
    type IntoIter = Iter<'a>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Report descriptor iterator. Yields `(tag, size, data)` values. For short
/// items, `size` is the number of bytes used for `data`. For long items, `size`
/// is `bDataSize`, `data` is `bLongItemTag`, and the actual data is skipped.
#[derive(Clone, Debug)]
pub struct Iter<'a>(&'a [u8]);

impl Iterator for Iter<'_> {
    type Item = (Tag, usize, u32);

    fn next(&mut self) -> Option<Self::Item> {
        use num_enum::TryFromPrimitive;
        let (&t, tail) = self.0.split_first()?;
        let n = 4 >> (3 - (t & 3));
        let t = Tag::try_from_primitive(t & !3).ok()?;
        if n > tail.len() {
            return None;
        }
        let mut v = [0_u8; 4];
        let (data, tail) = tail.split_at(n);
        v[..n].copy_from_slice(data);
        if !matches!(t, Tag::Long) {
            self.0 = tail;
            return Some((t, n, u32::from_le_bytes(v)));
        }
        let sz = usize::from(v[0]);
        if n != 2 || sz > tail.len() {
            return None;
        }
        self.0 = &tail[sz..];
        Some((t, sz, u32::from(v[1])))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.0.len()))
    }
}

impl FusedIterator for Iter<'_> {}

/// Report descriptor item (\[HID\] Section 5.2, 6.2.2).
///
/// Variants are prefixed with `M`, `G`, or `L` for Main, Global, or Local type,
/// respectively. One or more fields of data from controls are defined by a Main
/// item and further described by the preceding Global and Local items. Local
/// items only describe the data fields defined by the next Main item. Global
/// items become the default attributes for all subsequent data fields.
///
/// # Required items
///
/// * [`Item::GUsagePage`]
/// * [`Item::GLogicalMin`]
/// * [`Item::GLogicalMax`]
/// * [`Item::GReportSize`]
/// * [`Item::GReportCount`]
/// * [`Item::LUsage`]
/// * [`Item::MInput`] or [`Item::MOutput`] or [`Item::MFeature`]
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Item {
    /// Data from one or more similar controls on a device. For example,
    /// variable data reading the position of a single axis or a group of
    /// levers, or array data from one or more push buttons or switches.
    MInput(Flag),

    /// Data to one or more similar controls on a device. For example, variable
    /// data setting the position of a single axis or a group of levers, or
    /// array data to one or more LEDs.
    MOutput(Flag),

    /// Device input and output not intended for consumption by the end user.
    /// For example, a software feature or Control Panel toggle.
    MFeature(Flag),

    /// A meaningful grouping of Input, Output, and Feature items. For example,
    /// mouse, keyboard, joystick, and pointer.
    MCollection(Collection, Vec<Item>),

    /// Current Usage Page. Since usages are 32-bit values, Usage Page items can
    /// be used to conserve space in a report descriptor by setting the high
    /// order 16 bits of subsequent usages. Any Usage that follows which defines
    /// 16 bits or fewer is interpreted as a Usage ID and concatenated with the
    /// Usage Page to form a 32-bit usage.
    GUsagePage(super::usage::Page),

    /// Extent value in logical units. This is the minimum value that a variable
    /// or array item will report. For example, a mouse reporting X position
    /// values from 0 to 128 would have a Logical Minimum of 0 and a Logical
    /// Maximum of 128.
    GLogicalMin(i32),

    /// Extent value in logical units. This is the maximum value that a variable
    /// or array item will report.
    GLogicalMax(i32),

    /// Minimum value for the physical extent of a variable item. This
    /// represents the Logical Minimum with units applied to it.
    GPhysicalMin(i32),

    /// Maximum value for the physical extent of a variable item.
    GPhysicalMax(i32),

    /// Value of the unit exponent. See [`Item::unit_exp_compat`] for more
    /// information.
    GUnitExp(i8),

    /// Physical units.
    GUnit(Unit),

    /// Size of the report fields in bits. This allows the parser to build an
    /// item map for the report handler to use.
    GReportSize(u32),

    /// Allows the host to distinguish different types of reports arriving over
    /// a single interrupt in pipe, and allows the device to distinguish
    /// different types of reports arriving over a single interrupt out pipe.
    /// Report ID zero is reserved and will be omitted from the descriptor.
    ///
    /// If a Report ID is used anywhere in the descriptor, it must be declared
    /// prior to the first Input, Output, or Feature item, and all data reports
    /// for the device must specify the associated report ID. USB reports
    /// specify the Report ID with a 1-byte prefix. Bluetooth reports specify
    /// the Report ID in the Report Reference characteristic descriptor.
    GReportId(u8),

    /// Determines how many fields are included in the report for this
    /// particular item (and consequently how many bits are added to the
    /// report).
    GReportCount(u32),

    /// Places a copy of the global item state table on the stack.
    GPush,

    /// Replaces the item state table with the top structure from the stack.
    GPop,

    /// Represents a suggested usage for the item or collection. In the case
    /// where an item represents multiple controls, a Usage tag may suggest a
    /// usage for every variable or element in an array.
    ///
    /// While Local items do not carry over to the next Main item, they may
    /// apply to more than one control within a single item. For example, if an
    /// Input item defining five controls is preceded by three Usage tags, the
    /// three usages would be assigned sequentially to the first three controls,
    /// and the third usage would also be assigned to the fourth and fifth
    /// controls.
    LUsage(u32),

    /// Defines the starting usage associated with an array or bitmap. If a
    /// Usage Minimum is declared as and extended (32-bit) usage then the
    /// associated Usage Maximum must also be declared as an extended usage.
    LUsageMin(u32),

    /// Defines the ending usage associated with an array or bitmap.
    LUsageMax(u32),

    /// Determines the body part used for a control. Index points to a
    /// designator in the Physical descriptor.
    LDesignatorIndex(u32),

    /// Defines the index of the starting designator associated with an array or
    /// bitmap.
    LDesignatorMin(u32),

    /// Defines the index of the ending designator associated with an array or
    /// bitmap.
    LDesignatorMax(u32),

    /// String index in the string descriptor. Allows a string to be associated
    /// with a particular item or control.
    LStringIndex(u32),

    /// Specifies the first string index when assigning a group of sequential
    /// strings to controls in an array or bitmap.
    LStringMin(u32),

    /// Specifies the last string index when assigning a group of sequential
    /// strings to controls in an array or bitmap.
    LStringMax(u32),

    /// Allows aliases to be defined for a control so that an application can
    /// access it in more than one way. The usages that form a delimited set are
    /// organized in order of preference, where the first usage declared is the
    /// most preferred usage for the control.
    LDelim(Vec<Item>),
}

impl Item {
    /// Returns a Unit Exponent using 4-bit encoding for the range `[-8,7]`.
    ///
    /// There is a disagreement between the HID spec and parser implementations
    /// about the representation of Unit Exponent items. The value should be a
    /// regular signed integer, but some parsers and the HID Descriptor Tool
    /// treat it the same as the 4-bit exponents used in Unit items (likely due
    /// to unclear wording in the spec). This helper method returns the 4-bit
    /// encoding for exponents in the range `[-8,7]` and standard encoding for
    /// ranges `[-128,-9]` and `[16,127]`. Range `[8,15]` is ambiguous.
    ///
    /// # References
    ///
    /// * Linux: [[PATCH 1/1] HID: Fix unit exponent parsing again][linux]
    /// * Windows: [Unit Exponent item value encoding in HID report descriptors][windows]
    ///
    /// [linux]: https://lore.kernel.org/all/1381666192-25309-1-git-send-email-spbnick@gmail.com/
    /// [windows]: https://social.msdn.microsoft.com/Forums/WINDOWS/en-US/e87d0db1-486e-42ae-bf95-d1ac5ffc0b02/unit-exponent-item-value-encoding-in-hid-report-descriptors?forum=whck
    #[inline(always)]
    #[must_use]
    pub const fn unit_exp_compat(exp: i8) -> Self {
        Self::GUnitExp(if exp == exp << 4 >> 4 { exp & 0xF } else { exp })
    }

    /// Defines a delimited set of items.
    #[inline(always)]
    #[must_use]
    pub fn delim(items: impl AsRef<[Self]>) -> Self {
        Self::LDelim(items.as_ref().to_vec())
    }
}

/// Item tag specifying the function of the item
/// (\[HID\] Section 6.2.2.3, 6.2.2.4, 6.2.2.7, 6.2.2.8).
#[derive(Clone, Copy, Debug, Eq, PartialEq, num_enum::TryFromPrimitive)]
#[non_exhaustive]
#[repr(u8)]
pub enum Tag {
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
    DesignatorIndex = 0b0011_10 << 2,
    DesignatorMin = 0b0100_10 << 2,
    DesignatorMax = 0b0101_10 << 2,
    StringIndex = 0b0111_10 << 2, // HID Descriptor Tool does not encode String
    StringMin = 0b1000_10 << 2,   // Index, Min, and Max correctly. It uses tags
    StringMax = 0b1001_10 << 2,   // 6, 7, 8 instead of 7, 8, 9.
    Delim = 0b1010_10 << 2,

    // Long
    Long = 0b1111_11 << 2,
}

bitflags::bitflags! {
    /// Input, Output, and Feature item data flags (\[HID\] Section 6.2.2.5).
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
    pub fn physical(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Physical, items.as_ref().to_vec())
    }

    /// Defines an application collection.
    #[inline(always)]
    #[must_use]
    pub fn application(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Application, items.as_ref().to_vec())
    }

    /// Defines a logical collection.
    #[inline(always)]
    #[must_use]
    pub fn logical(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Logical, items.as_ref().to_vec())
    }

    /// Defines a report collection.
    #[inline(always)]
    #[must_use]
    pub fn report(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::Report, items.as_ref().to_vec())
    }

    /// Defines a named array collection.
    #[inline(always)]
    #[must_use]
    pub fn named_array(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::NamedArray, items.as_ref().to_vec())
    }

    /// Defines a usage switch collection.
    #[inline(always)]
    #[must_use]
    pub fn usage_switch(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::UsageSwitch, items.as_ref().to_vec())
    }

    /// Defines a usage modifier collection.
    #[inline(always)]
    #[must_use]
    pub fn usage_modifier(items: impl AsRef<[Item]>) -> Item {
        Item::MCollection(Self::UsageModifier, items.as_ref().to_vec())
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use crate::usage::{GenericDesktop, Page};

    use super::*;

    /// Keyboard report descriptor (\[HID\] Section E.6).
    #[test]
    fn keyboard_report_descriptor_e6() {
        use Item::*;
        assert_eq!(
            ReportDescriptor::new([
                GUsagePage(Page::GenericDesktop),
                LUsage(GenericDesktop::Keyboard as _),
                Collection::application([
                    GUsagePage(Page::Key),
                    LUsageMin(224),
                    LUsageMax(231),
                    GLogicalMin(0),
                    GLogicalMax(1),
                    GReportSize(1),
                    GReportCount(8),
                    MInput(Flag::VAR), // Modifier byte
                    GReportCount(1),
                    GReportSize(8),
                    MInput(Flag::CONST), // Reserved byte
                    GReportCount(5),
                    GReportSize(1),
                    GUsagePage(Page::Led),
                    LUsageMin(1),
                    LUsageMax(5),
                    MOutput(Flag::VAR), // LED report
                    GReportCount(1),
                    GReportSize(3),
                    MOutput(Flag::CONST), // LED report padding
                    GReportCount(6),
                    GReportSize(8),
                    GLogicalMin(0),
                    GLogicalMax(101),
                    GUsagePage(Page::Key),
                    LUsageMin(0),
                    LUsageMax(101),
                    MInput(Flag::empty()), // Key arrays (6 bytes)
                ]),
            ]),
            ReportDescriptor(vec![
                0x05, 0x01, // Usage Page (Generic Desktop)
                0x09, 0x06, // Usage (Keyboard)
                0xA1, 0x01, // Collection (Application)
                0x05, 0x07, // Usage Page (Key Codes)
                0x19, 0xE0, // Usage Minimum (224)
                0x29, 0xE7, // Usage Maximum (231)
                0x15, 0x00, // Logical Minimum (0)
                0x25, 0x01, // Logical Maximum (1)
                0x75, 0x01, // Report Size (1)
                0x95, 0x08, // Report Count (8)
                0x81, 0x02, // Input (Data, Variable, Absolute)  ; Modifier byte
                0x95, 0x01, // Report Count (1)
                0x75, 0x08, // Report Size (8)
                0x81, 0x01, // Input (Constant)                  ; Reserved byte
                0x95, 0x05, // Report Count (5)
                0x75, 0x01, // Report Size (1)
                0x05, 0x08, // Usage Page (Page# for LEDs)
                0x19, 0x01, // Usage Minimum (1)
                0x29, 0x05, // Usage Maximum (5)
                0x91, 0x02, // Output (Data, Variable, Absolute) ; LED report
                0x95, 0x01, // Report Count (1)
                0x75, 0x03, // Report Size (3)
                0x91, 0x01, // Output (Constant)                 ; LED report padding
                0x95, 0x06, // Report Count (6)
                0x75, 0x08, // Report Size (8)
                0x15, 0x00, // Logical Minimum (0)
                0x25, 0x65, // Logical Maximum(101)
                0x05, 0x07, // Usage Page (Key Codes)
                0x19, 0x00, // Usage Minimum (0)
                0x29, 0x65, // Usage Maximum (101)
                0x81, 0x00, // Input (Data, Array)               ; Key arrays (6 bytes)
                0xC0, // End Collection
            ])
        );
    }

    /// Mouse report descriptor (\[HID\] Section E.10).
    #[test]
    fn mouse_report_descriptor_e10() {
        use Item::*;
        assert_eq!(
            ReportDescriptor::new([
                GUsagePage(Page::GenericDesktop),
                LUsage(GenericDesktop::Mouse as _),
                Collection::application([
                    LUsage(GenericDesktop::Pointer as _),
                    Collection::physical([
                        GUsagePage(Page::Button),
                        LUsageMin(1),
                        LUsageMax(3),
                        GLogicalMin(0),
                        GLogicalMax(1),
                        GReportCount(3),
                        GReportSize(1),
                        MInput(Flag::VAR), // 3 button bits
                        GReportCount(1),
                        GReportSize(5),
                        MInput(Flag::CONST), // 5 bit padding
                        GUsagePage(Page::GenericDesktop),
                        LUsage(GenericDesktop::X as _),
                        LUsage(GenericDesktop::Y as _),
                        GLogicalMin(-127),
                        GLogicalMax(127),
                        GReportSize(8),
                        GReportCount(2),
                        MInput(Flag::VAR | Flag::REL), // 2 position bytes (X & Y)
                    ]),
                ]),
            ]),
            ReportDescriptor(vec![
                0x05, 0x01, // Usage Page (Generic Desktop)
                0x09, 0x02, // Usage (Mouse)
                0xA1, 0x01, // Collection (Application)
                0x09, 0x01, // Usage (Pointer)
                0xA1, 0x00, // Collection (Physical)
                0x05, 0x09, // Usage Page (Buttons)
                0x19, 0x01, // Usage Minimum (01)
                0x29, 0x03, // Usage Maximun (03)
                0x15, 0x00, // Logical Minimum (0)
                0x25, 0x01, // Logical Maximum (1)
                0x95, 0x03, // Report Count (3)
                0x75, 0x01, // Report Size (1)
                0x81, 0x02, // Input (Data, Variable, Absolute) ; 3 button bits
                0x95, 0x01, // Report Count (1)
                0x75, 0x05, // Report Size (5)
                0x81, 0x01, // Input (Constant)                 ; 5 bit padding
                0x05, 0x01, // Usage Page (Generic Desktop)
                0x09, 0x30, // Usage (X)
                0x09, 0x31, // Usage (Y)
                0x15, 0x81, // Logical Minimum (-127)
                0x25, 0x7F, // Logical Maximum (127)
                0x75, 0x08, // Report Size (8)
                0x95, 0x02, // Report Count (2)
                0x81, 0x06, // Input (Data, Variable, Relative) ; 2 position bytes (X & Y)
                0xC0, // End Collection
                0xC0, // End Collection
            ])
        );
    }

    #[test]
    fn items() {
        use Item::*;
        assert_eq!(
            ReportDescriptor::new([
                MInput(Flag::empty()),
                MOutput(Flag::VAR | Flag::WRAP | Flag::NO_PREF | Flag::VOLATILE),
                MFeature(Flag::all()),
                Collection::physical([]),
                GUsagePage(Page::GenericDesktop),
                GLogicalMin(1),
                GLogicalMax(2),
                GPhysicalMin(3),
                GPhysicalMax(4),
                Item::unit_exp_compat(-8),
                GUnit(Unit::INCHES),
                GReportSize(7),
                GReportId(8),
                GReportCount(9),
                GPush,
                GPop,
                LUsage(0),
                LUsageMin(1),
                LUsageMax(2),
                LDesignatorIndex(3),
                LDesignatorMin(4),
                LDesignatorMax(5),
                LStringIndex(7),
                LStringMin(8),
                LStringMax(9),
                Item::delim([]),
            ]),
            ReportDescriptor(vec![
                0x81, 0x00, // MInput
                0x91, 0xAA, // MOutput
                0xB2, 0xFF, 0x01, // MFeature
                0xA1, 0x00, // MCollection
                0xC0, // End MCollection
                0x05, 0x01, // GUsagePage
                0x15, 0x01, // GLogicalMin
                0x25, 0x02, // GLogicalMax
                0x35, 0x03, // GPhysicalMin
                0x45, 0x04, // GPhysicalMax
                0x55, 0x08, // GUnitExp
                0x65, 0x13, // GUnit
                0x75, 0x07, // GReportSize
                0x85, 0x08, // GReportId
                0x95, 0x09, // GReportCount
                0xA4, // GPush
                0xB4, // GPop
                0x09, 0x00, // LUsage
                0x19, 0x01, // LUsageMin
                0x29, 0x02, // LUsageMax
                0x39, 0x03, // LDesignatorIndex
                0x49, 0x04, // LDesignatorMin
                0x59, 0x05, // LDesignatorMax
                0x79, 0x07, // LStringIndex
                0x89, 0x08, // LStringMin
                0x99, 0x09, // LStringMax
                0xA9, 0x01, // LDelim
                0xA9, 0x00, // End LDelim
            ])
        );
    }

    #[test]
    fn u32_range() {
        use Item::LStringIndex;
        assert_eq!(
            ReportDescriptor::new([
                LStringIndex(u32::MIN),
                LStringIndex(u32::from(u8::MAX)),
                LStringIndex(u32::from(u8::MAX) + 1),
                LStringIndex(u32::from(u16::MAX)),
                LStringIndex(u32::from(u16::MAX) + 1),
                LStringIndex(u32::MAX),
            ]),
            // HID Descriptor Tool encodes the tag as 0x6X, which is a bug
            ReportDescriptor(vec![
                0x79, 0x00, // u32::MIN
                0x79, 0xFF, // u8::MAX
                0x7A, 0x00, 0x01, // u8::MAX + 1
                0x7A, 0xFF, 0xFF, // u16::MAX
                0x7B, 0x00, 0x00, 0x01, 0x00, // u16::MAX + 1
                0x7B, 0xFF, 0xFF, 0xFF, 0xFF, // u32::MAX
            ])
        );
    }

    #[test]
    fn i32_range() {
        use Item::GLogicalMin;
        assert_eq!(
            ReportDescriptor::new([
                GLogicalMin(-1),
                GLogicalMin(0),
                GLogicalMin(i32::from(i8::MIN)),
                GLogicalMin(i32::from(i8::MAX)),
                GLogicalMin(i32::from(i8::MIN) - 1),
                GLogicalMin(i32::from(i8::MAX) + 1),
                GLogicalMin(i32::from(i16::MIN)),
                GLogicalMin(i32::from(i16::MAX)),
                GLogicalMin(i32::from(i16::MIN) - 1),
                GLogicalMin(i32::from(i16::MAX) + 1),
                GLogicalMin(i32::MIN),
                GLogicalMin(i32::MAX),
            ]),
            ReportDescriptor(vec![
                0x15, 0xFF, // -1
                0x15, 0x00, // 0
                0x15, 0x80, // i8::MIN
                0x15, 0x7F, // i8::MAX
                0x16, 0x7F, 0xFF, // i8::MIN - 1
                0x16, 0x80, 0x00, // i8::MAX + 1
                0x16, 0x00, 0x80, // i16::MIN
                0x16, 0xFF, 0x7F, // i16::MAX
                0x17, 0xFF, 0x7F, 0xFF, 0xFF, // i16::MIN - 1
                0x17, 0x00, 0x80, 0x00, 0x00, // i16::MAX + 1
                0x17, 0x00, 0x00, 0x00, 0x80, // i32::MIN
                0x17, 0xFF, 0xFF, 0xFF, 0x7F, // i32::MAX
            ])
        );
    }

    #[test]
    fn iter() {
        use Item::*;
        let mut d = ReportDescriptor::new([
            GPush,
            GUsagePage(Page::GenericDesktop),
            LUsage(GenericDesktop::Keyboard as _),
            Collection::application([
                GLogicalMin(0),
                GLogicalMax(0x80),
                GPhysicalMin(0x7FFF),
                GPhysicalMax(0x8000),
                MInput(Flag::VAR),
            ]),
        ]);
        d.0.extend_from_slice(&[Tag::Long as u8 | 2, 6, 42, 0, 1, 2, 3, 4, 5, Tag::Pop as _]);
        let want = &[
            (Tag::Push, 0, 0),
            (Tag::UsagePage, 1, Page::GenericDesktop as _),
            (Tag::Usage, 1, GenericDesktop::Keyboard as _),
            (Tag::Collection, 1, Collection::Application as _),
            (Tag::LogicalMin, 1, 0),
            (Tag::LogicalMax, 2, 0x80),
            (Tag::PhysicalMin, 2, 0x7FFF),
            (Tag::PhysicalMax, 4, 0x8000),
            (Tag::Input, 1, u32::from(Flag::VAR.bits())),
            (Tag::EndCollection, 0, 0),
            (Tag::Long, 6, 42),
            (Tag::Pop, 0, 0),
        ];
        let mut it = d.iter();
        assert_eq!(want, (&mut it).collect::<Vec<_>>().as_slice());
        assert_eq!(it.0.len(), 0);
    }
}
