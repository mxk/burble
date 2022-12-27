use bitflags::bitflags;

bitflags! {
    /// Characteristic properties ([Vol 3] Part G, Section 3.3.1.1).
    #[repr(transparent)]
    pub struct CharProps: u8 {
        /// Permits broadcasts of the Characteristic Value using Server
        /// Characteristic Configuration Descriptor. If set, the Server
        /// Characteristic Configuration Descriptor shall exist.
        const BROADCAST = 0x01;
        /// Permits reads of the Characteristic Value.
        const READ = 0x02;
        /// Permit writes of the Characteristic Value without response.
        const WRITE_WITHOUT_RESPONSE = 0x04;
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
        const AUTHENTICATED_SIGNED_WRITES = 0x40;
        /// Additional characteristic properties are defined in the
        /// Characteristic Extended Properties Descriptor. If set, the
        /// Characteristic Extended Properties Descriptor shall exist.
        const EXTENDED_PROPERTIES = 0x80;
    }
}

bitflags! {
    /// Client Characteristic Configuration descriptor value
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[repr(transparent)]
    pub struct CCCD: u8 {
        /// The Characteristic Value shall be notified. This value can only be
        /// set if the characteristic's properties have the `NOTIFY` bit set.
        const NOTIFY = 1 << 0;
        /// The Characteristic Value shall be indicated. This value can only be
        /// set if the characteristic's properties have the `INDICATE` bit set.
        const INDICATE = 1 << 0;
    }
}

/// Characteristic presentation format types ([Assigned Numbers] Section 2.4).
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    PartialEq,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u8)]
pub enum FmtType {
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
    /// IEEE 11073-20601 16-bit SFLOAT.
    MedF16 = 0x16,
    /// IEEE 11073-20601 32-bit FLOAT.
    MedF32 = 0x17,
    /// IEEE 11073-20601 nomenclature code.
    U16x2 = 0x18,
    /// UTF-8 string.
    Utf8 = 0x19,
    /// UTF-16 string.
    Utf16 = 0x1A,
    /// Opaque structure.
    Struct = 0x1B,
}

/// Characteristic presentation format units ([Assigned Numbers] Section 3.5).
#[derive(
    Clone,
    Copy,
    Debug,
    Eq,
    Ord,
    PartialEq,
    PartialOrd,
    num_enum::IntoPrimitive,
    num_enum::TryFromPrimitive,
    strum::Display,
)]
#[non_exhaustive]
#[repr(u16)]
pub enum Unit {
    None = 0x2700,
    Metres = 0x2701,
    Kilograms = 0x2702,
    Seconds = 0x2703,
    Amperes = 0x2704,
    Kelvins = 0x2705,
    Moles = 0x2706,
    Candelas = 0x2707,
    SquareMetres = 0x2710,
    CubicMetres = 0x2711,
    MetresPerSecond = 0x2712,
    MetresPerSecondSquared = 0x2713,
    ReciprocalMetres = 0x2714,
    KilogramsPerCubicMetre = 0x2715,
    KilogramsPerSquareMetre = 0x2716,
    CubicMetresPerKilogram = 0x2717,
    AmperesPerSquareMetre = 0x2718,
    AmperesPerMetre = 0x2719,
    MolesPerCubicMetre = 0x271A,
    KilogramsPerCubicMetreConcentration = 0x271B,
    CandelasPerSquareMetre = 0x271C,
    RefractiveIndex = 0x271D,
    RelativePermeability = 0x271E,
    Radians = 0x2720,
    Steradians = 0x2721,
    Hertz = 0x2722,
    Newtons = 0x2723,
    Pascals = 0x2724,
    Joules = 0x2725,
    Watts = 0x2726,
    Coulombs = 0x2727,
    Volts = 0x2728,
    Farads = 0x2729,
    Ohms = 0x272A,
    Siemens = 0x272B,
    Webers = 0x272C,
    Tesla = 0x272D,
    Henries = 0x272E,
    Celsius = 0x272F,
    Lumens = 0x2730,
    Lux = 0x2731,
    Becquerels = 0x2732,
    Grays = 0x2733,
    Sieverts = 0x2734,
    Katals = 0x2735,
    PascalSeconds = 0x2740,
    NewtonMetres = 0x2741,
    NewtonsPerMetre = 0x2742,
    RadiansPerSecond = 0x2743,
    RadiansPerSecondSquared = 0x2744,
    WattsPerSquareMetreFlux = 0x2745,
    JoulesPerKelvin = 0x2746,
    JoulesPerKilogramKelvin = 0x2747,
    JoulesPerKilogram = 0x2748,
    WattsPerMetreKelvin = 0x2749,
    JoulesPerCubicMetre = 0x274A,
    VoltsPerMetre = 0x274B,
    CoulombsPerCubicMetre = 0x274C,
    CoulombsPerSquareMetre = 0x274D,
    CoulombsPerSquareMetreFlux = 0x274E,
    FaradsPerMetre = 0x274F,
    HenriesPerMetre = 0x2750,
    JoulesPerMole = 0x2751,
    JoulesPerMoleKelvin = 0x2752,
    CoulombsPerKilogram = 0x2753,
    GraysPerSecond = 0x2754,
    WattsPerSteradian = 0x2755,
    WattsPerSquareMetreSteradian = 0x2756,
    KatalsPerCubicMetre = 0x2757,
    Minutes = 0x2760,
    Hours = 0x2761,
    Days = 0x2762,
    Degrees = 0x2763,
    DegreeMinutes = 0x2764,
    DegreeSeconds = 0x2765,
    Hectares = 0x2766,
    Litres = 0x2767,
    Tonnes = 0x2768,
    Bars = 0x2780,
    MillimetresOfMercury = 0x2781,
    Angstroms = 0x2782,
    NauticalMiles = 0x2783,
    Barns = 0x2784,
    Knots = 0x2785,
    Nepers = 0x2786,
    Bels = 0x2787,
    Yards = 0x27A0,
    Parsecs = 0x27A1,
    Inches = 0x27A2,
    Feet = 0x27A3,
    Miles = 0x27A4,
    PoundsPerSquareInch = 0x27A5,
    KilometresPerHour = 0x27A6,
    MilesPerHour = 0x27A7,
    RevolutionsPerMinute = 0x27A8,
    GramCalories = 0x27A9,
    KilogramCalories = 0x27AA,
    KilowattHours = 0x27AB,
    Fahrenheit = 0x27AC,
    Percent = 0x27AD,
    PerMille = 0x27AE,
    BeatsPerMinute = 0x27AF,
    AmpereHours = 0x27B0,
    MilligramsPerDecilitre = 0x27B1,
    MillimolesPerLitre = 0x27B2,
    Years = 0x27B3,
    Months = 0x27B4,
    CountsPerCubicMetre = 0x27B5,
    WattsPerSquareMetre = 0x27B6,
    MillilitersPerKilogramPerMinute = 0x27B7,
    Pounds = 0x27B8,
    MetabolicEquivalent = 0x27B9,
    StepsPerMinute = 0x27BA,
    StrokesPerMinute = 0x27BC,
    KilometresPerMinute = 0x27BD,
    LumensPerWatt = 0x27BE,
    LumenHours = 0x27BF,
    LuxHours = 0x27C0,
    GramsPerSecond = 0x27C1,
    LitresPerSecond = 0x27C2,
    Decibels = 0x27C3,
    PartsPerMillion = 0x27C4,
    PartsPerBillion = 0x27C5,
    MilligramsPerDecilitrePerMinute = 0x27C6,
    KilovoltAmpereHours = 0x27C7,
    VoltAmperes = 0x27C8,
}
