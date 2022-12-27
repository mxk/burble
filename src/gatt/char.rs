use crate::gap::Uuid;

use super::*;

struct CharDecl {
    hdl: Handle,
    props: CharProp,
    val_hdl: Handle,
    uuid: Uuid,
}

bitflags::bitflags! {
    /// Characteristic properties ([Vol 3] Part G, Section 3.3.1.1).
    #[derive(Default)]
    #[repr(transparent)]
    pub(super) struct CharProp: u8 {
        /// Permits broadcasts of the Characteristic Value using Server Characteristic Configuration
        /// Descriptor. If set, the Server Characteristic Configuration Descriptor shall exist.
        //const BROADCAST = 0x01,
        /// Permits reads of the Characteristic Value using procedures defined in Section 4.8.
        const READ = 0x02;
        /// Permit writes of the Characteristic Value without response using procedures defined in
        /// Section 4.9.1.
        const WRITE_CMD = 0x04;
        /// Permits writes of the Characteristic Value with response using procedures defined in
        /// Section 4.9.3 or Section 4.9.4.
        const WRITE_REQ = 0x08;
        /// Permits notifications of a Characteristic Value without acknowledgment using the
        /// procedure defined in Section 4.10. If set, the Client Characteristic Configuration
        /// Descriptor shall exist.
        const NOTIFY = 0x10;
        /// Permits indications of a Characteristic Value with acknowledgment using the procedure
        /// defined in Section 4.11. If set, the Client Characteristic Configuration Descriptor
        /// shall exist.
        const INDICATE = 0x20;
        /// Permits signed writes to the Characteristic Value using the procedure defined in Section
        /// 4.9.2.
        //const WRITE_SIGNED_CMD = 0x40,
        /// Additional characteristic properties are defined in the Characteristic Extended
        /// Properties Descriptor defined in Section 3.3.3.1. If set, the Characteristic Extended
        /// Properties Descriptor shall exist.
        const EXT_PROPS = 0x80;
    }
}
