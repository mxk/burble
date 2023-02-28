use std::ops::{Deref, DerefMut};

use bitflags::bitflags;
use smallvec::SmallVec;
use structbuf::{Pack, Packer, StructBuf};

use burble_const::UuidPacker;

use super::*;

/// Database service definition marker type.
#[derive(Debug)]
pub struct ServiceDef;

/// Database characteristic definition marker type.
#[derive(Debug)]
pub struct CharacteristicDef;

/// Database include definition marker type.
#[derive(Debug)]
pub struct IncludeDef;

/// Database descriptor definition marker type.
#[derive(Debug)]
pub struct DescriptorDef;

/// Database builder used to define services, characteristics, and descriptors.
#[derive(Debug)]
#[repr(transparent)]
pub struct Builder<T>(DbBuilder, PhantomData<T>);

impl<T> Builder<T> {
    /// Creates a generic attribute with an externally stored value.
    fn attr(&mut self, typ: Uuid, perms: Perms) -> Handle {
        let typ = self.0.morph(typ);
        let typ16 = typ.as_uuid16();
        if typ16.is_none() {
            self.append_data(u128::from(typ).to_le_bytes());
        }
        let hdl = self.next_handle();
        self.append_attr(hdl, typ16, perms)
    }
}

impl<T> Deref for Builder<T> {
    type Target = DbBuilder;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Builder<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Builder<Db> {
    /// Creates a new database builder.
    #[inline]
    #[must_use]
    pub(super) fn new() -> Self {
        Self(
            DbBuilder {
                attr: Vec::with_capacity(128),
                data: Vec::with_capacity(512),
                ..DbBuilder::default()
            },
            PhantomData,
        )
    }

    /// Returns the final read-only database and I/O map.
    #[inline]
    #[must_use]
    pub(in crate::gatt) fn freeze(mut self) -> (Db, IoMap) {
        let hash = self.calc_hash().to_le_bytes();
        let hash = self.append_data(hash);
        for at in &mut self.0.attr {
            if matches!(at.typ, Some(Characteristic::DATABASE_HASH)) {
                at.val = hash;
                break; // Only one instance is allowed
            }
        }
        (
            Db {
                attr: self.0.attr.into_boxed_slice(),
                data: self.0.data.into_boxed_slice(),
            },
            IoMap(self.0.io),
        )
    }

    /// Defines a primary service ([Vol 3] Part G, Section 3.1).
    ///
    /// 16-bit UUID services should precede 128-bit ones.
    #[inline]
    pub fn primary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        include: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceDef>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Declaration::PrimaryService, uuid.into(), include.as_ref());
        self.service_chars(hdl, chars)
    }

    /// Defines a secondary service ([Vol 3] Part G, Section 3.1).
    ///
    /// 16-bit UUID services should precede 128-bit ones.
    #[inline]
    pub fn secondary_service<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        include: impl AsRef<[Handle]>,
        chars: impl FnOnce(&mut Builder<ServiceDef>) -> T,
    ) -> (Handle, T) {
        let hdl = self.service(Declaration::SecondaryService, uuid.into(), include.as_ref());
        self.service_chars(hdl, chars)
    }

    /// Morphs all UUIDs of the next service to allow debugging otherwise
    /// protected services on the client.
    #[cfg(debug_assertions)]
    #[inline(always)]
    pub fn morph_next(&mut self) {
        self.0.flag.insert(Bld::MORPH);
    }

    /// Declares a primary or secondary service and any included services
    /// ([Vol 3] Part G, Section 3.2).
    fn service(&mut self, typ: Declaration, uuid: Uuid, include: &[Handle]) -> Handle {
        let uuid = self.morph(uuid);
        if let Some((UuidType::Service(s), uuid16)) = uuid.as_uuid16().map(|u| (u.typ(), u)) {
            assert!(
                s.multi_instance() || !self.attr.iter().any(|at| at.typ == Some(uuid16)),
                "only one instance of the {s} service is allowed"
            );
        }
        let hdl = self.decl(typ, |v| v.uuid(uuid));
        for &inc in include {
            let s = self.service_group(inc).expect("invalid service handle");
            let uuid = (s.first().len() == 2).then(|| self.value(s.first()).unpack().u16());
            let end = s.last().hdl;
            self.decl(Declaration::Include, |v| {
                v.u16(inc).u16(end);
                uuid.map(|u| v.u16(u));
            });
        }
        hdl
    }

    /// Calls `f` to define service characteristics.
    #[inline(always)]
    fn service_chars<T>(
        &mut self,
        hdl: Handle,
        f: impl FnOnce(&mut Builder<ServiceDef>) -> T,
    ) -> (Handle, T) {
        let v = f(self.builder());
        self.0.flag.remove(Bld::MORPH);
        (hdl, v)
    }

    /// Calculates the database hash ([Vol 3] Part G, Section 7.3.1).
    #[must_use]
    fn calc_hash(&self) -> u128 {
        use Descriptor::*;
        let mut m = burble_crypto::AesCmac::db_hash();
        for at in &self.attr {
            let Some(typ) = at.typ else { continue };
            let val = match typ.typ() {
                UuidType::Declaration(_)
                | UuidType::Descriptor(CharacteristicExtendedProperties) => self.value(at),
                UuidType::Descriptor(
                    CharacteristicUserDescription
                    | ClientCharacteristicConfiguration
                    | ServerCharacteristicConfiguration
                    | CharacteristicPresentationFormat
                    | CharacteristicAggregateFormat,
                ) => &[],
                _ => continue,
            };
            m.update(u16::from(at.hdl).to_le_bytes())
                .update(u16::from(typ).to_le_bytes())
                .update(val);
        }
        m.finalize()
    }
}

impl Builder<ServiceDef> {
    /// Defines a single-value characteristic ([Vol 3] Part G, Section 3.3).
    ///
    /// Mandatory service characteristics must precede optional ones and 16-bit
    /// UUID characteristics should precede 128-bit ones.
    #[inline]
    pub fn characteristic<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        props: Prop,
        perms: impl Into<Perms>,
        io: impl Into<Io>,
        descs: impl FnOnce(&mut Builder<CharacteristicDef>) -> T,
    ) -> (Handle, T) {
        let hdl = self.decl_value(uuid.into(), props, perms.into());
        self.io.insert(hdl, io.into());
        let mut flag = Bld::empty();
        flag.set(Bld::NEED_EXT_PROPS, props.contains(Prop::EXT_PROPS));
        let b = self.builder(flag);
        let v = descs(b);
        b.finalize();
        (hdl, v)
    }

    /// Defines a read-only characteristic with a database-stored value
    /// ([Vol 3] Part G, Section 3.3).
    #[inline]
    pub fn ro_characteristic<T>(
        &mut self,
        uuid: impl Into<Uuid>,
        perms: impl Into<Perms>,
        val: impl AsRef<[u8]>,
        descs: impl FnOnce(&mut Builder<CharacteristicDef>) -> T,
    ) -> T {
        self.decl_value(uuid.into(), Prop::READ, perms.into());
        self.append_val(val);
        let b = self.builder(Bld::empty());
        let v = descs(b);
        b.finalize();
        v
    }

    // TODO: Method for multi-value characteristics

    /// Adds characteristic and characteristic value declarations.
    fn decl_value(&mut self, uuid: Uuid, props: Prop, perms: Perms) -> Handle {
        let uuid = self.0.morph(uuid);
        let val_hdl = self.next_handle().next().expect("maximum handle reached");
        self.decl(Declaration::Characteristic, |v| {
            v.u8(props.bits()).u16(val_hdl).uuid(uuid);
        });
        self.append_attr(val_hdl, uuid.as_uuid16(), perms)
    }

    /// Returns a new descriptor builder.
    fn builder(&mut self, flag: Bld) -> &mut Builder<CharacteristicDef> {
        self.flag = flag;
        // SAFETY: 0 is a valid length and there is nothing to drop
        unsafe { self.fmt.set_len(0) };
        self.0.builder()
    }
}

impl Builder<CharacteristicDef> {
    /// Declares a non-GATT profile characteristic descriptor
    /// ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn descriptor(
        &mut self,
        uuid: impl Into<Uuid>,
        perms: impl Into<Perms>,
        io: impl Into<Io>,
    ) -> Handle {
        let perms = perms.into();
        let hdl = self.attr(uuid.into(), perms);
        self.io.insert(hdl, io.into());
        hdl
    }

    /// Declares a read-only non-GATT profile characteristic descriptor with the
    /// value stored in the database ([Vol 3] Part G, Section 3.3.3).
    #[inline]
    pub fn ro_descriptor(
        &mut self,
        uuid: impl Into<Uuid>,
        perms: impl Into<Perms>,
        val: impl AsRef<[u8]>,
    ) {
        self.attr(uuid.into(), perms.into());
        self.append_val(val);
    }

    /// Declares a Characteristic Extended Properties descriptor
    /// ([Vol 3] Part G, Section 3.3.3.1).
    ///
    /// This descriptor will be added automatically if the characteristic
    /// properties contain `EXT_PROPS` flag.
    pub fn ext_props(&mut self, props: ExtProp) {
        assert!(
            self.flag.contains(Bld::NEED_EXT_PROPS),
            "EXT_PROPS not set or descriptor already exists"
        );
        self.flag.remove(Bld::NEED_EXT_PROPS);
        self.decl(Descriptor::CharacteristicExtendedProperties, |v| {
            v.u16(props.bits());
        });
    }

    /// Declares a Client Characteristic Configuration descriptor
    /// ([Vol 3] Part G, Section 3.3.3.3).
    #[inline]
    pub fn client_cfg(&mut self, perms: impl Into<Perms>) -> Handle {
        assert!(
            !self.flag.contains(Bld::HAVE_CCCD),
            "descriptor already exists"
        );
        self.flag.insert(Bld::HAVE_CCCD);
        self.attr(
            Descriptor::ClientCharacteristicConfiguration.uuid(),
            perms.into(),
        )
    }

    /// Declares a Characteristic Presentation Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.5).
    pub fn presentation_fmt(
        &mut self,
        fmt: Format,
        exp: i8,
        unit: Unit,
        desc: Description,
    ) -> Handle {
        let hdl = self.decl(Descriptor::CharacteristicPresentationFormat, |v| {
            v.u8(fmt).i8(exp).u16(unit).u8(desc.ns()).u16(desc);
        });
        self.fmt.push(hdl);
        hdl
    }

    /// Declares a Characteristic Aggregate Format descriptor
    /// ([Vol 3] Part G, Section 3.3.3.6).
    ///
    /// This descriptor will be added automatically when more than one
    /// Presentation Format descriptor is present.
    pub fn aggregate_fmt(&mut self, hdls: impl AsRef<[Handle]>) {
        assert!(
            !self.flag.contains(Bld::HAVE_AGGREGATE_FMT),
            "descriptor already exists"
        );
        self.flag.insert(Bld::HAVE_AGGREGATE_FMT);
        self.decl(Descriptor::CharacteristicAggregateFormat, |v| {
            for &hdl in hdls.as_ref() {
                v.u16(hdl);
            }
        });
    }

    /// Finalizes characteristic definition by adding required descriptors.
    fn finalize(&mut self) {
        // TODO: Add CCCD if NOTIFY or INDICATE properties are set
        if self.flag.contains(Bld::NEED_EXT_PROPS) {
            self.ext_props(ExtProp::empty());
        }
        if !self.flag.contains(Bld::HAVE_AGGREGATE_FMT) && self.fmt.len() > 1 {
            let fmt = mem::take(&mut self.fmt);
            self.aggregate_fmt(&fmt);
            self.fmt = fmt; // Preserve any allocated capacity
        }
    }
}

bitflags! {
    /// Builder flags.
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Bld: u8 {
        const MORPH = 1 << 0;
        const NEED_EXT_PROPS = 1 << 1;
        const HAVE_CCCD = 1 << 2;
        const HAVE_AGGREGATE_FMT = 1 << 3;
    }
}

/// Shared [`Db`] builder state.
#[derive(Debug, Default)]
pub struct DbBuilder {
    attr: Vec<Attr>,
    data: Vec<u8>,
    io: BTreeMap<Handle, Io>,
    flag: Bld,
    fmt: SmallVec<[Handle; 4]>,
}

impl DbBuilder {
    /// Creates a read-only GATT profile declaration with value set by `val`.
    #[inline]
    fn decl(&mut self, typ: impl Into<Uuid16>, val: impl FnOnce(&mut Packer)) -> Handle {
        fn append_attr(this: &mut DbBuilder, typ: Uuid16, val: &[u8]) -> Handle {
            let hdl = this.next_handle();
            let val = this.append_data(val);
            this.attr.push(Attr {
                hdl,
                typ: Some(typ),
                val,
                perms: Perms::new(Access::READ),
            });
            hdl
        }
        // Maximum length of the Characteristic declaration value, which is the
        // longest value stored in the database ([Vol 3] Part G, Section 3.3.1).
        let mut b = StructBuf::new(1 + 2 + 16);
        val(&mut b.append());
        append_attr(self, typ.into(), &b)
    }

    /// Returns the next unused handle.
    #[inline]
    fn next_handle(&self) -> Handle {
        self.attr.last().map_or(Handle::MIN, |at| {
            at.hdl.next().expect("maximum handle reached")
        })
    }

    /// Appends a new attribute entry. If `typ == None`, then the last 16 data
    /// bytes must contain the 128-bit UUID.
    #[inline]
    fn append_attr(&mut self, hdl: Handle, typ: Option<Uuid16>, perms: Perms) -> Handle {
        #[allow(clippy::cast_possible_truncation)]
        let i = match typ {
            None => self.data.len() as Idx,
            Some(_) => 0,
        };
        self.attr.push(Attr {
            hdl,
            typ,
            val: (i, i),
            perms,
        });
        hdl
    }

    /// Appends a read-only value for the last attribute entry.
    #[inline]
    fn append_val(&mut self, v: impl AsRef<[u8]>) {
        self.attr.last_mut().expect("empty database").val = self.append_data(v);
    }

    /// Appends `v` to the database and returns the resulting index range.
    #[inline]
    fn append_data(&mut self, v: impl AsRef<[u8]>) -> (Idx, Idx) {
        #[allow(clippy::cast_possible_truncation)]
        let start = self.data.len() as Idx;
        self.data.extend_from_slice(v.as_ref());
        let end = Idx::try_from(self.data.len())
            .expect("database data overflow (see Idx type in gatt/db.rs)");
        (start, end)
    }

    /// Returns a new builder.
    #[inline(always)]
    fn builder<T>(&mut self) -> &mut Builder<T> {
        // SAFETY: Builder is a `repr(transparent)` onetype
        unsafe { &mut *(self as *mut Self).cast() }
    }

    /// Returns the possibly morphed UUID.
    #[inline(always)]
    fn morph(&self, u: impl Into<Uuid>) -> Uuid {
        let u = u.into();
        if !cfg!(debug_assertions) || !self.flag.contains(Bld::MORPH) {
            return u;
        }
        // SAFETY: `u` is a non-zero 16-bit UUID
        (u.as_u16()).map_or(u, |u| unsafe { Uuid::new_unchecked(u128::from(u) << 96) })
    }
}

impl CommonOps for DbBuilder {
    #[inline(always)]
    fn attr(&self) -> &[Attr] {
        &self.attr
    }

    #[inline(always)]
    fn data(&self) -> &[u8] {
        &self.data
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    #[test]
    fn service_group() {
        fn eq(b: &DbBuilder, h: Handle, r: Range<usize>) {
            let s = b.service_group(h).unwrap();
            assert_eq!(s.off..s.off + s.attr.len(), r);
        }

        let mut db = Db::build();
        let (h1, _) = db.primary_service(Service::GenericAccess, [], |_| {});
        let (h2, _) = db.primary_service(Service::GenericAttribute, [h1], |_| {});
        eq(&db, h1, 0..1);
        eq(&db, h2, 1..3);

        let (h3, _) = db.primary_service(Service::Battery, [], |_| {});
        eq(&db, h2, 1..3);
        eq(&db, h3, 3..4);
    }

    /// Example database hash ([Vol 3] Part G, Appendix B).
    #[test]
    fn hash() {
        assert_eq!(
            appendix_b().hash(),
            0xF1_CA_2D_48_EC_F5_8B_AC_8A_88_30_BB_B9_FB_A9_90
        );
    }

    #[test]
    fn primary_services() {
        use Service::*;
        let mut s = appendix_b();

        let mut it = s.primary_services(Handle::MIN, None);
        group_eq(it.next(), 0x0001, 0x0005, GenericAccess);
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        group_eq(it.next(), 0x000E, 0x0013, Glucose);
        assert!(it.next().is_none());
        assert!(it.next().is_none());
        drop(it);

        let mut it = s.primary_services(Handle::MIN, Some(GenericAttribute.uuid()));
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        assert!(it.next().is_none());
        drop(it);

        // Remove the battery service
        let mut v = s.attr.to_vec();
        v.truncate(s.attr.len() - 3);
        s.attr = v.into_boxed_slice();

        let mut it = s.primary_services(Handle::new(0x0002).unwrap(), None);
        group_eq(it.next(), 0x0006, 0x000D, GenericAttribute);
        group_eq(it.next(), 0x000E, 0x0013, Glucose);
        assert!(it.next().is_none());
    }

    #[test]
    fn characteristics() {
        use Characteristic::*;

        let s = appendix_b();
        let mut pri = s.primary_services(Handle::MIN, None);

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0002, 0x0003, DeviceName);
        group_eq(it.next(), 0x0004, 0x0005, Appearance);
        assert!(it.next().is_none());

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0007, 0x0009, ServiceChanged);
        group_eq(it.next(), 0x000A, 0x000B, ClientSupportedFeatures);
        group_eq(it.next(), 0x000C, 0x000D, DatabaseHash);
        assert!(it.next().is_none());

        let mut it = s.characteristics(pri.next().unwrap().handle_range());
        group_eq(it.next(), 0x0010, 0x0013, GlucoseMeasurement);
        assert!(it.next().is_none());
    }

    #[test]
    fn descriptors() {
        let s = appendix_b();

        let mut pri = s.primary_services(Handle::new(0x0006).unwrap(), None);
        let mut chars = s.characteristics(pri.next().unwrap().handle_range());

        let hdl = chars.next().unwrap().value_handle().next().unwrap();
        let mut it = s.descriptors(HandleRange::new(hdl, hdl));
        let v = it.next().unwrap();
        assert_eq!(v.handle(), hdl);
        assert_eq!(
            v.uuid(),
            Descriptor::ClientCharacteristicConfiguration.uuid()
        );
        assert!(it.next().is_none());

        let mut it = s.descriptors(HandleRange::new(Handle::new(0x0008).unwrap(), hdl));
        assert!(it.next().is_none());
    }

    fn group_eq<T: Group>(v: Option<DbEntry<T>>, decl: u16, end: u16, uuid: impl Into<Uuid16>) {
        let v = v.unwrap();
        assert_eq!(
            v.handle_range(),
            HandleRange::new(Handle::new(decl).unwrap(), Handle::new(end).unwrap())
        );
        assert_eq!(v.uuid(), uuid.into().as_uuid());
    }

    fn appendix_b() -> Db {
        let mut db = Db::build();
        db.primary_service(Service::GenericAccess, [], |db| {
            db.characteristic(
                Characteristic::DeviceName,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                Io::NONE,
                |_| {},
            );
            db.characteristic(
                Characteristic::Appearance,
                Prop::READ,
                Access::READ,
                Io::NONE,
                |_| {},
            )
        });
        db.primary_service(Service::GenericAttribute, [], |db| {
            db.characteristic(
                Characteristic::ServiceChanged,
                Prop::INDICATE,
                Access::NONE,
                Io::NONE,
                |db| db.client_cfg(Access::READ_WRITE),
            );
            db.characteristic(
                Characteristic::ClientSupportedFeatures,
                Prop::READ | Prop::WRITE,
                Access::READ_WRITE,
                Io::NONE,
                |_| {},
            );
            db.characteristic(
                Characteristic::DatabaseHash,
                Prop::READ,
                Access::READ,
                Io::NONE,
                |_| {},
            );
        });
        db.primary_service(Service::Glucose, [], |db| {
            // Hack to include a service that hasn't been defined yet
            db.decl(Declaration::Include, |v| {
                v.u16(0x0014_u16).u16(0x0016_u16).u16(0x180F_u16);
            });
            db.characteristic(
                Characteristic::GlucoseMeasurement,
                Prop::READ | Prop::INDICATE | Prop::EXT_PROPS,
                Access::READ,
                Io::NONE,
                |db| db.client_cfg(Access::READ_WRITE),
            );
        });
        db.secondary_service(Service::Battery, [], |db| {
            db.characteristic(
                Characteristic::BatteryLevel,
                Prop::READ,
                Access::READ,
                Io::NONE,
                |_| {},
            );
        });
        let (db, _) = db.freeze();
        db
    }
}
