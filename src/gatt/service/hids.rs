//! Human Interface Device Service ([HIDS]).
//!
//! This service exposes HID reports and other data intended for HID hosts.
//!
//! The HID-over-GATT Profile ([HOGP]) defines how a device can support HID
//! services over the Bluetooth LE protocol stack using the Generic Attribute
//! Profile.
//!
//! [HIDS]: https://www.bluetooth.com/specifications/specs/human-interface-device-service-1-0/
//! [HOGP]: https://www.bluetooth.com/specifications/specs/hid-over-gatt-profile-1-0/

use std::collections::BTreeMap;
use std::sync::Arc;

use tracing::debug;

use burble_const::{Characteristic, Descriptor, Service};
use burble_hid::descriptor::{Locale, ReportDescriptor, Tag};
use burble_hid::kbd::{Keyboard, Led};
use burble_hid::mouse::Mouse;
use burble_hid::usage::{GenericDesktop, Page};
use burble_hid::{Device, Report, ReportType};

use crate::att::{Access, ErrorCode};
use crate::gatt::{Builder, Db, Io, IoReq, IoResult, Notify, NotifyReq, Prop};
use crate::SyncMutex;

/// HID-over-GATT service.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct HidService<T>(Arc<SyncMutex<Dev<T>>>);

impl<T: Device + Send + 'static> HidService<T> {
    /// Creates an HID service for the specified device.
    #[inline]
    #[must_use]
    pub fn new(dev: T) -> Self {
        Self(Dev::new(dev))
    }

    /// Calls `f` to control the device. Any updates are not reported to the
    /// remote host until [`Self::flush()`] is called.
    #[inline(always)]
    pub fn control<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(&mut self.0.lock().dev)
    }

    /// Sends all pending reports to the host. The current implementation also
    /// detects connection loss during this call.
    pub async fn flush(&self) {
        // TODO: Use a separate task to detect connection loss
        let next = || self.0.lock().next();
        while let Some(ntf) = next() {
            if let Err(e) = ntf.await {
                debug!("Notify error: {e}");
            }
        }
    }

    /// Defines the service structure.
    ///
    /// # Panics
    ///
    /// Panics if the report descriptor is too long.
    pub fn define(&self, db: &mut Builder<Db>) {
        const RO: Access = Access::READ.authn().encrypt();
        const WO: Access = Access::WRITE.authn().encrypt();
        const RW: Access = Access::READ_WRITE.authn().encrypt();
        db.primary_service(Service::HumanInterfaceDevice, [], |db| {
            use Characteristic::*;
            use GenericDesktop::{Keyboard, Mouse};
            let rd = self.0.lock().dev.descriptor();

            // HID Information ([HIDS] Section 2.10)
            let info = Self::hid_info(0x01_11, Locale::None, true, false);
            db.ro_characteristic(HidInformation, RO, info, |_| {});

            // Report Map ([HIDS] Section 2.6)
            assert!(rd.as_ref().len() <= 512, "report descriptor too long");
            db.ro_characteristic(ReportMap, RO, &rd, |_| {});

            // HID Control Point ([HIDS] Section 2.11)
            db.characteristic(
                HidControlPoint,
                Prop::WRITE_CMD,
                WO,
                Io::with(&self.0, |this, req| this.lock().control_point_io(req)),
                |_| {},
            );

            // Reports ([HIDS] Section 2.5, 2.7, 2.8, 2.9)
            for (key, app) in Self::reports(&rd, None)
                .chain(Self::reports(&rd, Some(Keyboard)))
                .chain(Self::reports(&rd, Some(Mouse)))
            {
                use ReportType::*;
                let uuid = match (app, key.typ) {
                    (None, _) => Report,
                    (Some(Keyboard), Input) => BootKeyboardInputReport,
                    (Some(Keyboard), Output) => BootKeyboardOutputReport,
                    (Some(Mouse), Input) => BootMouseInputReport,
                    _ => continue,
                };
                let (props, perms) = match key.typ {
                    Input => (Prop::NOTIFY, RO), // TODO: Optional write?
                    Output => (Prop::WRITE_CMD.union(Prop::WRITE), RW),
                    Feature => (Prop::WRITE, RW),
                };
                db.characteristic(
                    uuid,
                    Prop::READ.union(props),
                    perms,
                    Io::with(&self.0, move |this, req| this.lock().report_io(key, req)),
                    |db| {
                        if key.typ.is_input() {
                            db.cccd(RW);
                        }
                        if matches!(uuid, Report) {
                            db.ro_descriptor(
                                Descriptor::ReportReference,
                                RO,
                                [key.id, key.typ as _],
                            );
                        }
                    },
                );
            }

            // Protocol Mode ([HIDS] Section 2.4)
            if self.0.lock().dev.is_boot_mode().is_some() {
                db.characteristic(
                    ProtocolMode,
                    Prop::READ.union(Prop::WRITE_CMD),
                    RW,
                    Io::with(&self.0, |this, req| this.lock().protocol_mode_io(req)),
                    |_| {},
                );
            }
        });
    }

    /// Returns HID information characteristic value (\[HIDS\] Section 2.10).
    #[allow(clippy::cast_possible_truncation)]
    #[inline]
    #[must_use]
    const fn hid_info(ver: u16, loc: Locale, norm_conn: bool, wake: bool) -> [u8; 4] {
        let mut info = [0; 4];
        (info[0], info[1]) = (ver as u8, (ver >> 8) as u8); // bcdHID
        info[2] = loc as _; // bCountryCode
        info[3] = (norm_conn as u8) << 1 | wake as u8; // Flags
        info
    }

    /// Returns an iterator over all report types in the descriptor. If `col` is
    /// [`Some`], then only the reports in the specified collection are
    /// returned.
    fn reports(
        rd: &ReportDescriptor,
        col: Option<GenericDesktop>,
    ) -> impl Iterator<Item = (Key, Option<GenericDesktop>)> + '_ {
        let want = (Page::GenericDesktop as u32) << 16 | col.map_or(0xFFFF, |u| u as _);
        let (mut depth, mut app_match) = (0, if col.is_some() { 0 } else { usize::MAX });
        let (mut page, mut usage) = (0, 0);
        let mut report_id = 0;
        rd.iter().filter_map(move |(t, n, v)| {
            match t {
                Tag::Input | Tag::Output | Tag::Feature => {
                    if app_match != 0 {
                        let k = Key {
                            typ: t.as_report_type(),
                            id: report_id,
                            boot: col.is_some(),
                        };
                        return Some((k, col));
                    }
                }
                Tag::Collection => {
                    depth += 1;
                    if app_match == 0 && usage == want {
                        app_match = depth;
                    }
                }
                Tag::EndCollection => {
                    if app_match == depth {
                        app_match = 0;
                    }
                    depth -= 1;
                }
                Tag::UsagePage => page = v << 16,
                Tag::ReportId => report_id = u8::try_from(v).expect("invalid report ID"),
                Tag::Usage => usage = if n > 2 { v } else { page | v },
                _ => {}
            }
            if t.is_main() {
                usage = 0;
            }
            None
        })
    }
}

/// Report identification key. `id` is [`None`] for boot reports.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Key {
    typ: ReportType,
    id: u8,
    boot: bool,
}

/// HID device service state.
#[derive(Debug)]
struct Dev<T> {
    dev: T,
    ntf: BTreeMap<Key, NotifyReq>,
    w: tokio::sync::watch::Sender<DeviceState>,
}

impl<T: Device> Dev<T> {
    /// Creates HID service state for the specified device.
    #[inline]
    #[must_use]
    fn new(dev: T) -> Arc<SyncMutex<Self>> {
        let ntf = BTreeMap::new();
        let (w, _) = tokio::sync::watch::channel(DeviceState(Flag::empty()));
        Arc::new(SyncMutex::new(Self { dev, ntf, w }))
    }

    /// Handles Protocol Mode characteristic I/O (\[HIDS\] Section 2.4).
    fn protocol_mode_io(&mut self, req: IoReq) -> IoResult {
        match req {
            IoReq::Read(r) => r.complete([u8::from(!self.dev.is_boot_mode().unwrap_or_default())]),
            IoReq::Write(w) => {
                let mut v = [0; 1];
                w.update(&mut v)?;
                let boot = v[0] == 0;
                if self.dev.is_boot_mode().map_or(false, |b| b != boot) {
                    self.dev.set_boot_mode(boot);
                    self.w.send_modify(|s| s.0.set(Flag::BOOT, boot));
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    /// Handles control point characteristic I/O (\[HIDS\] Section 2.11).
    #[allow(clippy::needless_pass_by_value)]
    fn control_point_io(&mut self, req: IoReq) -> IoResult {
        let IoReq::Write(w) = req else { unreachable!() };
        let mut v = [0; 1];
        w.update(&mut v)?;
        let suspend = v[0] == 0;
        self.w.send_if_modified(|s| {
            let diff = s.0.contains(Flag::SUSPEND) != suspend;
            if diff {
                s.0.toggle(Flag::SUSPEND);
            }
            diff
        });
        Ok(())
    }

    /// Handles report characteristic I/O.
    fn report_io(&mut self, k: Key, req: IoReq) -> IoResult {
        match req {
            IoReq::Read(r) => {
                if Some(k.boot) != self.dev.is_boot_mode() {
                    return Err(ErrorCode::UnlikelyError);
                }
                (self.dev.get_report(k.typ, k.id))
                    .map_or(Err(ErrorCode::RequestNotSupported), |v| r.complete(v))
            }
            IoReq::Write(w) => {
                if w.off != 0 {
                    // TODO: Update current report from get_report?
                    return Err(ErrorCode::InvalidOffset);
                }
                if !self.dev.set_report(Report::new(k.typ, k.id, w.val)) {
                    return Err(ErrorCode::ValueNotAllowed);
                }
                Ok(())
            }
            IoReq::Notify(n) => {
                if self.ntf.is_empty() {
                    self.w.send_modify(|s| s.0 |= Flag::CONN);
                }
                self.ntf.insert(k, n);
                Ok(())
            }
        }
    }
}

impl<T: Device> Iterator for Dev<T> {
    type Item = Notify;

    fn next(&mut self) -> Option<Self::Item> {
        self.ntf.retain(|_, n| !n.is_closed());
        if self.ntf.is_empty() {
            self.dev.reset();
            self.w.send_if_modified(|s| {
                let diff = s.0.contains(Flag::CONN) != self.ntf.is_empty();
                if diff {
                    s.0.toggle(Flag::CONN);
                }
                diff
            });
            return None;
        }
        while let Some(r) = self.dev.next() {
            let key = Key {
                typ: r.typ(),
                id: r.id(),
                boot: self.dev.is_boot_mode().unwrap_or_default(),
            };
            if let Some(n) = self.ntf.get(&key) {
                return Some(n.notify(|p| {
                    p.put(r.as_ref());
                }));
            }
        }
        None
    }
}

/// HID device state.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct DeviceState(Flag);

bitflags::bitflags! {
    /// HID device state flags.
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    struct Flag: u8 {
        const CONN = 1 << 0;
        const BOOT = 1 << 1;
        const SUSPEND = 1 << 2;
    }
}

impl DeviceState {
    /// Returns whether the device is connected to a host as determined by
    /// having at least one active notification session.
    #[inline(always)]
    #[must_use]
    pub const fn is_connected(self) -> bool {
        self.0.contains(Flag::CONN)
    }

    /// Returns whether the device is using boot protocol mode.
    #[inline(always)]
    #[must_use]
    pub const fn is_boot_mode(self) -> bool {
        self.0.contains(Flag::BOOT)
    }

    /// Returns whether the host entered suspend state.
    #[inline(always)]
    #[must_use]
    pub const fn is_suspended(self) -> bool {
        self.0.contains(Flag::SUSPEND)
    }
}

/// Combined keyboard and mouse HID device.
#[derive(Debug)]
pub struct KeyboardMouse {
    k: Keyboard,
    m: Mouse,
    w: tokio::sync::watch::Sender<Led>,
}

impl KeyboardMouse {
    /// Creates a new keyboard/mouse device.
    ///
    /// # Panics
    ///
    /// Panics if the device report IDs are the same.
    #[inline]
    #[must_use]
    pub fn new(k: Keyboard, m: Mouse) -> Self {
        assert_ne!(k.report_id(), m.report_id(), "report ID collision");
        let (w, _) = tokio::sync::watch::channel(Led::empty());
        Self { k, m, w }
    }

    /// Returns a mutable keyboard reference.
    #[inline(always)]
    #[must_use]
    pub fn kbd(&mut self) -> &mut Keyboard {
        &mut self.k
    }

    /// Returns a mutable mouse reference.
    #[inline(always)]
    #[must_use]
    pub fn mouse(&mut self) -> &mut Mouse {
        &mut self.m
    }

    /// Returns a watch receiver that reflects keyboard LED changes.
    #[inline(always)]
    #[must_use]
    pub fn led(&self) -> tokio::sync::watch::Receiver<Led> {
        self.w.subscribe()
    }

    /// Performs approximate fair scheduling between `a` and `b`, returning
    /// whether `a` should be selected (i.e. decremented) next.
    #[allow(clippy::cast_possible_wrap)]
    #[inline]
    #[must_use]
    const fn select(a: usize, b: usize) -> bool {
        let na = usize::BITS.saturating_sub(a.leading_zeros()) as i32;
        let nb = usize::BITS.saturating_sub(b.leading_zeros()) as i32;
        let d = nb - na;
        let m = usize::MAX >> (usize::BITS - 1).saturating_sub(d.unsigned_abs());
        (d < 0) == (a.saturating_add(b) & m != 0)
    }
}

impl Device for KeyboardMouse {
    fn descriptor(&self) -> ReportDescriptor {
        let mut rd = self.k.descriptor();
        rd.append(&self.m.descriptor());
        rd
    }

    fn reset(&mut self) {
        self.k.reset();
        self.m.reset();
    }

    fn get_report(&self, typ: ReportType, id: u8) -> Option<Report> {
        if id == self.k.report_id() {
            self.k.get_report(typ, id)
        } else {
            self.m.get_report(typ, id)
        }
    }

    fn set_report(&mut self, r: Report) -> bool {
        if r.id() != self.k.report_id() {
            return self.m.set_report(r);
        }
        if !self.k.set_report(r) {
            return false;
        }
        self.w.send_if_modified(|s| {
            if *s == self.k.led() {
                return false;
            }
            *s = self.k.led();
            true
        });
        true
    }

    #[inline(always)]
    fn is_boot_mode(&self) -> Option<bool> {
        self.k.is_boot_mode()
    }

    #[inline]
    fn set_boot_mode(&mut self, boot: bool) {
        self.k.set_boot_mode(boot);
        self.m.set_boot_mode(boot);
    }
}

impl Iterator for KeyboardMouse {
    type Item = Report;

    fn next(&mut self) -> Option<Self::Item> {
        let (k, m) = (self.k.size_hint(), self.m.size_hint());
        if Self::select(k.1.unwrap_or(k.0), m.1.unwrap_or(m.0)) {
            self.k.next()
        } else {
            self.m.next()
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (k, m) = (self.k.size_hint(), self.m.size_hint());
        (k.0 + m.0, k.1.zip(m.1).map(|(a, b)| a + b))
    }
}
