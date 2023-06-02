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

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use tracing::debug;

use burble_const::{Characteristic, Descriptor, Service};
use burble_hid::descriptor::{Collection, Locale, ReportDescriptor, Tag};
use burble_hid::kbd::{Keyboard, Led};
use burble_hid::mouse::Mouse;
use burble_hid::usage::{GenericDesktop, Page};
use burble_hid::{Device, Report, ReportType};

use crate::att::{Access, ErrorCode};
use crate::gatt::{Builder, Db, Io, IoReq, IoResult, Notify, NotifyReq, Prop};
use crate::SyncMutex;

/// Human Interface Device service.
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

    /// Returns a watch receiver that reflects device state changes.
    #[inline]
    #[must_use]
    pub fn state(&self) -> tokio::sync::watch::Receiver<HidState> {
        self.0.lock().w.subscribe()
    }

    /// Calls `f` to control the device. Updates are not reported to the host
    /// until [`Self::flush()`] is called.
    #[inline(always)]
    pub fn control<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(&mut self.0.lock().dev)
    }

    /// Notifies the host of any pending reports. Reports that do not have
    /// notifications enabled are discarded. The current implementation also
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

    /// Calls `f` to control the device followed by a flush.
    #[inline(always)]
    pub async fn exec(&self, f: impl FnOnce(&mut T) + Send) {
        f(&mut self.0.lock().dev);
        self.flush().await;
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
        let ((ver, loc), rd, boot_support) = {
            let d = self.0.lock();
            (
                d.dev.hid_descriptor(),
                d.dev.report_descriptor(),
                d.dev.boot_mode().is_some(),
            )
        };
        db.primary_service(Service::HumanInterfaceDevice, [], move |db| {
            use Characteristic::*;
            use GenericDesktop::{Keyboard, Mouse};

            // HID Information ([HIDS] Section 2.10)
            let ver = ver.to_le_bytes();
            let flags = 0b10; // NormallyConnectable = 1, RemoteWake = 0
            db.ro_characteristic(
                HidInformation,
                RO,
                [ver[0], ver[1], loc as _, flags],
                |_| {},
            );

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
            let mut boot = BTreeSet::new();
            for (app, rref) in Self::reports(&rd, None)
                .chain(Self::reports(&rd, Some(Keyboard)))
                .chain(Self::reports(&rd, Some(Mouse)))
            {
                use ReportType::*;
                let uuid = match (app, rref.typ) {
                    (None, _) => Report,
                    (Some(Keyboard), Input) => BootKeyboardInputReport,
                    (Some(Keyboard), Output) => BootKeyboardOutputReport,
                    (Some(Mouse), Input) => BootMouseInputReport,
                    _ => continue,
                };
                let (props, perms) = match rref.typ {
                    Input => (Prop::NOTIFY, RO), // TODO: Optional Write?
                    Output => (Prop::WRITE.union(Prop::WRITE_CMD), RW),
                    Feature => (Prop::WRITE, RW),
                };
                db.characteristic(
                    uuid,
                    Prop::READ.union(props),
                    perms,
                    Io::with(&self.0, move |this, req| this.lock().report_io(rref, req)),
                    |db| {
                        if rref.typ.is_input() {
                            db.cccd(RW);
                        }
                        if matches!(uuid, Report) {
                            db.ro_descriptor(
                                Descriptor::ReportReference,
                                RO,
                                [rref.id, rref.typ as _],
                            );
                        }
                    },
                );
                if rref.boot {
                    boot.insert((rref.typ, rref.id));
                }
            }

            // Protocol Mode ([HIDS] Section 2.4)
            if boot_support {
                self.0.lock().boot = boot;
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

    /// Returns an iterator over all report types in the descriptor. If `app` is
    /// [`Some`], then only the reports in the specified application collections
    /// are returned.
    fn reports(
        rd: &ReportDescriptor,
        app: Option<GenericDesktop>,
    ) -> impl Iterator<Item = (Option<GenericDesktop>, ReportRef)> + '_ {
        let want = (Page::GenericDesktop as u32) << 16 | app.map_or(0xFFFF, |u| u as _);
        let (mut depth, mut app_match) = (0, if app.is_some() { 0 } else { usize::MAX });
        let (mut page, mut usage) = (0, 0);
        let mut known = BTreeSet::new();
        let mut rref = ReportRef {
            typ: ReportType::Input,
            id: 0,
            boot: app.is_some(),
        };
        rd.iter().filter_map(move |(t, n, v)| {
            match t {
                Tag::Input | Tag::Output | Tag::Feature => {
                    if app_match != 0 {
                        rref.typ = t.as_report_type();
                        if known.insert((rref.typ, rref.id)) {
                            usage = 0;
                            return Some((app, rref));
                        }
                    }
                }
                Tag::Collection => {
                    depth += 1;
                    if app_match == 0 && v == Collection::Application as u32 && usage == want {
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
                Tag::ReportId => rref.id = u8::try_from(v).expect("invalid report ID"),
                Tag::Pop => panic!("pop tag not supported"),
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

/// Report reference.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct ReportRef {
    typ: ReportType,
    id: u8,
    boot: bool,
}

/// HID device service state.
#[derive(Debug)]
struct Dev<T> {
    dev: T,
    boot: BTreeSet<(ReportType, u8)>,
    ntf: BTreeMap<ReportRef, NotifyReq>,
    w: tokio::sync::watch::Sender<HidState>,
}

impl<T: Device> Dev<T> {
    /// Creates HID service state for the specified device.
    #[inline]
    #[must_use]
    fn new(dev: T) -> Arc<SyncMutex<Self>> {
        let boot = BTreeSet::new();
        let ntf = BTreeMap::new();
        let mut s = HidState(Flag::empty());
        s.0.set(Flag::BOOT, Self::boot_mode(&dev));
        let (w, _) = tokio::sync::watch::channel(s);
        Arc::new(SyncMutex::new(Self { dev, boot, ntf, w }))
    }

    /// Updates connection state when an I/O request comes in.
    fn on_io(&mut self) {
        self.ntf.retain(|_, n| !n.is_closed());
        self.w.send_if_modified(|s| {
            let prev = *s;
            if s.is_active() && self.ntf.is_empty() {
                // This is a bit problematic because if the host just disables
                // notifications without actually disconnecting, then the next
                // I/O request will treat this as a new connection and reset the
                // device.
                s.0.remove(Flag::CONN.union(Flag::ACTIVE));
                debug!("Disconnected: {s:?}");
            }
            if !s.is_connected() {
                // [HIDS] Section 2.4.1.1
                self.dev.reset();
                s.0.insert(Flag::CONN);
                s.0.set(Flag::BOOT, Self::boot_mode(&self.dev));
                debug!("Connected: {s:?}");
            }
            *s != prev
        });
    }

    /// Handles control point characteristic I/O (\[HIDS\] Section 2.11).
    #[allow(clippy::needless_pass_by_value)]
    fn control_point_io(&mut self, req: IoReq) -> IoResult {
        self.on_io();
        let IoReq::Write(w) = req else { unreachable!() };
        let mut v = [u8::from(!self.w.borrow().is_suspended())];
        w.update(&mut v)?;
        self.w.send_if_modified(|s| {
            let diff = s.is_suspended() != (v[0] == 0);
            if diff {
                s.0.toggle(Flag::SUSPEND);
                debug!("Suspended: {}", s.is_suspended());
            }
            diff
        });
        Ok(())
    }

    /// Handles report characteristic I/O (\[HIDS\] Section 2.5, 2.7, 2.8, 2.9).
    fn report_io(&mut self, rref: ReportRef, req: IoReq) -> IoResult {
        self.on_io();
        match req {
            IoReq::Read(r) => {
                if Self::boot_mode(&self.dev) != rref.boot {
                    return Err(ErrorCode::UnlikelyError);
                }
                (self.dev.get_report(rref.typ, rref.id))
                    .map_or(Err(ErrorCode::RequestNotSupported), |v| r.complete(v))
            }
            IoReq::Write(w) => {
                let mut v = (self.dev.get_report(rref.typ, rref.id))
                    .ok_or(ErrorCode::RequestNotSupported)?;
                w.update(&mut v)?;
                if self.dev.set_report(v) {
                    Ok(())
                } else {
                    Err(ErrorCode::ValueNotAllowed)
                }
            }
            IoReq::Notify(n) => {
                if n.is_indicate() {
                    return Err(ErrorCode::CccdImproperlyConfigured);
                }
                if self.ntf.is_empty() {
                    self.w.send_modify(|s| {
                        s.0.insert(Flag::ACTIVE);
                        debug!("Active: true");
                    });
                }
                self.ntf.insert(rref, n);
                Ok(())
            }
        }
    }

    /// Handles Protocol Mode characteristic I/O (\[HIDS\] Section 2.4).
    fn protocol_mode_io(&mut self, req: IoReq) -> IoResult {
        self.on_io();
        let mut v = [u8::from(!Self::boot_mode(&self.dev))];
        match req {
            IoReq::Read(r) => r.complete(v),
            IoReq::Write(w) => {
                w.update(&mut v)?;
                let mut boot = v[0] == 0;
                if self.dev.boot_mode().map_or(false, |b| b != boot) {
                    self.dev.set_boot_mode(boot);
                    boot = Self::boot_mode(&self.dev);
                    self.w.send_modify(|s| {
                        s.0.set(Flag::BOOT, boot);
                        debug!("Boot protocol mode: {boot}");
                    });
                }
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    /// Returns the device boot protocol mode state.
    #[inline(always)]
    #[must_use]
    fn boot_mode(dev: &T) -> bool {
        matches!(dev.boot_mode(), Some(true))
    }
}

impl<T: Device> Iterator for Dev<T> {
    type Item = Notify;

    fn next(&mut self) -> Option<Self::Item> {
        let active = !self.ntf.is_empty();
        self.ntf.retain(|_, n| !n.is_closed());
        if active && self.ntf.is_empty() {
            self.dev.reset();
            self.w.send_modify(|s| {
                s.0.remove(Flag::CONN.union(Flag::ACTIVE));
                s.0.set(Flag::BOOT, Self::boot_mode(&self.dev));
                debug!("Disconnected: {s:?}");
            });
        }
        let boot = Self::boot_mode(&self.dev);
        for r in self.dev.by_ref() {
            let rref = ReportRef {
                typ: r.typ(),
                id: r.id(),
                boot: boot && self.boot.contains(&(r.typ(), r.id())),
            };
            if let Some(n) = self.ntf.get(&rref) {
                return Some(n.notify(|p| {
                    p.put(r);
                }));
            }
        }
        None
    }
}

/// HID device state.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[repr(transparent)]
pub struct HidState(Flag);

bitflags::bitflags! {
    /// HID device state flags.
    #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
    struct Flag: u8 {
        const CONN = 1 << 0;
        const ACTIVE = 1 << 1;
        const BOOT = 1 << 2;
        const SUSPEND = 1 << 3;
    }
}

impl HidState {
    /// Returns whether the device is connected to a host. Currently, a
    /// connection starts from the first I/O request and until the last
    /// notification session is closed.
    #[inline(always)]
    #[must_use]
    pub const fn is_connected(self) -> bool {
        self.0.contains(Flag::CONN)
    }

    /// Returns whether there is at least one active notification session (host
    /// receiving input).
    #[inline(always)]
    #[must_use]
    pub const fn is_active(self) -> bool {
        self.0.contains(Flag::ACTIVE)
    }

    /// Returns whether the device is in boot protocol mode.
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
    /// Panics if the keyboard and mouse report IDs are the same.
    #[inline]
    #[must_use]
    pub fn new(k: Keyboard, m: Mouse) -> Self {
        assert_ne!(
            k.report_id(),
            m.report_id(),
            "keyboard/mouse report ID collision"
        );
        let (w, _) = tokio::sync::watch::channel(k.led());
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

    /// Notifies watchers of any keyboard LED changes.
    #[inline]
    fn notify(&self) {
        self.w.send_if_modified(|s| {
            let prev = *s;
            *s = self.k.led();
            *s != prev
        });
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
    #[inline(always)]
    fn hid_descriptor(&self) -> (u16, Locale) {
        self.k.hid_descriptor()
    }

    #[inline]
    fn report_descriptor(&self) -> ReportDescriptor {
        let mut rd = self.k.report_descriptor();
        rd.append(&self.m.report_descriptor());
        rd
    }

    #[inline]
    fn reset(&mut self) {
        self.k.reset();
        self.m.reset();
        self.notify();
    }

    #[inline]
    fn get_report(&self, typ: ReportType, id: u8) -> Option<Report> {
        if id == self.k.report_id() {
            self.k.get_report(typ, id)
        } else {
            self.m.get_report(typ, id)
        }
    }

    #[inline]
    fn set_report(&mut self, r: Report) -> bool {
        if r.id() == self.k.report_id() {
            let ok = self.k.set_report(r);
            self.notify();
            ok
        } else {
            self.m.set_report(r)
        }
    }

    #[inline(always)]
    fn boot_mode(&self) -> Option<bool> {
        self.k.boot_mode()
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

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::pin::pin;
    use std::time::Duration;

    use structbuf::StructBuf;
    use tokio::sync::{mpsc, watch};
    use tokio::time::timeout;
    use tokio_util::sync::CancellationToken;

    use burble_hid::mouse::Button;
    use burble_hid::usage::Key;

    use crate::att::{Handle, Opcode};
    use crate::gatt::io::NotifyVal;
    use crate::gatt::{CharacteristicDef, DbEntry, ReadReq, WriteReq};

    use super::*;

    #[allow(clippy::items_after_statements)]
    #[allow(clippy::too_many_lines)]
    #[tokio::test]
    async fn keyboard_mouse() {
        const IN_PROPS: Prop = Prop::READ.union(Prop::NOTIFY);
        const OUT_PROPS: Prop = Prop::READ.union(Prop::WRITE).union(Prop::WRITE_CMD);

        // Create device
        let km = KeyboardMouse::new(Keyboard::us(1), Mouse::new(2, 0));
        let mut led = km.led();
        let rd = km.report_descriptor();

        // Define GATT service
        let mut db = Db::build();
        let hid = HidService::new(km);
        let mut state = hid.state();
        hid.define(&mut db);
        let (db, io) = db.freeze();
        let srv = db.primary_services(Handle::MIN, None).next().unwrap();
        let mut chars = db.characteristics(srv.handle_range());

        // Helper functions
        let mut next_char = |uuid: Characteristic, props: Prop| {
            let c = chars.next().unwrap();
            assert_eq!(c.uuid(), uuid);
            assert_eq!(c.properties(), props);
            c
        };
        fn assert_changed<T: Copy + Debug + PartialEq>(r: &mut watch::Receiver<T>, v: T) {
            assert!(r.has_changed().unwrap());
            assert_eq!(*r.borrow_and_update(), v);
        }
        let read = |char: DbEntry<CharacteristicDef>| {
            let mut req = ReadReq {
                op: Opcode::ReadReq,
                hdl: char.value_handle(),
                uuid: Some(char.uuid()),
                off: 0,
                buf: StructBuf::new(255),
            };
            io.read(&mut req).unwrap();
            req.buf
        };
        let write = |char: DbEntry<CharacteristicDef>, val: &[u8]| {
            let req = WriteReq {
                op: Opcode::WriteCmd,
                hdl: char.value_handle(),
                uuid: char.uuid(),
                off: 0,
                val,
            };
            io.write(&req).unwrap();
        };
        let enable_notify = |char: DbEntry<CharacteristicDef>| {
            let (tx, rx) = mpsc::channel(1);
            let ct = CancellationToken::new();
            let req = NotifyReq {
                hdl: char.value_handle(),
                uuid: char.uuid(),
                mtu: 255,
                ind: false,
                tx,
                ct: ct.clone(),
            };
            io.notify(req).unwrap();
            (rx, ct)
        };

        // HID Information
        let ch = next_char(Characteristic::HidInformation, Prop::READ);
        assert_eq!(
            db.get(ch.value_handle()).unwrap().1,
            [0x11, 0x01, Locale::Us as u8, 0b10].as_slice()
        );

        // Report Map
        let c = next_char(Characteristic::ReportMap, Prop::READ);
        assert_eq!(db.get(c.value_handle()).unwrap().1, rd.as_ref());

        // HID Control Point
        let c = next_char(Characteristic::HidControlPoint, Prop::WRITE_CMD);
        assert_eq!(state.borrow_and_update().0, Flag::empty());
        write(c, &[0]);
        assert_changed(&mut state, HidState(Flag::CONN.union(Flag::SUSPEND)));
        write(c, &[1]);
        assert_changed(&mut state, HidState(Flag::CONN));

        // Keyboard Input
        let c = next_char(Characteristic::Report, IN_PROPS);
        assert_eq!(read(c).as_ref(), &[0; 7]);
        let (mut krx, kct) = enable_notify(c);
        assert_changed(&mut state, HidState(Flag::CONN.union(Flag::ACTIVE)));

        // Keyboard Output
        let c = next_char(Characteristic::Report, OUT_PROPS);
        assert_eq!(*led.borrow_and_update(), Led::empty());
        write(c, &[Led::NUM_LOCK.bits()]);
        assert_changed(&mut led, Led::NUM_LOCK);
        assert_eq!(read(c).as_ref(), &[Led::NUM_LOCK.bits()]);

        // Mouse Input
        let c = next_char(Characteristic::Report, IN_PROPS);
        assert_eq!(read(c).as_ref(), &[0; 3]);
        let (mut mrx, mct) = enable_notify(c);

        // Boot Reports
        next_char(Characteristic::BootKeyboardInputReport, IN_PROPS);
        next_char(Characteristic::BootKeyboardOutputReport, OUT_PROPS);
        next_char(Characteristic::BootMouseInputReport, IN_PROPS);

        // Protocol Mode
        let c = next_char(
            Characteristic::ProtocolMode,
            Prop::READ.union(Prop::WRITE_CMD),
        );
        assert_eq!(read(c).as_ref(), &[1]);
        write(c, &[0]);
        assert_eq!(read(c).as_ref(), &[0]);
        assert_changed(
            &mut state,
            HidState(Flag::CONN.union(Flag::ACTIVE).union(Flag::BOOT)),
        );
        assert_eq!(hid.control(|km| km.boot_mode()), Some(true));
        write(c, &[1]);
        assert_changed(&mut state, HidState(Flag::CONN.union(Flag::ACTIVE)));
        assert_eq!(hid.control(|km| km.boot_mode()), Some(false));

        // Notifications
        let mut exec = pin!(timeout(
            Duration::from_secs(1),
            hid.exec(|km| {
                km.mouse().hold(Button::PRIMARY.union(Button::SECONDARY));
                km.kbd().press(Key::X);
                km.mouse().release(Button::SECONDARY);
            })
        ));
        let mut reports = Vec::new();
        loop {
            let (id, ntf): (u8, NotifyVal) = tokio::select! {
                r = &mut exec => {
                    r.unwrap();
                    break;
                }
                ntf = krx.recv() => (1, ntf.unwrap()),
                ntf = mrx.recv() => (2, ntf.unwrap()),
            };
            reports.push(Report::input(id, ntf.as_ref()));
            ntf.result(Ok(()));
        }
        assert_eq!(
            reports,
            vec![
                Report::input(1, &[0, Key::X as _, 0, 0, 0, 0, 0]),
                Report::input(2, &[3, 0, 0]),
                Report::input(1, &[0, 0, 0, 0, 0, 0, 0]),
                Report::input(2, &[1, 0, 0]),
            ]
        );

        // Disable notifications
        kct.cancel();
        mct.cancel();
        timeout(Duration::from_secs(1), hid.flush()).await.unwrap();
        assert_changed(&mut state, HidState(Flag::empty()));
    }
}
