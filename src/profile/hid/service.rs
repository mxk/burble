use std::sync::Arc;

use tracing::warn;

use burble_const::{Characteristic, Descriptor, Service};

use crate::att::{Access, ErrorCode};
use crate::gatt::{Builder, Db, Io, IoReq, IoResult, Notify, NotifyReq, Prop};
use crate::hid::kbd::{Kbd, Key};
use crate::hid::mouse::Mouse;
use crate::hid::{Dev, InputDev, OutputDev, ReportDescriptor};
use crate::{att, SyncMutex};

/// HID input event.
#[derive(Debug)]
#[non_exhaustive]
pub enum Input {
    /// Keyboard input event.
    Kbd(KbdIn),
    /// Mouse input event.
    Mouse(MouseIn),
}

/// Keyboard input.
#[derive(Debug)]
#[non_exhaustive]
pub enum KbdIn {
    /// Single key press.
    Press(Key),
    /// String of characters.
    Write(String),
}

/// Mouse input.
#[derive(Debug)]
#[non_exhaustive]
pub enum MouseIn {
    /// Mouse click (with button number).
    Click(u8),
    /// Relative mouse movement.
    MoveRel {
        /// Relative horizontal (X axis) movement.
        dx: i32,
        /// Relative vertical (Y axis) movement.
        dy: i32,
    },
    /// Vertical scroll.
    VScroll(i32),
}

/// HID GATT service.
#[derive(Debug)]
pub struct HidService {
    srv: SyncMutex<HidInner>,
}

impl HidService {
    /// Creates a new HID service.
    #[inline]
    #[must_use]
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            srv: SyncMutex::new(HidInner {
                kbd: DevServer::new(Kbd::new()),
                mouse: DevServer::new(Mouse::new()),
            }),
        })
    }

    /// Informs remote host of the specified input.
    pub async fn exec(&self, inp: Input) -> att::Result<()> {
        self.input(inp);
        while let Some(ntf) = self.notify() {
            ntf.await?;
        }
        Ok(())
    }

    fn input(&self, inp: Input) {
        let mut srv = self.srv.lock();
        match inp {
            Input::Kbd(inp) => match inp {
                KbdIn::Press(k) => srv.kbd.dev.press(k),
                KbdIn::Write(s) => srv.kbd.dev.write(&s),
            },
            Input::Mouse(inp) => match inp {
                MouseIn::Click(btn) => srv.mouse.dev.click(btn),
                MouseIn::MoveRel { dx, dy } => srv.mouse.dev.move_rel(dx, dy),
                MouseIn::VScroll(dv) => srv.mouse.dev.vscroll(dv),
            },
        }
    }

    fn notify(&self) -> Option<Notify> {
        let mut srv = self.srv.lock();
        // TODO: Fairness?
        srv.kbd.poll_notify().or_else(|| srv.mouse.poll_notify())
    }

    /// Defines the service structure.
    pub fn define(self: &Arc<Self>, db: &mut Builder<Db>) {
        const RO: Access = Access::READ.authn().encrypt();
        const WO: Access = Access::WRITE.authn().encrypt();
        const RW: Access = Access::READ_WRITE.authn().encrypt();
        db.primary_service(Service::HumanInterfaceDevice, [], |db| {
            use {Characteristic::*, Descriptor::*};
            // HID v1.11, normally connectable
            db.ro_characteristic(HidInformation, RO, [0x11, 0x01, 0, 0b10], |_| {});
            db.characteristic(
                HidControlPoint,
                Prop::WRITE_CMD,
                WO,
                //Io::map(self, |this, req| this.srv.lock().cp_io(req)),
                HidInner::cp_io,
                |_| {},
            );

            // Keyboard input report
            db.characteristic(
                Report,
                Prop::READ | Prop::NOTIFY,
                RO,
                Io::map(self, |this, req| this.srv.lock().kbd.inp_io(req)),
                |db| {
                    db.cccd(RW);
                    db.ro_descriptor(ReportReference, RO, [1, 1]);
                },
            );
            // Keyboard output report
            db.characteristic(
                Report,
                Prop::READ | Prop::WRITE_CMD,
                RW,
                Io::map(self, |this, req| this.srv.lock().kbd.out_io(req)),
                |db| {
                    db.ro_descriptor(ReportReference, RO, [1, 2]);
                },
            );
            // Mouse input report
            db.characteristic(
                Report,
                Prop::READ | Prop::NOTIFY,
                RO,
                Io::map(self, |this, req| this.srv.lock().mouse.inp_io(req)),
                |db| {
                    db.cccd(RW);
                    db.ro_descriptor(ReportReference, RO, [2, 1]);
                },
            );

            let srv = self.srv.lock();
            let mut rd = ReportDescriptor::new();
            rd.concat(srv.kbd.dev.report_descriptor(1));
            rd.concat(srv.mouse.dev.report_descriptor(2));
            db.ro_characteristic(ReportMap, RO, &rd.to_bytes(), |_| {});
        });
    }
}

#[derive(Debug)]
struct HidInner {
    kbd: DevServer<Kbd>,
    mouse: DevServer<Mouse>,
}

impl HidInner {
    #[allow(clippy::needless_pass_by_value)]
    #[allow(clippy::unnecessary_wraps)]
    fn cp_io(req: IoReq) -> IoResult {
        let IoReq::Write(w) = req else { unreachable!() };
        warn!("Ignoring control point write: {w:?}");
        Ok(())
    }
}

#[derive(Debug)]
struct DevServer<T> {
    dev: T,
    ntf: Option<NotifyReq>,
}

impl<T: InputDev> DevServer<T> {
    #[inline(always)]
    const fn new(dev: T) -> Self {
        Self { dev, ntf: None }
    }

    fn poll_notify(&mut self) -> Option<Notify> {
        if self.ntf.as_ref().map_or(false, NotifyReq::is_closed) {
            self.ntf = None;
        }
        if self.ntf.is_none() {
            self.dev.reset();
        } else if self.dev.poll() {
            return self.ntf.as_ref().map(|ntf| {
                ntf.notify(|p| {
                    p.put(self.dev.input());
                })
            });
        }
        None
    }

    fn inp_io(&mut self, req: IoReq) -> IoResult {
        match req {
            IoReq::Read(r) => r.complete(vec![0; T::IN_LEN as usize]),
            IoReq::Write(_) => unreachable!(),
            IoReq::Notify(n) => {
                self.ntf = Some(n);
                Ok(())
            }
        }
    }
}

impl<T: OutputDev> DevServer<T> {
    fn out_io(&mut self, req: IoReq) -> IoResult {
        match req {
            IoReq::Read(r) => r.complete(self.dev.output()),
            IoReq::Write(w) => {
                if w.value().len() != T::OUT_LEN as usize {
                    return Err(ErrorCode::InvalidAttributeValueLength);
                }
                self.dev.set_output(w.value());
                Ok(())
            }
            IoReq::Notify(_) => unreachable!(),
        }
    }
}
