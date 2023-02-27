#![allow(clippy::print_stdout)]
#![allow(clippy::print_stderr)]

use std::str::from_utf8;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use clap::Parser;
use futures_core::future::BoxFuture;
use tracing::info;

use burble::att::ErrorCode;
use burble::gap::Appearance;
use burble::gatt::{IoReq, IoResult};
use burble::*;
use burble_crypto::NumCompare;

#[derive(Debug, clap::Parser)]
struct Args {
    /// Vendor ID of the Bluetooth USB device
    #[arg(short, long, value_parser=hex16)]
    vid: Option<u16>,

    /// Product ID of the Bluetooth USB device
    #[arg(short, long, value_parser=hex16)]
    pid: Option<u16>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let args = Args::parse();
    let usb = host::Usb::new()?;
    let mut ctlr = if let (Some(vid), Some(pid)) = (args.vid, args.pid) {
        usb.open_first(vid, pid)?
    } else {
        println!("Available controllers (pass 'ID <VID>:<PID>' to '--vid' and '--pid' options):");
        for ctlr in usb.controllers()? {
            println!("{ctlr}");
        }
        return Ok(());
    };
    ctlr.init()?;
    let host = hci::Host::new(Arc::new(ctlr));

    let event_loop = host.event_loop();
    host.init().await?;
    info!("Local version: {:?}", host.read_local_version().await?);
    info!("Device address: {:?}", host.read_bd_addr().await?);

    let mut secdb = smp::SecDb::new(host.clone(), Arc::new(burble_fs::KeyStore::new()));
    tokio::task::spawn(async move { secdb.event_loop().await });

    let cm = tokio::task::spawn(server_loop(server(), l2cap::ChanManager::new(&host).await?));

    advertise(&host).await?;
    let _ = cm.await?;
    Ok(event_loop.stop().await?)
}

async fn advertise(host: &hci::Host) -> Result<()> {
    let mut adv = hci::Advertiser::new(host).await?;
    let params = hci::AdvParams {
        props: hci::AdvProp::CONNECTABLE | hci::AdvProp::INCLUDE_TX_POWER,
        pri_interval: (Duration::from_millis(20), Duration::from_millis(25)),
        sec_phy: hci::AdvPhy::Le2M,
        ..hci::AdvParams::default()
    };
    let (h, power) = adv.create(params).await?;
    let mut data = gap::ResponseDataMut::new();
    data.flags(gap::AdvFlag::LE_GENERAL | gap::AdvFlag::NO_BREDR)
        .local_name(true, "Burble")
        .appearance(gap::Appearance::GenericHumanInterfaceDevice)
        .tx_power(power);
    adv.set_data(h, data.get()).await?;
    let enable_params = hci::AdvEnableParams {
        handle: h,
        duration: Duration::from_secs(20),
        max_events: 0,
    };
    let adv_set = adv.enable(enable_params).await?;
    let r = adv_set.await;
    info!("Advertising result: {r:?}");
    adv.remove_all().await?;
    r?;
    Ok(())
}

async fn server_loop(srv: Arc<gatt::Server>, mut cm: l2cap::ChanManager) -> Result<()> {
    srv.db().dump();
    loop {
        let link = cm.recv().await?;
        let Some(att) = cm.att_chan(link) else { continue };
        tokio::task::spawn(srv.attach(&att).serve(att));
        let Some(mut smp) = cm.smp_chan(link) else { continue };
        tokio::task::spawn(async move {
            let mut dev = smp::Device::new()
                .with_display(Box::new(Dev))
                .with_confirm(Box::new(Dev));
            smp.respond(&mut dev, &burble_fs::KeyStore::new()).await
        });
    }
}

fn server() -> Arc<gatt::Server> {
    use burble::att::Access;
    use burble::gatt::{Characteristic, Db, Prop, Service};
    let mut db = Db::build();

    db.primary_service(Service::GenericAccess, [], |db| {
        db.characteristic(
            Characteristic::DeviceName,
            Prop::READ | Prop::WRITE,
            Access::READ_WRITE,
            dev_name_io,
            |_| {},
        );
        db.ro_characteristic(
            Characteristic::Appearance,
            Access::READ,
            (Appearance::GenericHumanInterfaceDevice as u16).to_le_bytes(),
            |_| {},
        );
    });
    dis::DeviceInfoService::new()
        .with_manufacturer_name("Blackrock Neurotech")
        .with_pnp_id(dis::PnpId::new(dis::VendorId::USB(0x1209), 0x0001, (1, 0, 0)).unwrap())
        .define(&mut db, Access::READ);
    bas::BatteryService::new().define(&mut db, Access::READ);
    gatt::Server::new(db, Arc::new(burble_fs::GattServerStore::new()))
}

fn dev_name_io(req: IoReq) -> IoResult {
    lazy_static::lazy_static! {
        static ref NAME: parking_lot::Mutex<String> = parking_lot::Mutex::new("Burble".to_owned());
    }
    match req {
        IoReq::Read(r) => r.complete(NAME.lock().as_str()),
        IoReq::Write(w) => {
            NAME.lock().replace_range(
                ..,
                from_utf8(w.value()).map_err(|_| ErrorCode::ValueNotAllowed)?,
            );
            Ok(())
        }
        IoReq::Notify(_) => unreachable!(),
    }
}

#[derive(Debug)]
struct Dev;

impl smp::Display for Dev {
    fn show(&mut self, n: NumCompare) -> BoxFuture<bool> {
        println!("Numeric comparison: {n}");
        Box::pin(std::future::ready(true))
    }
}

impl smp::Confirm for Dev {
    fn confirm(&mut self) -> BoxFuture<bool> {
        use tokio::io::AsyncBufReadExt;
        Box::pin(async {
            let buf = tokio::io::BufReader::new(tokio::io::stdin());
            let mut lines = buf.lines();
            loop {
                print!("Match? (yes/no) ");
                let Ok(Some(ln)) = lines.next_line().await else { return false };
                match ln.to_ascii_lowercase().as_str() {
                    "yes" | "y" => return true,
                    "no" | "n" => return false,
                    _ => {}
                }
            }
        })
    }
}

pub fn hex16(mut s: &str) -> Result<u16, String> {
    if s.starts_with("0x") || s.starts_with("0X") {
        s = &s[2..];
    }
    u16::from_str_radix(s, 16).map_err(|e| format!("{e}"))
}
