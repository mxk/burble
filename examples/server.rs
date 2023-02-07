#![allow(clippy::print_stdout)]
#![allow(clippy::print_stderr)]
#![allow(clippy::similar_names)]

use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use clap::Parser;
use futures_core::future::BoxFuture;
use tracing::info;

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
    let host = hci::Host::new(ctlr);

    let hci_mon = host.enable_events();
    host.init().await?;
    info!("Local version: {:?}", host.read_local_version().await?);
    let local_addr = host.read_bd_addr().await?;
    info!("Device address: {:?}", local_addr);

    let mut secdb = smp::SecDb::new(host.clone(), Arc::new(burble_fs::SecDb::new()))?;
    tokio::task::spawn(async move { secdb.event_loop().await });

    let cm = tokio::task::spawn(server_loop(
        db(),
        l2cap::ChanManager::new(&host, local_addr).await?,
    ));

    advertise(&host).await?;
    let _ = cm.await?;
    Ok(hci_mon.disable().await?)
}

async fn advertise<T: host::Transport>(host: &hci::Host<T>) -> Result<()> {
    let mut adv = hci::Advertiser::new(host.clone()).await?;
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
    let mut adv_mon = adv.enable(enable_params).await?;
    let r = adv_mon.event().await;
    info!("Advertising result: {r:?}");
    adv.remove_all().await?;
    r?;
    Ok(())
}

async fn server_loop<T: host::Transport + 'static>(
    db: Arc<gatt::Db>,
    mut cm: l2cap::ChanManager<T>,
) -> Result<()> {
    db.dump();
    loop {
        let link = cm.recv().await?;
        let Some(att) = cm.att_chan(link) else { continue };
        tokio::task::spawn(serve(gatt::Server::new(att, Arc::clone(&db))));
        let Some(mut smp) = cm.sm_chan(link) else { continue };
        tokio::task::spawn(async move {
            let mut dev = smp::Device::new()
                .with_display(Box::new(Dev))
                .with_confirm(Box::new(Dev));
            smp.respond(&mut dev, &burble_fs::SecDb::new()).await
        });
    }
}

async fn serve<T: host::Transport + 'static>(mut s: gatt::Server<T>) {
    s.configure().await.unwrap();
    s.serve().await.unwrap();
}

fn db() -> Arc<gatt::Db> {
    use burble::att::Access;
    use burble::gatt::{Characteristic, Prop, Schema, Service};
    let mut b = Schema::build();

    let (_, dev_name) = b.primary_service(Service::GenericAccess, [], |b| {
        let (dev_name, _) = b.characteristic(
            Characteristic::DeviceName,
            Prop::READ | Prop::WRITE,
            Access::READ_WRITE,
            |_| {},
        );
        b.characteristic(Characteristic::Appearance, Prop::READ, Access::READ, |_| {});
        dev_name
    });
    b.primary_service(Service::GenericAttribute, [], |b| {
        b.characteristic(
            Characteristic::ServiceChanged,
            Prop::INDICATE,
            Access::NONE,
            |b| b.client_cfg(Access::READ_WRITE),
        );
        b.characteristic(
            Characteristic::ClientSupportedFeatures,
            Prop::READ | Prop::WRITE,
            Access::READ_WRITE,
            |_| {},
        );
        b.characteristic(
            Characteristic::DatabaseHash,
            Prop::READ,
            Access::READ,
            |_| {},
        );
    });
    dis::DeviceInfoService::new()
        .with_manufacturer_name("Blackrock Neurotech")
        .with_pnp_id(dis::PnpId::new(dis::VendorId::USB(0x1209), 0x0001, (1, 0, 0)).unwrap())
        .define(&mut b, Access::READ);
    /*let (batt, _) = b.secondary_service(Service::Battery, [], |b| {
        b.characteristic(
            Characteristic::BatteryLevel,
            Prop::READ,
            Access::READ,
            |_| {},
        );
    });
    b.primary_service(Service::Glucose, [batt], |b| {
        b.characteristic(
            Characteristic::GlucoseMeasurement,
            Prop::READ | Prop::INDICATE | Prop::EXT_PROPS,
            Access::READ,
            |b| b.client_cfg(Access::READ_WRITE),
        );
    });*/
    let db = Arc::new(gatt::Db::new(b.freeze()));
    db.write().insert(dev_name, b"Burble".to_vec());
    db
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
