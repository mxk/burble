use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use clap::Parser;
use tracing::info;

use burble::*;

#[derive(Debug, clap::Parser)]
struct Args {
    /// Vendor ID of the Bluetooth USB device
    #[arg(short, long, value_parser=hex16)]
    vid: u16,

    /// Product ID of the Bluetooth USB device
    #[arg(short, long, value_parser=hex16)]
    pid: u16,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let args = Args::parse();
    let usb = host::Usb::new()?;
    let mut ctlr = usb.open_first(args.vid, args.pid)?;
    ctlr.init()?;
    let host = hci::Host::new(ctlr);

    let hci_mon = host.enable_events();
    host.init().await?;
    info!("Local version: {:?}", host.read_local_version().await?);
    let local_addr = host.read_bd_addr().await?;
    info!("Device address: {:?}", local_addr);

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
    let (batt, _) = b.secondary_service(Service::Battery, [], |b| {
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
    });
    let db = Arc::new(gatt::Db::new(b.freeze()));
    db.write().insert(dev_name, b"Burble".to_vec());
    db
}

pub fn hex16(mut s: &str) -> Result<u16, String> {
    if s.starts_with("0x") || s.starts_with("0X") {
        s = &s[2..];
    }
    u16::from_str_radix(s, 16).map_err(|e| format!("{e}"))
}
