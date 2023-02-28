#![allow(clippy::print_stdout)]
#![allow(clippy::print_stderr)]

use std::io::BufRead;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use clap::Parser;
use futures_core::future::BoxFuture;
use sscanf::sscanf;
use tracing::info;

use burble::att::Access;
use burble::gap::Appearance;
use burble::gatt::Db;
use burble::hci::AdvEvent;
use burble::hid::{Input, MouseIn};
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
    let r = serve(host).await;
    event_loop.stop().await?;
    r
}

fn read_input(srv: Arc<hid::HidService>) {
    let (tx, mut rx) = tokio::sync::mpsc::channel(1);
    std::thread::spawn(move || {
        // https://github.com/tokio-rs/tokio/issues/2466
        for ln in std::io::BufReader::new(std::io::stdin()).lines() {
            tx.blocking_send(ln?)?;
        }
        Ok::<_, anyhow::Error>(())
    });
    tokio::task::spawn(async move {
        use {Input::*, MouseIn::*};
        loop {
            let ln: String = tokio::select! {
                ln = rx.recv() => match ln {
                    None => return,
                    Some(ln) => ln,
                },
                //_ = tokio::signal::ctrl_c() => return,
            };
            let mut tok = ln.split_ascii_whitespace();
            let Some(cmd) = tok.next() else { continue };
            let inp = match (cmd, tok.collect::<Vec<&str>>().join(" ")) {
                ("click", params) => {
                    if params.is_empty() {
                        Mouse(Click(0))
                    } else {
                        let Ok(v) = sscanf!(params, "{u8}") else { continue };
                        Mouse(Click(v))
                    }
                }
                ("move", params) => {
                    let Ok(v) = sscanf!(params, "{i32} {i32}") else { continue };
                    Mouse(MoveRel { dx: v.0, dy: v.1 })
                }
                _ => continue,
            };
            srv.exec(inp).await.unwrap();
        }
    });
}

async fn serve(host: hci::Host) -> Result<()> {
    let mut db = Db::build();
    gatt::Server::define_service(&mut db);
    gap::GapService::new("Burble", Appearance::GenericHumanInterfaceDevice)
        .define(&mut db, Access::READ);
    dis::DeviceInfoService::new()
        .with_manufacturer_name("Blackrock Neurotech")
        .with_pnp_id(dis::PnpId::new(dis::VendorId::USB(0x1209), 0x0001, (1, 0, 0)).unwrap())
        .define(&mut db, Access::READ);
    bas::BatteryService::new().define(&mut db, Access::READ);
    let hid = hid::HidService::new();
    //#[cfg(debug_assertions)]
    //db.morph_next();
    hid.define(&mut db);
    read_input(hid);

    let srv = gatt::Server::new(db, Arc::new(burble_fs::GattServerStore::new()));
    srv.db().dump();

    let mut secdb = smp::SecDb::new(host.clone(), Arc::new(burble_fs::KeyStore::new()));
    tokio::task::spawn(async move { secdb.event_loop().await });

    // TODO: Redesign ChanManager for easier integration with advertisements
    let mut cm = l2cap::ChanManager::new(&host).await?;
    let mut adv_task = None;
    let mut srv_task = None;
    let mut link = None;
    loop {
        if srv_task.is_none() && adv_task.is_none() {
            info!("Enabling advertisements");
            adv_task = Some(tokio::task::spawn(advertise(host.clone())));
        }
        tokio::select! {
            adv = async { adv_task.as_mut().unwrap().await }, if adv_task.is_some() => {
                info!("Advertisement result: {:?}", adv);
                adv_task = None;
                if matches!(adv??, AdvEvent::Term(_)) {
                    continue;
                }
                let link = match link.take() {
                    Some(link) => link,
                    None => cm.recv().await?,
                };
                info!("Serving {link}");
                let mut smp = cm.smp_chan(link).unwrap();
                tokio::task::spawn(async move {
                    let mut dev = smp::Device::new().with_display(Box::new(Dev)).with_confirm(Box::new(Dev));
                    smp.respond(&mut dev, &burble_fs::KeyStore::new()).await
                });
                let br = cm.att_chan(link).unwrap();
                srv_task = Some(tokio::task::spawn(srv.attach(&br).serve(br)));
            }
            srv = async { srv_task.as_mut().unwrap().await }, if srv_task.is_some() => {
                info!("GATT server terminated: {:?}", srv);
                srv_task = None;
            }
            r = cm.recv() => {
                assert!(link.is_none());
                link = Some(r?);
            }
        }
    }
}

async fn advertise(host: hci::Host) -> hci::Result<AdvEvent> {
    let mut adv = hci::Advertiser::new(&host).await?;
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
        .appearance(Appearance::GenericHumanInterfaceDevice)
        .tx_power(power);
    adv.set_data(h, data.get()).await?;
    let enable_params = hci::AdvEnableParams {
        handle: h,
        duration: Duration::from_secs(20),
        max_events: 0,
    };
    let adv_set = adv.enable(enable_params).await?;
    let r = adv_set.await;
    adv.remove_all().await?;
    r
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
        Box::pin(std::future::ready(true))
        /*
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
        */
    }
}

pub fn hex16(mut s: &str) -> Result<u16, String> {
    if s.starts_with("0x") || s.starts_with("0X") {
        s = &s[2..];
    }
    u16::from_str_radix(s, 16).map_err(|e| format!("{e}"))
}
