#![allow(unused_crate_dependencies)]
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
use burble::hogp::{Input, MouseIn};
use burble::*;
use burble_const::Service;
use burble_crypto::NumCompare;

#[derive(Clone, Copy, Debug, clap::Parser)]
struct Args {
    /// Vendor ID of the Bluetooth USB device.
    #[arg(short, long, value_parser=hex16)]
    vid: Option<u16>,

    /// Product ID of the Bluetooth USB device.
    #[arg(short, long, value_parser=hex16)]
    pid: Option<u16>,

    /// Use legacy advertising.
    #[arg(short, long)]
    legacy: bool,
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
    host.le_set_default_phy(Some(hci::PhyMask::LE_2M), Some(hci::PhyMask::LE_2M))
        .await?;
    let r = serve(args, host).await;
    event_loop.stop().await?;
    r
}

fn read_input(srv: Arc<hogp::HidService>) -> tokio::task::JoinHandle<()> {
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
                _ = tokio::signal::ctrl_c() => return,
            };
            let mut tok = ln.split_ascii_whitespace();
            let Some(cmd) = tok.next() else { continue };
            let inp = match (cmd, tok.collect::<Vec<&str>>().join(" ")) {
                ("click" | "c", params) => {
                    if params.is_empty() {
                        Mouse(Click(0))
                    } else {
                        let Ok(v) = sscanf!(params, "{u8}") else { continue };
                        Mouse(Click(v))
                    }
                }
                ("move" | "m", params) => {
                    let Ok(v) = sscanf!(params, "{i32} {i32}") else { continue };
                    Mouse(MoveRel { dx: v.0, dy: v.1 })
                }
                _ => continue,
            };
            srv.exec(inp).await.unwrap();
        }
    })
}

async fn serve(args: Args, host: hci::Host) -> Result<()> {
    // [HOGP] Section 6.1
    const SEC: Access = Access::READ.authn().encrypt();
    let mut db = Db::build();
    gatt::Server::define_service(&mut db);
    gap::GapService::new("Burble", Appearance::GenericHumanInterfaceDevice).define(&mut db);
    dis::DeviceInfoService::new()
        .with_manufacturer_name("Blackrock Neurotech")
        // [HOGP] Section 3.3.2
        .with_pnp_id(dis::PnpId::new(dis::VendorId::USB(0x1209), 0x0001, (1, 0, 0)).unwrap())
        .define(&mut db, SEC);
    bas::BatteryService::new().define(&mut db, SEC);
    let hid = hogp::HidService::new();
    //#[cfg(debug_assertions)]
    //db.morph_next();
    hid.define(&mut db);
    let mut input_task = read_input(hid);

    let srv = gatt::Server::new(db, Arc::new(burble_fs::GattServerStore::per_user("burble")));
    srv.db().dump();

    let key_store: Arc<smp::KeyStore> = Arc::new(burble_fs::KeyStore::per_user("burble"));
    let mut secdb = smp::SecDb::new(host.clone(), Arc::clone(&key_store));
    tokio::task::spawn(async move { secdb.event_loop().await });

    // TODO: Redesign ChanManager for easier integration with advertisements
    let mut cm = l2cap::ChanManager::new(&host).await?;
    let mut adv_task = None;
    let mut srv_task = None;
    let mut link = None;
    loop {
        if srv_task.is_none() && adv_task.is_none() {
            info!("Enabling advertisements");
            adv_task = Some(tokio::task::spawn(advertise(args, host.clone())));
        }
        tokio::select! {
            _ = &mut input_task => return Ok(()),
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
                let key_store = Arc::clone(&key_store);
                tokio::task::spawn(async move {
                    let mut dev = smp::Device::new().with_display(Box::new(Dev)).with_confirm(Box::new(Dev));
                    smp.respond(&mut dev, key_store.as_ref()).await
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

async fn advertise(args: Args, host: hci::Host) -> hci::Result<AdvEvent> {
    let mut adv = hci::Advertiser::new(&host).await?;
    let params = if args.legacy {
        hci::AdvParams {
            props: hci::AdvProp::CONNECTABLE | hci::AdvProp::SCANNABLE | hci::AdvProp::LEGACY,
            pri_interval: (Duration::from_millis(20), Duration::from_millis(25)),
            ..hci::AdvParams::default()
        }
    } else {
        hci::AdvParams {
            props: hci::AdvProp::CONNECTABLE | hci::AdvProp::INCLUDE_TX_POWER,
            pri_interval: (Duration::from_millis(20), Duration::from_millis(25)),
            sec_phy: hci::Phy::Le2M,
            ..hci::AdvParams::default()
        }
    };
    let (h, power) = adv.create(params).await?;
    let mut data = gap::ResponseDataMut::new();
    data.flags(gap::AdvFlag::LE_GENERAL | gap::AdvFlag::NO_BREDR)
        // [HOGP] Section 3.1.3
        .service(false, [Service::HumanInterfaceDevice])
        // [HOGP] Section 3.1.4
        .local_name(true, "Burble")
        // [HOGP] Section 3.1.5
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
