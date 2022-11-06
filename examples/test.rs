use std::time::Duration;

use anyhow::Result;
use tracing::info;

use burble::*;

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let usb = host::Usb::new()?;
    let mut ctlr = usb.open_first(0x7392, 0xC611)?;
    ctlr.init()?;
    let host = hci::Host::new(ctlr);
    let mon = host.enable_events();
    host.init().await?;
    info!("Local version: {:?}", host.read_local_version().await?);
    info!("Device address: {:?}", host.read_bd_addr().await?);

    let mut adv = hci::AdvManager::new(host.clone()).await?;
    info!("Max len: {}", adv.max_len());
    let (h, power) = adv
        .alloc_handle(hci::AdvParams {
            props: hci::AdvProp::CONNECTABLE | hci::AdvProp::INCLUDE_TX_POWER,
            pri_interval: (Duration::from_millis(20), Duration::from_millis(25)),
            ..hci::AdvParams::default()
        })
        .await?;
    let mut rd = gap::ResponseDataMut::new();
    rd.flags(gap::AdvFlag::LE_GENERAL | gap::AdvFlag::NO_BREDR)
        .local_name(true, "Burble")
        .appearance(gap::Appearance::GenericHumanInterfaceDevice)
        .tx_power(power);
    adv.set_data(h, rd.freeze()).await?;
    let mut ag = adv
        .enable(hci::AdvEnableParams {
            handle: h,
            duration: Duration::from_secs(20),
            max_events: 0,
        })
        .await?;
    let r = ag.accept().await;
    info!("Result: {r:?}");

    tokio::time::sleep(Duration::from_secs(1)).await;
    Ok(mon.disable().await?)
}
