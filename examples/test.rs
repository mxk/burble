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
    let hci_mon = host.enable_events();
    host.init().await?;
    info!("Local version: {:?}", host.read_local_version().await?);
    info!("Device address: {:?}", host.read_bd_addr().await?);

    let mut _cm = l2cap::ChanManager::new(&host).await?;
    let mut adv = hci::Advertiser::new(host.clone()).await?;
    info!("Max len: {}", adv.max_data_len());
    let (h, power) = (adv.create(hci::AdvParams {
        props: hci::AdvProp::CONNECTABLE | hci::AdvProp::INCLUDE_TX_POWER,
        pri_interval: (Duration::from_millis(20), Duration::from_millis(25)),
        sec_phy: hci::AdvPhy::Le2M,
        ..hci::AdvParams::default()
    }))
    .await?;
    let mut rd = gap::ResponseDataMut::new();
    rd.flags(gap::AdvFlag::LE_GENERAL | gap::AdvFlag::NO_BREDR)
        .local_name(true, "Burble")
        .appearance(gap::Appearance::GenericHumanInterfaceDevice)
        .tx_power(power);
    adv.set_data(h, rd.get()).await?;
    let mut adv_mon = (adv.enable(hci::AdvEnableParams {
        handle: h,
        duration: Duration::from_secs(20),
        max_events: 0,
    }))
    .await?;

    let r = adv_mon.event().await;
    info!("Result: {r:?}");

    adv.remove_all().await?;
    Ok(hci_mon.disable().await?)
}
