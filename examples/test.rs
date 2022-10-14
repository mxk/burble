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
    let ver = host.read_local_version().await?;
    info!("Local version: {:?}", ver);
    Ok(mon.disable().await?)
}
