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
    let (_, _) = tokio::try_join!(host.event(), host.reset())?;
    let (_, ver) = tokio::try_join!(host.event(), host.read_local_version())?;
    info!("Local version: {:?}", ver);
    Ok(())
}
