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
    tokio::select! {
        r = host.next_event() => { r?; }
        v = host.read_local_version() => info!("Local version: {:?}", v?),
    }
    Ok(())
}
