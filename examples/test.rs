use tracing::{info, warn};

use burble::*;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    tracing_subscriber::fmt::init();
    let usb = host::Usb::new().unwrap();
    let mut all = usb.controllers().unwrap();
    if all.is_empty() {
        warn!("No Bluetooth controllers available");
        return;
    }
    for c in all.iter() {
        info!("Controller at {c}");
    }
    let mut ctlr = all.remove(all.len() - 1).open().unwrap();
    ctlr.init().unwrap();
    let mut host = hci::Host::new(ctlr);
    info!("Local version: {:?}", host.read_local_version().await);
}
