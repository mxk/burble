use tracing::{info, warn};

use burble::*;

fn main() {
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
    let host = hci::Host::new(all.remove(1).open().unwrap());
    info!("Local version: {:?}", host.read_local_version());
}
