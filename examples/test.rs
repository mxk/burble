use tracing::{info, warn};

use burble::*;

fn main() {
    tracing_subscriber::fmt::init();
    let h = Host::new().unwrap();
    let mut all = h.controllers().unwrap();
    if all.is_empty() {
        warn!("No Bluetooth controllers available");
        return;
    }
    for c in all.iter() {
        info!("Controller at {c}");
    }
    let hci = Hci::new(all.remove(1).open().unwrap());
    info!("Local version: {:?}", hci.read_local_version());
}
