use tracing::info;

use burble::*;

fn main() {
    tracing_subscriber::fmt::init();
    let h = Host::new().unwrap();
    for c in h.controllers().unwrap() {
        info!("Controller at {c}");
    }
}
