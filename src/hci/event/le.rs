use crate::le::{Addr, RawAddr};

use super::*;

/// `HCI_LE_Connection_Complete` and `HCI_LE_Enhanced_Connection_Complete` event
/// parameters ([Vol 4] Part E, Section 7.7.65.1 and 7.7.65.10).
#[derive(Clone, Debug)]
pub struct LeConnectionComplete {
    pub status: Status,
    pub handle: ConnHandle,
    pub role: Role,
    pub peer_addr: Addr,
    pub local_rpa: RawAddr,
    pub peer_rpa: RawAddr,
    pub conn_interval: Duration,
    pub peripheral_latency: u16,
    pub supervision_timeout: Duration,
    pub central_clock_accuracy: u16,
}

#[allow(clippy::fallible_impl_from)]
impl From<&mut Event<'_>> for LeConnectionComplete {
    fn from(e: &mut Event<'_>) -> Self {
        let role = Role::try_from(e.u8()).expect("invalid role");
        let peer_addr = Addr::peer(e.u8(), e.addr());
        let (local_rpa, peer_rpa) = match e.typ() {
            EventType::Le(SubeventCode::ConnectionComplete) => Default::default(),
            EventType::Le(SubeventCode::EnhancedConnectionComplete) => (e.addr(), e.addr()),
            t => panic!("Invalid event type: {t}"),
        };
        Self {
            status: e.status(),
            handle: e.conn_handle().unwrap(),
            role,
            peer_addr,
            local_rpa,
            peer_rpa,
            conn_interval: duration_1250us(e.u16()),
            peripheral_latency: e.u16(),
            supervision_timeout: duration_10ms(e.u16()),
            central_clock_accuracy: match e.u8() {
                0x00 => 500,
                0x01 => 250,
                0x02 => 150,
                0x03 => 100,
                0x04 => 75,
                0x05 => 50,
                0x06 => 30,
                0x07 => 20,
                _ => 0,
            },
        }
    }
}

/// `HCI_LE_Long_Term_Key_Request` event parameters
/// ([Vol 4] Part E, Section 7.7.65.5).
#[derive(Clone, Debug)]
pub struct LeLongTermKeyRequest {
    pub conn_handle: ConnHandle,
    pub rand: u64,
    pub ediv: u16,
}

#[allow(clippy::fallible_impl_from)]
impl From<&mut Event<'_>> for LeLongTermKeyRequest {
    fn from(e: &mut Event<'_>) -> Self {
        Self {
            conn_handle: ConnHandle::new(e.u16()).unwrap(),
            rand: e.u64(),
            ediv: e.u16(),
        }
    }
}

/// `HCI_LE_Advertising_Set_Terminated` event parameters
/// ([Vol 4] Part E, Section 7.7.65.18).
#[derive(Clone, Debug)]
pub struct LeAdvertisingSetTerminated {
    pub status: Status,
    pub adv_handle: AdvHandle,
    pub conn_handle: Option<ConnHandle>,
    pub num_events: u8,
}

#[allow(clippy::fallible_impl_from)]
impl From<&mut Event<'_>> for LeAdvertisingSetTerminated {
    fn from(e: &mut Event<'_>) -> Self {
        Self {
            status: e.status(),
            adv_handle: e.adv_handle().unwrap(),
            conn_handle: {
                let cn = e.u16();
                if e.status().is_ok() {
                    ConnHandle::new(cn)
                } else {
                    None
                }
            },
            num_events: e.u8(),
        }
    }
}
