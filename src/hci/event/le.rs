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

impl FromEvent for LeConnectionComplete {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(
            c,
            EventCode::LeConnectionComplete | EventCode::LeEnhancedConnectionComplete
        )
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        let role = Role::try_from(p.u8()).expect("invalid role");
        let peer_addr = Addr::peer(p.u8(), p.addr());
        let (local_rpa, peer_rpa) = match e.code() {
            EventCode::LeConnectionComplete => Default::default(),
            EventCode::LeEnhancedConnectionComplete => (p.addr(), p.addr()),
            code => unreachable!("unexpected {code} event"),
        };
        Self {
            status: e.status(),
            handle: e.conn_handle().unwrap(),
            role,
            peer_addr,
            local_rpa,
            peer_rpa,
            conn_interval: duration_1250us(p.u16()),
            peripheral_latency: p.u16(),
            supervision_timeout: duration_10ms(p.u16()),
            central_clock_accuracy: match p.u8() {
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
    pub handle: ConnHandle,
    pub rand: u64,
    pub ediv: u16,
}

impl FromEvent for LeLongTermKeyRequest {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(c, EventCode::LeLongTermKeyRequest)
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        Self {
            handle: e.conn_handle().unwrap(),
            rand: p.u64(),
            ediv: p.u16(),
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

impl FromEvent for LeAdvertisingSetTerminated {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(c, EventCode::LeAdvertisingSetTerminated)
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        Self {
            status: e.status(),
            adv_handle: e.adv_handle().unwrap(),
            conn_handle: {
                let cn = p.u16();
                if e.status().is_ok() {
                    ConnHandle::new(cn)
                } else {
                    None
                }
            },
            num_events: p.u8(),
        }
    }
}
