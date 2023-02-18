use matches::assert_matches;
use structbuf::Unpack;

use crate::hci::event::EventHeader;
use crate::hci::*;
use crate::le::RawAddr;

#[test]
fn hci() {
    let pkt = [1, 2, 3, 4];
    let (e, p) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(
        e,
        EventHeader {
            typ: EventType::Hci(EventCode::InquiryComplete),
            status: Status::HardwareFailure,
            ..EventHeader::default()
        }
    );
    assert_eq!(p, &pkt[3..]);
}

#[test]
fn le() {
    let pkt = [EventCode::LeMetaEvent as u8, 2, 2, 4];
    let (e, p) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(
        e,
        EventHeader {
            typ: EventType::Le(SubeventCode::AdvertisingReport),
            status: Status::Success,
            ..EventHeader::default()
        }
    );
    assert_eq!(p, &pkt[3..]);
}

#[test]
fn cmd_complete() {
    let mut pkt = vec![EventCode::CommandComplete as u8, 3, 3, 0x01, 0x10];
    let (e, _) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(
        e,
        EventHeader {
            typ: EventType::Hci(EventCode::CommandComplete),
            status: Status::Success,
            cmd_quota: 3,
            opcode: Opcode::ReadLocalVersionInformation,
            handle: 0,
        }
    );

    pkt[1] += 1;
    pkt.push(Status::UnknownCommand as _);
    let (e, _) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(e.status, Status::UnknownCommand);

    pkt[1] += 1;
    pkt.push(6);
    let (e, _) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(e.cmd_quota, 3);
    assert_eq!(e.opcode, Opcode::ReadLocalVersionInformation);
    assert_eq!(e.status, Status::UnknownCommand);
}

#[test]
fn cmd_status() {
    let pkt = [EventCode::CommandStatus as u8, 4, 0xff, 3, 0x01, 0x10];
    let (e, _) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(
        e,
        EventHeader {
            typ: EventType::Hci(EventCode::CommandStatus),
            status: Status::UnspecifiedError,
            cmd_quota: 3,
            opcode: Opcode::ReadLocalVersionInformation,
            handle: 0,
        }
    );
}

#[test]
fn error() {
    let event = |b: &[u8]| EventHeader::unpack(b).unwrap_err();
    assert_matches!(event(&[]), Error::InvalidEvent(_));
    assert_matches!(
        event(&[EventCode::InquiryComplete as u8, 1]),
        Error::InvalidEvent(_)
    );
    assert_matches!(
        event(&[0, 1, 2]),
        Error::UnknownEvent {
            code: 0,
            subevent: 0,
            ..
        }
    );
    assert_matches!(
        event(&[EventCode::LeMetaEvent as u8, 1, 0xff]),
        Error::UnknownEvent {
            code: 0x3e,
            subevent: 0xff,
            ..
        }
    );
}

#[test]
fn addr() {
    let pkt = [EventCode::Vendor as u8, 6, 0, 1, 2, 3, 4, 5];
    let (_, p) = EventHeader::unpack(pkt.as_ref()).unwrap();
    assert_eq!(
        p.unpack().addr(),
        RawAddr::from_le_bytes([0, 1, 2, 3, 4, 5])
    );
}
