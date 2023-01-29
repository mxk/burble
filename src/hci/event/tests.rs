use matches::assert_matches;

use crate::hci::*;
use crate::le::RawAddr;

#[test]
fn hci() {
    let pkt = [1, 2, 3, 4];
    let mut e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.typ(), EventType::Hci(EventCode::InquiryComplete));
    assert_eq!(e.status(), Status::HardwareFailure);
    assert_eq!(e.params.as_ref(), &pkt[3..]);
    assert_eq!(e.u8(), pkt[3]);
    assert!(e.params.is_empty());
}

#[test]
fn le() {
    let pkt = [EventCode::LeMetaEvent as u8, 2, 2, 4];
    let mut e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.typ(), EventType::Le(SubeventCode::AdvertisingReport));
    assert_eq!(e.params.as_ref(), &pkt[3..]);
    assert_eq!(e.u8(), pkt[3]);
}

#[test]
fn cmd_complete() {
    let mut pkt = vec![EventCode::CommandComplete as u8, 3, 3, 0x01, 0x10];
    let e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.cmd_quota, 3);
    assert_eq!(e.opcode(), Opcode::ReadLocalVersionInformation);
    assert_eq!(e.status(), Status::Success);

    pkt[1] += 1;
    pkt.push(Status::UnknownCommand as _);
    let e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.status(), Status::UnknownCommand);

    pkt[1] += 1;
    pkt.push(6);
    let e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.cmd_quota, 3);
    assert_eq!(e.opcode(), Opcode::ReadLocalVersionInformation);
    assert_eq!(e.status(), Status::UnknownCommand);
}

#[test]
fn cmd_status() {
    let pkt = [EventCode::CommandStatus as u8, 4, 0xff, 3, 0x01, 0x10];
    let e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.cmd_quota, 3);
    assert_eq!(e.opcode(), Opcode::ReadLocalVersionInformation);
    assert_eq!(e.status(), Status::UnspecifiedError);
}

#[test]
fn error() {
    let event = |b: &[u8]| Event::try_from(b).unwrap_err();
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
    let mut e = Event::try_from([EventCode::Vendor as u8, 6, 0, 1, 2, 3, 4, 5].as_ref()).unwrap();
    assert_eq!(e.addr(), RawAddr::from_le_bytes([0, 1, 2, 3, 4, 5]));
    assert!(e.params.is_empty());
}
