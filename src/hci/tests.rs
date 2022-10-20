use crate::dev::RawAddr;

use super::*;

#[test]
fn event() {
    let pkt = [1, 2, 3, 4];
    let mut e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.typ(), EventType::Hci(EventCode::InquiryComplete));
    assert_eq!(e.tail(), &pkt[2..]);
    assert_eq!(e.cmd_status(), None);
    assert_eq!(e.u8(), pkt[2]);
    assert_eq!(e.u8(), pkt[3]);
    assert!(e.tail().is_empty());
}

#[test]
fn event_le() {
    let pkt = [EventCode::LeMetaEvent as u8, 2, 3, 4];
    let mut e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(
        e.typ(),
        EventType::Le(SubeventCode::ConnectionUpdateComplete)
    );
    assert_eq!(e.tail(), &pkt[3..]);
    assert_eq!(e.u8(), pkt[3]);
}

#[test]
fn event_cmd_complete() {
    let mut pkt = vec![EventCode::CommandComplete as u8, 3, 3, 0x01, 0x10];
    let e = Event::try_from(pkt.as_ref()).unwrap();
    let mut want = CommandStatus {
        cmd_quota: 3,
        opcode: Opcode::ReadLocalVersionInformation,
        status: Status::Success,
    };
    assert_eq!(e.cmd_status(), Some(want));

    pkt[1] += 1;
    pkt.push(Status::UnknownCommand as _);
    let e = Event::try_from(pkt.as_ref()).unwrap();
    want.status = Status::UnknownCommand;
    assert_eq!(e.cmd_status(), Some(want));

    pkt[1] += 1;
    pkt.push(6);
    let e = Event::try_from(pkt.as_ref()).unwrap();
    assert_eq!(e.cmd_status(), Some(want));
}

#[test]
fn event_cmd_status() {
    let pkt = [EventCode::CommandStatus as u8, 4, 0xff, 3, 0x01, 0x10];
    let e = Event::try_from(pkt.as_ref()).unwrap();
    let want = CommandStatus {
        cmd_quota: 3,
        opcode: Opcode::ReadLocalVersionInformation,
        status: Status::UnspecifiedError,
    };
    assert_eq!(e.cmd_status(), Some(want));
}

#[test]
fn event_error() {
    let event = |b: &[u8]| Event::try_from(b).unwrap_err();
    assert!(matches!(event(&[]), Error::InvalidEvent(_)));
    assert!(matches!(
        event(&[EventCode::InquiryComplete as u8, 1]),
        Error::InvalidEvent(_)
    ));
    assert!(matches!(
        event(&[0, 1, 2]),
        Error::UnknownEvent {
            code: 0,
            subevent: 0,
            ..
        }
    ));
    assert!(matches!(
        event(&[EventCode::LeMetaEvent as u8, 1, 0xff]),
        Error::UnknownEvent {
            code: 0x3e,
            subevent: 0xff,
            ..
        }
    ));
}

#[test]
fn event_addr() {
    let mut e = Event::try_from([EventCode::Vendor as u8, 6, 0, 1, 2, 3, 4, 5].as_ref()).unwrap();
    assert_eq!(e.addr(), RawAddr::from([0, 1, 2, 3, 4, 5]));
    assert!(e.tail().is_empty());
}
