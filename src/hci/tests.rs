use super::*;

#[test]
fn event() {
    let mut e = Evt::new();
    assert_eq!(e.typ(), EvtType::Pending);
    assert!(e.as_ref().is_empty());
    assert!(e.tail().is_empty());

    // Allocate buffer for receiving the next event
    let buf = e.reset();
    let ptr = buf.as_ptr();
    assert_eq!(buf.len(), EVT_BUF);
    assert!(buf.iter().all(|&v| v == 0));

    // Parse received event
    let pkt = [EventCode::InquiryComplete as u8, 2, 3, 4];
    buf[..pkt.len()].copy_from_slice(&pkt);
    e.ready(pkt.len()).unwrap();
    assert_eq!(e.typ(), EvtType::Hci(EventCode::from_repr(pkt[0]).unwrap()));
    assert_eq!(e.as_ref(), &pkt);
    assert_eq!(e.tail(), &pkt[2..]);
    assert_eq!(e.u8(), pkt[2]);
    assert_eq!(e.u8(), pkt[3]);
    assert!(e.tail().is_empty());

    // Reuse buffer
    let buf = e.reset();
    assert_eq!(buf.as_ptr(), ptr);
    assert_eq!(buf.len(), EVT_BUF);
    assert_eq!(e.typ(), EvtType::Pending);
    assert!(e.as_ref().is_empty());
    assert!(e.tail().is_empty());
}

#[test]
fn event_error() {
    let mut e = Evt::new();
    let mut event = |b: &[u8]| {
        e.reset()[..b.len()].copy_from_slice(b);
        e.ready(b.len()).unwrap_err()
    };
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
