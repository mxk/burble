use smallvec::SmallVec;

use super::*;

/// `HCI_Disconnection_Complete` event parameters.
#[derive(Clone, Copy, Debug)]
pub struct DisconnectionComplete {
    pub status: Status,
    pub handle: ConnHandle,
    pub reason: Status,
}

#[allow(clippy::fallible_impl_from)]
impl FromEvent for DisconnectionComplete {
    #[inline]
    fn matches(e: &Event) -> bool {
        matches!(e.typ(), EventType::Hci(EventCode::DisconnectionComplete))
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        Self {
            status: e.status(),
            handle: e.conn_handle().unwrap(),
            reason: Status::from(p.u8()),
        }
    }
}

/// `HCI_Number_Of_Completed_Packets` event parameters.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct NumberOfCompletedPackets(SmallVec<[(ConnHandle, u16); 4]>);

impl FromEvent for NumberOfCompletedPackets {
    #[inline]
    fn matches(e: &Event) -> bool {
        matches!(e.typ(), EventType::Hci(EventCode::NumberOfCompletedPackets))
    }

    fn unpack(_: &Event, p: &mut Unpacker) -> Self {
        let n = usize::from(p.u8());
        let mut v = SmallVec::with_capacity(n);
        for _ in 0..n {
            if let (Some(cn), n) = (ConnHandle::new(p.u16()), p.u16()) {
                v.push((cn, n));
            }
        }
        Self(v)
    }
}

impl AsRef<[(ConnHandle, u16)]> for NumberOfCompletedPackets {
    #[inline]
    fn as_ref(&self) -> &[(ConnHandle, u16)] {
        self.0.as_ref()
    }
}
