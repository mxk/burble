use smallvec::SmallVec;

use super::*;

/// `HCI_Disconnection_Complete` event parameters
/// ([Vol 4] Part E, Section 7.7.5).
#[derive(Clone, Copy, Debug)]
pub struct DisconnectionComplete {
    pub status: Status,
    pub handle: ConnHandle,
    pub reason: Status,
}

impl FromEvent for DisconnectionComplete {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(c, EventCode::DisconnectionComplete)
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        Self {
            status: e.status(),
            handle: e.conn_handle().unwrap(),
            reason: Status::from(p.u8()),
        }
    }
}

/// `HCI_Encryption_Change` event parameters ([Vol 4] Part E, Section 7.7.8).
#[derive(Clone, Copy, Debug)]
pub struct EncryptionChange {
    pub status: Status,
    pub handle: ConnHandle,
    pub enabled: bool,
}

impl FromEvent for EncryptionChange {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(
            c,
            EventCode::EncryptionChangeV1 | EventCode::EncryptionChangeV2
        )
    }

    fn unpack(e: &Event, p: &mut Unpacker) -> Self {
        let enabled = p.bool();
        if matches!(e.code(), EventCode::EncryptionChangeV2) {
            // Encryption_Key_Size parameter must be consumed, but is ignored
            // for LE connections.
            let _ = p.u8();
        }
        Self {
            status: e.status(),
            handle: e.conn_handle().unwrap(),
            enabled,
        }
    }
}

/// `HCI_Number_Of_Completed_Packets` event parameters
/// ([Vol 4] Part E, Section 7.7.19).
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct NumberOfCompletedPackets(SmallVec<[(ConnHandle, u16); 4]>);

impl FromEvent for NumberOfCompletedPackets {
    #[inline(always)]
    fn matches(c: EventCode) -> bool {
        matches!(c, EventCode::NumberOfCompletedPackets)
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
