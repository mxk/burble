use smallvec::SmallVec;

use super::*;

/// `HCI_Number_Of_Completed_Packets` event parameters.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct NumberOfCompletedPackets(SmallVec<[(ConnHandle, u16); 4]>);

impl From<&mut Event<'_>> for NumberOfCompletedPackets {
    fn from(e: &mut Event<'_>) -> Self {
        let n = usize::from(e.u8());
        let mut v = SmallVec::with_capacity(n);
        for _ in 0..n {
            v.push((ConnHandle::from_raw(e.u16()), e.u16()))
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
