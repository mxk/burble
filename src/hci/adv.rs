use crate::dev::Addr;
use crate::host;

use super::*;

/// Advertising set manager.
#[derive(Debug)]
pub struct AdvManager<T: host::Transport> {
    host: Host<T>,
    _max_len: usize,
}

impl<T: host::Transport> AdvManager<T> {
    /// Creates a new advertisement manager.
    pub async fn new(host: Host<T>) -> Result<Self> {
        host.le_clear_advertising_sets().await?;
        let max_len = host.le_read_maximum_advertising_data_length().await?;
        Ok(Self {
            host,
            _max_len: max_len,
        })
    }

    /// Allocates a new advertising handle with the specified parameters.
    pub async fn alloc(&mut self, p: AdvParams) -> Result<(AdvHandle, TxPower)> {
        let h = AdvHandle(0); // TODO: Make dynamic
        self.host
            .le_set_extended_advertising_parameters(h, p)
            .await
            .map(|p| (h, p))
    }

    /// Sets advertising data.
    pub async fn set_data<V>(&mut self, h: AdvHandle, d: V) -> Result<()>
    where
        V: AsRef<[u8]> + Send + Sync,
    {
        // [Vol 4] Part E, Section 7.8.54
        for (op, chunk) in Self::op_chunks(d.as_ref(), 251) {
            self.host
                .le_set_extended_advertising_data(h, op, true, chunk)
                .await?;
        }
        Ok(())
    }

    /// Enable advertising.
    pub async fn enable(&mut self, cfg: &[AdvEnableParams]) -> Result<()> {
        self.host
            .le_set_extended_advertising_enable(true, cfg)
            .await
    }

    /// Disable advertising.
    pub async fn disable(&mut self, cfg: &[AdvHandle]) -> Result<()> {
        let cfg: Vec<AdvEnableParams> = cfg
            .iter()
            .map(|h| AdvEnableParams {
                handle: *h,
                ..AdvEnableParams::default()
            })
            .collect();
        self.host
            .le_set_extended_advertising_enable(false, cfg.as_slice())
            .await
    }

    /// Removes an advertising handle.
    pub async fn remove(&mut self, h: AdvHandle) -> Result<()> {
        self.host.le_remove_advertising_set(h).await
    }

    /// Removes all advertising handles.
    pub async fn remove_all(&mut self) -> Result<()> {
        self.host.le_clear_advertising_sets().await
    }

    /// Returns an iterator over chunks in `d`, specifying the appropriate
    /// data setting operation for each chunk.
    fn op_chunks(d: &[u8], chunk_size: usize) -> impl ExactSizeIterator<Item = (AdvDataOp, &[u8])> {
        let mut chunks = d.chunks(chunk_size);
        if chunks.len() == 0 {
            chunks = [0].chunks(1);
        }
        let last = chunks.len() - 1;
        chunks.enumerate().map(move |(i, c)| match i {
            0 if last == 0 => (AdvDataOp::Complete, d),
            0 => (AdvDataOp::First, c),
            i if last == i => (AdvDataOp::Last, c),
            _ => (AdvDataOp::Cont, c),
        })
    }
}

/// Advertising set handle. Dropping the handle does not disable the advertising
/// set.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct AdvHandle(u8);

impl Default for AdvHandle {
    fn default() -> Self {
        Self(0xFF) // Invalid handle
    }
}

impl From<AdvHandle> for u8 {
    #[inline]
    fn from(h: AdvHandle) -> Self {
        h.0
    }
}

/// `HCI_LE_Set_Extended_Advertising_Parameters` command parameters.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AdvParams {
    pub props: AdvProp,
    pub pri_interval: (Duration, Duration),
    pub pri_chan_map: AdvChanMap,
    pub addr_type: AdvAddrType,
    pub peer_addr: Addr,
    pub filter_policy: AdvFilterPolicy,
    pub tx_power: Option<TxPower>,
    pub pri_phy: AdvPhy,
    pub sec_max_skip: u8,
    pub sec_phy: AdvPhy,
    pub sid: u8,
    pub scan_request_notify: bool,
}

/// `HCI_LE_Set_Extended_Advertising_Enable` command parameters.
#[derive(Clone, Copy, Debug, Default)]
pub struct AdvEnableParams {
    pub handle: AdvHandle,
    pub duration: Duration,
    pub max_events: u8,
}

/// `HCI_LE_Set_Extended_Advertising_Enable` command parameters.
#[derive(Clone, Copy, Debug)]
pub struct AdvPeriodicParams {
    pub handle: AdvHandle,
    pub duration: Duration,
    pub max_events: u8,
}
