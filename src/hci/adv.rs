use crate::host;

use super::*;

/// Advertising set manager.
#[derive(Debug)]
pub struct AdvManager<T: host::Transport> {
    host: Host<T>,
    max_len: usize,
}

impl<T: host::Transport> AdvManager<T> {
    /// Creates a new advertisement manager.
    pub async fn new(host: Host<T>) -> Result<Self> {
        host.le_clear_advertising_sets().await?;
        let max_len = host.le_read_maximum_advertising_data_length().await?;
        Ok(Self { host, max_len })
    }

    /// Returns the maximum advertising data length.
    #[inline]
    #[must_use]
    pub const fn max_len(&self) -> usize {
        self.max_len
    }

    /// Allocates a new advertising handle with the specified parameters.
    pub async fn alloc_handle(&mut self, p: AdvParams) -> Result<(AdvHandle, TxPower)> {
        let h = AdvHandle::from_raw(0); // TODO: Make dynamic
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
    pub async fn enable(&mut self, p: AdvEnableParams) -> Result<AdvGuard<T>> {
        let waiter = Arc::clone(&self.host.router).register(EventFilter::AdvManager)?;
        self.host
            .le_set_extended_advertising_enable(true, &[p])
            .await?;
        Ok(AdvGuard { waiter, conn: None })
    }

    // Disable advertising.
    pub async fn disable(&mut self, h: AdvHandle) -> Result<()> {
        let p = AdvEnableParams {
            handle: h,
            ..AdvEnableParams::default()
        };
        self.host
            .le_set_extended_advertising_enable(false, &[p])
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
            // Ensure that there is always at least one chunk
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

#[derive(Debug)]
pub struct AdvGuard<T: host::Transport> {
    waiter: EventWaiterGuard<T>,
    conn: Option<LeConnectionComplete>,
}

impl<T: host::Transport> AdvGuard<T> {
    pub async fn accept(&mut self) -> Result<LeConnectionComplete> {
        use SubeventCode::*;
        loop {
            let evt = self
                .waiter
                .next()
                .await
                .ok_or_else(|| Error::from(Status::UnspecifiedError))?;
            match evt.typ() {
                EventType::Le(ConnectionComplete | EnhancedConnectionComplete) => {
                    self.conn = Some((&mut evt.get()).into());
                }
                EventType::Le(AdvertisingSetTerminated) => {
                    if let Some(conn) = self.conn.take() {
                        let evt = LeAdvertisingSetTerminated::from(&mut evt.get());
                        if conn.handle == evt.conn_handle {
                            return Ok(conn);
                        }
                    }
                    let mut s = evt.status();
                    if s.is_ok() {
                        s = Status::UnknownConnectionIdentifier;
                    }
                    return Err(Error::from(s));
                }
                _ => unreachable!(),
            }
        }
    }
}
