use std::collections::{HashMap, HashSet};

use crate::host;
use crate::le::TxPower;

use super::*;

/// Advertisement manager.
#[derive(Debug)]
pub struct Advertiser<T: host::Transport> {
    host: Host<T>,
    handles: HashSet<AdvHandle>,
    max_data_len: usize,
}

impl<T: host::Transport> Advertiser<T> {
    /// Creates a new advertisement manager.
    pub async fn new(host: Host<T>) -> Result<Self> {
        host.le_clear_advertising_sets().await?;
        let handles = HashSet::with_capacity(usize::from(
            host.le_read_number_of_supported_advertising_sets().await?,
        ));
        let max_data_len = host.le_read_maximum_advertising_data_length().await?;
        Ok(Self {
            host,
            handles,
            max_data_len,
        })
    }

    /// Returns the maximum advertising data length.
    #[inline]
    #[must_use]
    pub const fn max_data_len(&self) -> usize {
        self.max_data_len
    }

    /// Creates a new advertising handle with the specified parameters.
    pub async fn create(&mut self, p: AdvParams) -> Result<(AdvHandle, TxPower)> {
        // TODO: Allow using a random address.
        // TODO: Handle legacy advertisements?
        let h = self.alloc_handle()?;
        (self.host.le_set_extended_advertising_parameters(h, p).await).map(|p| {
            self.handles.insert(h);
            (h, p)
        })
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
    pub async fn enable(&mut self, p: AdvEnableParams) -> Result<AdvMonitor<T>> {
        let waiter = self.host.register(EventFilter::AdvManager)?;
        (self.host.le_set_extended_advertising_enable(true, &[p])).await?;
        Ok(AdvMonitor::new(waiter))
    }

    // Disable advertising.
    pub async fn disable(&mut self, h: AdvHandle) -> Result<()> {
        // TODO: Wake monitor(s).
        self.host
            .le_set_extended_advertising_enable(false, &[h.into()])
            .await
    }

    // Disable advertising.
    pub async fn disable_all(&mut self) -> Result<()> {
        // TODO: Wake monitor(s).
        (self.host.le_set_extended_advertising_enable(false, &[])).await
    }

    /// Removes an advertising handle.
    pub async fn remove(&mut self, h: AdvHandle) -> Result<()> {
        self.host.le_remove_advertising_set(h).await
    }

    /// Removes all advertising handles.
    pub async fn remove_all(&mut self) -> Result<()> {
        self.host.le_clear_advertising_sets().await
    }

    /// Allocates an unused advertising handle.
    fn alloc_handle(&self) -> Result<AdvHandle> {
        for i in 0..=AdvHandle::MAX {
            // SAFETY: `i` is always valid
            let h = unsafe { AdvHandle::new(i).unwrap_unchecked() };
            if !self.handles.contains(&h) {
                return Ok(h);
            }
        }
        Err(Status::LimitReached.into())
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

/// Result of enabling an advertising set.
#[allow(variant_size_differences)]
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum AdvEvent {
    Conn {
        conn: LeConnectionComplete,
        term: LeAdvertisingSetTerminated,
    },
    Term(LeAdvertisingSetTerminated),
}

#[derive(Debug)]
pub struct AdvMonitor<T: host::Transport> {
    waiter: EventWaiterGuard<T>,
    conn: HashMap<ConnHandle, LeConnectionComplete>,
}

impl<T: host::Transport> AdvMonitor<T> {
    /// Creates a new advertising monitor.
    #[inline]
    #[must_use]
    fn new(waiter: EventWaiterGuard<T>) -> Self {
        Self {
            waiter,
            conn: HashMap::with_capacity(1),
        }
    }

    /// Returns the next event for enabled advertising sets.
    pub async fn event(&mut self) -> Result<AdvEvent> {
        use SubeventCode::*;
        let term = loop {
            let evt = self.waiter.next().await?;
            match evt.typ() {
                EventType::Le(ConnectionComplete | EnhancedConnectionComplete) => {
                    // TODO: High duty cycle connectable directed advertisements
                    // may terminate without an AdvertisingSetTerminated event.
                    let conn = LeConnectionComplete::from(&mut evt.get());
                    self.conn.insert(conn.handle, conn);
                }
                EventType::Le(AdvertisingSetTerminated) => {
                    break LeAdvertisingSetTerminated::from(&mut evt.get())
                }
                _ => continue,
            }
        };

        // Handle AdvertisingSetTerminated event
        if !term.status.is_ok() || term.conn_handle.is_none() {
            return Ok(AdvEvent::Term(term));
        }
        let cn = term.conn_handle.unwrap();
        if let Some(conn) = self.conn.remove(&cn) {
            return Ok(AdvEvent::Conn { conn, term });
        }
        // The spec says that when a connection is created, the controller must
        // generate a ConnectionComplete event followed immediately by an
        // AdvertisingSetTerminated event ([Vol 4] Part E, Section 7.8.56). At
        // least one controller (RTL8761BUV) does the opposite, so we wait for a
        // short amount of time for a matching ConnectionComplete event, which
        // normally comes within 5ms.
        if let Ok(Ok(evt)) =
            tokio::time::timeout(Duration::from_millis(100), self.waiter.next()).await
        {
            if let EventType::Le(ConnectionComplete | EnhancedConnectionComplete) = evt.typ() {
                let conn = LeConnectionComplete::from(&mut evt.get());
                if conn.handle == cn {
                    return Ok(AdvEvent::Conn { conn, term });
                }
                self.conn.insert(conn.handle, conn);
            }
        }
        Ok(AdvEvent::Term(term))
    }
}
