use std::collections::BTreeSet;

use futures_core::FusedFuture;
use pin_project::pin_project;

use crate::le::TxPower;

use super::*;

/// Advertisement manager.
#[derive(Debug)]
pub struct Advertiser {
    host: Host,
    handles: BTreeSet<AdvHandle>,
    max_data_len: usize,
}

impl Advertiser {
    /// Creates a new advertisement manager.
    pub async fn new(host: &Host) -> Result<Self> {
        host.le_clear_advertising_sets().await?;
        Ok(Self {
            host: host.clone(),
            handles: BTreeSet::new(),
            max_data_len: host.le_read_maximum_advertising_data_length().await?,
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

    /// Sets scan response data.
    pub async fn set_scan_response<V>(&mut self, h: AdvHandle, d: V) -> Result<()>
    where
        V: AsRef<[u8]> + Send + Sync,
    {
        // [Vol 4] Part E, Section 7.8.55
        for (op, chunk) in Self::op_chunks(d.as_ref(), 31) {
            self.host
                .le_set_extended_scan_response_data(h, op, true, chunk)
                .await?;
        }
        Ok(())
    }

    /// Enable advertising.
    pub async fn enable(&mut self, p: impl Into<AdvEnableParams> + Send) -> Result<AdvFuture> {
        let p = p.into();
        let ctl = self.host.events();
        (self.host.le_set_extended_advertising_enable(true, &[p])).await?;
        Ok(AdvFuture::new(p.handle, ctl, self.host.info.addr))
    }

    // Disable advertising.
    pub async fn disable(&mut self, h: AdvHandle) -> Result<()> {
        self.host
            .le_set_extended_advertising_enable(false, &[h.into()])
            .await
    }

    // Disable advertising.
    pub async fn disable_all(&mut self) -> Result<()> {
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
        for i in AdvHandle::MIN..=AdvHandle::MAX {
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

/// Advertising set completion result.
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

/// Advertising set future.
#[pin_project(project = AdvFutureProj)]
#[derive(Debug)]
pub struct AdvFuture {
    hdl: Option<AdvHandle>,
    ctl: EventStream,
    local_addr: Addr,
    conn: BTreeMap<ConnHandle, LeConnectionComplete>,
    term: Option<LeAdvertisingSetTerminated>,
    #[pin]
    timeout: Option<tokio::time::Sleep>,
}

impl AdvFuture {
    /// Creates a new advertising set future.
    #[inline]
    #[must_use]
    fn new(hdl: AdvHandle, ctl: EventStream, local_addr: Addr) -> Self {
        Self {
            hdl: Some(hdl),
            ctl,
            local_addr,
            conn: BTreeMap::new(),
            term: None,
            timeout: None,
        }
    }
}

impl AdvFutureProj<'_> {
    /// Polls the timeout after [`LeAdvertisingSetTerminated`] event is
    /// received.
    #[inline]
    fn poll_timeout(&mut self, cx: &mut Context<'_>) -> Poll<<AdvFuture as Future>::Output> {
        match self.timeout.as_mut().as_pin_mut().map(|s| s.poll(cx)) {
            Some(Poll::Ready(_)) => {
                let term = self.term.take().unwrap();
                self.ready(Ok(AdvEvent::Term(term)))
            }
            _ => Poll::Pending,
        }
    }

    /// Resolves the future with the specified result.
    #[inline(always)]
    fn ready(&mut self, r: <AdvFuture as Future>::Output) -> Poll<<AdvFuture as Future>::Output> {
        *self.hdl = None;
        Poll::Ready(r)
    }

    /// Resolves the future with a successful connection.
    #[inline]
    fn ready_conn(
        &mut self,
        conn: LeConnectionComplete,
        term: LeAdvertisingSetTerminated,
    ) -> Poll<<AdvFuture as Future>::Output> {
        if conn.status.is_ok() {
            (self.ctl).update_conn(conn.handle, |cn| cn.local_addr = *self.local_addr);
        }
        self.ready(Ok(AdvEvent::Conn { conn, term }))
    }
}

impl Future for AdvFuture {
    type Output = Result<AdvEvent>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        use EventCode::*;
        let mut this = self.project();
        let hdl = this.hdl.expect("poll of a completed future");
        let evt = match this.ctl.poll(Some(cx)) {
            Poll::Ready(r) => match r {
                Ok(evt) => {
                    // Register a waker for the next event
                    let p = this.ctl.poll(Some(cx));
                    debug_assert!(p.is_pending());
                    evt
                }
                Err(e) => return this.ready(Err(e)),
            },
            Poll::Pending => return this.poll_timeout(cx),
        };
        // TODO: High duty cycle connectable directed advertisements may
        // terminate without an LeAdvertisingSetTerminated event.
        let term = match evt.code() {
            LeConnectionComplete | LeEnhancedConnectionComplete => {
                let conn: super::LeConnectionComplete = evt.get();
                if let Some(term) = this.term.as_ref() {
                    if conn.handle == term.conn_handle.unwrap() {
                        return this.ready_conn(conn, term.clone());
                    }
                }
                // Wait for LeAdvertisingSetTerminated event
                this.conn.insert(conn.handle, conn);
                return Poll::Pending;
            }
            LeAdvertisingSetTerminated => {
                let term: super::LeAdvertisingSetTerminated = evt.get();
                if term.adv_handle != hdl || this.term.is_some() {
                    return Poll::Pending;
                }
                term
            }
            _ => return Poll::Pending,
        };
        if !term.status.is_ok() || term.conn_handle.is_none() {
            return this.ready(Ok(AdvEvent::Term(term)));
        }
        if let Some(conn) = this.conn.remove(&term.conn_handle.unwrap()) {
            return this.ready_conn(conn, term);
        }
        // The spec says that when a connection is created, the controller
        // generates an LeConnectionComplete event followed immediately by an
        // LeAdvertisingSetTerminated event ([Vol 4] Part E, Section 7.8.56). At
        // least one controller (RTL8761BUV) does the opposite, so we wait for a
        // short amount of time for a matching ConnectionComplete event, which
        // normally comes within 5ms.
        *this.term = Some(term);
        (this.timeout).set(Some(tokio::time::sleep(Duration::from_millis(100))));
        this.poll_timeout(cx)
    }
}

impl FusedFuture for AdvFuture {
    #[inline(always)]
    fn is_terminated(&self) -> bool {
        self.hdl.is_none()
    }
}
