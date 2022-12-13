use std::future::Future;
use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::task::{Context, Poll};

/// Async conditional variable consisting of a mutex-protected value and a
/// notification mechanism to alert any waiters of changes to the value.
#[derive(Debug, Default)]
pub(crate) struct Condvar<T> {
    mutex: parking_lot::Mutex<T>,
    notify: tokio::sync::Notify,
}

impl<T> Condvar<T> {
    /// Creates a new conditional variable.
    #[inline]
    #[must_use]
    pub fn new(v: T) -> Self {
        Self {
            mutex: parking_lot::Mutex::new(v),
            notify: tokio::sync::Notify::new(),
        }
    }

    /// Blocks the current thread until the mutex is acquired.
    #[inline]
    pub fn lock(&self) -> CondvarGuard<T> {
        // TODO: It would be nice to clear the notify_one permit after acquiring
        // the lock here to prevent an immediate spurious wakeup.
        CondvarGuard {
            guard: self.mutex.lock(),
            notify: &self.notify,
            notify_all: AtomicBool::new(false),
        }
    }

    /// Notifies a single waiter, storing a permit if there are no current
    /// waiters. This ensures that the next waiter wakes up immediately and is
    /// safe to use without holding the mutex.
    #[allow(dead_code)] // TODO: Remove
    #[inline]
    pub fn notify_one(&self) {
        self.notify.notify_one();
    }
}

/// Condvar guard that releases the mutex when dropped.
#[must_use]
pub(crate) struct CondvarGuard<'a, T> {
    guard: lock_api::MutexGuard<'a, parking_lot::RawMutex, T>,
    notify: &'a tokio::sync::Notify,
    notify_all: AtomicBool,
}

impl<T> CondvarGuard<'_, T> {
    /// Returns a future that resolves when notified. The lock is dropped when
    /// the waiter is first polled and is reacquired upon notification.
    #[inline]
    pub fn notified(&mut self) -> CondvarWaiter<parking_lot::RawMutex> {
        CondvarWaiter::new(&mut self.guard, self.notify.notified())
    }

    /// Sets a flag to notify all waiters when the guard is dropped.
    #[inline]
    pub fn notify_all(&self) {
        self.notify_all.store(true, Ordering::Relaxed);
    }
}

impl<T> Drop for CondvarGuard<'_, T> {
    #[inline]
    fn drop(&mut self) {
        if *self.notify_all.get_mut() {
            self.notify.notify_waiters();
        }
    }
}

impl<T> Deref for CondvarGuard<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<T> DerefMut for CondvarGuard<'_, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

/// Condvar waiter that unlocks the mutex while awaiting notification. This is
/// similar to `MutexGuard::unlocked()`, but for use in an async context.
pub(crate) struct CondvarWaiter<'a, R: lock_api::RawMutex> {
    raw: &'a R,
    notified: tokio::sync::futures::Notified<'a>,
    unlocked: bool,
}

impl<'a, R: lock_api::RawMutex> CondvarWaiter<'a, R> {
    #[inline]
    fn new<T>(
        guard: &'a mut lock_api::MutexGuard<R, T>,
        notified: tokio::sync::futures::Notified<'a>,
    ) -> Self {
        Self {
            // SAFETY: MutexGuard always holds the lock and &mut ensures that we
            // have exclusive access.
            raw: unsafe { lock_api::MutexGuard::mutex(guard).raw() },
            notified,
            unlocked: false,
        }
    }
}

impl<R: lock_api::RawMutex> Future for CondvarWaiter<'_, R> {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We never move out of this
        let this = unsafe { self.get_unchecked_mut() };
        // SAFETY: this.notified is pinned via self
        if unsafe { Pin::new_unchecked(&mut this.notified) }
            .poll(cx)
            .is_ready()
        {
            if this.unlocked {
                this.raw.lock();
                this.unlocked = false;
            }
            return Poll::Ready(());
        }
        // The permit stored by notify_one() may cause an immediate spurious
        // wakeup, so we avoid unlocking the mutex until we know for sure that
        // we need to await notification.
        if !this.unlocked {
            // SAFETY: We have exclusive access to a locked MutexGuard, and we
            // guarantee that the mutex will be re-locked when the waiter either
            // resolves or gets dropped.
            unsafe { this.raw.unlock() };
            this.unlocked = true;
        }
        Poll::Pending
    }
}

impl<R: lock_api::RawMutex> Drop for CondvarWaiter<'_, R> {
    #[inline]
    fn drop(&mut self) {
        if self.unlocked {
            self.raw.lock();
        }
    }
}
