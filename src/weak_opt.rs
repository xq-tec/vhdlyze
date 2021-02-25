// Copyright (C) 2021 xq-Tec GmbH

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Arc, Weak};

#[repr(transparent)]
pub struct WeakOpt<T> {
    ptr: AtomicPtr<T>,
    phantom: PhantomData<Option<Weak<T>>>,
}

impl<T> WeakOpt<T> {
    pub fn new() -> WeakOpt<T> {
        WeakOpt {
            ptr: AtomicPtr::new(std::ptr::null_mut()),
            phantom: PhantomData,
        }
    }

    pub fn into_inner(v: WeakOpt<T>) -> Option<Weak<T>> {
        let mut v = ManuallyDrop::new(v);
        let raw: *mut T = *v.ptr.get_mut();
        if raw.is_null() {
            None
        } else {
            Some(unsafe { Weak::from_raw(raw) })
        }
    }

    pub fn set(&self, v: Weak<T>) {
        let raw = Weak::into_raw(v);
        // Success ordering is Release so that a subsequent deref/drop creates a
        // Release-Acquire pair.
        // Failure ordering is Relaxed, because in that case we don't do anything
        // with the current value of self.ptr.
        if self
            .ptr
            .compare_exchange(
                std::ptr::null_mut(),
                raw as *mut _,
                Ordering::Release,
                Ordering::Relaxed,
            )
            .is_err()
        {
            drop(unsafe { Weak::from_raw(raw) });
            panic!("WeakOpt has already been set");
        }
    }

    pub fn set_arc(&self, v: &Arc<T>) {
        self.set(Arc::downgrade(v))
    }

    pub fn upgrade(&self) -> Option<Arc<T>> {
        let raw = self.ptr.load(Ordering::Acquire);
        if raw.is_null() {
            None
        } else {
            let weak = ManuallyDrop::new(unsafe { Weak::from_raw(raw) });
            weak.upgrade()
        }
    }
}

impl<T> Clone for WeakOpt<T> {
    fn clone(&self) -> WeakOpt<T> {
        let raw = self.ptr.load(Ordering::Acquire);
        let new_raw = if raw.is_null() {
            std::ptr::null()
        } else {
            let weak = ManuallyDrop::new(unsafe { Weak::from_raw(raw) });
            Weak::into_raw((*weak).clone())
        };
        WeakOpt {
            ptr: AtomicPtr::new(new_raw as *mut _),
            phantom: PhantomData,
        }
    }
}

impl<T> Drop for WeakOpt<T> {
    fn drop(&mut self) {
        // No need for atomics because we have a &mut reference.
        let raw: *mut T = *self.ptr.get_mut();
        if !raw.is_null() {
            drop(unsafe { Weak::from_raw(raw) });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::sync::atomic::AtomicUsize;

    struct Indicator {
        value: Cell<u32>,
        drop_ctr: *const AtomicUsize,
    }

    impl Drop for Indicator {
        fn drop(&mut self) {
            unsafe {
                (*self.drop_ctr).fetch_add(1, Ordering::SeqCst);
            }
        }
    }

    #[test]
    fn set_1() {
        let drop_ctr = AtomicUsize::new(0);
        let arc = Arc::new(Indicator {
            value: Cell::new(12345),
            drop_ctr: &drop_ctr as *const _,
        });
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);

        let b1: WeakOpt<Indicator> = WeakOpt::new();
        b1.set(Arc::downgrade(&arc));
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);

        assert_eq!(b1.upgrade().unwrap().value.get(), 12345);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);

        drop(b1);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);

        drop(arc);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 1);
    }

    #[test]
    fn set_2() {
        let drop_ctr = AtomicUsize::new(0);
        let arc = Arc::new(Indicator {
            value: Cell::new(123456),
            drop_ctr: &drop_ctr as *const _,
        });
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);

        let b1: WeakOpt<Indicator> = WeakOpt::new();
        b1.set_arc(&arc);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);

        assert_eq!(b1.upgrade().unwrap().value.get(), 123456);
        drop(arc);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 1);
        assert!(b1.upgrade().is_none());

        drop(b1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 1);
    }

    #[test]
    #[should_panic]
    fn set_twice() {
        let drop_ctr = AtomicUsize::new(0);
        let b1: WeakOpt<Indicator> = WeakOpt::new();
        b1.set_arc(&Arc::new(Indicator {
            value: Cell::new(5),
            drop_ctr: &drop_ctr as *const _,
        }));
        b1.set_arc(&Arc::new(Indicator {
            value: Cell::new(6),
            drop_ctr: &drop_ctr as *const _,
        }));
    }

    #[test]
    fn upgrade_unset() {
        let b1: WeakOpt<Indicator> = WeakOpt::new();
        assert!(b1.upgrade().is_none());
    }

    #[test]
    fn into_inner() {
        let drop_ctr = AtomicUsize::new(0);
        let arc = Arc::new(Indicator {
            value: Cell::new(23456),
            drop_ctr: &drop_ctr as *const _,
        });
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);

        let b1: WeakOpt<Indicator> = WeakOpt::new();
        b1.set_arc(&arc);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);

        let opt_inner: Option<Weak<Indicator>> = WeakOpt::into_inner(b1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);

        let inner = opt_inner.unwrap();
        assert_eq!(inner.upgrade().unwrap().value.get(), 23456);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);

        drop(inner);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);
    }

    #[test]
    fn into_inner_unset() {
        let b1: WeakOpt<Indicator> = WeakOpt::new();
        let opt_inner: Option<Weak<Indicator>> = WeakOpt::into_inner(b1);
        assert!(opt_inner.is_none());
    }

    #[test]
    fn clone() {
        let drop_ctr = AtomicUsize::new(0);
        let arc = Arc::new(Indicator {
            value: Cell::new(15),
            drop_ctr: &drop_ctr as *const _,
        });

        let b1: WeakOpt<Indicator> = WeakOpt::new();
        b1.set_arc(&arc);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);

        let b2 = b1.clone();
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 2);

        b2.upgrade().unwrap().value.set(16);
        assert_eq!(b1.upgrade().unwrap().value.get(), 16);
        assert_eq!(b2.upgrade().unwrap().value.get(), 16);

        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 2);

        drop(b1);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 1);

        drop(b2);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 0);
        assert_eq!(Arc::strong_count(&arc), 1);
        assert_eq!(Arc::weak_count(&arc), 0);

        drop(arc);
        assert_eq!(drop_ctr.load(Ordering::Acquire), 1);
    }
}
