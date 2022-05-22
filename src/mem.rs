use std::{
    mem::size_of,
    ptr::{null_mut, NonNull},
    sync::atomic::{AtomicI32, AtomicU64, AtomicUsize, Ordering::SeqCst},
};

use crossbeam::utils::Backoff;

use crate::ObjCore;

pub struct Obj {
    core: Box<dyn ObjCore>,
    mark: ObjMark,
    prev: *mut Obj,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObjMark {
    Black,
    White,
    // Gray,
}

// want to use `null_mut`, but compiler don't like cast, while linter don't like
// transmute
static PREV_ALLOC: AtomicU64 = AtomicU64::new(0);
// const_assert!((0 as *mut Obj).is_null()); // and const `is_null` is not stable yet, shit

impl Obj {
    fn alloc(core: impl ObjCore) -> *mut Self {
        let mut obj = Box::new(Self {
            core: Box::new(core),
            mark: ObjMark::White,
            prev: NonNull::dangling().as_ptr(),
        });
        obj.prev = PREV_ALLOC.swap(&*obj as *const _ as _, SeqCst) as _;
        Box::leak(obj)
    }
}

pub struct Mem {
    alloc_size: AtomicUsize,
    cap: AtomicUsize,
    mutator_status: AtomicI32,
}

impl Mem {
    const INITIAL_CAP: usize = 4 << 10;
}

impl Default for Mem {
    fn default() -> Self {
        Self {
            alloc_size: AtomicUsize::new(0),
            cap: AtomicUsize::new(Self::INITIAL_CAP),
            mutator_status: AtomicI32::new(0),
        }
    }
}

impl Mem {
    fn mutator_enter(&self) {
        let backoff = Backoff::new();
        while self
            .mutator_status
            .fetch_update(SeqCst, SeqCst, |status| {
                if status != -1 {
                    Some(status + 1)
                } else {
                    None
                }
            })
            .is_err()
        {
            backoff.spin();
        }
    }

    fn mutator_exit(&self) {
        let old = self.mutator_status.fetch_sub(1, SeqCst);
        assert!(old > 0);
    }

    fn collector_enter(&self) {
        let backoff = Backoff::new();
        while self
            .mutator_status
            .fetch_update(
                SeqCst,
                SeqCst,
                |status| if status == 0 { Some(-1) } else { None },
            )
            .is_err()
        {
            backoff.spin();
        }
    }

    fn collector_exit(&self) {
        let old = self.mutator_status.swap(0, SeqCst);
        assert_eq!(old, -1);
    }

    unsafe fn mutator_read(&self, obj: *mut Obj) -> &dyn ObjCore {
        &*(*obj).core
    }

    unsafe fn mutator_write<'a>(&self, obj: *mut Obj) -> &'a mut dyn ObjCore {
        &mut *(*obj).core
    }

    // collector read/write if necessary

    fn alloc(&self, core: impl ObjCore) -> *mut Obj {
        self.alloc_size
            .fetch_add(core.alloc_size() + size_of::<Obj>(), SeqCst);
        Obj::alloc(core)
    }

    pub fn load_factor(&self) -> f32 {
        self.alloc_size.load(SeqCst) as f32 / self.cap.load(SeqCst) as f32
    }

    fn update_cap(&self) {
        while self.load_factor() > 0.75 {
            self.cap
                .fetch_update(SeqCst, SeqCst, |cap| Some(cap * 2))
                .unwrap();
        }
    }

    unsafe fn collect(&self, root_iter: impl Iterator<Item = *mut Obj>) {
        let mut mark_list = Vec::new();
        mark_list.extend(root_iter);
        while let Some(obj) = mark_list.pop() {
            let obj = &mut *obj;
            obj.mark = ObjMark::Black;
            obj.core.trace(&mut |traced| {
                if (*traced).mark == ObjMark::White {
                    mark_list.push(traced);
                }
            });
        }

        let mut scan_obj = PREV_ALLOC.load(SeqCst) as *mut Obj;
        let mut prev_obj = null_mut();
        let mut alloc_size = 0;
        while !scan_obj.is_null() {
            let obj = &mut *scan_obj;
            let next_obj = obj.prev;
            if obj.mark == ObjMark::Black {
                obj.mark = ObjMark::White;
                obj.prev = prev_obj;
                prev_obj = obj;
                alloc_size += obj.core.alloc_size() + size_of::<Obj>();
            } else {
                drop(Box::from_raw(obj));
            }
            scan_obj = next_obj;
        }
        PREV_ALLOC.store(prev_obj as _, SeqCst);

        self.alloc_size.store(alloc_size, SeqCst);
        self.update_cap();
    }
}

pub struct Mutator<'a>(&'a Mem);
pub struct Collector<'a>(&'a Mem);

impl Mem {
    pub fn mutator(&self) -> Mutator<'_> {
        self.mutator_enter();
        Mutator(self)
    }

    pub fn collector(&self) -> Collector<'_> {
        self.collector_enter();
        Collector(self)
    }
}

impl Mutator<'_> {
    pub fn alloc(&self, core: impl ObjCore) -> *mut Obj {
        self.0.alloc(core)
    }

    /// # Safety
    /// `obj` must be acquired by calling `alloc`. Thread safety is totally
    /// unchecked.
    pub unsafe fn read(&self, obj: *mut Obj) -> &dyn ObjCore {
        self.0.mutator_read(obj)
    }

    /// # Safety
    /// Same as `read`.
    pub unsafe fn write<'a>(&self, obj: *mut Obj) -> &'a mut dyn ObjCore {
        self.0.mutator_write(obj)
    }
}

impl Drop for Mutator<'_> {
    fn drop(&mut self) {
        self.0.mutator_exit();
    }
}

impl Collector<'_> {
    /// # Safety
    /// All allocated objects must only reference to other allocated objects.
    pub unsafe fn collect(&self, root_iter: impl Iterator<Item = *mut Obj>) {
        self.0.collect(root_iter);
    }
}

impl Drop for Collector<'_> {
    fn drop(&mut self) {
        self.0.collector_exit();
    }
}
