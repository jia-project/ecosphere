use std::{
    mem::size_of,
    ptr::{null_mut, NonNull},
    sync::{
        atomic::{AtomicU64, AtomicUsize, Ordering::SeqCst},
        RwLock, RwLockReadGuard, RwLockWriteGuard,
    },
};

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

pub struct Mem(RwLock<MemInner>);
struct MemInner {
    alloc_size: AtomicUsize,
    cap: usize,
}

impl Mem {
    const INITIAL_CAP: usize = 4 << 10;
}

impl Default for Mem {
    fn default() -> Self {
        Self(RwLock::new(MemInner {
            alloc_size: AtomicUsize::new(0),
            cap: Self::INITIAL_CAP,
        }))
    }
}

impl MemInner {
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
        self.alloc_size.load(SeqCst) as f32 / self.cap as f32
    }

    fn update_cap(&mut self) {
        while self.load_factor() > 0.75 {
            self.cap *= 2;
        }
    }

    unsafe fn collect(&mut self, root_iter: impl Iterator<Item = *mut Obj>) {
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

pub struct Mutator<'a>(RwLockReadGuard<'a, MemInner>);
pub struct Collector<'a>(RwLockWriteGuard<'a, MemInner>);

impl Mem {
    pub fn mutator(&self) -> Mutator<'_> {
        Mutator(self.0.read().unwrap())
    }

    pub fn collector(&self) -> Collector<'_> {
        Collector(self.0.write().unwrap())
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

impl Collector<'_> {
    /// # Safety
    /// All allocated objects must only reference to other allocated objects.
    pub unsafe fn collect(&mut self, root_iter: impl Iterator<Item = *mut Obj>) {
        self.0.collect(root_iter);
    }
}
