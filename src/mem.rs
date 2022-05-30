use std::{
    mem::size_of,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
    sync::{
        atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering::SeqCst},
        RwLock, RwLockReadGuard, RwLockWriteGuard,
    },
    thread::yield_now,
};

use crate::ObjCore;

pub struct Obj {
    core: Box<dyn ObjCore>,
    mark: ObjMark,
    rw: AtomicU32,
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
    fn make_boxed(core: Box<dyn ObjCore>) -> *mut Self {
        let mut obj = Box::new(Self {
            core,
            mark: ObjMark::White,
            rw: AtomicU32::new(0),
            prev: NonNull::dangling().as_ptr(),
        });
        obj.prev = PREV_ALLOC.swap(&*obj as *const _ as _, SeqCst) as _;
        Box::leak(obj)
    }

    const MUTATOR_WRITE: u32 = u32::MAX;
    const COLLECTOR_READ_MASK: u32 = !(u32::MAX >> 1);
}

struct ObjRead(*mut Obj);
impl ObjRead {
    unsafe fn new(obj: *mut Obj) -> Self {
        while {
            let rw = (*obj).rw.load(SeqCst);
            assert_ne!(rw, Obj::MUTATOR_WRITE);
            (rw & Obj::COLLECTOR_READ_MASK != 0)
                || (*obj)
                    .rw
                    .compare_exchange_weak(rw, rw + 1, SeqCst, SeqCst)
                    .is_err()
        } {
            yield_now();
        }
        Self(obj)
    }
}
impl Drop for ObjRead {
    fn drop(&mut self) {
        let rw = unsafe { &*self.0 }.rw.fetch_sub(1, SeqCst);
        assert_ne!(rw, 0);
        assert_eq!(rw & Obj::COLLECTOR_READ_MASK, 0);
    }
}
impl Deref for ObjRead {
    type Target = dyn ObjCore;
    fn deref(&self) -> &Self::Target {
        &*unsafe { &*self.0 }.core
    }
}

struct ObjWrite(*mut Obj);
impl ObjWrite {
    unsafe fn new(obj: *mut Obj) -> Self {
        while {
            let rw = (*obj).rw.load(SeqCst);
            assert_eq!(rw & !Obj::COLLECTOR_READ_MASK, 0);
            (rw & Obj::COLLECTOR_READ_MASK != 0)
                || (*obj)
                    .rw
                    .compare_exchange_weak(rw, Obj::MUTATOR_WRITE, SeqCst, SeqCst)
                    .is_err()
        } {
            yield_now();
        }
        Self(obj)
    }
}
impl Drop for ObjWrite {
    fn drop(&mut self) {
        let rw = unsafe { &*self.0 }.rw.swap(0, SeqCst);
        assert_eq!(rw, Obj::MUTATOR_WRITE);
    }
}
impl Deref for ObjWrite {
    type Target = dyn ObjCore;
    fn deref(&self) -> &Self::Target {
        &*unsafe { &*self.0 }.core
    }
}
impl DerefMut for ObjWrite {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *unsafe { &mut *self.0 }.core
    }
}

struct CollectorRead(*mut Obj);
impl CollectorRead {
    unsafe fn new(obj: *mut Obj) -> Self {
        while {
            let rw = (*obj).rw.load(SeqCst);
            assert_eq!(rw & Obj::COLLECTOR_READ_MASK, 0);
            rw == Obj::MUTATOR_WRITE
                || (*obj)
                    .rw
                    .compare_exchange_weak(rw, rw ^ Obj::COLLECTOR_READ_MASK, SeqCst, SeqCst)
                    .is_err()
        } {
            yield_now()
        }
        Self(obj)
    }
}
impl Drop for CollectorRead {
    fn drop(&mut self) {
        let rw = unsafe { &*self.0 }
            .rw
            .fetch_xor(Obj::COLLECTOR_READ_MASK, SeqCst);
        assert_ne!(rw, Obj::MUTATOR_WRITE);
        assert_ne!(rw & Obj::COLLECTOR_READ_MASK, 0);
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
    unsafe fn mutator_read(&self, obj: *mut Obj) -> impl Deref<Target = dyn ObjCore> {
        ObjRead::new(obj)
    }

    unsafe fn mutator_write(&self, obj: *mut Obj) -> impl DerefMut<Target = dyn ObjCore> {
        ObjWrite::new(obj)
    }

    // collector read/write if necessary

    fn make_boxed(&self, core: Box<dyn ObjCore>) -> *mut Obj {
        self.alloc_size
            .fetch_add(core.alloc_size() + size_of::<Obj>(), SeqCst);
        Obj::make_boxed(core)
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
            let obj = CollectorRead::new(obj);
            let obj = &mut *obj.0;
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
            let obj_guard = CollectorRead::new(scan_obj);
            let obj = &mut *obj_guard.0;
            let next_obj = obj.prev;
            if obj.mark == ObjMark::Black {
                obj.mark = ObjMark::White;
                obj.prev = prev_obj;
                prev_obj = obj;
                alloc_size += obj.core.alloc_size() + size_of::<Obj>();
            } else {
                drop(obj_guard);
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
    pub fn make(&self, core: impl ObjCore + 'static) -> *mut Obj {
        self.make_boxed(Box::new(core))
    }

    pub fn make_boxed(&self, core: Box<dyn ObjCore>) -> *mut Obj {
        self.0.make_boxed(core)
    }

    /// # Safety
    /// `obj` must be returned by `make`, and be traced, i.e. not get collected
    /// in all previous collection.
    pub unsafe fn read(&self, obj: *mut Obj) -> impl Deref<Target = dyn ObjCore> {
        self.0.mutator_read(obj)
    }

    /// # Safety
    /// Same as `read`.
    pub unsafe fn write(&self, obj: *mut Obj) -> impl DerefMut<Target = dyn ObjCore> {
        self.0.mutator_write(obj)
    }
}

impl Collector<'_> {
    /// # Safety
    /// All objects in `root_iter` must be valid and alive, i.e. the pointer
    /// must be returned by `make`, and pointed object must be either present
    /// in `root_iter`, or be traced, in all previous collection.
    ///
    /// All traced objects must implement `ObjCore::trace` correctly.
    pub unsafe fn collect(&mut self, root_iter: impl Iterator<Item = *mut Obj>) {
        self.0.collect(root_iter);
    }
}
