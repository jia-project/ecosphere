use std::{
    collections::BTreeMap,
    iter::repeat_with,
    ptr::{copy_nonoverlapping, null_mut, write, NonNull},
    slice,
    sync::{
        atomic::{AtomicI8, Ordering},
        Arc, Mutex,
    },
    thread::panicking,
    time::Instant,
};

use crate::{Object, ObjectAny, ObjectData};

pub struct Arena(Arc<ArenaShared>);

struct ArenaShared {
    // >= 0 means number of active mutator, -1 means no mutator and collecting
    mutate_status: AtomicI8,
    slabs: Mutex<BTreeMap<*mut Object, Slab>>,
    roots: Mutex<Vec<Box<dyn ObjectAny>>>,
    instant_zero: Instant,
}

pub struct ArenaUser {
    shared: Arc<ArenaShared>,
    slab: Slab,
    slab_index: *mut Object,
    allocate_len: usize,
}

#[derive(Clone, Copy)]
struct Slab {
    parts: (*mut Object, usize),
    status: SlabStatus,
}

#[derive(Clone, Copy)]
enum SlabStatus {
    Available,  // no object allocated at all
    Allocating, // no full yet, no need to copy out during collecting
    Full,       // wait for collecting
    To,         // to space during collecting
    Released,   // inner allocation dropped
}

impl Slab {
    fn new(object_count: usize, status: SlabStatus) -> Self {
        let slab = repeat_with(Default::default)
            .take(object_count)
            .collect::<Box<[Object]>>();
        let slab = Box::leak(slab);
        Slab {
            parts: (slab.as_mut_ptr(), slab.len()),
            status,
        }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self(Arc::new(ArenaShared {
            slabs: Mutex::new(
                repeat_with(|| {
                    let slab = Slab::new(Arena::SLAB_SIZE, SlabStatus::Available);
                    (slab.parts.0, slab)
                })
                .take(Arena::SLAB_COUNT)
                .collect(),
            ),
            mutate_status: AtomicI8::new(0),
            roots: Default::default(),
            instant_zero: Instant::now(),
        }))
    }
}

impl Arena {
    const SLAB_SIZE: usize = 32 << 10; // in terms of object count
    const SLAB_COUNT: usize = 64;

    /// # Safety
    /// `root` must outlive every `allocate` call. This is trivial if all `allocate` calls are made by `root`.
    pub unsafe fn add_user(&self, root: *mut impl ObjectAny) -> ArenaUser {
        struct RootObject<T>(*mut T);
        unsafe impl<T: ObjectAny> ObjectAny for RootObject<T> {
            fn on_scan(&mut self, scanner: &mut crate::arena::ObjectScanner<'_>) {
                unsafe { &mut *self.0 }.on_scan(scanner)
            }
        }

        // assert_eq!(self.0.mutate_status.load(Ordering::SeqCst), 0);
        let mut roots = self.0.roots.lock().unwrap();
        let mut slabs = self.0.slabs.lock().unwrap();
        roots.push(Box::new(RootObject(root)));
        let (slab_index, slab) = ArenaShared::select_slab(&mut slabs, null_mut()).unwrap();
        ArenaUser {
            shared: self.0.clone(),
            slab,
            slab_index,
            allocate_len: 0,
        }
    }
}

impl ArenaShared {
    fn mutate_enter(&self) {
        let mut status;
        while {
            status = self.mutate_status.load(Ordering::SeqCst);
            if status < 0 {
                true
            } else {
                self.mutate_status
                    .compare_exchange_weak(status, status + 1, Ordering::SeqCst, Ordering::SeqCst)
                    .is_err()
            }
        } {}
    }

    fn mutate_exit(&self) {
        let status = self.mutate_status.fetch_sub(1, Ordering::SeqCst);
        assert!(status > 0);
    }

    fn switch_slab(&self, user: &mut ArenaUser) {
        let mut slabs = self.slabs.lock().unwrap();
        assert!(matches!(
            slabs[&user.slab_index].status,
            SlabStatus::Allocating
        ));
        slabs.get_mut(&user.slab_index).unwrap().status = SlabStatus::Full;
        if let Some((slab_index, slab)) = Self::select_slab(&mut slabs, user.slab_index) {
            user.slab_index = slab_index;
            user.slab = slab;
            return;
        }

        drop(slabs);
        self.mutate_exit();
        self.collect();
        self.mutate_enter();
        (user.slab_index, user.slab) =
            Self::select_slab(&mut self.slabs.lock().unwrap(), null_mut()).unwrap();
    }

    fn select_slab(
        slabs: &mut BTreeMap<*mut Object, Slab>,
        index: *mut Object,
    ) -> Option<(*mut Object, Slab)> {
        for (slab_index, slab) in slabs.range_mut(index..) {
            if matches!(slab.status, SlabStatus::Available) {
                slab.status = SlabStatus::Allocating;
                return Some((*slab_index, *slab));
            }
        }
        None
    }

    fn collect(&self) {
        let collect_zero = Instant::now();
        // collector enter
        while self
            .mutate_status
            .compare_exchange_weak(0, -1, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {}

        let mut slabs = self.slabs.try_lock().unwrap();
        // to space is large enough to hold objects from all other slabs
        let mut to_slab = Slab::new(Arena::SLAB_COUNT * Arena::SLAB_SIZE, SlabStatus::To);
        let mut scanner = ObjectScanner {
            slabs: &slabs,
            to_slab: unsafe { slice::from_raw_parts_mut(to_slab.parts.0, to_slab.parts.1) },
            allocate_len: 0,
            process_len: 0,
        };
        for root in &mut *self.roots.try_lock().unwrap() {
            root.on_scan(&mut scanner);
        }
        scanner.collect();
        let alive_len = scanner.process_len;
        // marked as `Full` to
        // * avoid mutators allocating objects into it, which may exceed `to_slab` capacity in next collection
        // * make sure we discard this slot in next collecting, so we keep only one huge to space slab at a time
        to_slab.status = SlabStatus::Full;

        slabs.retain(|_, slab| {
            if matches!(slab.status, SlabStatus::Full) {
                slab.release();
                false
            } else {
                true
            }
        });
        // size of next to space (A): slab count * slab size
        // size of next to-be-collected (B): <= alive length + (extend length + slabs length) * slab size
        // (only equal when all not-collecting slabs are correctly full)
        // ensure A >= B
        let extend_len =
            (Arena::SLAB_COUNT * Arena::SLAB_SIZE - alive_len) / Arena::SLAB_SIZE - slabs.len();
        slabs.extend(
            repeat_with(|| {
                let slab = Slab::new(Arena::SLAB_SIZE, SlabStatus::Available);
                (slab.parts.0, slab)
            })
            .take(extend_len),
        );
        slabs.insert(to_slab.parts.0, to_slab);

        // collector exit
        let swapped = self.mutate_status.swap(0, Ordering::SeqCst);
        assert_eq!(swapped, -1);

        let now = Instant::now();
        println!(
            "[{:>9.3?}] Collected Stop {:?} Copy {}",
            now - self.instant_zero,
            now - collect_zero,
            alive_len
        );
    }
}

pub struct ObjectScanner<'a> {
    slabs: &'a BTreeMap<*mut Object, Slab>,
    to_slab: &'a mut [Object],
    allocate_len: usize,
    process_len: usize,
}

impl ObjectScanner<'_> {
    fn collect(&mut self) {
        while self.process_len < self.allocate_len {
            let object = &mut self.to_slab[self.process_len] as _;
            self.process(object);
            self.process_len += 1;
        }
    }

    fn copy(&mut self, object: &mut Object) -> NonNull<Object> {
        // the slab with maximum index that is not over `object` is the slab `object` belongs to
        let object_slab = self.slabs.range(..=object as *mut _).last().unwrap().1;
        assert!(object as *mut _ < unsafe { object_slab.parts.0.add(object_slab.parts.1) });
        assert!(!matches!(
            object.data,
            ObjectData::Vacant | ObjectData::Forwarded(_)
        ));
        match object_slab.status {
            SlabStatus::Available | SlabStatus::Released => unreachable!(),
            SlabStatus::Full => {
                let to = &mut self.to_slab[self.allocate_len];
                self.allocate_len += 1;
                unsafe {
                    copy_nonoverlapping(object, to, 1);
                    write(&mut object.data, ObjectData::Forwarded(to.into()));
                }
                // will be processed later
                to.into()
            }
            SlabStatus::To => NonNull::new(object).unwrap(),
            SlabStatus::Allocating => {
                // copy is skipped so process immediately
                self.process(object);
                NonNull::new(object).unwrap()
            }
        }
    }

    pub fn process(&mut self, object: *mut Object) {
        self.process2(object) // suppress public safe function with unsafe deref lint
    }

    fn process2(&mut self, object: *mut Object) {
        match unsafe { &mut (*object).data } {
            ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),
            ObjectData::Integer(_) | ObjectData::String(_) => {}
            ObjectData::Array(data) | ObjectData::Typed(_, data) => {
                for pointer in data.iter_mut() {
                    self.process_pointer(pointer)
                }
            }
            ObjectData::Any(object) => object.on_scan(self),
        }
    }

    pub fn process_pointer(&mut self, pointer: &mut NonNull<Object>) {
        let pointed = unsafe { pointer.as_mut() };
        if let ObjectData::Forwarded(to) = &pointed.data {
            *pointer = *to;
        } else {
            *pointer = self.copy(pointed);
        }
    }
}

impl Slab {
    fn release(&mut self) {
        assert!(!matches!(self.status, SlabStatus::Released));
        self.status = SlabStatus::Released;
        drop(unsafe { Box::from_raw(slice::from_raw_parts_mut(self.parts.0, self.parts.1)) })
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        if panicking() {
            return; // allow leaking in panic
        }
        assert_eq!(self.0.mutate_status.load(Ordering::SeqCst), 0);
        let slabs = Arc::get_mut(&mut self.0).unwrap().slabs.get_mut().unwrap();
        while let Some((_, mut slab)) = slabs.pop_first() {
            slab.release()
        }
    }
}

impl ArenaUser {
    pub fn allocate(&mut self, data: ObjectData) -> NonNull<Object> {
        if self.allocate_len == self.slab.parts.1 {
            // very not cool to clone and pass `self` but fine
            self.shared.clone().switch_slab(self);
            // dbg!(self.slab.parts);
            self.allocate_len = 0;
        }
        let slab = unsafe { slice::from_raw_parts_mut(self.slab.parts.0, self.slab.parts.1) };
        let object = &mut slab[self.allocate_len];
        object.data = data;
        self.allocate_len += 1;
        object.into()
    }

    pub fn mutate_enter(&self) {
        self.shared.mutate_enter()
    }

    pub fn mutate_exit(&self) {
        self.shared.mutate_exit()
    }

    pub fn instant_zero(&self) -> Instant {
        self.shared.instant_zero
    }
}
