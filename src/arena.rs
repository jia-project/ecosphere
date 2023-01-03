use std::{
    iter::repeat_with,
    ops::Deref,
    ptr::NonNull,
    slice,
    sync::{
        atomic::{AtomicI8, Ordering},
        Arc, Mutex,
    },
};

use crate::{Object, ObjectData};

pub struct Arena {
    shared: Arc<ArenaShared>,
    //
}

struct ArenaShared {
    // >= 0 means number of active mutator, -1 means no mutator and collecting
    mutate_status: AtomicI8,
    slabs: Mutex<Vec<Slab>>,
}

pub struct ArenaUser {
    shared: Arc<ArenaShared>,
    slab: Slab,
    slab_index: usize,
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
    Full,
}

impl Default for Arena {
    fn default() -> Self {
        let new_slab = || {
            let slab = repeat_with(Default::default)
                // initially allocate 32K * 32B = 1MB space for objects
                .take(32 << 10)
                .collect::<Box<[Object]>>();
            let slab = Box::leak(slab);
            Slab {
                parts: (slab.as_mut_ptr(), slab.len()),
                status: SlabStatus::Available,
            }
        };
        Self {
            shared: Arc::new(ArenaShared {
                slabs: Mutex::new(repeat_with(new_slab).take(64).collect()),
                mutate_status: AtomicI8::new(0),
            }),
        }
    }
}

impl Arena {
    pub fn add_user(&self) -> ArenaUser {
        assert_eq!(self.shared.mutate_status.load(Ordering::SeqCst), 0);
        let mut slabs = self.shared.slabs.try_lock().unwrap();
        for (i, slab) in slabs.iter_mut().enumerate() {
            if matches!(slab.status, SlabStatus::Available) {
                slab.status = SlabStatus::Allocating;
                return ArenaUser {
                    shared: self.shared.clone(),
                    slab: *slab,
                    slab_index: i,
                    allocate_len: 0,
                };
            }
        }
        unreachable!()
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
            slabs[user.slab_index].status,
            SlabStatus::Allocating
        ));
        slabs[user.slab_index].status = SlabStatus::Full;
        user.slab_index += 1;
        while user.slab_index < slabs.len() {
            let mut slab = slabs[user.slab_index];
            if matches!(slab.status, SlabStatus::Available) {
                slab.status = SlabStatus::Allocating;
                user.slab = slab;
                return;
            }
        }

        todo!() // GC
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        assert_eq!(self.shared.mutate_status.load(Ordering::SeqCst), 0);
        for slab in Arc::get_mut(&mut self.shared)
            .unwrap()
            .slabs
            .get_mut()
            .unwrap()
            .drain(..)
        {
            drop(unsafe { Box::from_raw(slice::from_raw_parts_mut(slab.parts.0, slab.parts.1)) })
        }
    }
}

impl ArenaUser {
    pub fn allocate(&mut self, data: ObjectData) -> NonNull<Object> {
        if self.allocate_len == self.slab.parts.1 {
            // very not cool to do this
            self.shared.clone().switch_slab(self);
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
}

pub struct ObjectScanner<'a>(&'a Arena);
impl ObjectScanner<'_> {
    pub fn process(&mut self, pointer: &mut NonNull<Object>) {
        //
    }

    fn process_object(&mut self, object: &mut Object) {
        match &mut object.data {
            ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),
            ObjectData::Integer(_) | ObjectData::String(_) => {}
            ObjectData::Product(_, data) => {
                for pointer in data.iter_mut() {
                    self.process(pointer)
                }
            }
            ObjectData::Sum(_, _, pointer) => self.process(pointer),
            ObjectData::Any(object) => object.on_scan(self),
        }
    }
}

pub struct ArenaObject<'a>(&'a Arena, *mut Object);

impl Deref for ArenaObject<'_> {
    type Target = *mut Object;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
