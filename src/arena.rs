use std::{
    iter::repeat_with,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{Object, ObjectData};

pub struct Arena {
    objects: Box<[Object]>,
    allocate_index: AtomicUsize,
}

impl Default for Arena {
    fn default() -> Self {
        Self {
            objects: repeat_with(|| Default::default())
                .take(4 << 20)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            allocate_index: AtomicUsize::new(0),
        }
    }
}

impl Object {
    fn is_occupied(&self) -> bool {
        self.bits.load(Ordering::SeqCst) & 0x1 == 1
    }

    fn set_occupied(&self, value: bool) {
        let bits = self.bits.load(Ordering::SeqCst);
        if (bits & 0x1 == 1) == value {
            return;
        }
        self.bits
            .compare_exchange(bits, bits ^ 0x1, Ordering::SeqCst, Ordering::SeqCst)
            .unwrap();
    }
}

impl Arena {
    pub fn allocate(&self, data: ObjectData) -> *mut Object {
        let mut index;
        while {
            index = self.allocate_index.load(Ordering::SeqCst);
            let previous_index = index;
            while index < self.objects.len() && self.objects[index].is_occupied() {
                index += 1;
            }
            if index == self.objects.len() {
                false
            } else {
                self.allocate_index
                    .compare_exchange_weak(
                        previous_index,
                        index + 1,
                        Ordering::SeqCst,
                        Ordering::SeqCst,
                    )
                    .is_err()
            }
        } {}
        if index == self.objects.len() {
            todo!()
        }
        self.objects[index].set_occupied(true);
        let object = &self.objects[index] as *const _ as *mut Object;
        unsafe { (*object).data = data }
        object
    }

    pub fn read(&self, object: *mut Object) -> ArenaRead<'_> {
        ArenaRead(self)
    }

    pub fn write(&self, object: *mut Object) -> ArenaWrite<'_> {
        ArenaWrite(self)
    }
}

pub struct ArenaRead<'a>(&'a Arena);
pub struct ArenaWrite<'a>(&'a Arena);
