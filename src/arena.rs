use std::{
    iter::repeat_with,
    slice,
    sync::atomic::{AtomicI8, AtomicUsize, Ordering},
};

use crate::{Object, ObjectData};

pub struct Arena {
    objects_data: AtomicUsize, // Box<[Object]> as *mut _ as usize
    objects_len: AtomicUsize,
    allocate_index: AtomicUsize,
    // >= 0 means number of active mutator, -1 means no mutator and collecting
    mutate_status: AtomicI8,
}

impl Default for Arena {
    fn default() -> Self {
        let objects = repeat_with(|| Default::default())
            // initially allocate 32K * 32B = 1MB space for objects
            .take(32 << 10)
            .collect::<Box<[Object]>>();
        let objects = Box::leak(objects);
        Self {
            objects_data: AtomicUsize::new(objects.as_mut_ptr() as _),
            objects_len: AtomicUsize::new(objects.len()),
            allocate_index: AtomicUsize::new(0),
            mutate_status: AtomicI8::new(0),
        }
    }
}

impl Arena {
    pub fn allocate(&self, data: ObjectData) -> *mut Object {
        assert!(!matches!(
            data,
            ObjectData::Vacant | ObjectData::Forwarded(_)
        ));

        self.mutate_enter();
        let mut objects_data = self.objects_data.load(Ordering::SeqCst);
        let mut objects_len = self.objects_len.load(Ordering::SeqCst);
        let mut index = self.allocate_index.fetch_add(1, Ordering::SeqCst);
        match index.cmp(&objects_len) {
            std::cmp::Ordering::Equal => todo!(),
            std::cmp::Ordering::Greater => {
                let previous_data = objects_data;
                while {
                    objects_data = self.objects_data.load(Ordering::SeqCst);
                    objects_data == previous_data
                } {}
                objects_len = self.objects_len.load(Ordering::SeqCst);
                index = self.allocate_index.fetch_add(1, Ordering::SeqCst);
            }
            std::cmp::Ordering::Less => {}
        }
        assert!(index < objects_len);

        let object = unsafe { (objects_data as *mut Object).add(index) };
        assert!(matches!(unsafe { &(*object).data }, ObjectData::Vacant));
        unsafe { (*object).data = data }

        self.mutate_exit();
        object
    }

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

    pub fn read(&self, _: *mut Object) -> ArenaRead<'_> {
        self.mutate_enter();
        ArenaRead(self)
    }

    pub fn write(&self, _: *mut Object) -> ArenaWrite<'_> {
        self.mutate_enter();
        ArenaWrite(self)
    }

    fn drop_read(&self) {
        self.mutate_exit()
    }

    fn drop_write(&self) {
        self.mutate_exit()
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        assert_eq!(self.mutate_status.load(Ordering::SeqCst), 0);
        unsafe {
            drop(Box::from_raw(slice::from_raw_parts_mut(
                self.objects_data.load(Ordering::SeqCst) as *mut Object,
                self.objects_len.load(Ordering::SeqCst),
            )))
        }
    }
}

pub struct ObjectScanner<'a>(&'a Arena);
impl ObjectScanner<'_> {
    pub fn process(&mut self, pointer: &mut *mut Object) {
        //
    }
}

pub struct ArenaRead<'a>(&'a Arena);
impl Drop for ArenaRead<'_> {
    fn drop(&mut self) {
        self.0.drop_read()
    }
}

pub struct ArenaWrite<'a>(&'a Arena);
impl Drop for ArenaWrite<'_> {
    fn drop(&mut self) {
        self.0.drop_write()
    }
}
