use std::{
    alloc::{alloc, Layout},
    mem::{replace, size_of},
    ptr::{copy_nonoverlapping, write, NonNull},
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
    space: Mutex<Space>,
    roots: Mutex<Vec<Box<dyn ObjectAny>>>,
    instant_zero: Instant,
}

struct Space {
    raw: Box<SpaceRaw>,
    status: Box<[SlabStatus]>,
}
type SpaceRaw = [Object; Space::SLAB_COUNT * Space::SLAB_SIZE];

impl Space {
    const SLAB_SIZE: usize = 32 << 10; // in terms of size_of<Object>
    const SLAB_COUNT: usize = 64;

    fn slab(&self, index: usize) -> Slab {
        let slab = &self.raw[index * Self::SLAB_SIZE..(index + 1) * Self::SLAB_SIZE];
        Slab {
            parts: (slab.as_ptr() as _, slab.len()),
            status: self.status[index],
        }
    }

    fn object_slab(&self, address: *const Object) -> Option<Slab> {
        let raw = self.raw.as_ptr();
        if (raw..unsafe { raw.add(Self::SLAB_COUNT * Self::SLAB_SIZE) }).contains(&address) {
            Some(self.slab(unsafe { address.offset_from(raw) } as usize / Self::SLAB_SIZE))
        } else {
            None
        }
    }
}

impl Default for Space {
    fn default() -> Self {
        let align = (size_of::<Object>() * Self::SLAB_SIZE).next_power_of_two();

        let raw = unsafe { alloc(Layout::from_size_align(size_of::<SpaceRaw>(), align).unwrap()) };
        assert_eq!(raw.align_offset(align), 0);
        Self {
            raw: unsafe { Box::from_raw(raw as _) },
            status: vec![SlabStatus::Available; Self::SLAB_COUNT].into(),
        }
    }
}

pub struct ArenaClient {
    shared: Arc<ArenaShared>,
    slab_index: usize,
    slab: Slab,
    allocate_len: usize,
}

#[derive(Clone, Copy)]
struct Slab {
    parts: (*mut Object, usize),
    status: SlabStatus,
}

#[derive(Clone, Copy)]
enum SlabStatus {
    Available,  // owned by `ArenaShared`
    Allocating, // owned by some `ArenaClient`
    Full,       // owned by `ArenaShared`
}

impl Default for Arena {
    fn default() -> Self {
        Self(Arc::new(ArenaShared {
            space: Mutex::new(Space::default()),
            mutate_status: AtomicI8::new(0),
            roots: Default::default(),
            instant_zero: Instant::now(),
        }))
    }
}

impl Arena {
    /// # Safety
    /// `root` must outlive every `allocate` call. This is trivial if all `allocate` calls are made by `root`.
    pub unsafe fn add_user(&self, root: *mut impl ObjectAny) -> ArenaClient {
        struct RootObject<T>(*mut T);
        unsafe impl<T: ObjectAny> ObjectAny for RootObject<T> {
            fn on_scan(&mut self, scanner: &mut crate::arena::ObjectScanner<'_>) {
                unsafe { &mut *self.0 }.on_scan(scanner)
            }
        }

        // assert_eq!(self.0.mutate_status.load(Ordering::SeqCst), 0);
        let mut roots = self.0.roots.lock().unwrap();
        let mut space = self.0.space.lock().unwrap();
        roots.push(Box::new(RootObject(root)));
        let slab_index = ArenaShared::select_slab(&mut space, 0).unwrap();
        ArenaClient {
            shared: self.0.clone(),
            slab: space.slab(slab_index),
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

    fn switch_slab(&self, index: &mut usize) -> Slab {
        let mut space = self.space.lock().unwrap();
        assert!(matches!(space.status[*index], SlabStatus::Allocating));
        space.status[*index] = SlabStatus::Full;
        if let Some(slab_index) = Self::select_slab(&mut space, *index + 1) {
            *index = slab_index;
            return space.slab(*index);
        }

        drop(space);
        self.mutate_exit();
        self.collect();
        self.mutate_enter();
        let mut space = self.space.lock().unwrap();
        *index = 0;
        *index = Self::select_slab(&mut space, *index).unwrap();
        space.slab(*index)
    }

    fn select_slab(space: &mut Space, index: usize) -> Option<usize> {
        space
            .status
            .iter_mut()
            .skip(index)
            .position(|status| {
                if matches!(status, SlabStatus::Available) {
                    *status = SlabStatus::Allocating;
                    true
                } else {
                    false
                }
            })
            .map(|i| i + index)
    }

    fn collect(&self) {
        let collect_zero = Instant::now();
        // collector enter
        while self
            .mutate_status
            .compare_exchange_weak(0, -1, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {}

        let mut space = self.space.try_lock().unwrap();
        let mut to_space = Space::default();
        let mut scanner = ObjectScanner {
            space: &mut space,
            to_space: &mut to_space,
            allocate_len: 0,
            process_len: 0,
        };
        for root in &mut *self.roots.try_lock().unwrap() {
            root.on_scan(&mut scanner);
        }
        scanner.collect();
        let alive_len = scanner.process_len;
        drop(scanner);
        let _space = replace(&mut *space, to_space);

        // collector exit
        let swapped = self.mutate_status.swap(0, Ordering::SeqCst);
        assert_eq!(swapped, -1);

        let now = Instant::now();
        println!(
            "[{:>9.3?}] collected Stop {:?} Copy {}",
            now - self.instant_zero,
            now - collect_zero,
            alive_len
        );
    }
}

pub struct ObjectScanner<'a> {
    space: &'a mut Space,
    to_space: &'a mut Space,
    allocate_len: usize,
    process_len: usize,
}

impl ObjectScanner<'_> {
    fn collect(&mut self) {
        while self.process_len < self.allocate_len {
            let object = &mut self.to_space.raw[self.process_len] as _;
            self.process(object);
            self.process_len += 1;
        }
    }

    fn copy(&mut self, object: &mut Object) -> NonNull<Object> {
        let Some(object_slab) = self.space.object_slab(object) else {
            assert!(self.to_space.object_slab(object).is_some());
            return object.into();
        };
        match object_slab.status {
            SlabStatus::Available => unreachable!(),
            SlabStatus::Full => {
                let to = &mut self.to_space.raw[self.allocate_len];
                self.allocate_len += 1;
                unsafe {
                    copy_nonoverlapping(object, to, 1);
                    write(&mut object.data, ObjectData::Forwarded(to.into()));
                }
                // will be processed later
                to.into()
            }
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
            ObjectData::Integer(_) | ObjectData::String(_) | ObjectData::Preallocate => {}
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

impl Drop for ObjectScanner<'_> {
    fn drop(&mut self) {
        assert_eq!(self.allocate_len, self.process_len);
        for status in &mut self.to_space.status[..=self.allocate_len / Space::SLAB_SIZE] {
            *status = SlabStatus::Full;
        }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        if !panicking() {
            assert_eq!(self.0.mutate_status.load(Ordering::SeqCst), 0);
        }
    }
}

impl ArenaClient {
    pub fn allocate(&mut self, data: ObjectData) -> NonNull<Object> {
        if self.allocate_len == self.slab.parts.1 {
            self.slab = self.shared.switch_slab(&mut self.slab_index);
            // dbg!(self.slab.parts);
            self.allocate_len = 0;
        }
        // safety
        // `slab` only get dangling by collecting, and (for now) collecting only happens when every client is in `switch_slab`
        // because if not, mutate status will block collecting
        // and finally slab will be correctly updated in `switch_slab`
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

#[cfg(test)]
mod tests {
    use super::Space;

    #[test]
    fn new_space() {
        let _ = Space::default();
    }
}
