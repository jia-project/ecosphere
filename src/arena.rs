use std::{
    alloc::{alloc_zeroed, Layout},
    collections::HashMap,
    mem::{replace, size_of},
    ptr::{copy_nonoverlapping, null_mut},
    slice,
    sync::{
        atomic::{AtomicI8, Ordering},
        Arc, Mutex,
    },
    time::Instant,
};

use crate::{shared::object::Object0, Object, TypeIndex};

struct Arena {
    virtuals: Mutex<Vec<ArenaVirtual>>,
    space: Mutex<Space>,
    roots: Mutex<HashMap<usize, RootVisit>>,
    rendezvous: AtomicI8,
    start: Instant,
}
type RootVisit = Box<dyn Fn(&mut CollectContext<'_>) + Send + Sync>;

#[derive(Clone, Copy)]
pub struct ArenaVirtual {
    pub visit: fn(&Object, &mut CollectContext<'_>),
    pub drop: fn(Object),
}

impl ArenaVirtual {
    fn default_visit(object: &Object, _: &mut CollectContext<'_>) {
        unreachable!("kind = {}", object.1)
    }

    fn default_drop(object: Object) {
        unreachable!("kind = {}", object.1)
    }
}

impl Default for ArenaVirtual {
    fn default() -> Self {
        Self {
            visit: Self::default_visit,
            drop: Self::default_drop,
        }
    }
}

struct Space {
    objects: Box<[Object]>,
    slab_len: usize,
}

impl Space {
    // slab length in terms of number of objects
    // 2^17 objects * 16 bytes/object = 2MB which matches some huge page spec
    const SLAB_LEN: usize = 1 << 17;
    fn new(slab_count: usize) -> Self {
        let slab_size = Self::SLAB_LEN * size_of::<Object>();
        let objects = unsafe {
            alloc_zeroed(Layout::from_size_align(slab_count * slab_size, slab_size).unwrap())
        }
        .cast();
        Self {
            objects: unsafe {
                Box::from_raw(slice::from_raw_parts_mut(
                    objects,
                    slab_count * Self::SLAB_LEN,
                ))
            },
            slab_len: Self::SLAB_LEN,
        }
    }

    // implement a `contains` if can find a way to do it efficiently
}

pub struct ArenaServer(Arc<Arena>);

impl Default for ArenaServer {
    fn default() -> Self {
        Self::new()
    }
}

impl ArenaServer {
    pub fn new() -> Self {
        let mut server = Self(Arc::new(Arena {
            virtuals: Default::default(),
            space: Mutex::new(Space::new(16)),
            roots: Mutex::new(Default::default()),
            rendezvous: AtomicI8::new(0),
            start: Instant::now(),
        }));
        for index in [Object::VACANT, Arena::FORWARDED, Arena::USED_SLAB] {
            server.register_virtual(
                index,
                ArenaVirtual {
                    visit: ArenaVirtual::default_visit,
                    drop: Object::drop_nop,
                },
            );
        }
        server
    }

    pub fn register_virtual(&mut self, index: TypeIndex, virt: ArenaVirtual) {
        let mut virtuals = self.0.virtuals.lock().unwrap();
        if virtuals.len() <= index as _ {
            virtuals.resize_with(index as usize + 1, Default::default);
        }
        virtuals[index as usize] = virt;
    }
}

pub struct ArenaClient {
    inner: Arc<Arena>,
    slab: *mut Object,
    slab_len: usize,
    len: usize,
}

impl Arena {
    const VIRTUALS_LEN: usize = 1 << 16;
    const TYPE_SECTION: TypeIndex = 0x1;
    #[allow(clippy::identity_op)]
    const USED_SLAB: TypeIndex = Self::TYPE_SECTION + 0x0;
    const FORWARDED: TypeIndex = Self::TYPE_SECTION + 0x1;

    fn add_client(self: Arc<Self>) -> ArenaClient {
        self.client_enter();
        let (slab, slab_len) = self.switch_slab(null_mut()).unwrap();
        ArenaClient {
            inner: self,
            slab,
            slab_len,
            len: 0,
        }
    }
}

impl Space {
    fn end(&mut self) -> *mut Object {
        unsafe { (*self.objects).as_mut_ptr().add(self.objects.len()) }
    }
}

impl ArenaServer {
    pub const TYPED_SECTION: TypeIndex = Arena::VIRTUALS_LEN as _;

    pub fn add_client(&self) -> ArenaClient {
        self.0.clone().add_client()
    }

    /// # Safety
    /// `root` must outlive between `add_root` and `remove_root`.
    /// No concurrent access during `collect` triggered by `preallocate` calling *by everyone*.
    pub unsafe fn add_root<T>(
        &self,
        root: *mut T,
        visit: impl Fn(&mut T, &mut CollectContext<'_>) + Send + Sync + 'static,
    ) {
        self.0.add_root(root, visit)
    }

    pub fn remove_root<T>(&self, root: *mut T) {
        self.0.remove_root(root);
    }
}

impl ArenaClient {
    /// # Safety
    /// Caller must make sure that by the time of calling this method, every `*mut Object`
    /// it may access later is reachable by visiting one of the roots
    pub unsafe fn preallocate(&mut self, len: usize) {
        if len + self.len <= self.slab_len {
            return;
        }

        // a little bit wasteful
        if let Some((slab, _)) = self.inner.switch_slab(self.slab) {
            self.slab = slab;
            assert!(len <= self.slab_len);
            self.len = 0;
            return;
        }

        self.inner.client_exit();
        self.inner.collect();
        self.inner.client_enter();
        (self.slab, self.slab_len) = self.inner.switch_slab(null_mut()).unwrap();
        assert!(len <= self.slab_len);
        self.len = 0;
    }

    pub fn allocate(&mut self) -> *mut Object {
        assert!(self.len < self.slab_len);
        let object = unsafe { self.slab.add(self.len) };
        self.len += 1;
        object
    }

    pub fn start_instant(&self) -> Instant {
        self.inner.start
    }
}

impl Drop for ArenaClient {
    fn drop(&mut self) {
        self.inner.client_exit()
    }
}

impl Arena {
    unsafe fn add_root<T>(
        &self,
        root: *mut T,
        visit: impl Fn(&mut T, &mut CollectContext<'_>) + Send + Sync + 'static,
    ) {
        let root = root as usize;
        self.roots.lock().unwrap().insert(
            root,
            Box::new(move |context| visit(unsafe { &mut *(root as *mut T) }, context)),
        );
    }

    fn remove_root<T>(&self, root: *mut T) {
        self.roots.lock().unwrap().remove(&(root as _));
    }

    fn client_enter(&self) {
        let mut value;
        while {
            value = self.rendezvous.load(Ordering::SeqCst);
            value < 0
                || self
                    .rendezvous
                    .compare_exchange_weak(value, value + 1, Ordering::SeqCst, Ordering::SeqCst)
                    .is_err()
        } {}
    }

    fn client_exit(&self) {
        let value = self.rendezvous.fetch_sub(1, Ordering::SeqCst);
        assert!(value > 0);
    }

    fn drop_space(&self, space: Space, virtuals: &[ArenaVirtual]) {
        for object in space.objects.into_vec() {
            if object.1 < Self::VIRTUALS_LEN as _ {
                (virtuals[object.1 as usize].drop)(object)
            } else {
                object.drop_typed()
            }
        }
    }

    fn switch_slab(&self, mut slab: *mut Object) -> Option<(*mut Object, usize)> {
        let mut space = self.space.lock().unwrap();
        if slab.is_null() {
            slab = (*space.objects).as_mut_ptr();
        }
        while slab < space.end() {
            if unsafe { (*slab).1 == Object::VACANT } {
                unsafe { (*slab).1 = Arena::USED_SLAB };
                return Some((slab, space.slab_len));
            }
            slab = unsafe { slab.add(space.slab_len) }
        }
        None
    }

    fn collect(&self) {
        while self
            .rendezvous
            .compare_exchange_weak(0, -1, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {}
        let start = Instant::now();

        let mut space = self.space.lock().unwrap();
        let virtuals = self.virtuals.lock().unwrap();
        let mut context = CollectContext {
            to_space: Space::new(space.objects.len() / Space::SLAB_LEN),
            len: 0,
            process_len: 0,
            virtuals: &virtuals,
        };
        for visit in self.roots.lock().unwrap().values() {
            visit(&mut context)
        }
        while context.process_len < context.len {
            let object = &mut context.to_space.objects[context.process_len] as *mut Object;
            context.process_len += 1;
            context.visit(unsafe { &mut *object })
        }
        let space = replace(&mut *space, context.to_space);
        self.drop_space(space, &virtuals);

        let now = Instant::now();
        println!(
            "[{:>9.3?}] collected Stop {:?} Copy {}",
            now - self.start,
            now - start,
            context.process_len
        );
        let value = self.rendezvous.swap(0, Ordering::SeqCst);
        assert_eq!(value, -1);
    }
}

pub struct CollectContext<'a> {
    to_space: Space,
    len: usize,
    process_len: usize,
    virtuals: &'a [ArenaVirtual],
}

impl CollectContext<'_> {
    pub fn process(&mut self, address: &mut *mut Object) {
        let to_space = (*self.to_space.objects).as_mut_ptr();
        if (to_space..unsafe { to_space.add(self.to_space.objects.len()) }).contains(address) {
            return;
        }
        *address = self.copy(*address);
    }

    fn copy(&mut self, address: *mut Object) -> *mut Object {
        if unsafe { (*address).1 } == Arena::FORWARDED {
            return unsafe { (*address).0.p1 };
        }
        assert!(self.len < self.to_space.objects.len());
        let cloned = &mut self.to_space.objects[self.len] as *mut _;
        self.len += 1;
        unsafe {
            copy_nonoverlapping(address, cloned, 1);
            address.write(Object(Object0 { p1: cloned }, Arena::FORWARDED, 0));
        }
        self.visit(unsafe { &mut *cloned });
        cloned
    }

    // visit an alive object who is already on its to-address
    // either get copied before, or directly referenced by root
    // not moving itself, but copy visited address reported by it
    pub fn visit(&mut self, object: &mut Object) {
        if (object.1 as usize) < Arena::VIRTUALS_LEN {
            let visit = self.virtuals[object.1 as usize].visit;
            visit(object, self);
        } else {
            object.visit_typed(self);
        }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        let space = replace(
            &mut self.space,
            Mutex::new(Space {
                objects: Default::default(),
                slab_len: 0,
            }),
        );
        let virtuals = self.virtuals.try_lock().unwrap();
        self.drop_space(space.into_inner().unwrap(), &virtuals);
    }
}
