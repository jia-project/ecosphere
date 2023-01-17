use std::{
    iter::repeat_with,
    mem::replace,
    ptr::{copy_nonoverlapping, null_mut},
    sync::{
        atomic::{AtomicI8, Ordering},
        Arc, Mutex,
    },
};

use crate::{Object, TypeIndex};

struct Arena {
    virtuals: [Virtual; TypeIndex::MAX as usize + 1],
    space: Mutex<Space>,
    roots: Mutex<Vec<Object>>,
    rendezvous: AtomicI8,
}

#[derive(Clone, Copy)]
pub struct Virtual {
    visit: fn(*const Object, &mut CollectContext<'_>),
    drop: fn(Object),
}

struct Space {
    objects: Box<[Object]>,
    slab_len: usize,
}

pub struct ArenaBuilder(Arena);
pub struct ArenaServer(Arc<Arena>);

impl ArenaBuilder {
    fn default_visit(_: *const Object, _: &mut CollectContext<'_>) {
        unreachable!()
    }

    fn default_drop(_: Object) {
        unreachable!()
    }

    fn drop_vacant(_: Object) {}

    pub fn new() -> Self {
        let objects = repeat_with(Default::default)
            .take(64 << 10)
            .collect::<Box<_>>();
        let mut virtuals = [Virtual {
            visit: Self::default_visit,
            drop: Self::default_drop,
        }; TypeIndex::MAX as usize + 1];
        virtuals[0].drop = Self::drop_vacant;
        Self(Arena {
            virtuals,
            space: Mutex::new(Space {
                objects,
                slab_len: 1 << 10,
            }),
            roots: Mutex::new(Default::default()),
            rendezvous: AtomicI8::new(0),
        })
    }

    pub fn register_visit(&mut self, index: TypeIndex, virt: Virtual) {
        self.0.virtuals[index as usize] = virt;
    }
}

impl From<ArenaBuilder> for ArenaServer {
    fn from(value: ArenaBuilder) -> Self {
        Self(Arc::new(value.0))
    }
}

pub struct ArenaClient {
    inner: Arc<Arena>,
    slab: *mut Object,
    slab_len: usize,
    len: usize,
}

impl Arena {
    fn add_client(self: Arc<Self>, root: Object) -> ArenaClient {
        self.client_enter();
        self.roots.lock().unwrap().push(root);
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
    pub fn add_client(&self, root: Object) -> ArenaClient {
        self.0.clone().add_client(root)
    }
}

impl ArenaClient {
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
}

impl Drop for ArenaClient {
    fn drop(&mut self) {
        self.inner.client_exit()
    }
}

impl Arena {
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

    fn drop_space(&self, space: Space) {
        for object in space.objects.into_vec() {
            (self.virtuals[object.1 as usize].drop)(object)
        }
    }

    fn switch_slab(&self, mut slab: *mut Object) -> Option<(*mut Object, usize)> {
        let mut space = self.space.lock().unwrap();
        if slab.is_null() {
            slab = (*space.objects).as_mut_ptr();
        }
        while slab < space.end() {
            if unsafe { (*slab).1 == Object::VACANT } {
                unsafe { (*slab).1 = Object::USED_SLAB };
                return Some((slab, space.slab_len));
            }
        }
        None
    }

    fn collect(&self) {
        while self
            .rendezvous
            .compare_exchange_weak(0, -1, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {}

        let mut space = self.space.lock().unwrap();
        let mut context = CollectContext {
            to_space: repeat_with(Default::default)
                .take(space.objects.len())
                .collect::<Box<_>>(),
            len: 0,
            process_len: 0,
            virtuals: &self.virtuals,
        };
        for root in &*self.roots.lock().unwrap() {
            (self.virtuals[root.1 as usize].visit)(root, &mut context);
        }
        while context.process_len < context.len {
            let object = &context.to_space[context.process_len];
            context.process_len += 1;
            let visit = self.virtuals[object.1 as usize].visit;
            visit(object, &mut context);
        }
        let slab_len = space.slab_len;
        let space = replace(
            &mut *space,
            Space {
                objects: context.to_space,
                slab_len,
            },
        );
        self.drop_space(space);

        let value = self.rendezvous.swap(0, Ordering::SeqCst);
        assert_eq!(value, -1);
    }
}

pub struct CollectContext<'a> {
    to_space: Box<[Object]>,
    len: usize,
    process_len: usize,
    virtuals: &'a [Virtual],
}

impl CollectContext<'_> {
    pub fn process(&mut self, address: &mut *mut Object) {
        let to_space = (*self.to_space).as_mut_ptr();
        if (to_space..unsafe { to_space.add(self.to_space.len()) }).contains(address) {
            return;
        }
        *address = self.copy(*address);
    }

    fn copy(&mut self, address: *mut Object) -> *mut Object {
        assert!(self.len < self.to_space.len());
        let cloned = &mut self.to_space[self.len];
        self.len += 1;
        unsafe {
            copy_nonoverlapping(address, cloned, 1);
            (*address).1 = Object::FORWARDED;
        }
        let visit = self.virtuals[cloned.1 as usize].visit;
        let cloned = cloned as *mut _;
        visit(cloned, self);
        cloned
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
        self.drop_space(space.into_inner().unwrap());
    }
}
