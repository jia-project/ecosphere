use std::mem::size_of;

use crate::{mem::Obj, HeaderId, ObjCore};

#[derive(Debug)]
pub struct Native<T>(pub T);
// do not provide blacket impl for all T because some T like String may own
// extra heap allocation
impl ObjCore for Native<i32> {
    fn trace(&self, _: &mut dyn FnMut(*mut Obj)) {}
    fn alloc_size(&self) -> usize {
        size_of::<Self>()
    }
}

pub struct Prod {
    pub header: HeaderId,
    pub data: Vec<*mut Obj>,
}

impl ObjCore for Prod {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.data.iter().copied().for_each(mark);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.data.capacity() * size_of::<*mut Obj>()
    }
}

pub struct Sum {
    pub header: HeaderId,
    pub variant: u32,
    pub inner: *mut Obj,
}

impl ObjCore for Sum {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        mark(self.inner);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>()
    }
}
