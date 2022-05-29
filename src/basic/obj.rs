use std::mem::size_of;

use crate::{mem::Obj, ObjCore};

pub struct List(Vec<*mut Obj>);
unsafe impl ObjCore for List {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.0.iter().copied().for_each(mark);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.0.capacity() * size_of::<*mut Obj>()
    }

    fn name(&self) -> &str {
        "basic.List"
    }
}
