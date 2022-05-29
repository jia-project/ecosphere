use std::mem::size_of;

use crate::{mem::Obj, Name, ObjCore};

pub struct List(pub Vec<*mut Obj>);
impl List {
    pub const NAME: &'static Name = "basic.List";
}
unsafe impl ObjCore for List {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.0.iter().copied().for_each(mark);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.0.capacity() * size_of::<*mut Obj>()
    }

    fn name(&self) -> &Name {
        Self::NAME
    }
}

pub struct Str(pub String);
impl Str {
    pub const NAME: &'static Name = "basic.Str";
}
unsafe impl ObjCore for Str {
    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.0.capacity()
    }

    fn name(&self) -> &Name {
        Self::NAME
    }
}
