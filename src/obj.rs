use crate::{
    def::HeaderId,
    mem::{Obj, ObjCore},
};

pub struct Native<T>(pub T);
impl<T: 'static> ObjCore for Native<T> {}

pub struct Prod {
    pub header: HeaderId,
    pub data: Vec<*mut Obj>,
}

impl ObjCore for Prod {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.data.iter().copied().for_each(mark);
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
}
