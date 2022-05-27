pub mod instr;
pub mod interp;
pub mod loader;
pub mod mem;
pub mod obj;
pub mod worker;

pub mod basic {
    pub mod op;
    pub use op::Op;
}

use std::{
    any::Any,
    ops::{Deref, DerefMut},
};

use crate::{instr::Val, interp::OpContext, mem::Obj};

pub type OpCode = String;
pub type AssetId = u32;
pub type Name = String;
pub type TagId = u32;

pub trait Op {
    type Worker;
    /// # Safety
    /// As long as all `*mut Obj` that accessible from arguments are valid, the
    /// returned pointer must be valid as well.
    fn perform(id: &OpCode, val: &[Val], context: &mut OpContext<Self::Worker>) -> *mut Obj;
}

pub trait AsAny {
    fn any_ref(&self) -> &dyn Any;
    fn any_mut(&mut self) -> &mut dyn Any;
}
impl<T: Any> AsAny for T {
    fn any_ref(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
impl Deref for dyn ObjCore {
    type Target = dyn Any;
    fn deref(&self) -> &Self::Target {
        self.any_ref()
    }
}
impl DerefMut for dyn ObjCore {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.any_mut()
    }
}

pub trait ObjCore: AsAny + 'static {
    #[allow(unused_variables)]
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj));
    fn alloc_size(&self) -> usize;
}
