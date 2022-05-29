pub mod instr;
pub mod interp;
pub mod loader;
pub mod mem;
pub mod worker;

pub mod basic {
    pub mod op;
    pub use op::Op;
    pub mod obj;
}

use std::{
    any::Any,
    mem::size_of_val,
    ops::{Deref, DerefMut},
};

use crate::{instr::Val, interp::OpContext, mem::Obj};

pub type AssetId = u32;
pub type Name = str;
pub type OwnedName = <Name as ToOwned>::Owned;
pub type TagId = u32;

/// # Safety
/// As long as all `*mut Obj` that accessible from arguments are valid, the
/// returned pointer must be valid as well.
pub unsafe trait Operator {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpContext) -> *mut Obj;
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
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {}
    fn alloc_size(&self) -> usize {
        size_of_val(self)
    }
    fn name(&self) -> &str; // well, &Name will not work
}
