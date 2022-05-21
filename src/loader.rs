use std::sync::Arc;

use crate::{
    def::{FuncId, OpId, ResourceId},
    instr::{Func, Val, ValConst},
    interp::OpContext,
    mem::{Obj, ObjCore},
    obj::{Native, Prod},
};

pub trait Perform: Fn(&[&Val], &OpContext) {}
impl<T: Fn(&[&Val], &OpContext)> Perform for T {}

pub struct Loader {
    //
}

impl Default for Loader {
    fn default() -> Self {
        Self {}
    }
}

impl Loader {
    pub fn register_func(&mut self, id: FuncId, func: Func) {
        //
    }

    pub fn dispatch_call(&self, id: &FuncId, arg_list: &[&dyn ObjCore]) -> Arc<Func> {
        todo!()
    }

    pub fn register_resource(&mut self, resource: ()) -> ResourceId {
        todo!()
    }

    /// # Safety
    /// All `val` must be valid allocation. Thread safety must be explicitly
    /// granted repect to concurrent performing.
    // later may support dynamical register op
    pub unsafe fn perform_op(&self, id: &OpId, val: &[Val], context: &OpContext) -> *mut Obj {
        match &**id {
            "intrinsic.alloc" => {
                let val = context.get_addr(val[0]);
                context.alloc(Prod {
                    header: 1,
                    data: vec![val],
                })
            }
            "intrinsic.load" => {
                let obj: &Prod = context.read_frame(val[0]).downcast_ref().unwrap();
                assert_eq!(obj.header, 1);
                obj.data[0]
            }
            "intrinsic.store" => {
                let obj: &mut Prod = context.write_frame(val[0]).downcast_mut().unwrap();
                assert_eq!(obj.header, 1);
                obj.data[0] = context.get_addr(val[1]);
                context.get_addr(Val::Const(ValConst::Unit))
            }
            "intrinsic.i32add" => {
                let (i1, i2) = (context.get_i32(val[0]), context.get_i32(val[1]));
                context.alloc(Native(i1 + i2))
            }
            "intrinsic.i32eq" => {
                let (i1, i2) = (context.get_i32(val[0]), context.get_i32(val[1]));
                context.get_addr(Val::Const(ValConst::Bool(i1 == i2)))
            }

            _ => unimplemented!(),
        }
    }
}
