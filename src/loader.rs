use std::{collections::HashMap, sync::Arc};

use crate::{
    def::{FuncId, OpId, ResourceId},
    instr::{Func, Val, ValConst},
    interp::OpContext,
    mem::{Mem, Obj, ObjCore},
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

    pub fn dispatch_call(&self, id: &FuncId, arg_list: &[Val]) -> Arc<Func> {
        todo!()
    }

    pub fn register_resource(&mut self, id: ResourceId, resource: ()) {
        //
    }

    // later may support dynamical register op
    pub unsafe fn perform_op(&self, id: &OpId, val: &[Val], context: &OpContext) -> *mut Obj {
        match &**id {
            "intrinsic.i32add" => {
                let (i1, i2) = (context.get_i32(val[0]), context.get_i32(val[1]));
                context.alloc(ObjCore::I32(i1 + i2))
            }
            "intrinsic.i32eq" => {
                let (i1, i2) = (context.get_i32(val[0]), context.get_i32(val[1]));
                context.get_addr(Val::Const(ValConst::Bool(i1 == i2)))
            }
            "intrinsic.alloc" => {
                let val = context.get_addr(val[0]);
                context.alloc(ObjCore::Prod(1, vec![val]))
            }
            "intrinsic.load" => {
                if let ObjCore::Prod(1, slot) = context.read_frame(val[0]) {
                    slot[0]
                } else {
                    panic!()
                }
            }
            "instrinsic.store" => {
                if let ObjCore::Prod(1, slot) = context.write_frame(val[0]) {
                    slot[0] = context.get_addr(val[1]);
                    context.get_addr(Val::Const(ValConst::Unit))
                } else {
                    panic!()
                }
            }
            _ => unimplemented!(),
        }
    }
}
