use crate::{
    instr::{Val, ValConst},
    interp::OpContext,
    mem::Obj,
    OpCode,
};

pub struct Op;

unsafe impl crate::Op for Op {
    type Worker = (); // TODO

    fn perform(code: &OpCode, val: &[Val], context: &mut OpContext<Self::Worker>) -> *mut Obj {
        match &**code {
            "intrinsic.i32add" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                context.make_addr(Val::Const(ValConst::I32(i1 + i2)))
            }
            "intrinsic.i32eq" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                context.make_addr(Val::Const(ValConst::Bool(i1 == i2)))
            }

            _ => unimplemented!(),
        }
    }
}
