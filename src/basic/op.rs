use std::{io::Write, time::Instant};

use crate::{
    instr::{Val, ValConst},
    interp::OpContext,
    mem::Obj,
    Operator,
};

use super::obj::Str;

pub struct Op {
    t0: Instant,
    trace_out: Box<dyn Write>,
}

impl Op {
    pub fn new(t0: Instant, trace_out: impl Write + 'static) -> Self {
        Self {
            t0,
            trace_out: Box::new(trace_out),
        }
    }
}

unsafe impl Operator for Op {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpContext) -> *mut Obj {
        match code {
            "intrinsic.i32add" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                context.make_addr(Val::Const(ValConst::I32(i1 + i2)))
            }
            "intrinsic.i32eq" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                context.make_addr(Val::Const(ValConst::Bool(i1 == i2)))
            }

            "basic.trace_str" => {
                let s = context.make_addr(val[0]);
                let s = unsafe { context.worker.mem.read(s) };
                let Str(s) = s.downcast_ref().unwrap();
                let t = (Instant::now() - self.t0).as_secs_f64();
                writeln!(self.trace_out, "[{t:11.6}] {s}").unwrap();
                context.make_addr(Val::Const(ValConst::Unit))
            }

            _ => unimplemented!(),
        }
    }
}
