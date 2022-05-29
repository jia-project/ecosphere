use std::{io::Write, time::Instant};

use crate::{
    instr::{Val, ValConst},
    interp::OpContext,
    loader::Loader,
    mem::Obj,
    Operator,
};

use super::obj::{List, Str};

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

    pub fn boot(loader: &mut Loader) {
        loader.make_tag(List::NAME);
        loader.make_tag(Str::NAME);
    }
}

unsafe impl Operator for Op {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpContext) -> Option<*mut Obj> {
        match code {
            "intrinsic.i32add" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 + i2))))
            }
            "intrinsic.i32eq" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::Bool(i1 == i2))))
            }

            "basic.str_trace" => {
                let s = context.make_addr(val[0]);
                let s = unsafe { context.worker.mem.read(s) };
                let Str(s) = s.downcast_ref().unwrap();
                let t = (Instant::now() - self.t0).as_secs_f64();
                writeln!(self.trace_out, "[{t:11.6}] {s}").unwrap();
                None
            }

            "basic.list_new" => Some(context.worker.mem.make(List(Vec::new()))),
            "basic.list_push" => {
                let l = context.make_addr(val[0]);
                let mut l = unsafe { context.worker.mem.write(l) };
                let List(l) = l.downcast_mut().unwrap();
                l.push(context.make_addr(val[1]));
                None
            }

            _ => unimplemented!(),
        }
    }
}
