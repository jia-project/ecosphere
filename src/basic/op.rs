use std::{io::Write, time::Instant};

use crate::{
    instr::{FuncBuilder, Instr, Val, ValConst},
    interp::OpContext,
    loader::{Loader, TagExpr},
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
}

unsafe impl Operator for Op {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpContext) -> Option<*mut Obj> {
        match code {
            "intrinsic.i32add" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 + i2))))
            }
            "intrinsic.i32sub" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 - i2))))
            }
            "intrinsic.i32mul" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 * i2))))
            }
            "intrinsic.i32div" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 / i2))))
            }
            "intrinsic.i32mod" => {
                let (i1, i2) = unsafe { (context.get_i32(val[0]), context.get_i32(val[1])) };
                Some(context.make_addr(Val::Const(ValConst::I32(i1 % i2))))
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
            "basic.str_push" => {
                let (s1, s2) = (context.make_addr(val[0]), context.make_addr(val[1]));
                let (mut s1, s2) =
                    unsafe { (context.worker.mem.write(s1), context.worker.mem.read(s2)) };
                let (Str(s1), Str(s2)) = (s1.downcast_mut().unwrap(), s2.downcast_ref().unwrap());
                s1.push_str(s2);
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

impl Op {
    pub fn boot(loader: &mut Loader) {
        loader.make_tag(List::NAME);
        loader.make_tag(Str::NAME);

        let mut func = FuncBuilder::default();
        func.push_instr(Instr::Op("basic.str_trace".to_string(), vec![Val::Arg(0)]));
        func.push_instr(Instr::Ret(Val::Const(ValConst::Unit)));
        loader.register_func(
            "basic.str_trace",
            &[TagExpr::Has(Str::NAME.to_owned())],
            func.finish(),
        );
    }
}
