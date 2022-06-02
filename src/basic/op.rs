use crate::{
    instr::{FuncBuilder, Instr, Val, ValConst},
    interp::{FrameObj, OpCtx, I32},
    loader::{Loader, MatchExpr, Param},
    Operator,
};

use super::obj::{List, Str};

pub struct Op {}

impl Op {
    pub fn new() -> Self {
        Self {}
    }
}

unsafe impl Operator for Op {
    fn perform(&mut self, code: &str, val: &[Val], context: &mut OpCtx) -> Option<FrameObj> {
        match code {
            "basic.str_copy" => {
                let s = context.make_addr(val[0]);
                let s = unsafe { context.worker.mem.read(s) };
                let Str(s) = s.downcast_ref().unwrap();
                Some(context.make(Str(s.clone())))
            }
            "basic.str_trace" => {
                let s = context.make_addr(val[0]);
                let s = unsafe { context.worker.mem.read(s) };
                let Str(s) = s.downcast_ref().unwrap();
                writeln!(context.worker.trace_out, "{s}").unwrap();
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

            "basic.list_new" => Some(context.make(List(Vec::new()))),
            "basic.list_push" => {
                let l = context.make_addr(val[0]);
                let mut l = unsafe { context.worker.mem.write(l) };
                let List(l) = l.downcast_mut().unwrap();
                l.push(context.make_addr(val[1]));
                None
            }
            "basic.list_pop" => {
                let res = {
                    let mut l = unsafe { context.write(val[0]) };
                    let List(l) = l.downcast_mut().unwrap();
                    l.pop().unwrap()
                };
                Some(FrameObj::Addr(res))
            }
            "basic.list_get" => {
                let l = context.make_addr(val[0]);
                let l = unsafe { context.worker.mem.read(l) };
                let List(l) = l.downcast_ref().unwrap();
                let i = unsafe { context.get_i32(val[1]) };
                Some(FrameObj::Addr(l[i as usize]))
            }
            "basic.list_len" => {
                let len = {
                    let l = unsafe { context.read(val[0]) };
                    let List(l) = l.downcast_ref().unwrap();
                    l.len()
                };
                Some(context.make_i32(len as _))
            }
            "basic.list_insert" => {
                let index = unsafe { context.get_i32(val[1]) };
                let addr = context.make_addr(val[2]);
                let mut l = unsafe { context.write(val[0]) };
                let List(l) = l.downcast_mut().unwrap();
                l.insert(index as _, addr);
                None
            }

            _ => unimplemented!(),
        }
    }
}

impl Op {
    pub fn load(loader: &mut Loader) {
        loader.make_tag(List::NAME);
        loader.make_tag(Str::NAME);

        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op("basic.str_copy".to_string(), vec![Val::Arg(0)]));
        func.push_instr(Instr::Ret(i1));
        loader.register_func(
            "basic.str",
            &[Param::Genuine(Str::NAME.to_owned())],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        func.push_instr(Instr::Op("basic.str_trace".to_string(), vec![Val::Arg(0)]));
        func.push_instr(Instr::Ret(Val::Const(ValConst::Unit)));
        loader.register_func(
            "basic.str_trace",
            &[Param::Genuine(Str::NAME.to_owned())],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        func.push_instr(Instr::Op(
            "basic.str_push".to_string(),
            vec![Val::Arg(0), Val::Arg(1)],
        ));
        func.push_instr(Instr::Ret(Val::Const(ValConst::Unit)));
        loader.register_func(
            "basic.str_push",
            &[
                Param::Genuine(Str::NAME.to_owned()),
                Param::Genuine(Str::NAME.to_owned()),
            ],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op("basic.list_new".to_string(), vec![]));
        func.push_instr(Instr::Ret(i1));
        loader.register_func("basic.list", &[], func.finish());

        let mut func = FuncBuilder::default();
        func.push_instr(Instr::Op(
            "basic.list_push".to_string(),
            vec![Val::Arg(0), Val::Arg(1)],
        ));
        func.push_instr(Instr::Ret(Val::Const(ValConst::Unit)));
        loader.register_func(
            "basic.list_push",
            &[
                Param::Genuine(List::NAME.to_owned()),
                Param::Match(MatchExpr::And(Vec::new())),
            ],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op("basic.list_pop".to_string(), vec![Val::Arg(0)]));
        func.push_instr(Instr::Ret(i1));
        loader.register_func(
            "basic.list_pop",
            &[Param::Genuine(List::NAME.to_owned())],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op(
            "basic.list_get".to_string(),
            vec![Val::Arg(0), Val::Arg(1)],
        ));
        func.push_instr(Instr::Ret(i1));
        loader.register_func(
            "basic.list_get",
            &[
                Param::Genuine(List::NAME.to_owned()),
                Param::Genuine(I32::NAME.to_owned()),
            ],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op("basic.list_len".to_string(), vec![Val::Arg(0)]));
        func.push_instr(Instr::Ret(i1));
        loader.register_func(
            "basic.list_len",
            &[Param::Genuine(List::NAME.to_owned())],
            func.finish(),
        );

        let mut func = FuncBuilder::default();
        func.push_instr(Instr::Op(
            "basic.list_insert".to_string(),
            vec![Val::Arg(0), Val::Arg(1), Val::Arg(2)],
        ));
        func.push_instr(Instr::Ret(Val::Const(ValConst::Unit)));
        loader.register_func(
            "basic.list_insert",
            &[
                Param::Genuine(List::NAME.to_owned()),
                Param::Genuine(I32::NAME.to_owned()),
                Param::Match(MatchExpr::And(Vec::new())),
            ],
            func.finish(),
        );
    }
}
