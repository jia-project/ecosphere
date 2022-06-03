use crate::{
    instr::{FuncBuilder, Instr, Val, ValConst},
    interp::{FrameObj, OpCtx, I32},
    loader::{Loader, MatchExpr, Param},
    Name, Operator,
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
            "basic.str_len" => {
                let len = {
                    let s = unsafe { context.read(val[0]) };
                    let Str(s) = s.downcast_ref().unwrap();
                    s.len()
                };
                Some(context.make_i32(len as _))
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
    fn wrap(loader: &mut Loader, op: &str, name: &Name, param_list: &[Param], val_op: bool) {
        let mut func = FuncBuilder::default();
        let i1 = func.push_instr(Instr::Op(
            "basic.".to_string() + op,
            (0..param_list.len()).map(|i| Val::Arg(i)).collect(),
        ));
        func.push_instr(Instr::Ret(if val_op {
            i1
        } else {
            Val::Const(ValConst::Unit)
        }));
        loader.register_func(&("basic.".to_string() + name), param_list, func.finish());
    }

    pub fn load(loader: &mut Loader) {
        loader.make_tag(List::NAME);
        loader.make_tag(Str::NAME);

        let param_str = || Param::Genuine(Str::NAME.to_owned());
        let param_i32 = || Param::Genuine(I32::NAME.to_owned());
        let param_list = || Param::Genuine(List::NAME.to_owned());
        let param_anything = || Param::Match(MatchExpr::Anything);

        Self::wrap(loader, "str_copy", "str", &[param_str()], true);
        Self::wrap(loader, "str_trace", "trace", &[param_str()], false);
        Self::wrap(
            loader,
            "str_push",
            "push",
            &[param_str(), param_str()],
            false,
        );
        Self::wrap(loader, "str_len", "len", &[param_str()], true);

        Self::wrap(loader, "list_new", "list", &[], true);
        Self::wrap(
            loader,
            "list_push",
            "push",
            &[param_list(), param_anything()],
            false,
        );
        Self::wrap(loader, "list_pop", "pop", &[param_list()], true);
        Self::wrap(
            loader,
            "list_get",
            "get",
            &[param_list(), param_i32()],
            true,
        );
        Self::wrap(loader, "list_len", "len", &[param_list()], true);
        Self::wrap(
            loader,
            "list_insert",
            "insert",
            &[param_list(), param_i32(), param_anything()],
            false,
        );
    }
}
