use std::mem::size_of;

use crate::{no_mark, Entity, EvalOp, Expr, Loader, Mem, TypeMeta};

#[derive(Debug, Clone, Copy)]
pub enum ExprOp {
    NilNew,
    BoolNot,
    IntNew(i32),
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    ProductNew(u32),
    ProductGet(usize),
    ProductSet(usize),
    Trace,
}

pub struct Eval {
    nil_type: u32,
    result_type: u32,
    int_type: u32,
}

impl Eval {
    pub fn install(loader: &mut Loader<Expr<ExprOp>>) -> Self {
        let nil_type = loader.register_type(TypeMeta {
            repr: String::from("Nil"),
            size: 0,
            mark_fn: no_mark,
        });
        let int_type = loader.register_type(TypeMeta {
            repr: String::from("Int"),
            size: 4,
            mark_fn: no_mark,
        });
        let result_type = loader.register_type(TypeMeta {
            repr: String::from("Result"),
            size: 5,
            mark_fn: no_mark,
        });
        for (name, op) in [
            ("add", ExprOp::IntAdd),
            ("sub", ExprOp::IntSub),
            ("mul", ExprOp::IntMul),
            ("div", ExprOp::IntDiv),
        ] {
            loader.register_op(String::from(name), vec![int_type, int_type], 0, op);
        }
        loader.register_op(String::from("trace"), vec![], 1, ExprOp::Trace);
        Self {
            nil_type,
            int_type,
            result_type,
        }
    }

    unsafe fn load_int(&self, entity: *mut Entity) -> i32 {
        let entity = unsafe { &*entity };
        assert_eq!(entity.ty, self.int_type);
        i32::from_ne_bytes(entity.raw[..].try_into().unwrap())
    }

    fn alloc_int(&self, val: i32, mem: &mut Mem) -> *mut Entity {
        mem.alloc(self.int_type, val.to_ne_bytes().to_vec())
    }
}

unsafe impl EvalOp<ExprOp> for Eval {
    fn alloc_nil(&self, mem: &mut Mem) -> *mut Entity {
        // TODO shared static entity
        mem.alloc(self.nil_type, vec![])
    }

    unsafe fn eval(
        &mut self,
        op: &ExprOp,
        args: &[*mut Entity],
        mem: &mut Mem,
        loader: &Loader<Expr<ExprOp>>,
    ) -> *mut Entity {
        match op {
            ExprOp::NilNew => self.alloc_nil(mem),
            ExprOp::BoolNot => {
                let a0 = unsafe { &*args[0] };
                assert_eq!(a0.ty, self.result_type);
                mem.alloc(self.result_type, [&[1 - a0.raw[0]], &a0.raw[1..]].concat())
            }
            &ExprOp::IntNew(lit) => mem.alloc(self.int_type, lit.to_ne_bytes().to_vec()),
            ExprOp::IntAdd => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_int(a0 + a1, mem)
            }
            ExprOp::IntSub => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_int(a0 - a1, mem)
            }
            ExprOp::IntMul => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_int(a0 * a1, mem)
            }
            ExprOp::IntDiv => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_int(a0 / a1, mem)
            }
            &ExprOp::ProductNew(ty) => mem.alloc(
                ty,
                args.iter()
                    .flat_map(|&arg| (arg as usize).to_ne_bytes())
                    .collect(),
            ),
            &ExprOp::ProductGet(index) => usize::from_ne_bytes(
                unsafe { &*args[0] }.raw
                    [index * size_of::<usize>()..(index + 1) * size_of::<usize>()]
                    .try_into()
                    .unwrap(),
            ) as _,
            &ExprOp::ProductSet(index) => {
                unsafe { &mut *args[0] }.raw
                    [index * size_of::<usize>()..(index + 1) * size_of::<usize>()]
                    .copy_from_slice(&(args[1] as usize).to_ne_bytes());
                self.alloc_nil(mem)
            }
            ExprOp::Trace => {
                println!(
                    "<{} entity at {:#x?}>",
                    loader.get_type(unsafe { &*args[0] }.ty).repr,
                    args[0]
                );
                self.alloc_nil(mem)
            }
        }
    }
}
