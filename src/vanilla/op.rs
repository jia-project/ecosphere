use std::{any::Any, mem::size_of};

use crate::{Entity, EntityModel, EvalOp, Expr, Loader, Mem};

#[derive(Debug, Clone, Copy)]
pub enum ExprOp {
    NilNew,
    BoolNot,
    IntNew(i32),
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntEq,
    ProductNew(u32),
    ProductGet(usize),
    ProductSet(usize),
    Trace,
    ListNew,
    ListPush,
    ListGet,
}

pub struct Eval {}

struct Product(u32, Vec<*mut Entity>);
impl EntityModel for Product {
    fn mark(&self, marker: &mut crate::MemMarker<'_>) {
        for &entity in &self.1 {
            marker.mark(entity)
        }
    }
    fn ty(&self) -> u32 {
        self.0
    }
}

struct Sum(u32, u8, *mut Entity);
impl EntityModel for Sum {
    fn mark(&self, marker: &mut crate::MemMarker<'_>) {
        marker.mark(self.2);
    }
    fn ty(&self) -> u32 {
        self.0
    }
}

struct Int(i32);
impl EntityModel for Int {
    fn mark(&self, _marker: &mut crate::MemMarker<'_>) {}
    fn ty(&self) -> u32 {
        Eval::INT_TYPE
    }
}

struct List(Vec<*mut Entity>);
impl EntityModel for List {
    fn mark(&self, marker: &mut crate::MemMarker<'_>) {
        for &entity in &self.0 {
            marker.mark(entity)
        }
    }
    fn ty(&self) -> u32 {
        Eval::LIST_TYPE
    }
}

impl Eval {
    pub const NIL_TYPE: u32 = 1;
    pub const INT_TYPE: u32 = 2;
    pub const RESULT_TYPE: u32 = 3;
    pub const LIST_TYPE: u32 = 4;

    pub fn install(loader: &mut Loader<Expr<ExprOp>>) -> Self {
        loader.register_type(Self::NIL_TYPE, String::from("Nil"));
        loader.register_type(Self::INT_TYPE, String::from("Int"));
        loader.register_type(Self::RESULT_TYPE, String::from("Result"));
        loader.register_type(Self::LIST_TYPE, String::from("List"));

        for (name, op) in [
            ("add", ExprOp::IntAdd),
            ("sub", ExprOp::IntSub),
            ("mul", ExprOp::IntMul),
            ("div", ExprOp::IntDiv),
            ("eq", ExprOp::IntEq),
        ] {
            loader.register_op(
                String::from(name),
                vec![Self::INT_TYPE, Self::INT_TYPE],
                0,
                op,
            );
        }
        loader.register_op(String::from("trace"), vec![], 1, ExprOp::Trace);
        Self {}
    }

    unsafe fn load_int(&self, entity: *mut Entity) -> i32 {
        let entity = unsafe { &*entity };
        assert_eq!(entity.core.ty(), Self::INT_TYPE);
        (&entity.core as &dyn Any).downcast_ref::<Int>().unwrap().0
    }

    fn alloc_int(&self, val: i32, mem: &mut Mem) -> *mut Entity {
        mem.alloc(Int(val))
    }

    fn alloc_bool(&self, val: bool, mem: &mut Mem) -> *mut Entity {
        let nil = self.alloc_nil(mem);
        mem.alloc(Sum(Self::RESULT_TYPE, val as _, nil))
    }
}

unsafe impl EvalOp<ExprOp> for Eval {
    fn alloc_nil(&self, mem: &mut Mem) -> *mut Entity {
        // TODO shared static entity
        mem.alloc(Product(Self::NIL_TYPE, Vec::new()))
    }

    unsafe fn is_truthy(&self, entity: *mut Entity) -> bool {
        (&unsafe { &*entity }.core as &dyn Any)
            .downcast_ref::<Sum>()
            .unwrap()
            .1
            != 0
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
                assert_eq!(a0.core.ty(), Self::RESULT_TYPE);
                let &Sum(ty, tag, entity) = (&a0.core as &dyn Any).downcast_ref().unwrap();
                mem.alloc(Sum(ty, 1 - tag, entity))
            }

            &ExprOp::IntNew(lit) => self.alloc_int(lit, mem),
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
            ExprOp::IntEq => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_bool(a0 == a1, mem)
            }

            &ExprOp::ProductNew(ty) => mem.alloc(Product(ty, args.to_vec())),
            &ExprOp::ProductGet(index) => {
                let product: &Product = (&unsafe { &*args[0] }.core as &dyn Any)
                    .downcast_ref()
                    .unwrap();
                product.1[index]
            }
            &ExprOp::ProductSet(index) => {
                let product: &mut Product = (&mut unsafe { &mut *args[0] }.core as &mut dyn Any)
                    .downcast_mut()
                    .unwrap();
                product.1[index] = args[1];
                self.alloc_nil(mem)
            }

            ExprOp::Trace => {
                println!(
                    "<{} entity at {:#x?}>",
                    loader.type_repr(unsafe { &*args[0] }.core.ty()),
                    args[0]
                );
                self.alloc_nil(mem)
            }

            ExprOp::ListNew => mem.alloc(List(Vec::new())),
            ExprOp::ListPush => {
                let list = &mut unsafe { &mut *args[0] }.core;
                assert_eq!(list.ty(), Self::LIST_TYPE);
                let list: &mut List = (list as &mut dyn Any).downcast_mut().unwrap();
                list.0.push(args[1]);
                self.alloc_nil(mem)
            }
            ExprOp::ListGet => {
                let list = &unsafe { &*args[0] }.core;
                assert_eq!(list.ty(), Self::LIST_TYPE);
                let list: &List = (list as &dyn Any).downcast_ref().unwrap();
                list.0[unsafe { self.load_int(args[1]) } as usize] // TODO reverse index?
            }
        }
    }
}
