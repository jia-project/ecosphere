use std::{any::Any, time::Instant};

use crate::{Entity, EntityModel, EvalOp, Expr, Loader, Mem};

#[derive(Debug, Clone)]
pub enum ExprOp {
    NilNew,
    ResultNot,
    IntNew(i32),
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,
    IntEq,
    ProductNew(u32),
    ProductGet(usize),
    ProductSet(usize),
    Trace,
    ListNew,
    ListPush,
    ListGet,
    StrNew(String), // TODO static entity
    StrAdd,
}

pub struct Eval {
    ref_instant: Instant,
}

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
        Eval::TYPE_INT
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
        Eval::TYPE_LIST
    }
}

struct Str(String);
impl EntityModel for Str {
    fn mark(&self, _marker: &mut crate::MemMarker<'_>) {}
    fn ty(&self) -> u32 {
        Eval::TYPE_STR
    }
}

impl Eval {
    pub const TYPE_NIL: u32 = 1;
    pub const TYPE_INT: u32 = 2;
    pub const TYPE_RESULT: u32 = 3;
    pub const TYPE_LIST: u32 = 4;
    pub const TYPE_STR: u32 = 5;

    pub fn install(loader: &mut Loader<Expr<ExprOp>>) -> Self {
        loader.register_type(Self::TYPE_NIL, String::from("Nil"));
        loader.register_type(Self::TYPE_INT, String::from("Int"));
        loader.register_type(Self::TYPE_RESULT, String::from("Result"));
        loader.register_type(Self::TYPE_LIST, String::from("List"));
        loader.register_type(Self::TYPE_STR, String::from("Str"));

        loader.register_op(
            String::from("not"),
            vec![Self::TYPE_RESULT],
            0,
            ExprOp::ResultNot,
        );
        for (name, op) in [
            ("add", ExprOp::IntAdd),
            ("sub", ExprOp::IntSub),
            ("mul", ExprOp::IntMul),
            ("div", ExprOp::IntDiv),
            ("rem", ExprOp::IntRem),
            ("eq", ExprOp::IntEq),
        ] {
            loader.register_op(
                String::from(name),
                vec![Self::TYPE_INT, Self::TYPE_INT],
                0,
                op,
            );
        }
        loader.register_op(String::from("trace"), vec![], 1, ExprOp::Trace);
        loader.register_op(String::from("List"), vec![], 0, ExprOp::ListNew);
        loader.register_op(
            String::from("push"),
            vec![Self::TYPE_LIST],
            1,
            ExprOp::ListPush,
        );
        loader.register_op(
            String::from("get"),
            vec![Self::TYPE_LIST, Self::TYPE_INT],
            0,
            ExprOp::ListGet,
        );

        loader.register_op(
            String::from("add"),
            vec![Self::TYPE_STR, Self::TYPE_STR],
            0,
            ExprOp::StrAdd,
        );

        Self {
            ref_instant: Instant::now(),
        }
    }

    unsafe fn load_entity<T: Any>(&self, entity: *mut Entity, ty: u32) -> &T {
        let entity = unsafe { &*entity };
        assert_eq!(entity.core.ty(), ty);
        entity.core.any_ref().downcast_ref().unwrap()
    }

    unsafe fn load_int(&self, entity: *mut Entity) -> i32 {
        unsafe { self.load_entity::<Int>(entity, Self::TYPE_INT) }.0
    }

    fn alloc_int(&self, val: i32, mem: &mut Mem) -> *mut Entity {
        mem.alloc(Int(val))
    }

    fn alloc_bool(&self, val: bool, mem: &mut Mem) -> *mut Entity {
        let nil = self.alloc_nil(mem);
        mem.alloc(Sum(Self::TYPE_RESULT, val as _, nil))
    }
}

unsafe impl EvalOp<ExprOp> for Eval {
    fn alloc_nil(&self, mem: &mut Mem) -> *mut Entity {
        // TODO shared static entity
        mem.alloc(Product(Self::TYPE_NIL, Vec::new()))
    }

    unsafe fn is_truthy(&self, entity: *mut Entity) -> bool {
        unsafe { &*entity }
            .core
            .any_ref()
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

            ExprOp::ResultNot => {
                let a0 = unsafe { &*args[0] };
                assert_eq!(a0.core.ty(), Self::TYPE_RESULT);
                let &Sum(ty, tag, entity) = a0.core.any_ref().downcast_ref().unwrap();
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
            ExprOp::IntRem => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_int(a0 % a1, mem)
            }
            ExprOp::IntEq => {
                let (a0, a1) = unsafe { (self.load_int(args[0]), self.load_int(args[1])) };
                self.alloc_bool(a0 == a1, mem)
            }

            &ExprOp::ProductNew(ty) => mem.alloc(Product(ty, args.to_vec())),
            &ExprOp::ProductGet(index) => {
                let product: &Product = unsafe { &*args[0] }.core.any_ref().downcast_ref().unwrap();
                product.1[index]
            }
            &ExprOp::ProductSet(index) => {
                let product: &mut Product = unsafe { &mut *args[0] }
                    .core
                    .any_mut()
                    .downcast_mut()
                    .unwrap();
                product.1[index] = args[1];
                self.alloc_nil(mem)
            }

            ExprOp::Trace => {
                let entity = &unsafe { &*args[0] }.core;
                println!(
                    "[{:8?}] <{} entity at {:#x?}>{}",
                    Instant::now() - self.ref_instant,
                    loader.type_repr(entity.ty()),
                    args[0],
                    if entity.ty() == Self::TYPE_STR {
                        String::from(" ") + &entity.any_ref().downcast_ref::<Str>().unwrap().0
                    } else {
                        String::from("")
                    }
                );
                self.alloc_nil(mem)
            }

            ExprOp::ListNew => mem.alloc(List(Vec::new())),
            ExprOp::ListPush => {
                let list = &mut unsafe { &mut *args[0] }.core;
                assert_eq!(list.ty(), Self::TYPE_LIST);
                let list: &mut List = list.any_mut().downcast_mut().unwrap();
                list.0.push(args[1]);
                self.alloc_nil(mem)
            }
            ExprOp::ListGet => {
                let list = &unsafe { &*args[0] }.core;
                assert_eq!(list.ty(), Self::TYPE_LIST);
                let list: &List = list.any_ref().downcast_ref().unwrap();
                list.0[unsafe { self.load_int(args[1]) } as usize] // TODO reverse index?
            }

            ExprOp::StrNew(lit) => mem.alloc(Str(lit.clone())),
            ExprOp::StrAdd => {
                let (s0, s1) = unsafe {
                    (
                        self.load_entity::<Str>(args[0], Self::TYPE_STR),
                        self.load_entity::<Str>(args[1], Self::TYPE_STR),
                    )
                };
                mem.alloc(Str(s0.0.clone() + &s1.0))
            }
        }
    }
}
