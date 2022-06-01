use std::{collections::HashMap, sync::Arc};

use crate::{
    instr::{ArgMorph, CoreOp, Func, Instr, InstrCall, InstrId, Val, ValConst},
    loader::{CallArg, Loader, TagId},
    mem::{Mutator, Obj},
    worker::WorkerInterface,
    Name, ObjCore, Operator,
};

pub struct Interp {
    frame_list: Vec<Frame>,
    result: Option<*mut Obj>,
}

struct Frame {
    func: Arc<Func>,
    instr_id: InstrId,
    val_table: HashMap<Val, *mut Obj>,
    // flow log for phi
}

impl Default for Interp {
    fn default() -> Self {
        Self {
            frame_list: Vec::new(),
            result: None,
        }
    }
}

pub struct Prod {
    pub tag: TagId,
    pub data: Vec<*mut Obj>,
}

impl Prod {
    pub const NAME: &'static Name = "intrinsic.Prod";
}
unsafe impl ObjCore for Prod {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.data.iter().copied().for_each(mark);
    }

    fn name(&self) -> &Name {
        Self::NAME
    }
}

pub struct Sum {
    pub tag: TagId,
    pub variant: u32,
    pub inner: *mut Obj,
}

impl Sum {
    pub const NAME: &'static Name = "intrinsic.Sum";
}
unsafe impl ObjCore for Sum {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        mark(self.inner);
    }

    fn name(&self) -> &Name {
        Self::NAME
    }
}

#[derive(Default)]
pub struct I32(pub i32);
impl I32 {
    pub const NAME: &'static Name = "intrinsic.I32";
}
unsafe impl ObjCore for I32 {
    fn name(&self) -> &Name {
        Self::NAME
    }
}

impl Interp {
    pub fn load(loader: &mut Loader) {
        // it may be possible to load these intrinsic from source text, like
        // Rust's std::intrinsic
        // but in this language source parser is not the part of essential, so
        // we don't want a inversing dependency, i.e. use basic::parse in here
        // outside of basic module
        loader.register_prod("intrinsic.Unit", &[]);
        loader.register_prod("intrinsic.Ref", &["content"]);
        loader.register_sum("intrinsic.Option", &["Some", "None"]);
        loader.make_tag(I32::NAME);
    }

    pub fn push_call(
        &mut self,
        func_id: &str,
        arg_list: &[(*mut Obj, Option<ArgMorph>)],
        mem: &Mutator<'_>,
        loader: &Loader,
    ) {
        let (val_list, as_list): (Vec<_>, Vec<_>) = arg_list
            .iter()
            .map(|(val, morph)| {
                let tag_list = if let Some(morph) = morph {
                    // TODO check valid morph
                    CallArg::Morph(
                        morph
                            .iter()
                            .map(|tag| loader.query_tag(tag))
                            .collect::<Vec<_>>(),
                    )
                } else {
                    CallArg::Genuine(unsafe { Self::get_tag(*val, mem, loader) })
                };
                (val, tag_list)
            })
            .unzip();
        let func = loader.dispatch_call(func_id, &as_list);
        let frame = Frame {
            func,
            instr_id: InstrId::default(),
            val_table: val_list
                .iter()
                .enumerate()
                .map(|(i, arg)| (Val::Arg(i), *arg))
                .collect(),
        };
        self.frame_list.push(frame);
    }

    pub fn step(&mut self, worker: &mut WorkerInterface<'_>, operator: &mut dyn Operator) {
        assert!(self.result.is_none());
        let mut ctx = OpCtx {
            frame: self.frame_list.last_mut().unwrap(),
            worker,
        };
        match ctx.frame.func.get_instr(&ctx.frame.instr_id) {
            Instr::Ret(val) => {
                let ret = ctx.make_addr(*val);
                drop(ctx); // or we cannot pop from frame list
                self.frame_list.pop().unwrap();
                if let Some(frame) = self.frame_list.last_mut() {
                    Self::finish_step(frame, Some(ret));
                } else {
                    self.result = Some(ret);
                }
            }
            Instr::Call(InstrCall {
                name: func_id,
                arg_list,
            }) => {
                let func_id = func_id.clone();
                let arg_list: Vec<_> = arg_list
                    .iter()
                    .map(|(val, morph)| (ctx.make_addr(*val), morph.clone()))
                    .collect();
                drop(ctx);
                self.push_call(&func_id, &arg_list, &worker.mem, worker.loader);
            }
            Instr::Spawn(InstrCall {
                name: func_id,
                arg_list,
            }) => {
                let arg_list: Vec<_> = arg_list
                    .into_iter()
                    .map(|(val, morph)| (ctx.make_addr(*val), morph.clone()))
                    .collect();
                let mut interp = Interp::default();
                interp.push_call(&func_id, &arg_list, &ctx.worker.mem, ctx.worker.loader);
                Self::finish_step(ctx.frame, Some(ctx.worker.spawn(interp)));
            }
            Instr::Wait(val) => {
                let val = ctx.make_addr(*val);
                unsafe { ctx.worker.wait(val) };
                if !ctx.worker.is_paused() {
                    Self::finish_step(ctx.frame, None);
                }
            }
            Instr::Op(id, val) => {
                let (id, val) = (id.clone(), val.clone());
                let res = operator.perform(&id, &val, &mut ctx);
                if !ctx.worker.is_paused() {
                    Self::finish_step(ctx.frame, res);
                } else {
                    assert!(res.is_none());
                }
            }
            Instr::Br(val, if_true, if_false) => {
                let val = unsafe { ctx.get_bool(*val) };
                ctx.frame.instr_id = (*if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::CoreOp(op) => Self::step_core_op(op.clone(), ctx),
            Instr::Alloc => Self::step_core_op(CoreOp::NewProd(Prod::NAME.to_owned()), ctx),
            Instr::Load(val) => Self::step_core_op(CoreOp::Get(*val, "content".to_string()), ctx),
            Instr::Store(place_val, val) => {
                Self::step_core_op(CoreOp::Set(*place_val, "content".to_string(), *val), ctx)
            }
        }
    }

    unsafe fn get_tag(addr: *mut Obj, mem: &Mutator<'_>, loader: &Loader) -> TagId {
        let obj = mem.read(addr);
        match obj.name() {
            Prod::NAME => {
                let Prod { tag, .. } = obj.downcast_ref().unwrap();
                *tag
            }
            Sum::NAME => {
                let Sum { tag, .. } = obj.downcast_ref().unwrap();
                *tag
            }
            name => loader.query_tag(name),
        }
    }

    fn finish_step(frame: &mut Frame, res: Option<*mut Obj>) {
        if let Some(res) = res {
            frame.val_table.insert(Val::Instr(frame.instr_id), res);
        }
        frame.instr_id.1 += 1;
    }

    pub fn resume(&mut self, result: Option<*mut Obj>) {
        assert!(self.result.is_none());
        Self::finish_step(self.frame_list.last_mut().unwrap(), result);
    }

    pub fn get_result(&self) -> Option<*mut Obj> {
        self.result
    }

    pub fn trace(&self, mut mark: impl FnMut(*mut Obj)) {
        if let Some(res) = self.result {
            mark(res);
            return;
        }
        for frame in &self.frame_list {
            frame.val_table.values().copied().for_each(&mut mark);
        }
    }
}

pub struct OpCtx<'a, 'b> {
    pub worker: &'b mut WorkerInterface<'a>,
    frame: &'b mut Frame,
}

impl OpCtx<'_, '_> {
    /// # Safety
    /// If `val` is not constant, it's frame record must be a valid allocation.
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.worker.mem.read(self.make_addr(val));
                let I32(content) = obj.downcast_ref().unwrap();
                *content
            }
            _ => unreachable!(),
        }
    }

    /// # Safety
    /// Same as `get_i32`.
    pub unsafe fn get_bool(&self, val: Val) -> bool {
        match val {
            Val::Const(ValConst::Bool(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.worker.mem.read(self.make_addr(val));
                let Sum { variant, .. } = obj.downcast_ref().unwrap();
                *variant == 0
            }
            _ => unreachable!(),
        }
    }

    /// # Safety
    /// Same as `get_i32`.
    pub unsafe fn assert_unit(&self, val: Val) {
        match val {
            Val::Const(ValConst::Unit) => {}
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.worker.mem.read(self.make_addr(val));
                let Prod { tag, .. } = obj.downcast_ref().unwrap();
                assert_eq!(*tag, 0);
            }
            _ => unreachable!(),
        }
    }

    pub fn make_addr(&self, val: Val) -> *mut Obj {
        if let Val::Const(val_const) = val {
            match val_const {
                ValConst::I32(content) => self.worker.mem.make(I32(content)),
                ValConst::Unit => self.worker.mem.make(Prod {
                    tag: 0,
                    data: Vec::new(),
                }),
                ValConst::Bool(content) => self.worker.mem.make(Sum {
                    tag: 2,
                    variant: if content { 0 } else { 1 },
                    inner: self.make_addr(Val::Const(ValConst::Unit)),
                }),
                ValConst::Asset(id) => self.worker.loader.query_asset(id),
            }
        } else {
            self.frame.val_table[&val]
        }
    }
}

impl Interp {
    fn step_core_op(core_op: CoreOp, ctx: OpCtx<'_, '_>) {
        match core_op {
            CoreOp::NewProd(name) => {
                let unit = ctx.make_addr(Val::Const(ValConst::Unit));
                let header = ctx.worker.loader.query_tag(&name);
                let prod = ctx.worker.mem.make(Prod {
                    tag: header,
                    data: vec![unit; ctx.worker.loader.prod_size(header)],
                });
                Self::finish_step(ctx.frame, Some(prod));
            }
            CoreOp::NewSum(name, variant, inner) => {
                let inner = ctx.make_addr(inner);
                let header = ctx.worker.loader.query_tag(&name);
                let sum = ctx.worker.mem.make(Sum {
                    tag: header,
                    variant: ctx.worker.loader.sum_variant(header, &variant),
                    inner,
                });
                Self::finish_step(ctx.frame, Some(sum));
            }
            CoreOp::Get(prod, key) => {
                let prod = ctx.make_addr(prod);
                let prod = unsafe { ctx.worker.mem.read(prod) };
                let prod: &Prod = prod.downcast_ref().unwrap();
                let res = prod.data[ctx.worker.loader.prod_offset(prod.tag, &key)];
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::Set(prod, key, val) => {
                let prod = ctx.make_addr(prod);
                let val = ctx.make_addr(val);
                let mut prod = unsafe { ctx.worker.mem.write(prod) };
                let prod: &mut Prod = prod.downcast_mut().unwrap();
                prod.data[ctx.worker.loader.prod_offset(prod.tag, &key)] = val;
                Self::finish_step(ctx.frame, None);
            }
            CoreOp::Is(sum, key) => {
                let worker = &ctx.worker;
                let sum = unsafe { worker.mem.read(ctx.make_addr(sum)) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                let res = sum.variant == worker.loader.sum_variant(sum.tag, &key);
                let res = ctx.make_addr(Val::Const(ValConst::Bool(res)));
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::As(sum, key) => {
                let sum = ctx.make_addr(sum);
                let sum = unsafe { ctx.worker.mem.read(sum) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                // consider a unchecked version
                assert_eq!(sum.variant, ctx.worker.loader.sum_variant(sum.tag, &key));
                let res = sum.inner;
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Add(v1, v2)
            | CoreOp::I32Sub(v1, v2)
            | CoreOp::I32Mul(v1, v2)
            | CoreOp::I32Div(v1, v2)
            | CoreOp::I32Mod(v1, v2)
            | CoreOp::I32Eq(v1, v2)
            | CoreOp::I32Lt(v1, v2) => {
                let (i1, i2) = unsafe { (ctx.get_i32(v1), ctx.get_i32(v2)) };
                todo!()
            }
            CoreOp::BoolNeg(val) => {
                let b = unsafe { ctx.get_bool(val) };
                Self::finish_step(
                    ctx.frame,
                    Some(ctx.make_addr(Val::Const(ValConst::Bool(!b)))),
                );
            }
        }
    }
}
