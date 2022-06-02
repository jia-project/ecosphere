use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::Arc,
};

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
    val_table: HashMap<Val, FrameObj>,
    // flow log for phi
}

pub enum FrameObj {
    Addr(*mut Obj),
    Stub,
}

impl FrameObj {
    unsafe fn read<'a>(&'a self, mem: &'a Mutator<'a>) -> impl Deref<Target = dyn ObjCore> + 'a {
        match self {
            Self::Addr(addr) => mem.read(*addr),
            Self::Stub => unreachable!(),
        }
    }
    unsafe fn write<'a>(
        &'a mut self,
        mem: &'a Mutator<'a>,
    ) -> impl DerefMut<Target = dyn ObjCore> + 'a {
        match self {
            Self::Addr(addr) => mem.write(*addr),
            Self::Stub => unreachable!(),
        }
    }

    fn make_addr(&mut self, _mem: &Mutator<'_>) -> *mut Obj {
        match self {
            Self::Addr(addr) => *addr,
            Self::Stub => unreachable!(),
        }
    }
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
        arg_list: Vec<(FrameObj, Option<ArgMorph>)>,
        mem: &Mutator<'_>,
        loader: &Loader,
    ) {
        let (val_list, as_list): (Vec<_>, Vec<_>) = arg_list
            .into_iter()
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
                    CallArg::Genuine(unsafe { Self::get_tag(&val, mem, loader) })
                };
                (val, tag_list)
            })
            .unzip();
        let func = loader.dispatch_call(func_id, &as_list);
        let frame = Frame {
            func,
            instr_id: InstrId::default(),
            val_table: val_list
                .into_iter()
                .enumerate()
                .map(|(i, arg)| (Val::Arg(i), arg))
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
        match ctx.frame.func.get_instr(&ctx.frame.instr_id).clone() {
            Instr::Ret(val) => {
                let mut ret = ctx.replace(val, FrameObj::Stub);
                drop(ctx); // or we cannot pop from frame list
                let mut arg_table = self.frame_list.pop().unwrap().val_table;
                if let Some(frame) = self.frame_list.last_mut() {
                    let arg_list = if let Instr::Call(InstrCall { arg_list, .. }) =
                        frame.func.get_instr(&frame.instr_id)
                    {
                        arg_list
                    } else {
                        unreachable!()
                    };
                    for (i, (val, _)) in arg_list.iter().enumerate() {
                        if matches!(val, Val::Arg(..)) || matches!(val, Val::Instr(..)) {
                            // don't have ctx.replace any more...
                            let stub = frame
                                .val_table
                                .insert(*val, arg_table.remove(&Val::Arg(i)).unwrap())
                                .unwrap();
                            assert!(matches!(stub, FrameObj::Stub));
                        }
                    }
                    Self::finish_step(frame, Some(ret));
                } else {
                    self.result = Some(ret.make_addr(worker.mem));
                }
            }
            Instr::Call(InstrCall {
                name: func_id,
                arg_list,
            }) => {
                let func_id = func_id.clone();
                let arg_list: Vec<_> = arg_list
                    .iter()
                    .map(|(val, morph)| (ctx.replace(*val, FrameObj::Stub), morph.clone()))
                    .collect();
                drop(ctx);
                self.push_call(&func_id, arg_list, &worker.mem, worker.loader);
            }
            Instr::Spawn(InstrCall {
                name: func_id,
                arg_list,
            }) => {
                let func_id = func_id.clone();
                let arg_list: Vec<_> = arg_list
                    .iter()
                    .map(|(val, morph)| (FrameObj::Addr(ctx.make_addr(*val)), morph.clone()))
                    .collect();
                let mut interp = Interp::default();
                interp.push_call(&func_id, arg_list, &ctx.worker.mem, ctx.worker.loader);
                Self::finish_step(ctx.frame, Some(FrameObj::Addr(ctx.worker.spawn(interp))));
            }
            Instr::Wait(val) => {
                let val = ctx.make_addr(val);
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
                let val = unsafe { ctx.get_bool(val) };
                ctx.frame.instr_id = (if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::CoreOp(op) => unsafe { Self::step_core_op(op, ctx) },
            Instr::Alloc => unsafe {
                Self::step_core_op(CoreOp::NewProd("intrinsic.Ref".to_owned()), ctx)
            },
            Instr::Load(val) => unsafe {
                Self::step_core_op(CoreOp::Get(val, "content".to_string()), ctx)
            },
            Instr::Store(place_val, val) => unsafe {
                Self::step_core_op(CoreOp::Set(place_val, "content".to_string(), val), ctx)
            },
        }
    }

    unsafe fn get_tag(obj: &FrameObj, mem: &Mutator<'_>, loader: &Loader) -> TagId {
        let obj = obj.read(mem);
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

    fn finish_step(frame: &mut Frame, res: Option<FrameObj>) {
        if let Some(res) = res {
            frame.val_table.insert(Val::Instr(frame.instr_id), res);
        }
        frame.instr_id.1 += 1;
    }

    pub fn resume(&mut self, result: Option<*mut Obj>) {
        assert!(self.result.is_none());
        Self::finish_step(
            self.frame_list.last_mut().unwrap(),
            result.map(FrameObj::Addr),
        );
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
            for obj in frame.val_table.values() {
                if let FrameObj::Addr(addr) = obj {
                    mark(*addr);
                }
            }
        }
    }
}

pub struct OpCtx<'a, 'b> {
    pub worker: &'b mut WorkerInterface<'a>,
    frame: &'b mut Frame,
}

impl<'b> OpCtx<'_, 'b> {
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.read(val);
                let I32(content) = obj.downcast_ref().unwrap();
                *content
            }
            _ => unreachable!(),
        }
    }

    pub fn make_i32(&mut self, content: i32) -> FrameObj {
        FrameObj::Addr(self.make_addr(Val::Const(ValConst::I32(content))))
    }

    pub unsafe fn get_bool(&self, val: Val) -> bool {
        match val {
            Val::Const(ValConst::Bool(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.read(val);
                let Sum { variant, .. } = obj.downcast_ref().unwrap();
                *variant == 0
            }
            _ => unreachable!(),
        }
    }

    pub fn make_bool(&mut self, content: bool) -> FrameObj {
        FrameObj::Addr(self.make_addr(Val::Const(ValConst::Bool(content))))
    }

    pub unsafe fn assert_unit(&self, val: Val) {
        match val {
            Val::Const(ValConst::Unit) => {}
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.read(val);
                let Prod { tag, .. } = obj.downcast_ref().unwrap();
                assert_eq!(*tag, 0);
            }
            _ => unreachable!(),
        }
    }

    // make unit

    pub fn make(&self, core: impl ObjCore + 'static) -> FrameObj {
        FrameObj::Addr(self.worker.mem.make(core))
    }

    pub fn replace(&mut self, val: Val, obj: FrameObj) -> FrameObj {
        if matches!(val, Val::Const(..)) {
            FrameObj::Addr(self.make_addr(val)) // TODO
        } else {
            self.frame.val_table.insert(val, obj).unwrap()
        }
    }

    pub unsafe fn read(&self, val: Val) -> impl Deref<Target = dyn ObjCore> + '_ {
        self.frame.val_table[&val].read(self.worker.mem)
    }

    pub unsafe fn write(&mut self, val: Val) -> impl DerefMut<Target = dyn ObjCore> + '_ {
        self.frame
            .val_table
            .get_mut(&val)
            .unwrap()
            .write(self.worker.mem)
    }

    pub fn make_addr(&mut self, val: Val) -> *mut Obj {
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
            self.frame
                .val_table
                .get_mut(&val)
                .unwrap()
                .make_addr(self.worker.mem)
        }
    }
}

impl Interp {
    unsafe fn step_core_op(core_op: CoreOp, mut ctx: OpCtx<'_, '_>) {
        match core_op {
            CoreOp::NewProd(name) => {
                let unit = ctx.make_addr(Val::Const(ValConst::Unit));
                let tag = ctx.worker.loader.query_tag(&name);
                let prod = ctx.make(Prod {
                    tag,
                    data: vec![unit; ctx.worker.loader.prod_size(tag)],
                });
                Self::finish_step(ctx.frame, Some(prod));
            }
            CoreOp::NewSum(name, variant, inner) => {
                let inner = ctx.make_addr(inner);
                let tag = ctx.worker.loader.query_tag(&name);
                let sum = ctx.make(Sum {
                    tag,
                    variant: ctx.worker.loader.sum_variant(tag, &variant),
                    inner,
                });
                Self::finish_step(ctx.frame, Some(sum));
            }
            CoreOp::Get(prod, key) => {
                let res = {
                    let prod = ctx.read(prod);
                    let prod: &Prod = prod.downcast_ref().unwrap();
                    prod.data[ctx.worker.loader.prod_offset(prod.tag, &key)]
                };
                Self::finish_step(ctx.frame, Some(FrameObj::Addr(res)));
            }
            CoreOp::Set(prod, key, val) => {
                let val = ctx.make_addr(val);
                let offset = {
                    let prod = ctx.read(prod);
                    let prod: &Prod = prod.downcast_ref().unwrap();
                    ctx.worker.loader.prod_offset(prod.tag, &key)
                };
                {
                    let mut prod = ctx.write(prod);
                    let prod: &mut Prod = prod.downcast_mut().unwrap();
                    prod.data[offset] = val;
                }
                Self::finish_step(ctx.frame, None);
            }
            CoreOp::Is(sum, key) => {
                let res = {
                    let sum = ctx.read(sum);
                    let sum: &Sum = sum.downcast_ref().unwrap();
                    sum.variant == ctx.worker.loader.sum_variant(sum.tag, &key)
                };
                let res = ctx.make_bool(res);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::As(sum, key) => {
                let res = {
                    let sum = ctx.read(sum);
                    let sum: &Sum = sum.downcast_ref().unwrap();
                    // consider a unchecked version
                    assert_eq!(sum.variant, ctx.worker.loader.sum_variant(sum.tag, &key));
                    sum.inner
                };
                Self::finish_step(ctx.frame, Some(FrameObj::Addr(res)));
            }
            CoreOp::I32Add(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_i32(i1 + i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Sub(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_i32(i1 - i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Mul(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_i32(i1 * i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Div(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_i32(i1 / i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Mod(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_i32(i1 % i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Eq(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_bool(i1 == i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::I32Lt(v1, v2) => {
                let (i1, i2) = (ctx.get_i32(v1), ctx.get_i32(v2));
                let res = ctx.make_bool(i1 < i2);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::BoolNeg(val) => {
                let b = ctx.get_bool(val);
                let res = ctx.make_bool(!b);
                Self::finish_step(ctx.frame, Some(res));
            }
            CoreOp::BoolAnd(v1, v2) => {
                let (b1, b2) = (ctx.get_bool(v1), ctx.get_bool(v2));
                let res = ctx.make_bool(b1 && b2);
                Self::finish_step(ctx.frame, Some(res));
            }
        }
    }
}
