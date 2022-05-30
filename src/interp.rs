use std::{collections::HashMap, mem::size_of, sync::Arc};

use crate::{
    instr::{ArgMorph, Func, Instr, InstrCall, InstrId, Val, ValConst, I32},
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

    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.data.capacity() * size_of::<*mut Obj>()
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

impl Interp {
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
        let frame = self.frame_list.last_mut().unwrap();

        let instr = match frame.func.get_instr(&frame.instr_id).clone() {
            Instr::Alloc => Instr::NewProd("intrinsic.Ref".to_string()),
            Instr::Load(val) => Instr::Get(val, "content".to_string()),
            Instr::Store(place_val, val) => Instr::Set(place_val, "content".to_string(), val),
            instr => instr,
        };

        let mut context = OpContext { frame, worker };
        match instr {
            Instr::Ret(val) => {
                let ret = context.make_addr(val);
                drop(context);
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
                let arg_list: Vec<_> = arg_list
                    .into_iter()
                    .map(|(val, morph)| (context.make_addr(val), morph))
                    .collect();
                drop(context);
                self.push_call(&func_id, &arg_list, &worker.mem, worker.loader);
            }
            Instr::Spawn(InstrCall {
                name: func_id,
                arg_list,
            }) => {
                let arg_list: Vec<_> = arg_list
                    .into_iter()
                    .map(|(val, morph)| (context.make_addr(val), morph))
                    .collect();
                drop(context);
                let mut interp = Interp::default();
                interp.push_call(&func_id, &arg_list, &worker.mem, worker.loader);
                Self::finish_step(frame, Some(worker.spawn(interp)));
            }
            Instr::Wait(val) => {
                let val = context.make_addr(val);
                drop(context);
                unsafe { worker.wait(val) };
                if !worker.is_paused() {
                    Self::finish_step(frame, None);
                }
            }
            Instr::Op(id, val) => {
                let res = operator.perform(&id, &val, &mut context);
                if !context.worker.is_paused() {
                    drop(context);
                    Self::finish_step(frame, res);
                } else {
                    assert!(res.is_none());
                }
            }
            Instr::Br(val, if_true, if_false) => {
                let val = unsafe { context.get_bool(val) };
                drop(context);
                frame.instr_id = (if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::NewProd(name) => {
                let unit = context.make_addr(Val::Const(ValConst::Unit));
                drop(context);
                let header = worker.loader.query_tag(&name);
                let prod = worker.mem.make(Prod {
                    tag: header,
                    data: vec![unit; worker.loader.prod_size(header)],
                });
                Self::finish_step(frame, Some(prod));
            }
            Instr::NewSum(name, variant, inner) => {
                let inner = context.make_addr(inner);
                drop(context);
                let header = worker.loader.query_tag(&name);
                let sum = worker.mem.make(Sum {
                    tag: header,
                    variant: worker.loader.sum_variant(header, &variant),
                    inner,
                });
                Self::finish_step(frame, Some(sum));
            }
            Instr::Get(prod, key) => {
                let prod = context.make_addr(prod);
                drop(context);
                let prod = unsafe { worker.mem.read(prod) };
                let prod: &Prod = prod.downcast_ref().unwrap();
                let res = prod.data[worker.loader.prod_offset(prod.tag, &key)];
                Self::finish_step(frame, Some(res));
            }
            Instr::Set(prod, key, val) => {
                let prod = context.make_addr(prod);
                let val = context.make_addr(val);
                drop(context);
                let mut prod = unsafe { worker.mem.write(prod) };
                let prod: &mut Prod = prod.downcast_mut().unwrap();
                prod.data[worker.loader.prod_offset(prod.tag, &key)] = val;
                Self::finish_step(frame, None);
            }
            Instr::Is(sum, key) => {
                let worker = &context.worker;
                let sum = unsafe { worker.mem.read(context.make_addr(sum)) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                let res = sum.variant == worker.loader.sum_variant(sum.tag, &key);
                let res = context.make_addr(Val::Const(ValConst::Bool(res)));
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::As(sum, key) => {
                let sum = context.make_addr(sum);
                drop(context);
                let sum = unsafe { worker.mem.read(sum) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                // consider a unchecked version
                assert_eq!(sum.variant, worker.loader.sum_variant(sum.tag, &key));
                let res = sum.inner;
                Self::finish_step(frame, Some(res));
            }
            _ => unreachable!(),
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

pub struct OpContext<'a, 'b> {
    pub worker: &'b mut WorkerInterface<'a>,
    frame: &'b Frame,
}

impl OpContext<'_, '_> {
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
