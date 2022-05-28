use std::{collections::HashMap, mem::size_of, sync::Arc};

use crate::{
    instr::{Func, Instr, InstrId, Val, ValConst, I32},
    loader::Loader,
    mem::{Mutator, Obj},
    worker::WorkerInterface,
    ObjCore, Op, TagId,
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

impl ObjCore for Prod {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.data.iter().copied().for_each(mark);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>() + self.data.capacity() * size_of::<*mut Obj>()
    }

    fn name(&self) -> &str {
        "intrinsic.Prod"
    }
}

pub struct Sum {
    pub tag: TagId,
    pub variant: u32,
    pub inner: *mut Obj,
}

impl ObjCore for Sum {
    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        mark(self.inner);
    }

    fn alloc_size(&self) -> usize {
        size_of::<Self>()
    }

    fn name(&self) -> &str {
        "intrinsic.Sum"
    }
}

impl Interp {
    pub fn push_call(&mut self, func: Arc<Func>, arg_list: &[*mut Obj]) {
        let frame = Frame {
            func,
            instr_id: InstrId::default(),
            val_table: arg_list
                .iter()
                .enumerate()
                .map(|(i, arg)| (Val::Arg(i), *arg))
                .collect(),
        };
        self.frame_list.push(frame);
    }

    pub fn step<O: Op>(
        &mut self,
        mem: Mutator<'_>,
        loader: &Loader,
        external: &mut O::Worker,
        worker: &WorkerInterface,
    ) {
        assert!(self.result.is_none());
        let frame = self.frame_list.last_mut().unwrap();

        let instr = match frame.func.get_instr(&frame.instr_id).clone() {
            Instr::Alloc => Instr::NewProd("intrinsic.Ref".to_string()),
            Instr::Load(val) => Instr::Get(val, "content".to_string()),
            Instr::Store(place_val, val) => Instr::Set(place_val, "content".to_string(), val),
            instr @ _ => instr,
        };

        let mut context = OpContext {
            external,
            frame,
            mem,
            loader,
            worker,
        };
        match instr {
            Instr::Ret(val) => {
                drop(context);
                let ret = frame.val_table[&val];
                self.frame_list.pop().unwrap();
                if let Some(frame) = self.frame_list.last_mut() {
                    Self::finish_step(frame, Some(ret));
                } else {
                    self.result = Some(ret);
                }
            }
            Instr::Call(func_id, arg_list) => {
                let arg_list: Vec<_> = arg_list
                    .into_iter()
                    .map(|(val, morph)| (context.make_addr(val), morph))
                    .collect();
                let mem = context.into_mem();
                let (val_list, as_list): (Vec<_>, Vec<_>) = arg_list
                    .into_iter()
                    .map(|(val, morph)| {
                        let morph = if morph.is_empty() {
                            vec![unsafe { Self::get_tag(val, &mem, loader) }]
                        } else {
                            morph
                                .iter()
                                .map(|tag| loader.query_tag(tag))
                                .collect::<Vec<_>>()
                        };
                        (val, morph)
                    })
                    .unzip();
                drop(mem);
                let func = loader.dispatch_call(
                    &func_id,
                    &*as_list.iter().map(|val_as| &**val_as).collect::<Vec<_>>(),
                );
                self.push_call(func, &val_list);
            }
            Instr::Op(id, val) => {
                let res = O::perform(&id, &val, &mut context);
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::Br(val, if_true, if_false) => {
                let val = unsafe { context.get_bool(val) };
                drop(context);
                frame.instr_id = (if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::NewProd(name) => {
                let header = loader.query_tag(&name);
                let prod = context.mem.alloc(Prod {
                    tag: header,
                    data: vec![
                        context.make_addr(Val::Const(ValConst::Unit));
                        loader.prod_size(header)
                    ],
                });
                drop(context);
                Self::finish_step(frame, Some(prod));
            }
            Instr::NewSum(name, variant, inner) => {
                let header = loader.query_tag(&name);
                let sum = context.mem.alloc(Sum {
                    tag: header,
                    variant: loader.sum_variant(header, &variant),
                    inner: context.make_addr(inner),
                });
                drop(context);
                Self::finish_step(frame, Some(sum));
            }
            Instr::Get(prod, key) => {
                let prod = unsafe { context.mem.read(context.make_addr(prod)) };
                let prod: &Prod = prod.downcast_ref().unwrap();
                let res = prod.data[loader.prod_offset(prod.tag, &key)];
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::Set(prod, key, val) => {
                let mut prod = unsafe { context.mem.write(context.make_addr(prod)) };
                let prod: &mut Prod = prod.downcast_mut().unwrap();
                prod.data[loader.prod_offset(prod.tag, &key)] = context.make_addr(val);
                drop(context);
                Self::finish_step(frame, None);
            }
            Instr::Is(sum, key) => {
                let sum = unsafe { context.mem.read(context.make_addr(sum)) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                let res = sum.variant == loader.sum_variant(sum.tag, &key);
                let res = context.make_addr(Val::Const(ValConst::Bool(res)));
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::As(sum, key) => {
                let sum = unsafe { context.mem.read(context.make_addr(sum)) };
                let sum: &Sum = sum.downcast_ref().unwrap();
                // consider a unchecked version
                assert_eq!(sum.variant, loader.sum_variant(sum.tag, &key));
                let res = sum.inner;
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            _ => unreachable!(),
        }
    }

    unsafe fn get_tag(addr: *mut Obj, mem: &Mutator<'_>, loader: &Loader) -> TagId {
        let obj = mem.read(addr);
        match obj.name() {
            "intrinsic.Prod" => {
                let Prod { tag, .. } = obj.downcast_ref().unwrap();
                *tag
            }
            "intrinsic.Sum" => {
                let Sum { tag, .. } = obj.downcast_ref().unwrap();
                *tag
            }
            name @ _ => loader.query_tag(name),
        }
    }

    fn finish_step(frame: &mut Frame, res: Option<*mut Obj>) {
        if let Some(res) = res {
            frame.val_table.insert(Val::Instr(frame.instr_id), res);
        }
        frame.instr_id.1 += 1;
    }

    pub fn resume(&mut self, result: *mut Obj) {
        assert!(self.result.is_none());
        Self::finish_step(self.frame_list.last_mut().unwrap(), Some(result));
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

pub struct OpContext<'a, W> {
    pub external: &'a mut W,
    pub mem: Mutator<'a>,
    pub loader: &'a Loader,
    pub worker: &'a WorkerInterface,
    frame: &'a Frame,
}

impl<W> OpContext<'_, W> {
    /// # Safety
    /// If `val` is not constant, it's frame record must be a valid allocation.
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj = self.mem.read(self.make_addr(val));
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
                let obj = self.mem.read(self.make_addr(val));
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
                let obj = self.mem.read(self.make_addr(val));
                let Prod { tag, .. } = obj.downcast_ref().unwrap();
                assert_eq!(*tag, 0);
            }
            _ => unreachable!(),
        }
    }

    pub fn make_addr(&self, val: Val) -> *mut Obj {
        if let Val::Const(val_const) = val {
            match val_const {
                ValConst::I32(content) => self.mem.alloc(I32(content)),
                ValConst::Unit => self.mem.alloc(Prod {
                    tag: 0,
                    data: Vec::new(),
                }),
                ValConst::Bool(content) => self.mem.alloc(Sum {
                    tag: 2,
                    variant: if content { 0 } else { 1 },
                    inner: self.make_addr(Val::Const(ValConst::Unit)),
                }),
                ValConst::Asset(id) => self.loader.query_asset(id),
            }
        } else {
            self.frame.val_table[&val]
        }
    }
}

impl<'a, W> OpContext<'a, W> {
    fn into_mem(self) -> Mutator<'a> {
        self.mem
    }
}
