use std::{collections::HashMap, iter, sync::Arc};

use crate::{
    instr::{Func, Instr, InstrId, Val, ValConst},
    loader::Loader,
    mem::{Mutator, Obj},
    obj::{Native, Prod, Sum},
    worker::WorkerInterface,
    Op,
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

    /// # Safety
    /// If the stepping instr is `Instr::Op`, it must be safe to perform.
    pub unsafe fn step<O: Op>(
        &mut self,
        mem: Mutator<'_>,
        loader: &Loader,
        external: &mut O::Worker,
        worker: &WorkerInterface,
    ) {
        assert!(self.result.is_none());

        let frame = self.frame_list.last_mut().unwrap();
        let func = frame.func.clone();
        let mut context = OpContext {
            external,
            frame,
            mem,
            loader,
            worker,
        };

        let instr = match func.get_instr(&frame.instr_id).clone() {
            Instr::Alloc => Instr::NewProd("intrinsic.Ref".to_string()),
            Instr::Load(val) => Instr::Get(val, "content".to_string()),
            Instr::Store(place_val, val) => Instr::Set(place_val, "content".to_string(), val),
            instr @ _ => instr,
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
                let arg_list: Vec<_> = arg_list.iter().map(|arg| context.make_addr(*arg)).collect();
                let dispatch_list: Vec<_> =
                    arg_list.iter().map(|arg| context.mem.read(*arg)).collect();
                let func = loader.dispatch_call(&func_id, &dispatch_list);
                drop(context);
                self.push_call(func, &arg_list);
            }
            Instr::Op(id, val) => {
                let res = O::perform(&id, &val, &mut context);
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::Br(val, if_true, if_false) => {
                let val = context.get_bool(val);
                drop(context);
                frame.instr_id = (if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::NewProd(name) => {
                let header = loader.query_type(&name);
                let prod = context.mem.alloc(Prod {
                    header,
                    data: iter::repeat(context.make_addr(Val::Const(ValConst::Unit)))
                        .take(loader.query_header_size(header))
                        .collect(),
                });
                drop(context);
                Self::finish_step(frame, Some(prod));
            }
            Instr::NewSum(name, variant, inner) => {
                let header = loader.query_type(&name);
                let sum = context.mem.alloc(Sum {
                    header,
                    variant: loader.query_header_index(header, &variant),
                    inner: context.make_addr(inner),
                });
                drop(context);
                Self::finish_step(frame, Some(sum));
            }
            Instr::Get(prod, key) => {
                let prod: &Prod = context
                    .mem
                    .read(context.make_addr(prod))
                    .downcast_ref()
                    .unwrap();
                let res = prod.data[loader.query_header_index(prod.header, &key) as usize];
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::Set(prod, key, val) => {
                let prod: &mut Prod = context
                    .mem
                    .write(context.make_addr(prod))
                    .downcast_mut()
                    .unwrap();
                prod.data[loader.query_header_index(prod.header, &key) as usize] =
                    context.make_addr(val);
                drop(context);
                Self::finish_step(frame, None);
            }
            Instr::Is(sum, key) => {
                let sum: &Sum = context
                    .mem
                    .read(context.make_addr(sum))
                    .downcast_ref()
                    .unwrap();
                let res = sum.variant == loader.query_header_index(sum.header, &key);
                let res = context.make_addr(Val::Const(ValConst::Bool(res)));
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            Instr::As(sum, key) => {
                let sum: &Sum = context
                    .mem
                    .read(context.make_addr(sum))
                    .downcast_ref()
                    .unwrap();
                // consider a unchecked version
                assert_eq!(sum.variant, loader.query_header_index(sum.header, &key));
                let res = sum.inner;
                drop(context);
                Self::finish_step(frame, Some(res));
            }
            _ => unreachable!(),
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
    /// Thread safety is totally unchecked.
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj: &Native<i32> = self.mem.read(self.make_addr(val)).downcast_ref().unwrap();
                obj.0
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
                let obj: &Sum = self.mem.read(self.make_addr(val)).downcast_ref().unwrap();
                obj.variant == 0
            }
            _ => unreachable!(),
        }
    }

    pub fn make_addr(&self, val: Val) -> *mut Obj {
        if let Val::Const(val_const) = val {
            match val_const {
                ValConst::I32(content) => self.mem.alloc(Native(content)),
                ValConst::Unit => self.mem.alloc(Prod {
                    header: 0,
                    data: Vec::new(),
                }),
                ValConst::Bool(content) => self.mem.alloc(Sum {
                    header: 2,
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
