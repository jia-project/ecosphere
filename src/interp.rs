use std::{collections::HashMap, slice, sync::Arc};

use crate::{
    def::OpId,
    instr::{Func, Instr, InstrId, Val, ValConst},
    loader::Loader,
    mem::{Mem, Mutator, Obj, ObjCore},
    obj::{Native, Prod, Sum},
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
    /// # Safety
    /// The underlying `Instr::Op` must be safe.
    pub unsafe fn step(&mut self, mem: &Mem, loader: &Loader) {
        let frame = self.frame_list.last_mut().unwrap();
        match &frame.func.get_instr(&frame.instr_id).clone() {
            Instr::Ret(val) => {
                let ret = frame.val_table[val];
                self.frame_list.pop().unwrap();
                if let Some(frame) = self.frame_list.last_mut() {
                    frame.val_table.insert(Val::Instr(frame.instr_id), ret);
                    frame.instr_id.1 += 1;
                } else {
                    self.result = Some(ret);
                }
            }
            Instr::Call(func_id, arg_list) => {
                let context = OpContext {
                    mem: mem.mutator(),
                    frame,
                };
                let frame = Frame {
                    func: loader.dispatch_call(func_id, arg_list),
                    instr_id: InstrId::default(),
                    val_table: arg_list
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (Val::Arg(i), context.get_addr(*arg)))
                        .collect(),
                };
                drop(context);
                self.frame_list.push(frame);
            }
            Instr::Op(id, val) => {
                Self::step_op(id, val, mem, frame, loader);
            }
            Instr::Br(val, if_true, if_false) => {
                let context = OpContext {
                    mem: mem.mutator(),
                    frame,
                };
                let val = context.get_bool(*val);
                drop(context);
                frame.instr_id = (*if val { if_true } else { if_false }, 0);
            }
            Instr::Phi(..) => todo!(),
            Instr::Alloc(initial) => {
                Self::step_op(
                    &"intrinsic.alloc".to_string(),
                    slice::from_ref(initial),
                    mem,
                    frame,
                    loader,
                );
            }
            Instr::Load(val) => Self::step_op(
                &"intrinsic.load".to_string(),
                slice::from_ref(val),
                mem,
                frame,
                loader,
            ),
            Instr::Store(place_val, val) => Self::step_op(
                &"intrinsic.store".to_string(),
                &[*place_val, *val],
                mem,
                frame,
                loader,
            ),
        }
    }

    unsafe fn step_op(id: &OpId, val: &[Val], mem: &Mem, frame: &mut Frame, loader: &Loader) {
        let context = OpContext {
            mem: mem.mutator(),
            frame,
        };
        let res = loader.perform_op(id, val, &context);
        drop(context);
        frame.val_table.insert(Val::Instr(frame.instr_id), res);
        frame.instr_id.1 += 1;
    }

    pub fn get_result(&self) -> Option<*mut Obj> {
        self.result
    }
}

pub struct OpContext<'a> {
    mem: Mutator<'a>,
    frame: &'a Frame,
}

impl OpContext<'_> {
    /// # Safety
    /// If `val` is not constant, it's frame record must be a valid allocation.
    /// Thread safety is totally unchecked.
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj: &Native<i32> = self.read_frame(val).any_ref().downcast_ref().unwrap();
                obj.0
            }
            _ => panic!(),
        }
    }

    /// # Safety
    /// Same as `get_i32`.
    pub unsafe fn get_bool(&self, val: Val) -> bool {
        match val {
            Val::Const(ValConst::Bool(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                let obj: &Sum = self.read_frame(val).any_ref().downcast_ref().unwrap();
                obj.variant == 0
            }
            _ => panic!(),
        }
    }

    /// # Safety
    /// Same as `get_i32`.
    pub unsafe fn read_frame(&self, val: Val) -> &dyn ObjCore {
        let obj = self.frame.val_table[&val];
        self.mem.read(obj)
    }

    /// # Safety
    /// Same as `get_i32`.
    pub unsafe fn write_frame<'a>(&self, val: Val) -> &'a mut dyn ObjCore {
        let obj = self.frame.val_table[&val];
        self.mem.write(obj)
    }

    pub fn alloc(&self, core: impl ObjCore + 'static) -> *mut Obj {
        self.mem.alloc(core)
    }

    pub fn get_addr(&self, val: Val) -> *mut Obj {
        if let Val::Const(val_const) = val {
            match val_const {
                ValConst::I32(content) => self.alloc(Native(content)),
                ValConst::Unit => self.alloc(Prod {
                    header: 0,
                    data: Vec::new(),
                }),
                ValConst::Bool(content) => self.alloc(Sum {
                    header: 2,
                    variant: if content { 0 } else { 1 },
                    inner: self.get_addr(Val::Const(ValConst::Unit)),
                }),
                ValConst::Resource(..) => todo!(),
            }
        } else {
            self.frame.val_table[&val]
        }
    }
}
