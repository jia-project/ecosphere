use std::{collections::HashMap, slice, sync::Arc};

use crate::{
    def::OpId,
    instr::{Func, Instr, InstrId, Val, ValConst},
    loader::Loader,
    mem::{Mem, Mutator, Obj, ObjCore},
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
                } else {
                    self.result = Some(ret);
                }
            }
            Instr::Call(func_id, arg_list) => {
                let context = OpContext {
                    mem: mem.mutator(),
                    frame,
                    loader,
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
                let val = if let Val::Const(ValConst::Bool(content)) = val {
                    *content
                } else {
                    let obj = frame.val_table[val];
                    if let ObjCore::Sum(_, variant, _) = mem.mutator().read(obj) {
                        *variant == 0
                    } else {
                        panic!()
                    }
                };
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
            loader,
        };
        let res = loader.perform_op(id, val, &context);
        drop(context);
        frame.val_table.insert(Val::Instr(frame.instr_id), res);
    }

    pub fn get_result(&self) -> Option<*mut Obj> {
        self.result
    }
}

pub struct OpContext<'a> {
    mem: Mutator<'a>,
    frame: &'a Frame,
    loader: &'a Loader,
}

impl OpContext<'_> {
    /// # Safety
    /// If `val` is not constant, it's frame record must be a valid allocation.
    pub unsafe fn get_i32(&self, val: Val) -> i32 {
        match val {
            Val::Const(ValConst::I32(content)) => content,
            val @ (Val::Arg(..) | Val::Instr(..)) => {
                if let ObjCore::I32(content) = self.read_frame(val) {
                    *content
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    }

    /// # Safety
    /// `val` on frame must be a valid allocation
    pub unsafe fn read_frame(&self, val: Val) -> &ObjCore {
        let obj = self.frame.val_table[&val];
        self.mem.read(obj)
    }

    /// # Safety
    /// `val` on frame must be a valid allocation
    pub unsafe fn write_frame<'a>(&self, val: Val) -> &'a mut ObjCore {
        let obj = self.frame.val_table[&val];
        self.mem.write(obj)
    }

    pub fn alloc(&self, core: ObjCore) -> *mut Obj {
        self.mem.alloc(core)
    }

    pub fn get_addr(&self, val: Val) -> *mut Obj {
        if let Val::Const(val_const) = val {
            let core = match val_const {
                ValConst::I32(content) => ObjCore::I32(content),
                ValConst::Unit => ObjCore::Prod(0, Vec::new()),
                ValConst::Bool(content) => ObjCore::Sum(
                    2,
                    if content { 0 } else { 1 },
                    self.get_addr(Val::Const(ValConst::Unit)),
                ),
                ValConst::Resource(..) => todo!(),
            };
            self.alloc(core)
        } else {
            self.frame.val_table[&val]
        }
    }
}
