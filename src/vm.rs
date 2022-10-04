use std::{collections::HashMap, ptr::null_mut};

use crate::{Entity, EvalOp, Instr, InstrId, Loader, Mem};

pub struct Context<'a, T, Op> {
    op_context: &'a mut T,
    loader: &'a Loader<Op>,
    mem: &'a mut Mem,
    frames: Vec<ContextFrame>,
    //
}

struct ContextFrame {
    slots: HashMap<InstrId, *mut Entity>,
    blocks_id: usize,
    instr_id: InstrId,
}

impl<'a, O, T> Context<'a, T, O>
where
    O: std::fmt::Debug,
    T: EvalOp<O>,
{
    pub fn new(mem: &'a mut Mem, loader: &'a Loader<O>, op_context: &'a mut T) -> Self {
        Self {
            mem,
            loader,
            op_context,
            frames: Vec::new(),
        }
    }

    pub fn eval_call(&mut self, name: &str, args: &[InstrId]) {
        let args = args
            .iter()
            .map(|arg| self.frames.last().unwrap().slots[arg])
            .collect::<Vec<_>>();
        let arg_types = args
            .iter()
            .map(|&arg| unsafe { &*arg }.core.ty())
            .collect::<Vec<_>>();
        let blocks_id = self.loader.dispatch(name, &arg_types).unwrap_or_else(|| {
            panic!(
                "no dispatch for {name}({:?})",
                arg_types
                    .iter()
                    .map(|&ty| self.loader.type_repr(ty))
                    .collect::<Vec<_>>()
            )
        });
        let slots = args
            .iter()
            .enumerate()
            .map(|(i, &arg)| ((0, i), arg))
            .collect();
        self.frames.push(ContextFrame {
            slots,
            blocks_id,
            instr_id: (1, 0),
        });
    }

    fn eval_op(&mut self, op: &O, args: &[InstrId]) {
        let args = args
            .iter()
            .map(|arg| self.frames.last().unwrap().slots[arg])
            .collect::<Vec<_>>();
        let entity = unsafe { self.op_context.eval(op, &args, self.mem, self.loader) };
        let frame = self.frames.last_mut().unwrap();
        assert_ne!(entity, null_mut());
        frame.slots.insert(frame.instr_id, entity);
        frame.instr_id.1 += 1;
    }

    fn eval_return(&mut self, instr: InstrId) {
        let entity = self.frames.last().unwrap().slots[&instr];
        self.frames.pop();
        if let Some(frame) = self.frames.last_mut() {
            frame.slots.insert(frame.instr_id, entity);
            frame.instr_id.1 += 1;
        }
    }

    fn eval_branch(&mut self, instr: InstrId, then_block: usize, else_block: usize) {
        let frame = self.frames.last_mut().unwrap();
        if then_block == else_block || unsafe { self.op_context.is_truthy(frame.slots[&instr]) } {
            frame.instr_id = (then_block, 0);
        } else {
            frame.instr_id = (else_block, 0);
        }
    }

    pub fn eval_instr(&mut self) -> bool {
        let (blocks_id, instr_id) = {
            let frame = self.frames.last().unwrap();
            (frame.blocks_id, frame.instr_id)
        };
        let instr = self.loader.fetch(blocks_id, instr_id);
        match instr {
            Instr::Op(op, args) => self.eval_op(op, args),
            Instr::Call(name, args) => self.eval_call(name, args),
            &Instr::Return(instr) => self.eval_return(instr),
            &Instr::Branch(instr, then_block, else_block) => {
                self.eval_branch(instr, then_block, else_block)
            }
        }
        !self.frames.is_empty()
    }
}
