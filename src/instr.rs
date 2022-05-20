use std::{collections::HashMap, fmt::Display};

use crate::def::{FuncId, OpId, ResourceId};

pub type LabelId = usize;
pub type InstrId = (LabelId, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Val {
    Instr(InstrId),
    Arg(usize),
    Const(ValConst),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValConst {
    I32(i32),
    Bool(bool),
    Unit,
    Resource(ResourceId),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Instr((label, id)) => write!(f, "i{label}:{id}"),
            Self::Arg(index) => write!(f, "arg{index}"),
            Self::Const(content) => write!(f, "const {content:?}"),
        }
    }
}

pub enum Instr {
    Ret(Val),
    Br(Val, LabelId, LabelId),
    Phi(HashMap<LabelId, Val>),
    Op(OpId, Vec<Val>),
    Call(FuncId, Vec<Val>),
    Alloc(Val),
    Load(Val),
    Store(Val, Val),
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ret(val) => write!(f, "ret {val}"),
            Self::Br(val, if_true, if_false) => write!(f, "br {val}, l{if_true}, l{if_false}"),
            Self::Phi(..) => write!(f, "phi"), // TODO
            Self::Op(id, val_list) => write!(
                f,
                "op {id}<{}>",
                val_list
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Call(id, val_list) => write!(
                f,
                "call {id}({})",
                val_list
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Alloc(val) => write!(f, "alloc <- {val}"),
            Self::Load(val) => write!(f, "load {val}"),
            Self::Store(place_val, val) => write!(f, "store {place_val} <- {val}"),
        }
    }
}

pub struct Func {
    param_list: Vec<()>,
    block_list: Vec<Vec<Instr>>,
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO param
        for (block_i, block) in self.block_list.iter().enumerate() {
            writeln!(f, "l{block_i}:")?;
            for (i, instr) in block.iter().enumerate() {
                writeln!(f, "  i{block_i}:{i} = {instr}",)?;
            }
        }
        Ok(())
    }
}

pub struct FuncBuilder {
    param_list: Vec<()>,
    block_list: Vec<Vec<Instr>>,
    block_index: usize,
}

impl Default for FuncBuilder {
    fn default() -> Self {
        Self {
            param_list: Default::default(),
            block_list: vec![Vec::new()],
            block_index: 0,
        }
    }
}

impl FuncBuilder {
    pub fn push_instr(&mut self, instr: Instr) -> Val {
        let id = (self.block_index, self.block_list[self.block_index].len());
        self.block_list[self.block_index].push(instr);
        Val::Instr(id)
    }

    pub fn push_block(&mut self) -> LabelId {
        let id = self.block_list.len();
        self.block_list.push(Vec::new());
        id
    }

    pub fn with_block(&mut self, label: LabelId) {
        assert!(label < self.block_list.len());
        self.block_index = label;
    }

    pub fn finish(self) -> Func {
        Func {
            param_list: self.param_list,
            block_list: self.block_list,
        }
    }
}
