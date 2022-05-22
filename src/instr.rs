use std::{collections::HashMap, fmt::Display};

use crate::{AssetId, Name, OpCode};

pub type LabelId = usize;
pub type InstrId = (LabelId, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Val {
    Instr(InstrId),
    Arg(usize),
    Const(ValConst),
}

// design choice: since now we have generic asset support, conceptually ValConst
// may be reduced into single AssetId
// we want to keep explicit const bool for control flow analyze
// i32 and unit are just temporarily kept, no special meaning
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValConst {
    I32(i32),
    Bool(bool),
    Unit,
    Asset(AssetId),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    // control flow
    Call(Name, Vec<Val>),
    Ret(Val),
    Pause,
    Br(Val, LabelId, LabelId),
    Phi(HashMap<LabelId, Val>),

    Op(OpCode, Vec<Val>),

    // writable memory simulation
    Alloc,
    Load(Val),
    Store(Val, Val),

    // compound type operation
    NewProd(Name),
    Get(Val, String),
    Set(Val, String, Val),
    NewSum(Name, String, Val),
    Is(Val, String),
    As(Val, String),
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let join = |val_list: &[Val]| {
            val_list
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        };
        match self {
            Self::Ret(val) => write!(f, "ret {val}"),
            Self::Br(val, if_true, if_false) => write!(f, "br {val}, l{if_true}, l{if_false}"),
            Self::Phi(..) => write!(f, "phi"), // TODO
            Self::Op(id, val_list) => write!(f, "op {id}<{}>", join(val_list)),
            Self::Call(id, val_list) => write!(f, "call {id}({})", join(val_list)),
            Self::Pause => write!(f, "pause"),
            Self::Alloc => write!(f, "alloc"),
            Self::Load(val) => write!(f, "load {val}"),
            Self::Store(place_val, val) => write!(f, "store {place_val} <- {val}"),
            Self::NewProd(name) => write!(f, "newprod {name}"),
            Self::NewSum(name, variant, inner) => write!(f, "newsum {name}.{variant} {inner}"),
            Self::Get(prod, key) => write!(f, "get {prod} .{key}"),
            Self::Set(prod, key, val) => write!(f, "set {prod} .{key} <- {val}"),
            Self::Is(sum, variant) => write!(f, "is {sum} .{variant}"),
            Self::As(sum, variant) => write!(f, "as {sum} .{variant}"),
        }
    }
}

pub struct Func {
    param_list: Vec<()>,
    block_list: Vec<Vec<Instr>>,
    // debug symbol
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

impl Func {
    pub fn get_instr(&self, id: &InstrId) -> &Instr {
        &self.block_list[id.0][id.1]
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
