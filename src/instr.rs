use std::{collections::HashMap, fmt::Display};

use crate::{AssetId, OwnedName};

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
// i32 and unit are tentatively kept, may be useful for future middle-level
// optimization
// anyway they are so universal that every pratical env need them
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValConst {
    // represented as Prod{tag=0}
    Unit,
    // represented as Sum{tag=2}, reusing Some(Unit) as True, and None as False
    Bool(bool),
    // represented as interp::I32
    I32(i32),
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
    Call(InstrCall),
    Ret(Val),
    Br(Val, LabelId, LabelId),
    Phi(HashMap<LabelId, Val>),
    // task flow
    Spawn(InstrCall), // pause is in op level
    Wait(Val),

    Op(String, Vec<Val>),
    CoreOp(CoreOp),

    // keep in explicit top level because they are optimizible with phi
    Alloc,
    Load(Val),
    Store(Val, Val),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoreOp {
    NewProd(OwnedName),
    NewSum(OwnedName, String, Val),
    Get(Val, String),
    Set(Val, String, Val),
    Is(Val, String),
    As(Val, String),
    I32Add(Val, Val),
    I32Neg(Val),
    I32Mul(Val, Val),
    I32Div(Val, Val),
    I32Eq(Val, Val),
    I32Lt(Val, Val),
    I32And(Val, Val),
    I32Inv(Val),
    I32Shl(Val, Val),
    I32Shr(Val, Val),
    BoolNeg(Val),
    BoolAnd(Val, Val),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstrCall {
    pub name: OwnedName,
    pub arg_list: Vec<(Val, Option<ArgMorph>)>,
}
pub type ArgMorph = Vec<OwnedName>;

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ret(val) => write!(f, "ret {val}"),
            Self::Br(val, if_true, if_false) => write!(f, "br {val}, l{if_true}, l{if_false}"),
            Self::Phi(..) => write!(f, "phi"), // TODO
            Self::Op(id, val_list) => write!(f, "op {id}<{}>", fmt_val_list(val_list)),
            Self::CoreOp(op) => write!(f, "coreop {op}"),
            Self::Call(call) => write!(f, "call {call}"),
            Self::Spawn(call) => write!(f, "spawn {call}"),
            Self::Wait(val) => write!(f, "wait {val}"),
            Self::Alloc => write!(f, "alloc"),
            Self::Load(val) => write!(f, "load {val}"),
            Self::Store(place_val, val) => write!(f, "store {place_val} <- {val}"),
        }
    }
}
fn fmt_val_list(val_list: &[Val]) -> String {
    val_list
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}
impl Display for InstrCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arg_list = self
            .arg_list
            .iter()
            .map(|(val, morph)| {
                format!(
                    "{val}{}",
                    if let Some(morph) = morph {
                        " as ".to_string() + &morph.join(" & ")
                    } else {
                        String::new()
                    }
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}({arg_list})", self.name)
    }
}
impl Display for CoreOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NewProd(name) => write!(f, "newprod {name}"),
            Self::NewSum(name, variant, inner) => write!(f, "newsum {name}.{variant} {inner}"),
            Self::Get(prod, key) => write!(f, "get {prod} .{key}"),
            Self::Set(prod, key, val) => write!(f, "set {prod} .{key} <- {val}"),
            Self::Is(sum, variant) => write!(f, "is {sum} .{variant}"),
            Self::As(sum, variant) => write!(f, "as {sum} .{variant}"),
            Self::I32Add(i1, i2) => write!(f, "i32add {i1} {i2}"),
            Self::I32Neg(i) => write!(f, "i32neg {i}"),
            Self::I32Mul(i1, i2) => write!(f, "i32mul {i1} {i2}"),
            Self::I32Div(i1, i2) => write!(f, "i32div {i1} {i2}"),
            Self::I32Eq(i1, i2) => write!(f, "i32eq {i1} {i2}"),
            Self::I32Lt(i1, i2) => write!(f, "i32lt {i1} {i2}"),
            Self::I32And(i1, i2) => write!(f, "i32and {i1} {i2}"),
            Self::I32Inv(i) => write!(f, "i32inv {i}"),
            Self::I32Shl(i1, i2) => write!(f, "i32shl {i1} {i2}"),
            Self::I32Shr(i1, i2) => write!(f, "i32shr {i1} {i2}"),
            Self::BoolNeg(b) => write!(f, "boolneg {b}"),
            Self::BoolAnd(b1, b2) => write!(f, "booland {b1} {b2}"),
        }
    }
}

pub struct Func {
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
    block_list: Vec<Vec<Instr>>,
    block_index: usize,
}

impl Default for FuncBuilder {
    fn default() -> Self {
        Self {
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
            block_list: self.block_list,
        }
    }
}
