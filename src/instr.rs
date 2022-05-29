use std::{collections::HashMap, fmt::Display};

use crate::{AssetId, ObjCore, OwnedName};

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
    // represented as Sum{tag=2}
    Bool(bool),
    // represented as I32 below
    // should/can we move I32 op here as well?
    I32(i32),
    Asset(AssetId),
}

pub struct I32(pub i32);
impl I32 {
    pub const NAME: &'static str = "instrinsic.I32";
}
unsafe impl ObjCore for I32 {
    fn name(&self) -> &str {
        Self::NAME
    }
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

    // optimizible with phi
    Alloc,
    Load(Val),
    Store(Val, Val),

    // compound type operation
    NewProd(OwnedName),
    Get(Val, String),
    Set(Val, String, Val),
    NewSum(OwnedName, String, Val),
    Is(Val, String),
    As(Val, String),
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
            Self::Call(call) => write!(f, "call {call}"),
            Self::Spawn(call) => write!(f, "spawn {call}"),
            Self::Wait(val) => write!(f, "wait {val}"),
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
fn fmt_val_list(val_list: &[Val]) -> String {
    val_list
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}
impl Display for InstrCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.arg_list
                .iter()
                .map(|(val, morph)| format!(
                    "{val}{}",
                    morph
                        .as_ref()
                        .map(|morph| morph
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(" & "))
                        .unwrap_or("".to_string())
                ))
                .collect::<Vec<_>>()
                .join(", ")
        )
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
