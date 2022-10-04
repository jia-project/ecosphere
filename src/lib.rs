#![warn(unsafe_op_in_unsafe_fn)]

use std::{
    any::Any,
    collections::{HashMap, HashSet},
    fmt::Debug,
    ptr::null_mut,
};

pub struct Entity {
    linked: *mut Entity,
    core: Box<dyn EntityModel>,
}

pub trait EntityModel: EntityCast {
    fn ty(&self) -> u32;
    fn mark(&self, marker: &mut MemMarker<'_>);
}

pub trait EntityCast {
    fn any_ref(&self) -> &dyn Any;
    fn any_mut(&mut self) -> &mut dyn Any;
}
impl<T: Any> EntityCast for T {
    fn any_ref(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub struct Mem {
    linked: *mut Entity,
}

impl Default for Mem {
    fn default() -> Self {
        Self { linked: null_mut() }
    }
}

pub struct MemMarker<'a> {
    black_set: &'a HashSet<*const Entity>,
    gray_set: &'a mut HashSet<*const Entity>,
}
impl MemMarker<'_> {
    pub fn mark(&mut self, entity: *const Entity) {
        if !self.black_set.contains(&entity) {
            self.gray_set.insert(entity);
        }
    }
}

impl Mem {
    pub fn alloc(&mut self, raw: impl EntityModel + 'static) -> *mut Entity {
        let entity = Entity {
            linked: self.linked,
            core: Box::new(raw),
        };
        self.linked = Box::leak(Box::new(entity));
        self.linked
    }

    pub unsafe fn sweep<E>(&self, root: *const Entity) {
        todo!()
    }
}

pub type InstrId = (usize, usize);
#[derive(Debug, Clone)]
pub enum Instr<Op> {
    Op(Op, Vec<InstrId>),
    Call(String, Vec<InstrId>),
    Return(InstrId),
    Branch(InstrId, usize, usize),
}

impl<O> Instr<O> {
    pub fn goto(block_id: usize) -> Self {
        Self::Branch(Default::default(), block_id, block_id)
    }
}

pub unsafe trait EvalOp<Op> {
    unsafe fn eval(
        &mut self,
        op: &Op,
        args: &[*mut Entity],
        mem: &mut Mem,
        loader: &Loader<Op>,
    ) -> *mut Entity;
    unsafe fn is_truthy(&self, entity: *mut Entity) -> bool;
}

pub struct Loader<Op> {
    fn_defs: Vec<FnDef<Op>>,
    fn_blocks: Vec<InstrBlocks<Op>>,
    dispatch: HashMap<DispatchKey, usize>,
    type_names: HashMap<String, u32>,
    type_reprs: Vec<String>,
}
pub type InstrBlocks<Op> = Vec<Vec<Instr<Op>>>;

impl<E> Default for Loader<E> {
    fn default() -> Self {
        Self {
            fn_defs: Default::default(),
            fn_blocks: Default::default(),
            dispatch: Default::default(),
            type_names: Default::default(),
            type_reprs: Default::default(),
        }
    }
}

// function name, parameter types, number of untyped parameters
// vararg?
pub type DispatchKey = (String, Vec<u32>, usize);
// parameter names, function expression
pub type DispatchValue<E> = (Vec<String>, E);

#[derive(Debug, Clone)]
pub struct FnDef<Op> {
    pub name: String,
    pub param_names: Vec<String>,
    pub param_types: Vec<String>,
    pub untyped_len: usize,
    pub blocks: Vec<Vec<Instr<Op>>>,
}

impl<O> Loader<O> {
    pub fn register_type(&mut self, mut id: u32, name: String) -> u32 {
        if id == 0 {
            id = (self.type_names.len() + 1) as _;
        }
        assert!(!self.type_names.contains_key(&name));
        self.type_names.insert(name.clone(), id);
        if id as usize >= self.type_reprs.len() {
            self.type_reprs
                .resize(id as usize + 1, String::from("(invalid)"));
        }
        self.type_reprs[id as usize] = name;
        id
    }

    pub fn type_repr(&self, ty: u32) -> &str {
        &self.type_reprs[ty as usize]
    }

    pub fn register_fn(&mut self, fn_def: FnDef<O>) {
        assert_eq!(
            fn_def.param_names.len(),
            fn_def.param_types.len() + fn_def.untyped_len
        );
        self.fn_defs.push(fn_def);
    }
}

impl<O> Loader<O> {
    pub fn register_op(&mut self, name: String, param_types: Vec<u32>, untyped_len: usize, op: O) {
        let args_len = param_types.len() + untyped_len;
        self.dispatch
            .insert((name, param_types, untyped_len), self.fn_blocks.len());
        self.fn_blocks.push(vec![
            vec![],
            vec![
                Instr::Op(op, (0..args_len).map(|i| (0, i)).collect()),
                Instr::Return((1, 0)),
            ],
        ]);
    }
}

impl<O> Loader<O> {
    pub fn populate(&mut self)
    where
        FnDef<O>: Clone,
    {
        for fn_def in self.fn_defs.clone() {
            let param_types = fn_def
                .param_types
                .iter()
                .map(|name| self.type_names[name])
                .collect();
            self.dispatch.insert(
                (fn_def.name, param_types, fn_def.untyped_len),
                self.fn_blocks.len(),
            );
            self.fn_blocks.push(fn_def.blocks);
        }
    }

    pub fn dispatch(&self, name: &str, arg_types: &[u32]) -> Option<usize> {
        for i in (0..=arg_types.len()).rev() {
            if let Some(&blocks_id) = self.dispatch.get(&(
                name.to_string(),
                arg_types[..i].to_vec(),
                arg_types.len() - i,
            )) {
                return Some(blocks_id);
            }
        }
        None
    }

    pub fn fetch(&self, blocks_id: usize, instr_id: InstrId) -> &Instr<O> {
        assert_ne!(instr_id.0, 0);
        &self.fn_blocks[blocks_id][instr_id.0][instr_id.1]
    }

    pub fn dump_blocks(&self)
    where
        O: Debug,
    {
        for (dispatch, &blocks_id) in &self.dispatch {
            println!("{dispatch:?}");
            for (i, block) in self.fn_blocks[blocks_id].iter().enumerate() {
                println!("{i}:");
                for (i, instr) in block.iter().enumerate() {
                    println!("{i:3} {instr:?}");
                }
            }
        }
    }
}

pub mod vanilla {
    pub mod op;
    pub mod spec;
}
pub mod vm;
