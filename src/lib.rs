#![warn(unsafe_op_in_unsafe_fn)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    mem::size_of,
    ptr::null_mut,
};

pub struct Entity {
    pub ty: u32,
    linked: *mut Entity,
    pub raw: Vec<u8>,
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
    pub fn alloc(&mut self, ty: u32, raw: Vec<u8>) -> *mut Entity {
        let entity = Entity {
            ty,
            linked: self.linked,
            raw,
        };
        self.linked = Box::leak(Box::new(entity));
        self.linked
    }

    pub unsafe fn sweep<E>(&self, root: *const Entity, loader: &Loader<E>) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<Op> {
    Expr(Expr<Op>),
    AllocName(String),
    Mutate(String, Expr<Op>),
    IfElse(Expr<Op>, Expr<Op>, Expr<Op>),
    While(Expr<Op>, Expr<Op>),
    Break,
    Continue,
    Return(Expr<Op>),
}

#[derive(Debug, Clone)]
pub enum Expr<Op> {
    Name(String),
    Call(String, Vec<Expr<Op>>),
    Op(Op, Vec<Expr<Op>>),
    Scope(Vec<Stmt<Op>>), // TODO add tail expr
}

pub unsafe trait EvalOp<Op> {
    unsafe fn eval(
        &mut self,
        op: &Op,
        args: &[*mut Entity],
        mem: &mut Mem,
        loader: &Loader<Expr<Op>>,
    ) -> *mut Entity;
    fn alloc_nil(&self, mem: &mut Mem) -> *mut Entity;
}

pub fn op_target<O>(op: O, args_len: usize) -> (Vec<String>, Expr<O>) {
    (
        (0..args_len).map(|i| format!("${i}")).collect(),
        Expr::Op(
            op,
            (0..args_len).map(|i| Expr::Name(format!("${i}"))).collect(),
        ),
    )
}

pub struct Loader<Expr> {
    fn_specs: Vec<FnSpec<Expr>>,
    dispatch: HashMap<DispatchKey, DispatchValue<Expr>>,
    types: Vec<TypeMeta>,
    type_names: HashMap<String, u32>,
}

impl<E> Default for Loader<E> {
    fn default() -> Self {
        Self {
            fn_specs: Default::default(),
            dispatch: Default::default(),
            types: Default::default(),
            type_names: Default::default(),
        }
    }
}

// function name, parameter types, number of untyped parameters
// vararg?
pub type DispatchKey = (String, Vec<u32>, usize);
// parameter names, function expression
pub type DispatchValue<E> = (Vec<String>, E);
pub struct TypeMeta {
    pub repr: String,
    pub size: usize,
    pub mark_fn: MarkFn,
}
pub type MarkFn = unsafe fn(&Entity, &mut MemMarker<'_>);
pub fn no_mark(_: &Entity, _: &mut MemMarker<'_>) {}
pub unsafe fn mark_product(entity: &Entity, marker: &mut MemMarker<'_>) {
    for p in entity.raw.chunks_exact(size_of::<usize>()) {
        marker.mark(usize::from_ne_bytes(p.try_into().unwrap()) as _);
    }
}
pub unsafe fn mark_sum(entity: &Entity, marker: &mut MemMarker<'_>) {
    marker.mark(usize::from_ne_bytes(entity.raw[4..].try_into().unwrap()) as _);
}

#[derive(Debug, Clone)]
pub struct FnSpec<Expr> {
    pub name: String,
    pub param_names: Vec<String>,
    pub param_types: Vec<String>,
    pub untyped_len: usize,
    pub expr: Expr,
}

impl<E> Loader<E> {
    pub fn register_type(&mut self, ty: TypeMeta) -> u32 {
        let id = self.types.len() as _;
        self.type_names.insert(ty.repr.clone(), id);
        self.types.push(ty);
        id
    }

    pub fn get_type(&self, ty: u32) -> &TypeMeta {
        &self.types[ty as usize]
    }

    pub fn register_fn(&mut self, fn_spec: FnSpec<E>) {
        assert_eq!(
            fn_spec.param_names.len(),
            fn_spec.param_types.len() + fn_spec.untyped_len
        );
        self.fn_specs.push(fn_spec);
    }
}

impl<O> Loader<Expr<O>> {
    pub fn register_op(&mut self, name: String, param_types: Vec<u32>, untyped_len: usize, op: O) {
        let args_len = param_types.len() + untyped_len;
        self.dispatch.insert(
            (name, param_types, untyped_len),
            (
                (0..args_len).map(|i| format!("${i}")).collect(),
                Expr::Op(
                    op,
                    (0..args_len).map(|i| Expr::Name(format!("${i}"))).collect(),
                ),
            ),
        );
    }
}

impl<E> Loader<E> {
    pub fn populate(&mut self)
    where
        FnSpec<E>: Clone,
    {
        for fn_spec in self.fn_specs.clone() {
            let param_types = fn_spec
                .param_types
                .iter()
                .map(|name| self.type_names[name])
                .collect();
            self.dispatch.insert(
                (fn_spec.name, param_types, fn_spec.untyped_len),
                (fn_spec.param_names, fn_spec.expr),
            );
        }
    }

    pub fn dispatch(&self, name: &str, arg_types: &[u32]) -> Option<(&[String], &E)> {
        for i in (0..=arg_types.len()).rev() {
            if let Some((names, expr)) = self.dispatch.get(&(
                name.to_string(),
                arg_types[..i].to_vec(),
                arg_types.len() - i,
            )) {
                return Some((&**names, expr));
            }
        }
        None
    }

    pub fn try_it(&self)
    where
        E: Debug,
    {
        for (name, dispatch) in &self.dispatch {
            println!("{name:?} => {dispatch:?}");
        }
    }
}

pub mod vanilla {
    pub mod op;
    pub mod spec;
}
pub mod vm;
