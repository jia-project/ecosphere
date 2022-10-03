use std::{collections::HashMap, mem::replace};

use crate::{Entity, EvalOp, Expr, Loader, Mem, Stmt};

pub struct Context<'a, T, Expr> {
    op_context: &'a mut T,
    loader: &'a Loader<Expr>,
    mem: &'a mut Mem,
    frames: Vec<ContextFrame>,
    //
}

struct ContextFrame {
    scopes: Vec<HashMap<String, *mut Entity>>,
    control: FrameControl,
}

#[derive(PartialEq, Eq)]
enum FrameControl {
    Run,
    Break,
    Continue,
    Return(*mut Entity),
}

impl<'a, O, T> Context<'a, T, Expr<O>>
where
    O: std::fmt::Debug,
    T: EvalOp<O>,
{
    pub fn new(mem: &'a mut Mem, loader: &'a Loader<Expr<O>>, op_context: &'a mut T) -> Self {
        Self {
            mem,
            loader,
            op_context,
            frames: Vec::new(),
        }
    }

    pub fn eval_call(&mut self, name: &str, args: &[*mut Entity]) -> *mut Entity {
        let arg_types = args
            .iter()
            .map(|&arg| unsafe { &*arg }.core.ty())
            .collect::<Vec<_>>();
        let (names, expr) = self.loader.dispatch(name, &arg_types).unwrap_or_else(|| {
            panic!(
                "no dispatch for {name}({:?})",
                arg_types
                    .iter()
                    .map(|&ty| self.loader.type_repr(ty))
                    .collect::<Vec<_>>()
            )
        });
        let scope = names
            .iter()
            .zip(args)
            .map(|(name, entity)| (name.clone(), *entity))
            .collect();
        self.frames.push(ContextFrame {
            scopes: vec![scope],
            control: FrameControl::Run,
        });
        let entity = self.eval_expr(expr);
        if let FrameControl::Return(entity) = self.frames.pop().unwrap().control {
            entity
        } else {
            entity
        }
    }

    fn eval_expr(&mut self, expr: &Expr<O>) -> *mut Entity {
        match expr {
            Expr::Name(name) => {
                for scope in self.frames.last_mut().unwrap().scopes.iter().rev() {
                    if let Some(&slot) = scope.get(name) {
                        return slot;
                    }
                }
                panic!("name not found: {name}");
            }
            Expr::Scope(scope) => {
                for stmt in scope {
                    if self.frames.last().unwrap().control != FrameControl::Run {
                        break;
                    }
                    self.eval_stmt(stmt);
                }
                self.op_context.alloc_nil(self.mem)
            }
            Expr::Op(op, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Vec<_>>();
                unsafe { self.op_context.eval(op, &args, self.mem, self.loader) }
            }
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Vec<_>>();
                self.eval_call(name, &args)
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<O>) {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr);
            }
            Stmt::AllocName(name) => {
                let nil = self.op_context.alloc_nil(self.mem);
                self.frames
                    .last_mut()
                    .unwrap()
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), nil);
            }
            Stmt::Mutate(name, expr) => {
                let entity = self.eval_expr(expr);
                let mut mutated = false;
                for scope in self.frames.last_mut().unwrap().scopes.iter_mut().rev() {
                    if let Some(slot) = scope.get_mut(name) {
                        *slot = entity;
                        mutated = true;
                        break;
                    }
                }
                assert!(mutated);
            }
            Stmt::IfElse(expr, then_expr, else_expr) => {
                let entity = self.eval_expr(expr);
                if unsafe { self.op_context.is_truthy(entity) } {
                    self.eval_expr(then_expr);
                } else {
                    self.eval_expr(else_expr);
                }
            }
            Stmt::While(expr, then_expr) => {
                while {
                    let entity = self.eval_expr(expr);
                    unsafe { self.op_context.is_truthy(entity) }
                } {
                    self.eval_expr(then_expr);
                    match replace(
                        &mut self.frames.last_mut().unwrap().control,
                        FrameControl::Run,
                    ) {
                        FrameControl::Run => {}
                        FrameControl::Break => break,
                        FrameControl::Continue => continue,
                        control @ FrameControl::Return(..) => {
                            self.frames.last_mut().unwrap().control = control;
                            return;
                        }
                    }
                }
            }
            Stmt::Break => self.frames.last_mut().unwrap().control = FrameControl::Break,
            Stmt::Continue => self.frames.last_mut().unwrap().control = FrameControl::Continue,
            Stmt::Return(expr) => {
                let entity = self.eval_expr(expr);
                self.frames.last_mut().unwrap().control = FrameControl::Return(entity);
            }
        }
    }
}
