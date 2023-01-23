use std::collections::{HashMap, HashSet};

use petgraph::{graph::NodeIndex, prelude::DiGraph, visit::EdgeRef, Direction::Incoming};

use crate::{Instruction, Module, RegisterIndex};

pub fn optimize(module: &Module, arity: usize) -> Module {
    let construction = Construction::new(module, arity);
    module.clone()
}

impl Instruction {
    fn define(&self) -> Option<RegisterIndex> {
        let mut define = None;
        self.clone().rewrite(|r, place| {
            if matches!(place, RewriteFor::Define) {
                assert!(define.is_none());
                define = Some(r);
            }
            RegisterIndex::MAX
        });
        define
    }

    fn use_registers(&self) -> HashSet<RegisterIndex> {
        let mut use_registers = HashSet::new();
        self.clone().rewrite(|r, place| {
            if matches!(place, RewriteFor::Use) {
                use_registers.insert(r);
            }
            RegisterIndex::MAX
        });
        use_registers
    }

    fn rewrite(
        self,
        mut rewrite_value: impl FnMut(RegisterIndex, RewriteFor) -> RegisterIndex,
    ) -> Self {
        use Instruction::*;
        match self {
            ParsePlaceholder(..) => unreachable!(),
            Jump(x, positive, negative) => {
                Jump(rewrite_value(x, RewriteFor::Use), positive, negative)
            }
            Phi(i, sources) => {
                let sources = sources
                    .to_vec()
                    .into_iter()
                    .map(|x| rewrite_value(x, RewriteFor::Use))
                    .collect();
                Phi(rewrite_value(i, RewriteFor::Define), sources)
            }
            Call(i, context_arguments, name, arguments) => {
                let context_arguments = context_arguments
                    .into_vec()
                    .into_iter()
                    .map(|x| rewrite_value(x, RewriteFor::Use))
                    .collect();
                let arguments = arguments
                    .into_vec()
                    .into_iter()
                    .map(|x| rewrite_value(x, RewriteFor::Use))
                    .collect();
                Call(
                    rewrite_value(i, RewriteFor::Define),
                    context_arguments,
                    name,
                    arguments,
                )
            }
            Return(x) => Return(rewrite_value(x, RewriteFor::Use)),
            MakeLiteralObject(i, literal) => {
                MakeLiteralObject(rewrite_value(i, RewriteFor::Define), literal)
            }
            MakeTypedObject(i, name, items) => {
                let items = items
                    .into_vec()
                    .into_iter()
                    .map(|(name, x)| (name, rewrite_value(x, RewriteFor::Use)))
                    .collect();
                MakeTypedObject(rewrite_value(i, RewriteFor::Define), name, items)
            }
            Load(i, name) => Load(rewrite_value(i, RewriteFor::Define), name),
            Store(x, name) => Store(rewrite_value(x, RewriteFor::Use), name),
            Get(i, x, name) => Get(
                rewrite_value(i, RewriteFor::Define),
                rewrite_value(x, RewriteFor::Use),
                name,
            ),
            Put(x, name, y) => Put(
                rewrite_value(x, RewriteFor::Use),
                name,
                rewrite_value(y, RewriteFor::Use),
            ),
            Is(i, x, name) => Is(
                rewrite_value(i, RewriteFor::Define),
                rewrite_value(x, RewriteFor::Use),
                name,
            ),
            As(i, x, name) => As(
                rewrite_value(i, RewriteFor::Define),
                rewrite_value(x, RewriteFor::Use),
                name,
            ),
            Operator1(i, op, x) => Operator1(
                rewrite_value(i, RewriteFor::Define),
                op,
                rewrite_value(x, RewriteFor::Use),
            ),
            Operator2(i, op, x, y) => Operator2(
                rewrite_value(i, RewriteFor::Define),
                op,
                rewrite_value(x, RewriteFor::Use),
                rewrite_value(y, RewriteFor::Use),
            ),
            Inspect(x, source) => Inspect(rewrite_value(x, RewriteFor::Use), source),
            Assert(x, source) => Assert(rewrite_value(x, RewriteFor::Use), source),
            _ => self,
        }
    }
}

enum RewriteFor {
    Define,
    Use,
}

struct Construction {
    users: DiGraph<Value, RegisterIndex>,
    blocks: DiGraph<Block, bool>,
    block_offsets: HashSet<usize>,
    // var -> block (index in `blocks`) -> val (index in `users`)
    definitions: HashMap<RegisterIndex, HashMap<NodeIndex, NodeIndex>>,
    sealed: HashSet<NodeIndex>,
    // block -> var -> val
    phi_placeholders: HashMap<NodeIndex, HashMap<RegisterIndex, NodeIndex>>,
}

struct Value {
    instruction: Instruction,
    block: NodeIndex,
}

#[derive(Default)]
struct Block {
    phi: HashSet<NodeIndex>,
    instructions: Vec<NodeIndex>,
}

impl Construction {
    fn new(module: &Module, arity: usize) -> Self {
        let mut blocks = DiGraph::new();
        let mut users = DiGraph::new();
        let mut definitions = HashMap::<_, HashMap<_, _>>::new();
        let b = blocks.add_node(Block::default());
        for i in 0..arity {
            let value = users.add_node(Value {
                instruction: Instruction::OptimizePlaceholder,
                block: b,
            });
            blocks[b].instructions.push(value);
            definitions.entry(i as _).or_default().insert(b, value);
        }
        let mut c = Self {
            users,
            blocks,
            block_offsets: HashSet::from([0]),
            definitions,
            sealed: HashSet::from([b]),
            phi_placeholders: Default::default(),
        };
        c.visit_module(module, 0, b);
        c
    }

    fn visit_module(&mut self, module: &Module, i: usize, block: NodeIndex) {
        for instruction in &module.instructions[i..] {
            let value = self.users.add_node(Value {
                instruction: instruction.clone(),
                block,
            });
            for variable in instruction.use_registers() {
                let use_value = self.read_variable(variable, block);
                self.users.update_edge(value, use_value, variable);
            }
            if let Some(variable) = instruction.define() {
                self.write_variable(variable, block, value);
            }
            match instruction {
                Instruction::Return(_) => return,
                Instruction::Jump(_, positive, negative) => {
                    if self.block_offsets.insert(positive.0) {
                        let block = self.blocks.add_node(Block::default());
                        self.visit_module(module, positive.0, block);
                    }
                    if self.block_offsets.insert(negative.0) {
                        let block = self.blocks.add_node(Block::default());
                        self.visit_module(module, negative.0, block);
                    }
                    return;
                }
                _ => {}
            }
        }
    }

    fn write_variable(&mut self, variable: RegisterIndex, block: NodeIndex, value: NodeIndex) {
        self.definitions
            .entry(variable)
            .or_default()
            .insert(block, value);
    }

    fn read_variable(&mut self, variable: RegisterIndex, block: NodeIndex) -> NodeIndex {
        if let Some(value) = self.definitions.entry(variable).or_default().get(&block) {
            return *value;
        }
        self.read_variable_recursive(variable, block)
    }

    fn read_variable_recursive(&mut self, variable: RegisterIndex, block: NodeIndex) -> NodeIndex {
        let mut value;
        if !self.sealed.contains(&block) {
            value = self.users.add_node(Value {
                instruction: Instruction::Phi(RegisterIndex::MAX, Box::new([])),
                block,
            });
            self.blocks[block].phi.insert(value);
            self.phi_placeholders
                .entry(block)
                .or_default()
                .insert(variable, value);
        } else {
            let predecessors = self
                .blocks
                .neighbors_directed(block, Incoming)
                .collect::<Vec<_>>();
            if let Ok([p]) = <[NodeIndex; 1]>::try_from(predecessors.clone()) {
                value = self.read_variable(variable, p);
            } else {
                value = self.users.add_node(Value {
                    instruction: Instruction::Phi(
                        RegisterIndex::MAX,
                        vec![variable; predecessors.len()].into(),
                    ),
                    block,
                });
                self.write_variable(variable, block, value);
                value = self.add_phi_uses(variable, value, &predecessors);
            }
        }
        value
    }

    fn add_phi_uses(
        &mut self,
        variable: RegisterIndex,
        phi: NodeIndex,
        predecessors: &[NodeIndex],
    ) -> NodeIndex {
        for &p in predecessors {
            let use_value = self.read_variable(variable, p);
            self.users.update_edge(phi, use_value, variable);
        }
        self.remove_trivial_phi(phi)
    }

    fn remove_trivial_phi(&mut self, phi: NodeIndex) -> NodeIndex {
        let mut equivalent = None;
        for op in self.users.neighbors(phi) {
            if Some(op) == equivalent || op == phi || op.index() == usize::MAX {
                continue;
            }
            if equivalent.is_some() {
                return phi;
            }
            equivalent = Some(op);
        }
        let equivalent = equivalent.unwrap();
        let edges = self
            .users
            .edges_directed(phi, Incoming)
            .map(|edge| (edge.source(), *edge.weight()))
            .collect::<Vec<_>>();
        self.users.remove_node(phi);
        for (user, variable) in edges {
            self.users.add_edge(user, equivalent, variable);
        }
        equivalent
    }
}

impl From<Construction> for Module {
    fn from(value: Construction) -> Self {
        todo!()
    }
}
