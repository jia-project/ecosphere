use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Display,
};

use crate::{Instruction, Module, RegisterIndex};

pub fn optimize(module: Module) -> Module {
    let mut graph = Graph::from(module.clone());
    graph.dead_code_elimination();
    // graph.construction();
    println!("{graph}");
    module
}

#[derive(Debug, Clone)]
struct Block {
    phi: BTreeMap<RegisterIndex, Vec<RegisterIndex>>,
    instructions: Vec<Instruction>,
    exit: BlockExit,
}

#[derive(Debug, Clone)]
enum BlockExit {
    Jump(RegisterIndex, usize, usize), // replace instruction index with block index
    Return(RegisterIndex),             // just the same as in instruction
}

#[derive(Debug, Clone)]
struct Graph {
    source: usize,
    blocks: BTreeMap<usize, Block>,
    register_level: RegisterIndex,
}

impl From<Module> for Graph {
    fn from(module: Module) -> Self {
        let mut leaders = BTreeSet::from([0]);
        for instruction in &*module.instructions {
            // simplified a little bit by not treating immediate-follower of control instruction as leader
            // cause some redundant instructions cross blocks and makes blocks larger
            if let Instruction::Jump(_, positive, negative) = instruction {
                leaders.extend([positive, negative])
            }
        }
        let leaders = leaders
            .into_iter()
            .enumerate()
            .map(|(i, offset)| (offset, i))
            .collect::<BTreeMap<_, _>>();
        let mut blocks = Vec::new();
        for (leader, i) in &leaders {
            assert_eq!(*i, blocks.len());
            let block_end = module.instructions[*leader..]
                .iter()
                .position(|instruction| {
                    matches!(instruction, Instruction::Jump(..) | Instruction::Return(_))
                })
                .unwrap()
                + leader;
            let exit = match &module.instructions[block_end] {
                Instruction::Jump(x, positive, negative) => {
                    BlockExit::Jump(*x, leaders[positive], leaders[negative])
                }
                Instruction::Return(x) => BlockExit::Return(*x),
                _ => unreachable!(),
            };
            blocks.push(Block {
                phi: Default::default(),
                instructions: module.instructions[*leader..block_end].to_vec(),
                exit,
            });
        }
        Self {
            source: 0,
            blocks: blocks.into_iter().enumerate().collect(),
            register_level: module.register_level,
        }
    }
}

impl Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, block) in &self.blocks {
            let predecessors = self.predecessors(*i);
            if predecessors.is_empty() {
                writeln!(f, "'{i}:")?;
            } else {
                let entries = predecessors
                    .iter()
                    .map(|i| format!("'{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "'{i} from [{entries}]:",)?;
            }
            for (r, merged) in &block.phi {
                writeln!(f, "  {r} = Phi{merged:?}")?;
            }
            for instruction in &block.instructions {
                writeln!(f, "  {instruction:?}")?;
            }
            match &block.exit {
                BlockExit::Jump(x, positive, negative) => {
                    if positive == negative {
                        writeln!(f, "  Goto '{positive}")?
                    } else {
                        writeln!(f, "  If {x} Then '{positive} Else '{negative}")?
                    }
                }
                BlockExit::Return(x) => writeln!(f, "  Return {x}")?,
            }
        }
        Ok(())
    }
}

impl From<Graph> for Module {
    fn from(graph: Graph) -> Self {
        let mut offset = 0;
        let mut block_offsets = HashMap::new();
        for (i, block) in &graph.blocks {
            // phi staff
            block_offsets.insert(*i, offset);
            offset += block.instructions.len() + 1;
        }
        let mut instructions = Vec::new();
        for block in graph.blocks.into_values() {
            instructions.extend(block.instructions);
            let exit = match block.exit {
                BlockExit::Jump(x, positive, negative) => {
                    Instruction::Jump(x, block_offsets[&positive], block_offsets[&negative])
                }
                BlockExit::Return(x) => Instruction::Return(x),
            };
            instructions.push(exit);
        }
        Self {
            instructions: instructions.into(),
            register_level: graph.register_level,
        }
    }
}

impl Graph {
    fn dead_code_elimination(&mut self) {
        let mut alive_blocks = BTreeMap::new();
        let mut worklist =
            BTreeMap::from([(self.source, self.blocks.remove(&self.source).unwrap())]);
        while let Some((i, block)) = worklist.pop_first() {
            for successor in block.successors() {
                if let Some(succ_block) = self.blocks.remove(&successor) {
                    worklist.insert(successor, succ_block);
                }
            }
            alive_blocks.insert(i, block);
        }
        self.blocks = alive_blocks;
    }
}

impl Instruction {
    fn define(&self) -> Option<RegisterIndex> {
        use Instruction::*;
        match self {
            ParsingPlaceholder(..) => unreachable!(),
            Call(i, ..)
            | MakeLiteralObject(i, ..)
            | MakeTypedObject(i, ..)
            | Load(i, ..)
            | Get(i, ..)
            | Is(i, ..)
            | As(i, ..)
            | Operator1(i, ..)
            | Operator2(i, ..) => Some(*i),
            _ => None,
        }
    }

    fn use_registers(&self) -> HashSet<RegisterIndex> {
        use Instruction::*;
        match self {
            ParsingPlaceholder(..) => unreachable!(),
            Jump(x, ..)
            | Return(x)
            | Get(_, x, ..)
            | Is(_, x, ..)
            | As(_, x, ..)
            | Operator1(_, _, x)
            | Inspect(x, _)
            | Assert(x, _)
            | Store(x, _) => [*x].into(),
            Put(x, _, y) | Operator2(_, _, x, y) => [*x, *y].into(),
            Call(_, context_arguments, _, arguments) => [&**context_arguments, arguments]
                .concat()
                .into_iter()
                .collect(),
            MakeTypedObject(_, _, items) => items.iter().map(|(_, x)| *x).collect(),
            _ => HashSet::new(),
        }
    }
}

impl BlockExit {
    fn use_register(&self) -> RegisterIndex {
        match self {
            Self::Jump(x, ..) | Self::Return(x) => *x,
        }
    }
}

impl Block {
    fn successors(&self) -> HashSet<usize> {
        match self.exit {
            BlockExit::Jump(_, positive, negative) => [positive, negative].into(),
            BlockExit::Return(_) => HashSet::new(),
        }
    }
}

impl Graph {
    fn predecessors(&self, i: usize) -> HashSet<usize> {
        self.blocks
            .iter()
            .filter_map(|(p, block)| {
                if block.successors().contains(&i) {
                    Some(*p)
                } else {
                    None
                }
            })
            .collect()
    }
}
