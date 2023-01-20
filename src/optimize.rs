use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

use crate::{Instruction, Module, RegisterIndex};

pub fn optimize(module: Module) -> Module {
    let graph = Graph::from(module);
    println!("{graph}");
    graph.into()
}

struct Block {
    entries: Vec<usize>,
    phi: HashMap<RegisterIndex, Vec<RegisterIndex>>,
    instructions: Vec<Instruction>,
    exit: BlockExit,
}

enum BlockExit {
    Jump(RegisterIndex, usize, usize), // replace instruction index with block index
    Return(RegisterIndex),             // just the same as in instruction
}

struct Graph {
    blocks: Vec<Block>,
    register_level: usize,
}

impl From<Module> for Graph {
    fn from(module: Module) -> Self {
        let mut block_offsets = [0].into_iter().collect::<BTreeSet<_>>();
        for instruction in &*module.instructions {
            if let Instruction::Jump(_, positive, negative) = instruction {
                block_offsets.extend([positive, negative])
            }
        }
        let block_offsets = block_offsets
            .into_iter()
            .enumerate()
            .map(|(i, offset)| (offset, i))
            .collect::<BTreeMap<_, _>>();
        let mut block_entries = vec![BTreeSet::new(); block_offsets.len()];
        let mut blocks = Vec::new();
        for (block_offset, i) in &block_offsets {
            assert_eq!(*i, blocks.len());
            let block_end = module.instructions[*block_offset..]
                .iter()
                .position(|instruction| {
                    matches!(instruction, Instruction::Jump(..) | Instruction::Return(_))
                })
                .unwrap()
                + block_offset;
            let exit;
            match &module.instructions[block_end] {
                Instruction::Jump(x, positive, negative) => {
                    exit = BlockExit::Jump(*x, block_offsets[positive], block_offsets[negative]);
                    block_entries[block_offsets[positive]].insert(*i);
                    block_entries[block_offsets[negative]].insert(*i);
                }
                Instruction::Return(x) => exit = BlockExit::Return(*x),
                _ => unreachable!(),
            }
            blocks.push(Block {
                entries: Default::default(),
                phi: Default::default(),
                instructions: module.instructions[*block_offset..block_end].to_vec(),
                exit,
            });
        }
        for (block, block_entry) in blocks.iter_mut().zip(block_entries) {
            block.entries = block_entry.into_iter().collect();
        }
        Self {
            blocks,
            register_level: module.register_level,
        }
    }
}

impl Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, block) in self.blocks.iter().enumerate() {
            if block.entries.is_empty() {
                writeln!(f, "'{i}:")?;
            } else {
                let entries = block
                    .entries
                    .iter()
                    .map(|i| format!("'{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "'{i} (from {entries}):",)?;
            }
            // phi
            for instruction in &block.instructions {
                writeln!(f, "  {instruction:?}")?;
            }
            match &block.exit {
                BlockExit::Jump(x, positive, negative) => {
                    if positive == negative {
                        writeln!(f, "  Goto {positive}")?
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
        let mut block_offsets = Vec::new();
        for block in &graph.blocks {
            // phi staff
            block_offsets.push(offset);
            offset += block.instructions.len() + 1;
        }
        let mut instructions = Vec::new();
        for block in graph.blocks {
            instructions.extend(block.instructions);
            let exit = match block.exit {
                BlockExit::Jump(x, positive, negative) => {
                    Instruction::Jump(x, block_offsets[positive], block_offsets[negative])
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
