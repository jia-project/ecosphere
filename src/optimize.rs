use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    fmt::Display,
    mem::replace,
};

use crate::{Instruction, Module, RegisterIndex};

pub fn optimize(module: &Module, arity: usize) -> Module {
    let mut graph = Graph::new(&module, arity);
    graph.dead_code_elimination();
    Construction::new(&mut graph).run();
    println!("{graph}");
    module.clone()
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
    arity: usize,
}

impl Graph {
    fn new(module: &Module, arity: usize) -> Self {
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
            arity,
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
        let mut define = None;
        self.clone().rewrite(|r, place| {
            if matches!(place, Place::Define) {
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
            if matches!(place, Place::Use) {
                use_registers.insert(r);
            }
            RegisterIndex::MAX
        });
        use_registers
    }

    fn rewrite(self, mut map_register: impl FnMut(RegisterIndex, Place) -> RegisterIndex) -> Self {
        use Instruction::*;
        match self {
            Call(i, context_arguments, name, arguments) => {
                let context_arguments = context_arguments
                    .into_vec()
                    .into_iter()
                    .map(|x| map_register(x, Place::Use))
                    .collect();
                let arguments = arguments
                    .into_vec()
                    .into_iter()
                    .map(|x| map_register(x, Place::Use))
                    .collect();
                Call(
                    map_register(i, Place::Define),
                    context_arguments,
                    name,
                    arguments,
                )
            }
            MakeLiteralObject(i, literal) => {
                MakeLiteralObject(map_register(i, Place::Define), literal)
            }
            MakeTypedObject(i, name, items) => {
                let items = items
                    .into_vec()
                    .into_iter()
                    .map(|(name, x)| (name, map_register(x, Place::Use)))
                    .collect();
                MakeTypedObject(map_register(i, Place::Define), name, items)
            }
            Load(i, name) => Load(map_register(i, Place::Define), name),
            Store(x, name) => Store(map_register(x, Place::Use), name),
            Get(i, x, name) => Get(
                map_register(i, Place::Define),
                map_register(x, Place::Use),
                name,
            ),
            Put(x, name, y) => Put(
                map_register(x, Place::Use),
                name,
                map_register(y, Place::Use),
            ),
            Is(i, x, name) => Is(
                map_register(i, Place::Define),
                map_register(x, Place::Use),
                name,
            ),
            As(i, x, name) => As(
                map_register(i, Place::Define),
                map_register(x, Place::Use),
                name,
            ),
            Operator1(i, op, x) => Operator1(
                map_register(i, Place::Define),
                op,
                map_register(x, Place::Use),
            ),
            Operator2(i, op, x, y) => Operator2(
                map_register(i, Place::Define),
                op,
                map_register(x, Place::Use),
                map_register(y, Place::Use),
            ),
            Inspect(x, source) => Inspect(map_register(x, Place::Use), source),
            Assert(x, source) => Assert(map_register(x, Place::Use), source),
            Jump(..) | Return(..) | ParsingPlaceholder(..) => unreachable!(),
            _ => self,
        }
    }
}

enum Place {
    Define,
    Use,
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

    fn rewrite(
        &mut self,
        block: usize,
        i: usize,
        mut map_register: impl FnMut(RegisterIndex, Place) -> RegisterIndex,
    ) {
        let block = self.blocks.get_mut(&block).unwrap();
        if i != usize::MAX {
            block.instructions[i] =
                replace(&mut block.instructions[i], Instruction::OptimizePlaceholder)
                    .rewrite(map_register);
        } else {
            match &mut block.exit {
                BlockExit::Return(x) | BlockExit::Jump(x, ..) => *x = map_register(*x, Place::Use),
            }
        }
    }
}

struct GraphMeta {
    predecessors: HashMap<usize, HashSet<usize>>,
    successors: HashMap<usize, HashSet<usize>>,
    definitions: HashMap<RegisterIndex, (usize, usize)>,
    uses: HashMap<RegisterIndex, HashSet<(usize, usize)>>,
}

impl GraphMeta {
    fn new(graph: &Graph) -> Self {
        let mut definitions = (0..graph.arity as _)
            .map(|i| (i, (graph.source, usize::MAX)))
            .collect::<HashMap<_, _>>();
        let mut uses = HashMap::<_, HashSet<_>>::new();
        for (b, block) in &graph.blocks {
            for (i, instruction) in block.instructions.iter().enumerate() {
                if let Some(r) = instruction.define() {
                    definitions.insert(r, (*b, i));
                }
                for use_register in instruction.use_registers() {
                    uses.entry(use_register).or_default().insert((*b, i));
                }
            }
            uses.entry(block.exit.use_register())
                .or_default()
                .insert((*b, usize::MAX));
        }
        Self {
            predecessors: graph
                .blocks
                .keys()
                .map(|&i| (i, graph.predecessors(i)))
                .collect(),
            successors: graph
                .blocks
                .iter()
                .map(|(&i, block)| (i, block.successors()))
                .collect(),
            definitions,
            uses,
        }
    }
}

struct Construction<'a> {
    graph: &'a mut Graph,
    meta: GraphMeta,
    // var -> block -> val
    definitions: HashMap<RegisterIndex, HashMap<usize, RegisterIndex>>,
    sealed: HashSet<usize>,
    // block -> var -> val
    imcomplete_phi: HashMap<usize, HashMap<RegisterIndex, RegisterIndex>>,
    register_level: RegisterIndex,
}

impl<'a> Construction<'a> {
    fn new(graph: &'a mut Graph) -> Self {
        Self {
            // 0..arity registers are fixed for parameters
            definitions: (0..graph.arity as _)
                .map(|i| (i, HashMap::from([(graph.source, i)])))
                .collect(),
            imcomplete_phi: graph
                .blocks
                .keys()
                .map(|i| (*i, Default::default()))
                .collect(),
            sealed: Default::default(),
            meta: GraphMeta::new(graph),
            register_level: graph.arity as _,
            graph,
        }
    }

    fn allocate(&mut self) -> RegisterIndex {
        let v = self.register_level;
        self.register_level += 1;
        v
    }

    fn allocate_write(&mut self, var: RegisterIndex, block: usize) -> RegisterIndex {
        let v = self.allocate();
        self.write_variable(var, block, v);
        v
    }

    fn run(&mut self) {
        let mut pending_seal = HashMap::new();
        let mut filled = HashSet::<usize>::new();
        let mut worklist = VecDeque::from([self.graph.source]);
        while let Some(b) = worklist.pop_front() {
            let not_filled = self.meta.predecessors[&b]
                .iter()
                .filter(|p| !filled.contains(p))
                .copied()
                .collect::<HashSet<_>>();
            if not_filled.is_empty() {
                self.seal_block(b);
            } else {
                pending_seal.insert(b, not_filled);
            }

            let instructions = self.graph.blocks[&b]
                .instructions
                .clone()
                .into_iter()
                .map(|instruction| {
                    instruction.rewrite(|r, place| match place {
                        Place::Define => self.allocate_write(r, b),
                        Place::Use => self.read_variable(r, b),
                    })
                })
                .collect();
            self.graph.blocks.get_mut(&b).unwrap().instructions = instructions;
            let exit = match self.graph.blocks[&b].exit.clone() {
                BlockExit::Jump(x, positive, negative) => {
                    BlockExit::Jump(self.read_variable(x, b), positive, negative)
                }
                BlockExit::Return(x) => BlockExit::Return(self.read_variable(x, b)),
            };
            self.graph.blocks.get_mut(&b).unwrap().exit = exit;

            filled.insert(b);
            for block in pending_seal.keys().copied().collect::<Vec<_>>() {
                let not_filled = pending_seal.get_mut(&block).unwrap();
                not_filled.remove(&b);
                if not_filled.is_empty() {
                    pending_seal.remove(&block);
                    self.seal_block(block);
                }
            }

            for s in &self.meta.successors[&b] {
                if !filled.contains(s) && !worklist.contains(s) {
                    worklist.push_back(*s);
                }
            }
        }

        self.meta = GraphMeta::new(&self.graph);
        for b in self.graph.blocks.keys().copied().collect::<Vec<_>>() {
            //
        }
    }

    fn write_variable(&mut self, var: RegisterIndex, block: usize, value: RegisterIndex) {
        self.definitions
            .entry(var)
            .or_default()
            .insert(block, value);
    }

    fn read_variable(&mut self, var: RegisterIndex, block: usize) -> RegisterIndex {
        if let Some(value) = self.definitions.entry(var).or_default().get(&block) {
            *value
        } else {
            self.read_variable_recursive(var, block)
        }
    }

    fn read_variable_recursive(&mut self, var: RegisterIndex, block: usize) -> RegisterIndex {
        let value;
        if !self.sealed.contains(&block) {
            value = self.allocate();
            self.imcomplete_phi
                .get_mut(&block)
                .unwrap()
                .insert(var, value);
        } else if let Ok([p]) = <[usize; 1]>::try_from(
            self.meta.predecessors[&block]
                .iter()
                .copied()
                .collect::<Vec<_>>(),
        ) {
            value = self.read_variable(var, p);
        } else {
            value = self.allocate_write(var, block);
            self.add_phi(var, block, value);
        }
        value
    }

    fn add_phi(&mut self, var: RegisterIndex, block: usize, value: RegisterIndex) {
        let operands = self.meta.predecessors[&block]
            .clone()
            .into_iter()
            .map(|p| self.read_variable(var, p))
            .collect();
        self.graph
            .blocks
            .get_mut(&block)
            .unwrap()
            .phi
            .insert(value, operands);
    }

    fn prune_phi(&mut self, value: RegisterIndex, block: usize) {
        const UNREACHABLE: RegisterIndex = RegisterIndex::MAX;
        let mut same = None;
        for &operand in &self.graph.blocks[&block].phi[&value] {
            if Some(operand) == same || operand == value || operand == UNREACHABLE {
                continue;
            }
            if same.is_some() {
                return;
            }
            same = Some(operand);
        }
        let same = same.unwrap_or(UNREACHABLE);
        self.graph
            .blocks
            .get_mut(&block)
            .unwrap()
            .phi
            .remove(&value);
        //
    }

    fn seal_block(&mut self, block: usize) {
        // assert all predecessors filled
        self.sealed.insert(block);
        for (var, value) in self.imcomplete_phi.remove(&block).unwrap() {
            self.add_phi(var, block, value);
        }
    }
}
