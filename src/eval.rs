use std::{
    borrow::Cow,
    collections::HashMap,
    iter::repeat_with,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
    ptr::NonNull,
    time::Instant,
};

use crate::{
    arena::{Arena, ArenaUser},
    Instruction, InstructionLiteral, Object, ObjectAny, ObjectData, RegisterIndex, TypeIndex,
    TypeOperator,
};

#[derive(Clone)]
struct Symbol {
    type_names: HashMap<String, usize>,
    types: Vec<SymbolType>,
    function_dispatches: HashMap<(Cow<'static, [TypeIndex]>, Cow<'static, str>, usize), usize>,
}

impl Default for Symbol {
    fn default() -> Self {
        let mut type_names = HashMap::new();
        let mut types = Vec::new();
        let s = String::from;

        // intrinsic types should be scoped with namespace
        type_names.insert(s("Integer"), Machine::TYPE_INTEGER as usize);
        type_names.insert(s("String"), Machine::TYPE_STRING as usize);
        type_names.insert(s("Array"), Machine::TYPE_ARRAY as usize);

        let mut variant_names = HashMap::new();
        variant_names.insert(s("False"), types.len() as u32);
        types.push(SymbolType::SumVariant {
            type_name: s("Bool"),
            name: s("False"),
        });
        variant_names.insert(s("True"), types.len() as _);
        types.push(SymbolType::SumVariant {
            type_name: s("Bool"),
            name: s("True"),
        });
        type_names.insert(s("Bool"), types.len());
        types.push(SymbolType::Sum { variant_names });

        let mut variant_names = HashMap::new();
        variant_names.insert(s("None"), types.len() as u32);
        types.push(SymbolType::SumVariant {
            type_name: s("Option"),
            name: s("None"),
        });
        variant_names.insert(s("Some"), types.len() as _);
        types.push(SymbolType::SumVariant {
            type_name: s("Option"),
            name: s("Some"),
        });
        type_names.insert(s("Option"), types.len());
        types.push(SymbolType::Sum { variant_names });

        Self {
            type_names,
            types,
            function_dispatches: Default::default(),
        }
    }
}

#[derive(Clone)]
enum SymbolType {
    Product {
        name: String,
        components: HashMap<String, usize>,
    },
    Sum {
        // name: String,
        variant_names: HashMap<String, u32>,
    },
    SumVariant {
        type_name: String,
        name: String,
    },
}

pub struct Machine {
    symbol: Symbol,
    frames: Vec<Frame>,
    arena: ArenaUser,
    instant_zero: Instant,
}

struct Frame {
    registers: [FrameRegister; Self::REGISTER_COUNT], //
    return_register: RegisterIndex,
    function_index: usize,
    program_counter: usize,
}

impl Frame {
    const REGISTER_COUNT: usize = 256;
}

#[derive(Debug, Default, Clone)]
enum FrameRegister {
    #[default]
    Vacant,
    Address(NonNull<Object>),
    // inline
}

impl FrameRegister {
    fn view(&self) -> &ObjectData {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { &address.as_ref().data },
            //
        }
    }

    fn view_mut(&mut self) -> &mut ObjectData {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { &mut address.as_mut().data },
            //
        }
    }

    fn escape(&mut self, arena: &mut ArenaUser) -> NonNull<Object> {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => *address,
            //
        }
    }
}

impl Machine {
    pub fn run_initial(instructions: Box<[Instruction]>) {
        let arena = Arena::default();
        let mut machine = Box::new(MaybeUninit::<Self>::uninit());
        let arena = unsafe { arena.add_user(machine.as_mut_ptr()) };
        machine.write(Machine {
            symbol: Default::default(),
            frames: Default::default(),
            arena,
            instant_zero: Instant::now(),
        });
        // port unstable `assume_init`
        let mut machine = unsafe { Box::from_raw(Box::into_raw(machine) as *mut Self) };
        machine.push_frame(0, &[], RegisterIndex::MAX);
        machine.run(vec![instructions]);
    }

    fn push_frame(
        &mut self,
        index: usize,
        arguments: &[NonNull<Object>],
        return_register: RegisterIndex,
    ) {
        let mut frame = Frame {
            registers: repeat_with(Default::default)
                .take(Frame::REGISTER_COUNT)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
            return_register,
            function_index: index,
            program_counter: 0,
        };
        for (r, argument) in frame.registers[..arguments.len()].iter_mut().zip(arguments) {
            *r = FrameRegister::Address(*argument);
        }
        self.frames.push(frame);
    }

    fn run(&mut self, mut functions: Vec<Box<[Instruction]>>) {
        'control: while !self.is_finished() {
            let depth = self.frames.len();
            for instruction in &functions[self.frames.last().unwrap().function_index]
                [self.frames.last().unwrap().program_counter..]
            {
                let counter = self.frames.last().unwrap().program_counter + 1;
                self.frames.last_mut().unwrap().program_counter = counter;

                if matches!(instruction, Instruction::MakeFunction(..)) {
                    self.make_function(instruction.clone(), &mut functions);
                    // continue;
                    // logically this is ok, but Rust does not allow it because
                    // we are in the middle of iterating `functions[...]`
                    // with the `&mut functions` above this cannot resume
                    // not on hot path so just conform it
                    continue 'control;
                }

                self.arena.mutate_enter();
                self.execute(instruction);
                self.arena.mutate_exit();
                if self.frames.len() != depth
                    || self.frames.last().unwrap().program_counter != counter
                {
                    continue 'control;
                }
            }
            unreachable!("function instructions not ending with Return")
        }
    }

    const TYPE_INTEGER: TypeIndex = TypeIndex::MAX - 1;
    const TYPE_STRING: TypeIndex = TypeIndex::MAX - 2;
    const TYPE_ARRAY: TypeIndex = TypeIndex::MAX - 3;

    fn make_function(&mut self, instruction: Instruction, functions: &mut Vec<Box<[Instruction]>>) {
        let Instruction::MakeFunction(context_types, name, n_argument, instructions) = instruction else {
            unreachable!()
        };
        let context_types = context_types
            .iter()
            .map(|name| self.symbol.type_names[name] as _)
            .collect();
        let index = functions.len();
        let present = self
            .symbol
            .function_dispatches
            .insert((context_types, name.into(), n_argument), index);
        assert!(present.is_none());
        functions.push(instructions);
    }

    fn is_finished(&self) -> bool {
        self.frames.is_empty() // extra conditions, such as canceled
    }

    fn execute(&mut self, instruction: &Instruction) {
        struct R<'a>(&'a mut Vec<Frame>);
        impl Index<&usize> for R<'_> {
            type Output = FrameRegister;
            fn index(&self, index: &usize) -> &Self::Output {
                &self.0.last().unwrap().registers[*index]
            }
        }
        impl IndexMut<&usize> for R<'_> {
            fn index_mut(&mut self, index: &usize) -> &mut Self::Output {
                &mut self.0.last_mut().unwrap().registers[*index]
            }
        }
        let mut r = R(&mut self.frames);

        use Instruction::*;
        match instruction {
            ParsingPlaceholder(_) => unreachable!(),

            MakeLiteralObject(i, InstructionLiteral::Nil) => {
                r[i] =
                    FrameRegister::Address(self.arena.allocate(ObjectData::Typed(2, Box::new([]))));
            }
            MakeLiteralObject(i, InstructionLiteral::Bool(value)) => {
                let type_index = u32::from(*value);
                let data = self.arena.allocate(ObjectData::Typed(2, Box::new([])));
                r[i] = FrameRegister::Address(
                    self.arena
                        .allocate(ObjectData::Typed(type_index, Box::new([data]))),
                )
            }
            MakeLiteralObject(i, InstructionLiteral::Integer(value)) => {
                r[i] = FrameRegister::Address(self.arena.allocate(ObjectData::Integer(*value)))
            }
            MakeLiteralObject(i, InstructionLiteral::String(value)) => {
                r[i] = FrameRegister::Address(
                    self.arena
                        .allocate(ObjectData::String(value.clone().into())),
                )
            }

            MakeDataObject(i, name, items) => {
                let type_index = self.symbol.type_names[name];
                let data = match &self.symbol.types[type_index] {
                    SymbolType::Product { components, .. } => {
                        let mut data =
                            repeat_with(|| self.arena.allocate(ObjectData::Typed(0, Box::new([]))))
                                .take(components.len())
                                .collect::<Box<_>>();
                        for (name, x) in items.iter() {
                            data[components[name]] = r[x].escape(&mut self.arena);
                        }
                        ObjectData::Typed(type_index as _, data)
                    }
                    SymbolType::Sum { variant_names, .. } => {
                        let [(name, x)] = &**items else {
                            panic!()
                        };
                        ObjectData::Typed(
                            variant_names[name],
                            Box::new([r[x].escape(&mut self.arena)]), //
                        )
                    }
                    SymbolType::SumVariant { .. } => panic!(),
                };
                r[i] = FrameRegister::Address(self.arena.allocate(data))
            }

            Jump(x, positive, negative) => {
                let target = *if positive == negative {
                    positive
                } else {
                    let ObjectData::Typed(type_index, _) = r[x].view() else {
                        panic!()
                    };
                    match *type_index {
                        0 => negative,
                        1 => positive,
                        _ => panic!(),
                    }
                };
                self.frames.last_mut().unwrap().program_counter = target;
            }
            Return(x) => {
                let x = r[x].escape(&mut self.arena);
                let frame = self.frames.pop().unwrap();
                if !self.is_finished() {
                    let mut r = R(&mut self.frames);
                    r[&frame.return_register] = FrameRegister::Address(x);
                }
            }
            Call(i, context_xs, name, argument_xs) => {
                let mut xs = Vec::new();
                let mut context = Vec::new();
                for x in context_xs.iter() {
                    let x = &mut r[x];
                    context.push(match x.view() {
                        ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),
                        ObjectData::Typed(type_index, _) => *type_index,
                        ObjectData::Integer(_) => Self::TYPE_INTEGER,
                        ObjectData::String(_) => Self::TYPE_STRING,
                        ObjectData::Any(_) => todo!(),
                    });
                    xs.push(x.escape(&mut self.arena));
                }
                xs.extend(argument_xs.iter().map(|x| r[x].escape(&mut self.arena)));
                let index = self.symbol.function_dispatches
                    [&(context.into(), name.into(), argument_xs.len())];
                self.push_frame(index, &xs, *i);
            }

            Load(i, name) => todo!(),
            Store(x, name) => todo!(),
            Inspect(x) => {
                let repr = match r[x].view() {
                    ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),

                    ObjectData::Integer(value) => format!("Integer {value}"),
                    ObjectData::String(value) => format!("String {value}"),
                    ObjectData::Typed(type_index, _) => {
                        match &self.symbol.types[*type_index as usize] {
                            SymbolType::Product { name, .. } => format!("{name}[...]"),
                            SymbolType::SumVariant {
                                type_name, name, ..
                            } => format!("{type_name}.{name}"),
                            SymbolType::Sum { .. } => unreachable!(),
                        }
                    }
                    ObjectData::Any(value) => format!("(any) {}", value.type_name()),
                };
                println!(
                    "[{:>9.3?}] {:x?} {repr}",
                    Instant::now() - self.instant_zero,
                    r[x]
                );
            }
            Assert(x) => {
                let ObjectData::Typed(type_index, _) = r[x].view() else {
                    panic!()
                };
                assert_eq!(*type_index, 1);
            }
            Get(i, x, name) => {
                let ObjectData::Typed(type_index, data) = r[x].view() else {
                    panic!()
                };
                let SymbolType::Product{components,..} = &self.symbol.types[*type_index as usize] else {
                    panic!()
                };
                r[i] = FrameRegister::Address(data[components[name]])
            }
            Set(x, name, y) => {
                let y = r[y].escape(&mut self.arena);
                let ObjectData::Typed(type_index,  data) = r[x].view_mut() else {
                    panic!()
                };
                let SymbolType::Product{components,..} = &self.symbol.types[*type_index as usize] else {
                    panic!()
                };
                data[components[name]] = y
            }
            Is(i, x, name) => {
                let ObjectData::Typed(type_index, _) = r[x].view() else {
                    panic!()
                };
                let SymbolType::SumVariant{name: n, ..} = &self.symbol.types[*type_index as usize] else {
                    panic!()
                };
                let type_index = 1 + u32::from(name == n);
                let data = self.arena.allocate(ObjectData::Typed(0, Box::new([])));
                r[i] = FrameRegister::Address(
                    self.arena
                        .allocate(ObjectData::Typed(type_index, Box::new([data]))),
                )
            }
            As(i, x, name) => {
                let ObjectData::Typed(type_index, data) = r[x].view() else {
                    panic!()
                };
                assert!(
                    matches!(&self.symbol.types[*type_index as usize], SymbolType::SumVariant {name: n, ..} if n == name)
                );
                r[i] = FrameRegister::Address(data[0]) // more check?
            }
            Operator1(i, op, x) => {
                use {crate::Operator1::*, ObjectData::*};
                let mut b = |value: bool| {
                    let nil = self.arena.allocate(Typed(2, Box::new([])));
                    Typed(TypeIndex::from(value), Box::new([nil]))
                };
                let object = match (op, r[x].view()) {
                    (Copy, _) => {
                        // TODO escape for large inline objects
                        r[i] = r[x].clone();
                        return;
                    }
                    (Not, Typed(0, _)) => b(true),
                    (Not, Typed(1, _)) => b(false),
                    (Neg, Integer(x)) => Integer(-x),
                    _ => panic!(),
                };
                r[i] = FrameRegister::Address(self.arena.allocate(object));
            }
            Operator2(i, op, x, y) => {
                use {crate::Operator2::*, ObjectData::*};
                let mut b = |value: bool| {
                    let nil = self.arena.allocate(Typed(2, Box::new([])));
                    Typed(TypeIndex::from(value), Box::new([nil]))
                };
                let object = {
                    match (op, r[x].view(), r[y].view()) {
                        (Add, Integer(x), Integer(y)) => Integer(*x + *y),
                        (Sub, Integer(x), Integer(y)) => Integer(*x - *y),
                        (Mul, Integer(x), Integer(y)) => Integer(*x * *y),
                        (Div, Integer(x), Integer(y)) => Integer(*x / *y),
                        (Rem, Integer(x), Integer(y)) => Integer(*x % *y),
                        (Lt, Integer(x), Integer(y)) => b(x < y),
                        (Gt, Integer(x), Integer(y)) => b(x > y),
                        (Eq, Integer(x), Integer(y)) => b(x == y),
                        (Ne, Integer(x), Integer(y)) => b(x != y),
                        (Le, Integer(x), Integer(y)) => b(x <= y),
                        (Ge, Integer(x), Integer(y)) => b(x >= y),
                        (Add, String(x), String(y)) => String([&**x, &**y].concat().into()),
                        _ => panic!(),
                    }
                };
                r[i] = FrameRegister::Address(self.arena.allocate(object))
            }

            MakeType(name, op, items) => {
                match op {
                    TypeOperator::Product => {
                        let type_index = self.symbol.types.len();
                        self.symbol.type_names.insert(name.clone(), type_index);
                        self.symbol.types.push(SymbolType::Product {
                            name: name.clone(),
                            components: items
                                .iter()
                                .enumerate()
                                .map(|(i, component)| (component.clone(), i))
                                .collect(),
                        })
                    }
                    TypeOperator::Sum => {
                        let mut variant_names = HashMap::new();
                        for (i, variant) in items.iter().enumerate() {
                            variant_names.insert(variant.clone(), i as _);
                            self.symbol.types.push(SymbolType::SumVariant {
                                type_name: name.clone(),
                                name: variant.clone(),
                            });
                        }
                        let type_index = self.symbol.types.len();
                        self.symbol.type_names.insert(name.clone(), type_index);
                        self.symbol.types.push(SymbolType::Sum {
                            // name: name.clone(),
                            variant_names,
                        });
                    }
                }
            }
            MakeFunction(..) => unreachable!(),
        }
    }
}

unsafe impl ObjectAny for Machine {
    fn on_scan(&mut self, scanner: &mut crate::arena::ObjectScanner<'_>) {
        for frame in &mut self.frames {
            for register in &mut frame.registers {
                if let FrameRegister::Address(address) = register {
                    scanner.process(address)
                }
            }
        }
    }
}
