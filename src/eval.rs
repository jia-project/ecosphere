use std::{
    borrow::Cow,
    collections::HashMap,
    iter::repeat_with,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
    ptr::NonNull,
    sync::Arc,
    time::Instant,
};

use crate::{
    arena::{Arena, ArenaUser},
    CastData, Instruction, InstructionLiteral, Object, ObjectAny, ObjectData, RegisterIndex,
    TypeIndex, TypeOperator,
};

#[derive(Clone)]
struct Symbol {
    type_names: HashMap<Box<str>, usize>,
    types: Vec<SymbolType>,
    function_dispatches: HashMap<DispatchKey, Dispatch>,
}

type DispatchKey = (Cow<'static, [TypeIndex]>, Cow<'static, str>, usize);
#[derive(Clone)]
enum Dispatch {
    Index(usize),
    Native(Arc<dyn Fn(&mut Machine, &[NonNull<Object>]) -> NonNull<Object> + Send + Sync>),
}

impl Default for Symbol {
    fn default() -> Self {
        let mut type_names = HashMap::new();
        let mut types = Vec::new();
        let s = <Box<str>>::from;

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

        let mut symbol = Self {
            type_names,
            types,
            function_dispatches: Default::default(),
        };

        symbol.make_native_function([], s("Array.new"), 1, |machine, args| {
            let &[len] = args else {
                unreachable!()
            };
            let len = *unsafe { len.as_ref() }.cast_ref::<i64>().unwrap() as usize;
            let nil = machine.arena.allocate(ObjectData::Typed(2, Box::new([])));
            machine
                .arena
                .allocate(ObjectData::Array(vec![nil; len].into()))
        });
        symbol.make_native_function([s("Array")], s("length"), 0, |machine, args| {
            let &[a] = args else {
                unreachable!()
            };
            let a = unsafe { a.as_ref() }
                .cast_ref::<Box<[NonNull<Object>]>>()
                .unwrap();
            machine.arena.allocate(ObjectData::Integer(a.len() as _))
        });
        symbol.make_native_function([s("Array")], s("clone"), 2, |machine, args| {
            let &[a, offset, length] = args else {
                unreachable!()
            };
            let a = unsafe { a.as_ref() }
                .cast_ref::<Box<[NonNull<Object>]>>()
                .unwrap();
            let offset = *unsafe { offset.as_ref() }.cast_ref::<i64>().unwrap() as usize;
            let length = *unsafe { length.as_ref() }.cast_ref::<i64>().unwrap() as usize;
            machine.arena.allocate(ObjectData::Array(
                a[offset..offset + length].to_vec().into(),
            ))
        });
        symbol.make_native_function([s("Array")], s("at"), 1, |_, args| {
            let &[a, position] = args else {
                unreachable!()
            };
            let a = unsafe { a.as_ref() }
                .cast_ref::<Box<[NonNull<Object>]>>()
                .unwrap();
            let position = *unsafe { position.as_ref() }.cast_ref::<i64>().unwrap() as usize;
            a[position]
        });
        symbol.make_native_function([s("Array")], s("at"), 2, |machine, args| {
            let &[mut a, position, element] = args else {
                unreachable!()
            };
            let a = unsafe { a.as_mut() }
                .cast_mut::<Box<[NonNull<Object>]>>()
                .unwrap();
            let position = *unsafe { position.as_ref() }.cast_ref::<i64>().unwrap() as usize;
            a[position] = element;
            machine.arena.allocate(ObjectData::Typed(2, Box::new([])))
        });
        symbol
    }
}

impl Symbol {
    fn make_native_function(
        &mut self,
        context_types: impl IntoIterator<Item = Box<str>>,
        name: Box<str>,
        arity: usize,
        function: impl Fn(&mut Machine, &[NonNull<Object>]) -> NonNull<Object> + Send + Sync + 'static,
    ) {
        self.make_dispatch(
            context_types,
            name,
            arity,
            Dispatch::Native(Arc::new(function)),
        );
    }

    fn make_dispatch(
        &mut self,
        context_types: impl IntoIterator<Item = Box<str>>,
        name: impl Into<String>,
        arity: usize,
        dispatch: Dispatch,
    ) {
        let context_types = context_types
            .into_iter()
            .map(|name| self.type_names[&name] as _)
            .collect();
        let present = self
            .function_dispatches
            .insert((context_types, name.into().into(), arity), dispatch);
        assert!(present.is_none());
    }
}

#[derive(Clone)]
enum SymbolType {
    Product {
        name: Box<str>,
        components: HashMap<Box<str>, usize>,
    },
    Sum {
        // name: Box<str>,
        variant_names: HashMap<Box<str>, u32>,
    },
    SumVariant {
        type_name: Box<str>,
        name: Box<str>,
    },
}

pub struct Machine {
    symbol: Symbol,
    frames: Vec<Frame>,
    names: HashMap<Box<str>, NonNull<Object>>,
    preallocate: MachineStatic,
    arena: ArenaUser,
    instant_zero: Instant,
}

struct MachineStatic {
    nil: NonNull<Object>,
    bool_true: NonNull<Object>,
    bool_false: NonNull<Object>,
    integers: [NonNull<Object>; 256],
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
        let mut arena = unsafe { arena.add_user(machine.as_mut_ptr()) };

        let nil = arena.allocate(ObjectData::Typed(2, Box::new([])));
        let preallocate = MachineStatic {
            nil,
            bool_false: arena.allocate(ObjectData::Typed(0, Box::new([nil]))),
            bool_true: arena.allocate(ObjectData::Typed(1, Box::new([nil]))),
            integers: (0..256)
                .map(|i| arena.allocate(ObjectData::Integer(i)))
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        };
        machine.write(Machine {
            symbol: Default::default(),
            frames: Default::default(),
            names: Default::default(),
            preallocate,
            instant_zero: arena.instant_zero(),
            arena,
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
        let Instruction::MakeFunction(context_types, name, arity, instructions) = instruction else {
            unreachable!()
        };
        self.symbol.make_dispatch(
            context_types.to_vec(),
            name,
            arity,
            Dispatch::Index(functions.len()),
        );
        functions.push(instructions);
    }

    fn is_finished(&self) -> bool {
        self.frames.is_empty() // extra conditions, such as canceled
    }

    fn execute(&mut self, instruction: &Instruction) {
        struct R<'a>(&'a mut Vec<Frame>);
        impl Index<&RegisterIndex> for R<'_> {
            type Output = FrameRegister;
            fn index(&self, index: &RegisterIndex) -> &Self::Output {
                #[allow(clippy::unnecessary_cast)]
                &self.0.last().unwrap().registers[*index as usize]
            }
        }
        impl IndexMut<&RegisterIndex> for R<'_> {
            fn index_mut(&mut self, index: &RegisterIndex) -> &mut Self::Output {
                #[allow(clippy::unnecessary_cast)]
                &mut self.0.last_mut().unwrap().registers[*index as usize]
            }
        }
        let mut r = R(&mut self.frames);

        use Instruction::*;
        match instruction {
            ParsingPlaceholder(_) => unreachable!(),

            MakeLiteralObject(i, InstructionLiteral::Nil) => {
                r[i] = FrameRegister::Address(self.preallocate.nil)
            }
            MakeLiteralObject(i, InstructionLiteral::Bool(true)) => {
                r[i] = FrameRegister::Address(self.preallocate.bool_true)
            }
            MakeLiteralObject(i, InstructionLiteral::Bool(false)) => {
                r[i] = FrameRegister::Address(self.preallocate.bool_false)
            }
            MakeLiteralObject(i, InstructionLiteral::Integer(value))
                if (0..256).contains(value) =>
            {
                r[i] = FrameRegister::Address(self.preallocate.integers[*value as usize])
            }
            MakeLiteralObject(i, InstructionLiteral::Integer(value)) => {
                r[i] = FrameRegister::Address(self.arena.allocate(ObjectData::Integer(*value)))
            }
            MakeLiteralObject(i, InstructionLiteral::String(value)) => {
                r[i] =
                    FrameRegister::Address(self.arena.allocate(ObjectData::String(value.clone())))
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
                        ObjectData::Array(_) => Self::TYPE_ARRAY,
                        ObjectData::Any(_) => todo!(),
                    });
                    xs.push(x.escape(&mut self.arena));
                }
                xs.extend(argument_xs.iter().map(|x| r[x].escape(&mut self.arena)));
                match &self.symbol.function_dispatches
                    [&(context.into(), (&**name).into(), argument_xs.len())]
                {
                    Dispatch::Index(index) => self.push_frame(*index, &xs, *i),
                    Dispatch::Native(function) => {
                        let result = function.clone()(self, &xs);
                        R(&mut self.frames)[i] = FrameRegister::Address(result);
                    }
                }
            }

            Load(i, name) => r[i] = FrameRegister::Address(self.names[name]),
            Store(x, name) => {
                self.names
                    .insert(name.clone(), r[x].escape(&mut self.arena));
            }
            Inspect(x) => {
                let repr = match r[x].view() {
                    ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),

                    ObjectData::Integer(value) => format!("Integer {value}"),
                    ObjectData::String(value) => format!("String {value}"),
                    ObjectData::Array(value) => format!("Array[{}]", value.len()),
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
            Put(x, name, y) => {
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
                        (BitAnd, Integer(x), Integer(y)) => Integer(*x & *y),
                        (BitOr, Integer(x), Integer(y)) => Integer(*x | *y),
                        (BitXor, Integer(x), Integer(y)) => Integer(*x ^ *y),
                        (Shl, Integer(x), Integer(y)) => Integer(*x << *y),
                        (Shr, Integer(x), Integer(y)) => Integer(*x >> *y),
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
                        let present = self.symbol.type_names.insert(name.clone(), type_index);
                        assert!(present.is_none());
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
                        let present = self.symbol.type_names.insert(name.clone(), type_index);
                        assert!(present.is_none());
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
        scanner.process_pointer(&mut self.preallocate.nil);
        scanner.process_pointer(&mut self.preallocate.bool_true);
        scanner.process_pointer(&mut self.preallocate.bool_false);
        for address in &mut self.preallocate.integers {
            scanner.process_pointer(address);
        }
        for address in self.names.values_mut() {
            scanner.process_pointer(address);
        }
        for frame in &mut self.frames {
            for register in &mut frame.registers {
                if let FrameRegister::Address(address) = register {
                    scanner.process_pointer(address);
                }
            }
        }
    }
}

impl CastData for i64 {
    fn cast_ref(data: &ObjectData) -> Option<&Self> {
        if let ObjectData::Integer(value) = data {
            Some(value)
        } else {
            None
        }
    }

    fn cast_mut(data: &mut ObjectData) -> Option<&mut Self> {
        if let ObjectData::Integer(value) = data {
            Some(value)
        } else {
            None
        }
    }
}

impl CastData for Box<[NonNull<Object>]> {
    fn cast_ref(data: &ObjectData) -> Option<&Self> {
        if let ObjectData::Array(value) = data {
            Some(value)
        } else {
            None
        }
    }

    fn cast_mut(data: &mut ObjectData) -> Option<&mut Self> {
        if let ObjectData::Array(value) = data {
            Some(value)
        } else {
            None
        }
    }
}
