use std::{
    borrow::Cow,
    fmt::Debug,
    mem::{take, Discriminant, MaybeUninit},
    ops::{Index, IndexMut},
    ptr::NonNull,
    sync::Arc,
    time::Instant,
};

use crate::{
    arena::{Arena, ArenaUser},
    instruction::Literal,
    instruction::TypeOperator,
    CastData, Instruction, Object, ObjectAny, ObjectData, RegisterIndex, TypeIndex,
};
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

#[derive(Clone)]
struct Symbol {
    type_names: HashMap<Box<str>, usize>,
    types: Vec<SymbolType>,
    object_names: HashMap<Box<str>, NonNull<Object>>,
}

type DispatchKey = (Cow<'static, [TypeIndex]>, Cow<'static, str>, usize);
#[derive(Clone)]
enum Dispatch {
    Index(usize),
    Native(Arc<dyn Fn(&mut Machine) + Send + Sync>),
}

impl Default for Symbol {
    fn default() -> Self {
        let mut type_names = HashMap::default();
        let mut types = Vec::new();
        let s = <Box<str>>::from;

        // intrinsic types should be scoped with namespace
        type_names.insert(s("Integer"), Machine::TYPE_INTEGER as usize);
        type_names.insert(s("String"), Machine::TYPE_STRING as usize);
        type_names.insert(s("Array"), Machine::TYPE_ARRAY as usize);

        let mut variant_names = HashMap::default();
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

        let mut variant_names = HashMap::default();
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
            object_names: Default::default(),
        }
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
    arena: ArenaUser,
    frames: Vec<Frame>,
    registers: Vec<FrameRegister>,
    local: MachineStatic,
    arguments: Vec<FrameRegister>,
    return_register: RegisterIndex, // for native functions
    stats: HashMap<Discriminant<Instruction>, u32>,
}

struct MachineStatic {
    nil: NonNull<Object>,
    bool_true: NonNull<Object>,
    bool_false: NonNull<Object>,
    // integers: [NonNull<Object>; 256],
}

impl MachineStatic {
    fn make(&self, literal: &Literal) -> FrameRegister {
        match literal {
            Literal::Nil => FrameRegister::Address(self.nil),
            Literal::Bool(true) => FrameRegister::Address(self.bool_true),
            Literal::Bool(false) => FrameRegister::Address(self.bool_false),
            // InstructionLiteral::Integer(value) if (0..256).contains(value) => {
            //     FrameRegister::Address(self.integers[*value as usize])
            // }
            Literal::Integer(value) => FrameRegister::Inline(Object {
                data: ObjectData::Integer(*value),
            }),
            Literal::String(value) => FrameRegister::Inline(Object {
                data: ObjectData::String(value.clone()),
            }),
        }
    }
}

struct Frame {
    offset: RegisterIndex,
    return_register: RegisterIndex,
    function_index: usize,
    program_counter: usize,
}

impl Frame {
    const REGISTER_COUNT: usize = 256;
}

#[derive(Default)]
enum FrameRegister {
    #[default]
    Vacant,
    Address(NonNull<Object>),
    Inline(Object),
}

impl Debug for FrameRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vacant => f.pad("Vacant"),
            Self::Address(address) => f.pad(&format!(
                "Address({address:?}, {})",
                unsafe { address.as_ref() }.type_name()
            )),
            Self::Inline(object) => f.pad(&format!("Inline({})", object.type_name())),
        }
    }
}

impl FrameRegister {
    fn view(&self) -> &ObjectData {
        &self.view2().data
    }

    fn view_mut(&mut self) -> &mut ObjectData {
        &mut self.view2_mut().data
    }

    fn view2(&self) -> &Object {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { address.as_ref() },
            Self::Inline(object) => object,
        }
    }

    fn view2_mut(&mut self) -> &mut Object {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { address.as_mut() },
            Self::Inline(object) => object,
        }
    }

    fn escape(&mut self, arena: &mut ArenaUser) -> NonNull<Object> {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => *address,
            Self::Inline(_) => {
                let Self::Inline(object) = take(self) else {
                    unreachable!()
                };
                let address = arena.allocate(object.data);
                *self = Self::Address(address);
                address
            }
        }
    }

    fn copy(&mut self, arena: &mut ArenaUser) -> FrameRegister {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => Self::Address(*address),
            Self::Inline(object) => match &object.data {
                ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),
                ObjectData::Integer(value) => Self::Inline(Object {
                    data: ObjectData::Integer(*value),
                }),
                _ => Self::Address(self.escape(arena)),
            },
        }
    }
}

impl Machine {
    pub fn run_initial(instructions: Box<[Instruction]>) {
        let arena = Arena::default();
        let mut machine = Box::new(MaybeUninit::<Self>::uninit());
        let mut arena = unsafe { arena.add_user(machine.as_mut_ptr()) };

        arena.mutate_enter();
        let nil = arena.allocate(ObjectData::Typed(2, Box::new([])));
        let preallocate = MachineStatic {
            nil,
            bool_false: arena.allocate(ObjectData::Typed(0, Box::new([nil]))),
            bool_true: arena.allocate(ObjectData::Typed(1, Box::new([nil]))),
            // integers: (0..256)
            //     .map(|i| arena.allocate(ObjectData::Integer(i)))
            //     .collect::<Vec<_>>()
            //     .try_into()
            //     .unwrap(),
        };
        arena.mutate_exit();
        machine.write(Machine {
            symbol: Default::default(),
            frames: Default::default(),
            registers: Default::default(),
            local: preallocate,
            arena,
            arguments: Default::default(),
            return_register: RegisterIndex::MAX,
            stats: Default::default(),
        });
        // port unstable `assume_init`
        let mut machine = unsafe { Box::from_raw(Box::into_raw(machine) as *mut Self) };

        machine.push_frame(0, RegisterIndex::MAX);
        machine.run(machine.native_dispatch(), vec![instructions]);
        if !machine.stats.is_empty() {
            println!("{:?}", machine.stats);
        }
    }

    fn push_frame(&mut self, index: usize, return_register: RegisterIndex) {
        let frame_offset = self
            .frames
            .last()
            .map(|frame| frame.offset + Frame::REGISTER_COUNT)
            .unwrap_or_default();
        let frame = Frame {
            offset: frame_offset,
            return_register,
            function_index: index,
            program_counter: 0,
        };
        // TODO allocate less registers if we know it's ok
        self.registers
            .resize_with(frame_offset + Frame::REGISTER_COUNT, Default::default);
        for (r, argument) in self.registers[frame_offset..frame_offset + self.arguments.len()]
            .iter_mut()
            .zip(self.arguments.drain(..))
        {
            *r = argument;
        }
        self.frames.push(frame);
    }

    fn run(
        &mut self,
        mut function_dispatches: HashMap<DispatchKey, Dispatch>,
        mut functions: Vec<Box<[Instruction]>>,
    ) {
        self.arena.mutate_enter();
        'control: while !self.is_finished() {
            let depth = self.frames.len();
            for instruction in &functions[self.frames.last().unwrap().function_index]
                [self.frames.last().unwrap().program_counter..]
            {
                let counter = self.frames.last().unwrap().program_counter + 1;
                self.frames.last_mut().unwrap().program_counter = counter;

                // *self.stats.entry(std::mem::discriminant(instruction)).or_default() += 1;

                if matches!(instruction, Instruction::MakeFunction(..)) {
                    self.make_function(
                        instruction.clone(),
                        &mut function_dispatches,
                        &mut functions,
                    );
                    // logically this is ok, but Rust does not allow it because
                    // we are in the middle of iterating `functions[...]`
                    // with the `&mut functions` above this cannot resume
                    // not on hot path so just conform it
                    // continue;
                    continue 'control;
                }

                self.execute(instruction, &function_dispatches);
                if self.frames.len() != depth
                    || self.frames.last().unwrap().program_counter != counter
                {
                    continue 'control;
                }
            }
            unreachable!("function instructions not ending with Return")
        }
        self.arena.mutate_exit();
    }

    const TYPE_INTEGER: TypeIndex = TypeIndex::MAX - 1;
    const TYPE_STRING: TypeIndex = TypeIndex::MAX - 2;
    const TYPE_ARRAY: TypeIndex = TypeIndex::MAX - 3;

    fn make_dispatch(
        &self,
        function_dispatches: &mut HashMap<DispatchKey, Dispatch>,
        context_types: impl IntoIterator<Item = Box<str>>,
        name: impl Into<String>,
        arity: usize,
        dispatch: Dispatch,
    ) {
        let context_types = context_types
            .into_iter()
            .map(|name| self.symbol.type_names[&name] as _)
            .collect();
        let present =
            function_dispatches.insert((context_types, name.into().into(), arity), dispatch);
        assert!(present.is_none());
    }

    fn make_function(
        &mut self,
        instruction: Instruction,
        function_dispatches: &mut HashMap<DispatchKey, Dispatch>,
        functions: &mut Vec<Box<[Instruction]>>,
    ) {
        let Instruction::MakeFunction(context_types, name, arity, instructions) = instruction else {
            unreachable!()
        };
        self.make_dispatch(
            function_dispatches,
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

    fn execute(
        &mut self,
        instruction: &Instruction,
        function_dispatches: &HashMap<DispatchKey, Dispatch>,
    ) {
        struct R<'a>(&'a mut [FrameRegister], usize);
        impl Index<&RegisterIndex> for R<'_> {
            type Output = FrameRegister;
            fn index(&self, index: &RegisterIndex) -> &Self::Output {
                #[allow(clippy::unnecessary_cast)]
                &self.0[self.1 + *index as usize]
            }
        }
        impl IndexMut<&RegisterIndex> for R<'_> {
            fn index_mut(&mut self, index: &RegisterIndex) -> &mut Self::Output {
                #[allow(clippy::unnecessary_cast)]
                &mut self.0[self.1 + *index as usize]
            }
        }
        let mut r = R(&mut self.registers, self.frames.last().unwrap().offset);

        use Instruction::*;
        // println!("{instruction:?}");
        match instruction {
            ParsingPlaceholder(_) => unreachable!(),

            MakeLiteralObject(i, literal) => r[i] = self.local.make(literal),
            MakeTypedObject(i, name, items) => {
                let type_index = self.symbol.type_names[name];
                match &self.symbol.types[type_index] {
                    SymbolType::Product { components, .. } => {
                        let data = ObjectData::Typed(type_index as _, Box::new([]));
                        r[i] = FrameRegister::Address(self.arena.allocate(data));
                        *r[i].view_mut() = ObjectData::Typed(
                            type_index as _,
                            vec![self.local.nil; components.len()].into(),
                        );
                        for (name, x) in &**items {
                            let x = r[x].escape(&mut self.arena);
                            let ObjectData::Typed(_, data) = r[i].view_mut() else {
                                unreachable!()
                            };
                            data[components[name]] = x;
                        }
                    }
                    SymbolType::Sum { variant_names, .. } => {
                        let [(name, x)] = &**items else {
                            panic!()
                        };
                        let data = ObjectData::Typed(variant_names[name], Box::new([])); //
                        r[i] = FrameRegister::Address(self.arena.allocate(data));
                        let data = r[x].escape(&mut self.arena);
                        *r[i].view_mut() = ObjectData::Typed(variant_names[name], Box::new([data]));
                    }
                    SymbolType::SumVariant { .. } => panic!(),
                }
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
                let returned = take(&mut r[x]);
                let frame = self.frames.pop().unwrap();
                for (i, register) in self.registers[frame.offset..].iter_mut().enumerate() {
                    match register {
                        // a safe optimization for now because any `Vacant` indicate every higher register is not used
                        // if `Move` operation is added, make sure to distinguish `Vacant` and `Moved`
                        FrameRegister::Vacant if i != *x => break,
                        FrameRegister::Address(_) => {
                            take(register);
                        }
                        _ => {}
                    }
                }
                if !self.is_finished() {
                    self.registers[frame.return_register] = returned;
                }
            }
            Call(i, context_xs, name, argument_xs) => {
                let mut context = Vec::new(); //
                for x in &**context_xs {
                    context.push(match r[x].view() {
                        ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),
                        ObjectData::Typed(type_index, _) => *type_index,
                        ObjectData::Integer(_) => Self::TYPE_INTEGER,
                        ObjectData::String(_) => Self::TYPE_STRING,
                        ObjectData::Array(_) => Self::TYPE_ARRAY,
                        ObjectData::Any(_) => todo!(),
                    });
                    self.arguments.push(r[x].copy(&mut self.arena));
                }
                // self.arguments
                //     .extend(argument_xs.iter().map(|x| r[x].escape(&mut self.arena)));
                for x in &**argument_xs {
                    self.arguments.push(r[x].copy(&mut self.arena));
                }
                let return_register = r.1 + *i;
                match function_dispatches.get(&(
                    context.into(),
                    (&**name).into(),
                    argument_xs.len(),
                )) {
                    None => panic!("No dispatch for (..).{name}/{}", argument_xs.len()),
                    Some(Dispatch::Index(index)) => self.push_frame(*index, return_register),
                    Some(Dispatch::Native(function)) => {
                        self.return_register = return_register;
                        function(self);
                        assert_eq!(self.return_register, RegisterIndex::MAX);
                    }
                }
            }

            Load(i, name) => r[i] = FrameRegister::Address(self.symbol.object_names[name]),
            Store(x, name) => {
                self.symbol
                    .object_names
                    .insert(name.clone(), r[x].escape(&mut self.arena));
            }
            Inspect(x, source) => {
                let repr = match r[x].view() {
                    ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),

                    ObjectData::Integer(value) => format!("Integer {value}"),
                    ObjectData::String(value) => format!("String {value}"),
                    ObjectData::Array(value) => {
                        format!("Array[{:?}; {}]", value.as_ptr(), value.len())
                    }
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
                    "[{:>9.3?}] {source}  => {:x?} {repr}",
                    Instant::now() - self.arena.instant_zero(),
                    r[x]
                );
            }
            Assert(x, source) => {
                let ObjectData::Typed(type_index, _) = r[x].view() else {
                    panic!()
                };
                assert_eq!(*type_index, 1, "{source}");
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
                r[i] = self.local.make(&Literal::Bool(n == name));
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
                use {crate::instruction::Operator1::*, Literal as L, ObjectData::*};
                r[i] = match (op, r[x].view()) {
                    (Copy, _) => r[x].copy(&mut self.arena),
                    (Not, Typed(0, _)) => self.local.make(&L::Bool(true)),
                    (Not, Typed(1, _)) => self.local.make(&L::Bool(false)),
                    (Neg, Integer(x)) => self.local.make(&L::Integer(-*x)),
                    _ => panic!(),
                }
            }
            Operator2(i, op, x, y) => {
                use {crate::instruction::Operator2::*, Literal as L, ObjectData::*};
                r[i] = self.local.make(&match (op, r[x].view(), r[y].view()) {
                    (Add, Integer(x), Integer(y)) => L::Integer(*x + *y),
                    (Sub, Integer(x), Integer(y)) => L::Integer(*x - *y),
                    (Mul, Integer(x), Integer(y)) => L::Integer(*x * *y),
                    (Div, Integer(x), Integer(y)) => L::Integer(*x / *y),
                    (Rem, Integer(x), Integer(y)) => L::Integer(*x % *y),
                    (Lt, Integer(x), Integer(y)) => L::Bool(x < y),
                    (Gt, Integer(x), Integer(y)) => L::Bool(x > y),
                    (Eq, Integer(x), Integer(y)) => L::Bool(x == y),
                    (Ne, Integer(x), Integer(y)) => L::Bool(x != y),
                    (Le, Integer(x), Integer(y)) => L::Bool(x <= y),
                    (Ge, Integer(x), Integer(y)) => L::Bool(x >= y),
                    (BitAnd, Integer(x), Integer(y)) => L::Integer(*x & *y),
                    (BitOr, Integer(x), Integer(y)) => L::Integer(*x | *y),
                    (BitXor, Integer(x), Integer(y)) => L::Integer(*x ^ *y),
                    (Shl, Integer(x), Integer(y)) => L::Integer(*x << *y),
                    (Shr, Integer(x), Integer(y)) => L::Integer(*x >> *y),
                    (Add, String(x), String(y)) => L::String([&**x, &**y].concat().into()),
                    _ => panic!("{instruction:?} x = {:?} y = {:?}", r[x], r[y]),
                })
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
                        let mut variant_names = HashMap::default();
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

    fn make_native_dispatch<const N: usize, const M: usize>(
        &self,
        dispatches: &mut HashMap<DispatchKey, Dispatch>,
        context_type: [Box<str>; N],
        name: Box<str>,
        // rust cannot do [FrameRegister; N + M] for now
        function: impl Fn(&mut Machine, [FrameRegister; N], [FrameRegister; M]) -> FrameRegister
            + Send
            + Sync
            + 'static,
    ) {
        self.make_dispatch(
            dispatches,
            context_type,
            name,
            M,
            Dispatch::Native(Arc::new(move |machine| {
                assert_eq!(machine.arguments.len(), N + M);
                let arguments = machine
                    .arguments
                    .drain(N..)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                let context_arguments = machine
                    .arguments
                    .drain(..)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                let r = function(machine, context_arguments, arguments);
                machine.native_return(r);
            })),
        )
    }

    fn native_return(&mut self, x: FrameRegister) {
        // ensure native function return once and only once
        assert_ne!(self.return_register, RegisterIndex::MAX);
        self.registers[self.return_register] = x;
        self.return_register = RegisterIndex::MAX;
    }

    fn native_dispatch(&self) -> HashMap<DispatchKey, Dispatch> {
        let s = <Box<str>>::from;
        let mut function_dispatches = Default::default();
        self.make_native_dispatch(
            &mut function_dispatches,
            [],
            s("Array.new"),
            |machine, [], [len]| {
                let len = *len.view2().cast_ref::<i64>().unwrap() as usize;
                // allocate with dummy data first, make sure that placeholder `nil` has correct address
                // even it get copied in this `allocate`
                // we should give `nil` a static address...
                let mut array = machine.arena.allocate(ObjectData::Array(Box::new([])));
                unsafe { array.as_mut() }.data =
                    ObjectData::Array(vec![machine.local.nil; len].into());
                FrameRegister::Address(array)
            },
        );
        self.make_native_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("length"),
            |machine, [a], []| {
                let a = a.view2().cast_ref::<Box<[NonNull<Object>]>>().unwrap();
                machine.local.make(&Literal::Integer(a.len() as _))
            },
        );
        self.make_native_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("clone"),
            |machine, [a], [offset, length]| {
                let a = a.view2().cast_ref::<Box<[NonNull<Object>]>>().unwrap();
                let offset = *offset.view2().cast_ref::<i64>().unwrap() as usize;
                let length = *length.view2().cast_ref::<i64>().unwrap() as usize;
                let mut cloned = machine.arena.allocate(ObjectData::Array(Box::new([])));
                unsafe { cloned.as_mut() }.data =
                    ObjectData::Array(a[offset..offset + length].to_vec().into());
                FrameRegister::Address(cloned)
            },
        );
        self.make_native_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("at"),
            |_, [a], [position]| {
                let a = a.view2().cast_ref::<Box<[NonNull<Object>]>>().unwrap();
                let position = *position.view2().cast_ref::<i64>().unwrap() as usize;
                FrameRegister::Address(a[position])
            },
        );
        self.make_native_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("at"),
            |machine, [mut a], [position, mut element]| {
                let a = a.view2_mut().cast_mut::<Box<[NonNull<Object>]>>().unwrap();
                let position = *position.view2().cast_ref::<i64>().unwrap() as usize;
                a[position] = element.escape(&mut machine.arena);
                machine.local.make(&Literal::Nil)
            },
        );
        function_dispatches
    }
}

unsafe impl ObjectAny for Machine {
    fn on_scan(&mut self, scanner: &mut crate::arena::ObjectScanner<'_>) {
        scanner.process_pointer(&mut self.local.nil);
        scanner.process_pointer(&mut self.local.bool_true);
        scanner.process_pointer(&mut self.local.bool_false);
        // for address in &mut self.preallocate.integers {
        //     scanner.process_pointer(address);
        // }
        for address in self.symbol.object_names.values_mut() {
            scanner.process_pointer(address);
        }
        for register in &mut self.arguments {
            match register {
                FrameRegister::Vacant => unreachable!(),
                FrameRegister::Address(address) => scanner.process_pointer(address),
                FrameRegister::Inline(object) => scanner.process(object),
            }
        }
        for register in
            &mut self.registers[..self.frames.last().unwrap().offset + Frame::REGISTER_COUNT]
        {
            match register {
                FrameRegister::Vacant => {}
                FrameRegister::Address(address) => scanner.process_pointer(address),
                FrameRegister::Inline(object) => scanner.process(object),
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
