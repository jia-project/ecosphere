use std::{
    borrow::Cow,
    fmt::Debug,
    mem::{take, Discriminant},
    ops::{Index, IndexMut},
    slice,
    sync::Arc,
    time::Instant,
};

use crate::{
    arena::{ArenaClient, ArenaServer, ArenaVirtual, CollectContext},
    instruction::Literal,
    instruction::TypeOperator,
    shared::object::Object0,
    Instruction, Module, Object, RegisterIndex, TypeIndex,
};
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

impl Machine {
    const NATIVE_SECTION: TypeIndex = 0x00000010;
    #[allow(clippy::identity_op)]
    const INTEGER: TypeIndex = Self::NATIVE_SECTION + 0;
    const STRING: TypeIndex = Self::NATIVE_SECTION + 1;
    const ARRAY: TypeIndex = Self::NATIVE_SECTION + 2;

    #[allow(clippy::identity_op)]
    const FALSE: TypeIndex = ArenaServer::TYPED_SECTION + 0;
    const TRUE: TypeIndex = ArenaServer::TYPED_SECTION + 1;
    const NONE: TypeIndex = ArenaServer::TYPED_SECTION + 2;
}

impl Object {
    fn new_integer(value: i64) -> Self {
        Self(Object0 { i: value }, Machine::INTEGER, 0)
    }

    fn new_string(value: Box<str>) -> Self {
        let value = Box::leak(value);
        let (p, len) = (value.as_mut_ptr(), value.len());
        if len > u32::MAX as _ {
            todo!()
        }
        Self(Object0 { p: p as _ }, Machine::STRING, len as _)
    }

    fn new_array(value: Box<[*mut Object]>) -> Self {
        let value = Box::leak(value);
        let (p, len) = (value.as_mut_ptr(), value.len());
        if len > u32::MAX as _ {
            todo!()
        }
        Self(Object0 { p: p as _ }, Machine::ARRAY, len as _)
    }

    fn new_typed(index: TypeIndex, data: Box<[*mut Object]>) -> Self {
        assert!(data.len() <= u32::MAX as _);
        match data.len() {
            0 => Self(Object0 { p: 0 }, index, 0),
            1 => Self(Object0 { p: data[0] as _ }, index, 1),
            _ => {
                let data = Box::leak(data);
                let (p, len) = (data.as_mut_ptr(), data.len());
                Self(Object0 { p: p as _ }, index, len as _)
            }
        }
    }

    fn visit_array(&self, context: &mut CollectContext<'_>) {
        for address in unsafe { self.array() }.unwrap() {
            context.process(address)
        }
    }

    pub fn visit_typed(&mut self, context: &mut CollectContext<'_>) {
        for address in unsafe { self.typed_data_mut() }.unwrap() {
            context.process(address)
        }
    }

    fn drop_string(self) {
        assert_eq!(self.1, Machine::STRING);
        drop(unsafe {
            Box::from_raw(std::str::from_utf8_unchecked_mut(
                slice::from_raw_parts_mut(self.0.p as _, self.2 as _),
            ))
        })
    }

    fn drop_array(self) {
        assert_eq!(self.1, Machine::ARRAY);
        drop(unsafe {
            Box::from_raw(slice::from_raw_parts_mut(
                self.0.p as *mut *mut Object,
                self.2 as _,
            ))
        })
    }

    pub fn drop_typed(self) {
        assert!(self.1 >= ArenaServer::TYPED_SECTION);
        if self.2 > 1 {
            drop(unsafe {
                Box::from_raw(slice::from_raw_parts_mut(
                    self.0.p as *mut *mut Object,
                    self.2 as _,
                ))
            })
        }
    }

    fn to_integer(&self) -> Option<i64> {
        if self.1 == Machine::INTEGER {
            Some(unsafe { self.0.i })
        } else {
            None
        }
    }

    unsafe fn string(&self) -> Option<&mut str> {
        if self.1 == Machine::STRING {
            Some(unsafe {
                std::str::from_utf8_unchecked_mut(slice::from_raw_parts_mut(
                    self.0.p as _,
                    self.2 as _,
                ))
            })
        } else {
            None
        }
    }

    unsafe fn array(&self) -> Option<&mut [*mut Object]> {
        if self.1 == Machine::ARRAY {
            Some(unsafe { slice::from_raw_parts_mut(self.0.p as _, self.2 as _) })
        } else {
            None
        }
    }

    unsafe fn typed_data(&self) -> Option<&[*mut Object]> {
        if self.1 < ArenaServer::TYPED_SECTION {
            return None;
        }
        Some(match self.2 {
            0 => &[],
            1 => slice::from_ref(unsafe { &self.0.p1 }),
            _ => unsafe { slice::from_raw_parts(self.0.p as _, self.2 as _) },
        })
    }

    unsafe fn typed_data_mut(&mut self) -> Option<&mut [*mut Object]> {
        if self.1 < ArenaServer::TYPED_SECTION {
            return None;
        }
        Some(match self.2 {
            0 => &mut [],
            1 => slice::from_mut(unsafe { &mut self.0.p1 }),
            _ => unsafe { slice::from_raw_parts_mut(self.0.p as _, self.2 as _) },
        })
    }
}

#[derive(Clone, Default)]
struct Symbol {
    type_names: HashMap<Box<str>, TypeIndex>,
    types: Vec<SymbolType>,
    native_types: Vec<SymbolType>,
    object_names: HashMap<Box<str>, *mut Object>,
}

impl Index<TypeIndex> for Symbol {
    type Output = SymbolType;

    fn index(&self, index: TypeIndex) -> &Self::Output {
        if index < ArenaServer::TYPED_SECTION {
            &self.native_types[(index - Machine::NATIVE_SECTION) as usize]
        } else {
            &self.types[(index - ArenaServer::TYPED_SECTION) as usize]
        }
    }
}

// not used for now
impl IndexMut<TypeIndex> for Symbol {
    fn index_mut(&mut self, index: TypeIndex) -> &mut Self::Output {
        if index < ArenaServer::TYPED_SECTION {
            &mut self.native_types[(index - Machine::NATIVE_SECTION) as usize]
        } else {
            &mut self.types[(index - ArenaServer::TYPED_SECTION) as usize]
        }
    }
}

impl Symbol {
    fn add_type(&mut self, name: Option<Box<str>>, symbol_type: SymbolType) -> TypeIndex {
        let index;
        if matches!(symbol_type, SymbolType::Native { .. }) {
            index = (self.native_types.len() as u32) + Machine::NATIVE_SECTION;
            self.native_types.push(symbol_type);
        } else {
            index = (self.types.len() as u32) + ArenaServer::TYPED_SECTION;
            self.types.push(symbol_type);
        }
        if let Some(name) = name {
            let value = self.type_names.insert(name, index);
            assert!(value.is_none());
        }
        index
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
    Native {
        name: Box<str>,
    },
}

type DispatchKey = (Cow<'static, [TypeIndex]>, Cow<'static, str>, usize);

#[derive(Clone)]
enum Dispatch {
    Index(usize),
    Native(Arc<dyn Fn(&mut Machine) + Send + Sync>),
}

pub struct Machine {
    symbol: Symbol,
    arena: ArenaClient,
    frames: Vec<Frame>,
    registers: Vec<FrameRegister>,
    local: MachineStatic,
    arguments: Vec<FrameRegister>,
    return_register: usize, // for native functions
    stats: HashMap<Discriminant<Instruction>, u32>,
}

struct MachineStatic {
    none: *mut Object,
    bool_true: *mut Object,
    bool_false: *mut Object,
    // integers: [NonNull<Object>; 256],
}

impl MachineStatic {
    fn make(&self, literal: &Literal) -> FrameRegister {
        match literal {
            Literal::Nil => FrameRegister::Address(self.none),
            Literal::Bool(true) => FrameRegister::Address(self.bool_true),
            Literal::Bool(false) => FrameRegister::Address(self.bool_false),
            // InstructionLiteral::Integer(value) if (0..256).contains(value) => {
            //     FrameRegister::Address(self.integers[*value as usize])
            // }
            Literal::Integer(value) => FrameRegister::Inline(Object::new_integer(*value)),
            Literal::String(value) => FrameRegister::Inline(Object::new_string(value.clone())),
        }
    }
}

struct Frame {
    offset: usize,
    len: usize,
    return_register: usize,
    function_index: usize,
    program_counter: usize,
}

#[derive(Default)]
enum FrameRegister {
    #[default]
    Vacant,
    Address(*mut Object),
    Inline(Object),
}

impl Debug for FrameRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vacant => f.pad("Vacant"),
            Self::Address(address) => f.pad(&format!("Address({address:?}, {:#x})", unsafe {
                (**address).1
            })),
            Self::Inline(object) => f.pad(&format!("Inline({:#x})", object.1)),
        }
    }
}

impl FrameRegister {
    fn view(&self) -> &Object {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { &**address },
            Self::Inline(object) => object,
        }
    }

    fn view_mut(&mut self) -> &mut Object {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => unsafe { &mut **address },
            Self::Inline(object) => object,
        }
    }

    fn escape(&mut self, arena: &mut ArenaClient) -> *mut Object {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => *address,
            Self::Inline(_) => {
                let Self::Inline(object) = take(self) else {
                    unreachable!()
                };
                let address = arena.allocate();
                unsafe { address.write(object) }
                *self = Self::Address(address);
                address
            }
        }
    }

    fn copy(&mut self, arena: &mut ArenaClient) -> FrameRegister {
        match self {
            Self::Vacant => panic!(),
            Self::Address(address) => Self::Address(*address),
            Self::Inline(object) if object.1 == Machine::INTEGER => {
                Self::Inline(Object(object.0, object.1, object.2))
            }
            Self::Inline(_) => Self::Address(self.escape(arena)),
        }
    }

    fn clear(&mut self) {
        let register = take(self);
        if let Self::Inline(object) = register {
            match object.1 {
                Machine::STRING => object.drop_string(),
                Machine::ARRAY => object.drop_array(),
                type_index if type_index >= ArenaServer::TYPED_SECTION => object.drop_typed(),
                _ => {}
            }
        }
    }
}

impl Machine {
    pub fn run_initial(module: Module) {
        let mut arena_server = ArenaServer::new();
        arena_server.register_virtual(
            Self::INTEGER,
            ArenaVirtual {
                visit: Object::visit_nop,
                drop: Object::drop_nop,
            },
        );
        arena_server.register_virtual(
            Self::STRING,
            ArenaVirtual {
                visit: Object::visit_nop,
                drop: Object::drop_string,
            },
        );
        arena_server.register_virtual(
            Self::ARRAY,
            ArenaVirtual {
                visit: Object::visit_array,
                drop: Object::drop_array,
            },
        );

        let mut arena = arena_server.add_client();
        unsafe { arena.preallocate(64) }
        let none = arena.allocate();
        unsafe { none.write(Object::new_typed(Self::NONE, Box::new([]))) }
        let bool_true = arena.allocate();
        unsafe { bool_true.write(Object::new_typed(Self::TRUE, Box::new([]))) }
        let bool_false = arena.allocate();
        unsafe { bool_false.write(Object::new_typed(Self::FALSE, Box::new([]))) }
        let local = MachineStatic {
            none,
            bool_true,
            bool_false,
        };

        let mut machine = Self {
            symbol: Self::symbol(),
            frames: Default::default(),
            registers: Default::default(),
            local,
            arena,
            arguments: Default::default(),
            return_register: usize::MAX,
            stats: Default::default(),
        };
        unsafe { arena_server.add_root(&mut machine, Self::visit) }

        machine.push_frame(0, usize::MAX, 0, module.register_level);
        machine.run(machine.native_dispatch(), vec![module]);
        if !machine.stats.is_empty() {
            println!("{:?}", machine.stats);
        }
    }

    fn push_frame(&mut self, index: usize, return_register: usize, offset: usize, len: usize) {
        let frame = Frame {
            offset,
            len,
            return_register,
            function_index: index,
            program_counter: 0,
        };
        // TODO allocate less registers if we know it's ok
        if self.registers.len() < offset + len {
            self.registers.resize_with(offset + len, Default::default);
        }
        for (r, argument) in self.registers[offset..offset + self.arguments.len()]
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
        mut functions: Vec<Module>,
    ) {
        let mut shall_allocate = true;
        'control: while !self.is_finished() {
            let depth = self.frames.len();
            let mut counter = self.frames.last_mut().unwrap().program_counter;
            for instruction in
                &functions[self.frames.last().unwrap().function_index].instructions[counter..]
            {
                counter += 1;
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

                if shall_allocate {
                    unsafe { self.arena.preallocate(16) }
                }
                shall_allocate = self.execute(instruction, &function_dispatches, &functions);
                if self.frames.len() != depth
                    || self.frames.last().unwrap().program_counter != counter
                {
                    continue 'control;
                }
            }
            unreachable!("function instructions not ending with Return")
        }
    }

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
        functions: &mut Vec<Module>,
    ) {
        let Instruction::MakeFunction(context_types, name, arity, module) = instruction else {
            unreachable!()
        };
        self.make_dispatch(
            function_dispatches,
            context_types.to_vec(),
            name,
            arity,
            Dispatch::Index(functions.len()),
        );
        functions.push(module);
    }

    fn is_finished(&self) -> bool {
        self.frames.is_empty() // extra conditions, such as canceled
    }

    fn execute(
        &mut self,
        instruction: &Instruction,
        function_dispatches: &HashMap<DispatchKey, Dispatch>,
        functions: &[Module],
    ) -> bool {
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
            ParsingPlaceholder(_) | MakeFunction(..) => unreachable!(),
            MakeLiteralObject(i, literal) => {
                r[i] = self.local.make(literal);
                false
            }
            MakeTypedObject(i, name, items) => {
                let type_index = self.symbol.type_names[name];
                match &self.symbol[type_index] {
                    SymbolType::Product { components, .. } => {
                        let mut data = <Box<[_]>>::from(vec![self.local.none; components.len()]);
                        for (name, x) in &**items {
                            data[components[name]] = r[x].escape(&mut self.arena);
                        }
                        r[i] = FrameRegister::Inline(Object::new_typed(type_index as _, data));
                    }
                    SymbolType::Sum { variant_names, .. } => {
                        let [(name, x)] = &**items else {
                                        panic!()
                                    };
                        r[i] = FrameRegister::Inline(Object::new_typed(
                            variant_names[name],
                            Box::new([r[x].escape(&mut self.arena)]),
                        ));
                    }
                    SymbolType::SumVariant { .. } | SymbolType::Native { .. } => panic!(),
                }
                true
            }

            Jump(x, positive, negative) => {
                let target = *if positive == negative {
                    positive
                } else {
                    [negative, positive][(r[x].view().1 - ArenaServer::TYPED_SECTION) as usize]
                };
                self.frames.last_mut().unwrap().program_counter = target;
                false
            }
            Return(x) => {
                let returned = take(&mut r[x]);
                let frame = self.frames.pop().unwrap();
                for (i, register) in self.registers[frame.offset..].iter_mut().enumerate() {
                    match register {
                        // a safe optimization for now because any `Vacant` indicate every higher register is not used
                        // if `Move` operation is added, make sure to distinguish `Vacant` and `Moved`
                        FrameRegister::Vacant if i as RegisterIndex != *x => break,
                        register => register.clear(),
                    }
                }
                if !self.is_finished() {
                    self.registers[frame.return_register] = returned;
                }
                false
            }
            Call(i, context_xs, name, argument_xs) => {
                let mut context = Vec::new(); //
                for x in &**context_xs {
                    context.push(r[x].view().1);
                    self.arguments.push(r[x].copy(&mut self.arena));
                }
                self.arguments
                    .extend(argument_xs.iter().map(|x| r[x].copy(&mut self.arena)));
                #[allow(clippy::unnecessary_cast)]
                let return_register = r.1 + *i as usize;
                match function_dispatches.get(&(
                    context.into(),
                    (&**name).into(),
                    argument_xs.len(),
                )) {
                    None => panic!("No dispatch for (..).{name}/{}", argument_xs.len()),
                    Some(Dispatch::Index(index)) => self.push_frame(
                        *index,
                        return_register,
                        {
                            let frame = self.frames.last().unwrap();
                            frame.offset + frame.len
                        },
                        functions[*index].register_level,
                    ),
                    Some(Dispatch::Native(function)) => {
                        self.return_register = return_register;
                        function(self);
                        assert_eq!(self.return_register, usize::MAX);
                    }
                }
                true
            }

            Load(i, name) => {
                r[i] = FrameRegister::Address(self.symbol.object_names[name]);
                false
            }
            Store(x, name) => {
                self.symbol
                    .object_names
                    .insert(name.clone(), r[x].escape(&mut self.arena));
                true
            }
            Inspect(x, source) => {
                let object = r[x].view();
                let repr = match object.1 {
                    Self::INTEGER => format!("Integer {}", object.to_integer().unwrap()),
                    Self::STRING => format!("String {}", unsafe { object.string() }.unwrap()),
                    Self::ARRAY => {
                        let array = unsafe { object.array() }.unwrap();
                        format!("Array[{:#x?}; {}]", array.as_ptr(), array.len())
                    }
                    type_index => match &self.symbol[type_index] {
                        SymbolType::Product { name, .. } => format!("{name}[...]"),
                        SymbolType::SumVariant {
                            type_name, name, ..
                        } => format!("{type_name}.{name}"),
                        SymbolType::Sum { .. } => unreachable!(),
                        SymbolType::Native { name } => format!("{name}[(native)]"),
                    },
                };
                println!(
                    "[{:>9.3?}] {source}  => {:x?} {repr}",
                    Instant::now() - self.arena.start_instant(),
                    r[x]
                );
                false
            }
            Assert(x, source) => {
                assert_eq!(r[x].view().1, Self::TRUE, "{source}");
                false
            }
            Get(i, x, name) => {
                let x = r[x].view();
                let SymbolType::Product{components,..} = &self.symbol[x.1] else {
                        panic!()
                    };
                r[i] = FrameRegister::Address(unsafe { x.typed_data() }.unwrap()[components[name]]);
                false
            }
            Put(x, name, y) => {
                let y = r[y].escape(&mut self.arena);
                let x = r[x].view_mut();
                let SymbolType::Product{components,..} = &self.symbol[x.1] else {
                        panic!()
                    };
                unsafe { x.typed_data_mut() }.unwrap()[components[name]] = y;
                true
            }
            Is(i, x, name) => {
                let SymbolType::SumVariant{name: n, ..} = &self.symbol[r[x].view().1] else {
                        panic!()
                    };
                r[i] = self.local.make(&Literal::Bool(n == name));
                false
            }
            As(i, x, name) => {
                let x = r[x].view();
                assert!(
                    matches!(&self.symbol[x.1], SymbolType::SumVariant {name: n, ..} if n == name)
                );
                r[i] = FrameRegister::Address(unsafe { x.typed_data() }.unwrap()[0]); // more check?
                false
            }
            Operator1(i, op, x) => {
                use {crate::instruction::Operator1::*, Literal::*};
                r[i] = match (op, r[x].view().1) {
                    (Copy, _) => r[x].copy(&mut self.arena),
                    (Not, Self::FALSE) => self.local.make(&Bool(true)),
                    (Not, Self::TRUE) => self.local.make(&Bool(false)),
                    (Neg, Self::INTEGER) => self
                        .local
                        .make(&Integer(-r[x].view().to_integer().unwrap())),
                    _ => panic!(),
                };
                false
            }
            Operator2(i, op, x, y) => {
                use {crate::instruction::Operator2::*, Literal::*};
                let (x, y) = (r[x].view(), r[y].view());
                r[i] = match (x.1, y.1) {
                    (Self::INTEGER, Self::INTEGER) => {
                        let (x, y) = (x.to_integer().unwrap(), y.to_integer().unwrap());
                        self.local.make(&match op {
                            Add => Integer(x + y),
                            Sub => Integer(x - y),
                            Mul => Integer(x * y),
                            Div => Integer(x / y),
                            Rem => Integer(x % y),
                            BitAnd => Integer(x & y),
                            BitOr => Integer(x | y),
                            BitXor => Integer(x ^ y),
                            Shl => Integer(x << y),
                            Shr => Integer(x >> y),
                            Lt => Bool(x < y),
                            Gt => Bool(x > y),
                            Eq => Bool(x == y),
                            Ne => Bool(x != y),
                            Le => Bool(x <= y),
                            Ge => Bool(x >= y),
                            // _ => panic!()
                        })
                    }
                    (Self::STRING, Self::STRING) => {
                        let (x, y) = unsafe { (x.string().unwrap(), y.string().unwrap()) };
                        FrameRegister::Inline(Object::new_string([x, y].concat().into()))
                    }
                    _ => panic!(),
                };
                false
            }

            MakeType(name, op, items) => {
                match op {
                    TypeOperator::Product => {
                        self.symbol.add_type(
                            Some(name.clone()),
                            SymbolType::Product {
                                name: name.clone(),
                                components: items
                                    .iter()
                                    .enumerate()
                                    .map(|(i, component)| (component.clone(), i))
                                    .collect(),
                            },
                        );
                    }
                    TypeOperator::Sum => {
                        let variant_names = HashMap::default();
                        for variant in &**items {
                            self.symbol.add_type(
                                None,
                                SymbolType::SumVariant {
                                    type_name: name.clone(),
                                    name: variant.clone(),
                                },
                            );
                        }
                        self.symbol.add_type(
                            Some(name.clone()),
                            SymbolType::Sum {
                                // name: name.clone(),
                                variant_names,
                            },
                        );
                    }
                }
                false
            }
        }
    }

    fn symbol() -> Symbol {
        let mut symbol = Symbol::default();
        let s = <Box<str>>::from;
        let index = symbol.add_type(
            None,
            SymbolType::SumVariant {
                type_name: s("Bool"),
                name: s("False"),
            },
        );
        assert_eq!(index, Self::FALSE);
        let index = symbol.add_type(
            None,
            SymbolType::SumVariant {
                type_name: s("Bool"),
                name: s("True"),
            },
        );
        assert_eq!(index, Self::TRUE);
        let index = symbol.add_type(
            Some(s("Integer")),
            SymbolType::Native { name: s("Integer") },
        );
        assert_eq!(index, Self::INTEGER);
        let index = symbol.add_type(Some(s("String")), SymbolType::Native { name: s("String") });
        assert_eq!(index, Self::STRING);
        let index = symbol.add_type(Some(s("Array")), SymbolType::Native { name: s("Array") });
        assert_eq!(index, Self::ARRAY);
        let mut variant_names = HashMap::default();
        let index = symbol.add_type(
            None,
            SymbolType::SumVariant {
                type_name: s("Option"),
                name: s("None"),
            },
        );
        assert_eq!(index, Self::NONE);
        variant_names.insert(s("None"), Self::NONE);
        let index = symbol.add_type(
            None,
            SymbolType::SumVariant {
                type_name: s("Option"),
                name: s("Some"),
            },
        );
        variant_names.insert(s("Some"), index);
        symbol.add_type(Some(s("Option")), SymbolType::Sum { variant_names });
        symbol.add_type(
            Some(s("Bool")),
            SymbolType::Sum {
                variant_names: [(s("False"), Self::FALSE), (s("True"), Self::TRUE)]
                    .into_iter()
                    .collect(),
            },
        );

        symbol
    }

    fn native_return(&mut self, x: FrameRegister) {
        // ensure native function return once and only once
        assert_ne!(self.return_register, usize::MAX);
        self.registers[self.return_register] = x;
        self.return_register = usize::MAX;
    }

    fn native_dispatch(&self) -> HashMap<DispatchKey, Dispatch> {
        let s = <Box<str>>::from;
        fn n(f: impl Fn(&mut Machine) + Send + Sync + 'static) -> Dispatch {
            Dispatch::Native(Arc::new(f))
        }
        let mut function_dispatches = Default::default();
        self.make_dispatch(
            &mut function_dispatches,
            [],
            s("Array.new"),
            1,
            n(|machine| {
                let mut args = machine.arguments.drain(..);
                let len = args.next().unwrap().view().to_integer().unwrap() as _;
                drop(args);
                let a = machine.arena.allocate();
                unsafe { a.write(Object::new_array(vec![machine.local.none; len].into())) }
                machine.native_return(FrameRegister::Address(a));
            }),
        );
        self.make_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("length"),
            0,
            n(|machine| {
                let mut args = machine.arguments.drain(..);
                let a = args.next().unwrap();
                let a = unsafe { a.view().array() }.unwrap();
                drop(args);
                let len = machine.local.make(&Literal::Integer(a.len() as _));
                machine.arguments.drain(..);
                machine.native_return(len);
            }),
        );
        self.make_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("clone"),
            2,
            n(|machine| {
                let mut args = machine.arguments.drain(..);
                let a = args.next().unwrap();
                let a = unsafe { a.view().array() }.unwrap();
                let offset = args.next().unwrap().view().to_integer().unwrap() as usize;
                let length = args.next().unwrap().view().to_integer().unwrap() as usize;
                drop(args);
                let cloned = machine.arena.allocate();
                unsafe {
                    cloned.write(Object::new_array(
                        a[offset..offset + length].to_vec().into(),
                    ))
                }
                machine.native_return(FrameRegister::Address(cloned));
            }),
        );
        self.make_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("at"),
            1,
            n(|machine| {
                let mut args = machine.arguments.drain(..);
                let a = args.next().unwrap();
                let a = unsafe { a.view().array() }.unwrap();
                let position = args.next().unwrap().view().to_integer().unwrap() as usize;
                drop(args);
                machine.native_return(FrameRegister::Address(a[position]));
                machine.arguments.drain(..);
            }),
        );
        self.make_dispatch(
            &mut function_dispatches,
            [s("Array")],
            s("at"),
            2,
            n(|machine| {
                let mut args = machine.arguments.drain(..);
                let a = args.next().unwrap();
                let a = unsafe { a.view().array() }.unwrap();
                let position = args.next().unwrap().view().to_integer().unwrap() as usize;
                let element = args.next().unwrap().escape(&mut machine.arena);
                drop(args);
                a[position] = element;
                let none = machine.local.make(&Literal::Nil);
                machine.native_return(none);
            }),
        );
        function_dispatches
    }

    fn visit(&mut self, context: &mut CollectContext<'_>) {
        for address in [
            &mut self.local.none,
            &mut self.local.bool_true,
            &mut self.local.bool_false,
        ] {
            context.process(address)
        }
        for address in self.symbol.object_names.values_mut() {
            context.process(address)
        }
        assert!(self.arguments.is_empty());
        let frame = self.frames.last().unwrap();
        for register in &mut self.registers[..frame.offset + frame.len] {
            match register {
                FrameRegister::Vacant => {}
                FrameRegister::Inline(object) => context.visit(object),
                FrameRegister::Address(address) => context.process(address),
            }
        }
    }
}
