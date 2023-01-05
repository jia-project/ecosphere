use std::{
    collections::HashMap,
    iter::repeat_with,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
    ptr::NonNull,
    time::Instant,
};

use crate::{
    arena::{Arena, ArenaUser},
    Instruction, InstructionLiteral, InstructionType, Object, ObjectAny, ObjectData, RegisterIndex,
    TypeIndex,
};

#[derive(Clone)]
struct Symbol {
    type_names: HashMap<String, usize>,
    types: Vec<TypeLayout>,
    function_dispatches: HashMap<(Box<[TypeIndex]>, usize), HashMap<String, usize>>,
}

impl Default for Symbol {
    fn default() -> Self {
        let mut type_names = HashMap::new();
        let mut types = Vec::new();
        type_names.insert(String::from(":intrinsic.nil"), types.len());
        types.push(TypeLayout {
            name: String::from("Nil"),
            layout: Default::default(),
        });
        Self {
            type_names,
            types,
            function_dispatches: Default::default(),
        }
    }
}

#[derive(Clone, Default)]
struct TypeLayout {
    name: String,
    layout: Box<[HashMap<String, usize>]>,
}

pub struct Machine {
    symbol: Symbol,
    frames: Vec<Frame>,
    arena: ArenaUser,
    instant_zero: Instant,
}

struct Frame {
    registers: [FrameRegister; 1 << RegisterIndex::BITS],
    return_register: RegisterIndex,
    function_index: usize,
    program_counter: usize,
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
                .take(1 << RegisterIndex::BITS)
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
        let mut function;
        'control: while !self.is_finished() {
            function = &functions[self.frames.last().unwrap().function_index];
            for instruction in &function[self.frames.last().unwrap().program_counter..] {
                self.frames.last_mut().unwrap().program_counter += 1;

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
                let control = self.execute(instruction);
                self.arena.mutate_exit();
                if control {
                    continue 'control;
                }
            }
            unreachable!("function instructions not ending with Return")
        }
    }

    fn make_function(&mut self, instruction: Instruction, functions: &mut Vec<Box<[Instruction]>>) {
        let Instruction::MakeFunction(context_types, name, n_argument, instructions) = instruction else {
            unreachable!()
        };
        let context_types = context_types
            .iter()
            .map(|name| match (&**name, self.symbol.type_names.get(name)) {
                (":intrinsic.nil", _) => todo!(),
                (":intrinsic.integer", _) => todo!(),
                (":intrinsic.string", _) => todo!(),
                (_, Some(type_index)) => *type_index as _,
                _ => panic!(),
            })
            .collect();
        let index = functions.len();
        let present = self
            .symbol
            .function_dispatches
            .entry((context_types, n_argument))
            .or_default()
            .insert(name, index);
        assert!(present.is_none());
        functions.push(instructions);
    }

    fn is_finished(&self) -> bool {
        self.frames.is_empty() // extra conditions like flags
    }

    fn execute(&mut self, instruction: &Instruction) -> bool {
        struct R<'a>(&'a mut Vec<Frame>);
        impl Index<&u8> for R<'_> {
            type Output = FrameRegister;
            fn index(&self, index: &u8) -> &Self::Output {
                &self.0.last().unwrap().registers[*index as usize]
            }
        }
        impl IndexMut<&u8> for R<'_> {
            fn index_mut(&mut self, index: &u8) -> &mut Self::Output {
                &mut self.0.last_mut().unwrap().registers[*index as usize]
            }
        }
        let mut r = R(&mut self.frames);

        use Instruction::*;
        match instruction {
            MakeLiteralObject(i, InstructionLiteral::Nil) => {
                r[i] = FrameRegister::Address(self.arena.allocate(ObjectData::Data(
                    0,
                    0,
                    Box::new([]),
                )))
            }
            MakeLiteralObject(i, InstructionLiteral::Integer(value)) => {
                r[i] = FrameRegister::Address(self.arena.allocate(ObjectData::Integer(*value)))
            }
            MakeLiteralObject(i, InstructionLiteral::String(value)) => {
                r[i] =
                    FrameRegister::Address(self.arena.allocate(ObjectData::String(value.clone())))
            }
            // make data object
            JumpIf(x, offset) => {
                let jumping = if *x == RegisterIndex::MAX {
                    true
                } else {
                    let ObjectData::Data(_, variant, _) = r[x].view() else {
                        panic!()
                    };
                    *variant != 0
                };
                if jumping {
                    assert_ne!(*offset, 0);
                    let program_counter = &mut self.frames.last_mut().unwrap().program_counter;
                    *program_counter = program_counter.checked_add_signed(*offset as _).unwrap();
                    return true;
                }
            }
            Return(x) => {
                let x = r[x].escape(&mut self.arena);
                let frame = self.frames.pop().unwrap();
                if !self.is_finished() {
                    let mut r = R(&mut self.frames);
                    r[&frame.return_register] = FrameRegister::Address(x);
                } //
                return true;
            }
            Call(i, context_xs, name, argument_xs) => {
                let mut xs = Vec::new();
                let mut context = Vec::new();
                for x in context_xs.iter() {
                    let x = &mut r[x];
                    context.push(match x.view() {
                        ObjectData::Data(type_index, _, _) => *type_index,
                        _ => todo!(),
                    });
                    xs.push(x.escape(&mut self.arena));
                }
                xs.extend(argument_xs.iter().map(|x| r[x].escape(&mut self.arena)));
                let index =
                    self.symbol.function_dispatches[&(context.into_boxed_slice(), xs.len())][name];
                self.push_frame(index, &xs, *i);
                return true;
            }

            Load(i, name) => todo!(),
            Inspect(x) => {
                let repr = match r[x].view() {
                    ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),

                    ObjectData::Integer(value) => format!("Integer {value}"),
                    ObjectData::String(value) => format!("String {value}"),
                    ObjectData::Data(type_index, variant, _) => {
                        format!(
                            "(data) {}.{variant}",
                            self.symbol.types[*type_index as usize].name
                        )
                    }
                    ObjectData::Any(value) => format!("(any) {}", value.type_name()),
                };
                println!(
                    "[{:>9.3?}] {:x?} {repr}",
                    Instant::now() - self.instant_zero,
                    r[x]
                );
            }
            ProductObjectGet(i, x, name) => {
                let ObjectData::Data(type_index, variant, data) = r[x].view() else {
                    panic!()
                };
                assert_eq!(*variant, 0);
                let layout = &self.symbol.types[*type_index as usize].layout;
                r[i] = FrameRegister::Address(data[layout[0][name]])
            }
            ProductObjectSet(x, name, y) => {
                let y = r[y].escape(&mut self.arena);
                let ObjectData::Data(type_index, variant, data) = r[x].view_mut() else {
                    panic!()
                };
                assert_eq!(*variant, 0);
                let layout = &self.symbol.types[*type_index as usize].layout;
                data[layout[0][name]] = y
            }
            Operator1(..) => unreachable!(),
            Operator2(i, op, x, y) => {
                use {crate::Operator2::*, ObjectData::*};
                let object = {
                    match (op, r[x].view(), r[y].view()) {
                        (Add, Integer(x), Integer(y)) => Integer(*x + *y),
                        (Add, String(x), String(y)) => String(x.clone() + y),
                        _ => panic!(),
                    }
                };
                r[i] = FrameRegister::Address(self.arena.allocate(object))
            }

            MakeType(name, layout) => {
                let type_index = self.symbol.types.len();
                self.symbol.type_names.insert(name.clone(), type_index);
                self.symbol.types.push(TypeLayout {
                    name: name.clone(),
                    layout: Self::generate_layout(layout),
                });
            }
            MakeFunction(..) => unreachable!(),
        }
        false
    }

    fn generate_layout(layout: &InstructionType) -> Box<[HashMap<String, usize>]> {
        todo!()
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
