use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    ptr::null_mut,
    sync::Arc,
    time::Instant,
};

use crate::{
    arena::Arena, Instruction, InstructionLiteral, Object, ObjectData, RegisterIndex, TypeIndex,
};

#[derive(Clone, Default)]
struct Symbol {
    product_type_names: HashMap<String, usize>,
    product_types: Vec<ProductType>,
    sum_type_names: HashMap<String, usize>,
    sum_types: Vec<SumType>,
    function_dispatches: HashMap<(Box<[TypeIndex]>, usize), HashMap<String, usize>>,
}

#[derive(Clone, Default)]
struct ProductType {
    name: String,
    fields: HashMap<String, usize>,
}

#[derive(Clone, Default)]
struct SumType {
    name: String,
    variants: HashMap<String, u8>,
    variant_names: Box<[String]>,
}

pub struct Machine {
    symbol: Symbol,
    frames: Vec<Frame>,
    arena: Arc<Arena>,
    instant_zero: Instant,
}

struct Frame {
    registers: [*mut Object; 1 << RegisterIndex::BITS],
    return_register: RegisterIndex,
    function_index: usize,
    program_counter: usize,
}

impl Machine {
    pub fn run_initial(instructions: Box<[Instruction]>) {
        let mut machine = Machine {
            symbol: Default::default(),
            frames: Default::default(),
            arena: Default::default(),
            instant_zero: Instant::now(),
        };
        machine.push_frame(0, &[], RegisterIndex::MAX);
        machine.run(vec![instructions]);
    }

    fn push_frame(
        &mut self,
        index: usize,
        arguments: &[*mut Object],
        return_register: RegisterIndex,
    ) {
        let mut frame = Frame {
            registers: [null_mut(); 1 << RegisterIndex::BITS],
            return_register,
            function_index: index,
            program_counter: 0,
        };
        frame.registers[..arguments.len()].copy_from_slice(arguments);
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
                    // we are in the middle of iterating `functions`
                    // not on hot path so just conform it
                    continue 'control;
                }

                if self.execute(instruction) {
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
            .map(|name| {
                match (
                    &**name,
                    self.symbol.product_type_names.get(name),
                    self.symbol.sum_type_names.get(name),
                ) {
                    ("Integer", _, _) => todo!(),
                    ("String", _, _) => todo!(),
                    (_, Some(type_index), _) => *type_index as _,
                    (_, _, Some(type_index)) => todo!(),
                    _ => panic!(),
                }
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
            type Output = *mut Object;
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
            MakeLiteralObject(i, InstructionLiteral::Integer(value)) => {
                r[i] = self.arena.allocate(ObjectData::Integer(*value))
            }
            MakeLiteralObject(i, InstructionLiteral::String(value)) => {
                r[i] = self.arena.allocate(ObjectData::String(value.clone()))
            }
            MakeProductTypeObject(i, type_name, fields) => {
                let type_index = self.symbol.product_type_names[type_name];
                let layout = &self.symbol.product_types[type_index].fields;
                let mut data = vec![null_mut(); layout.len()].into_boxed_slice();
                for (name, i) in fields.iter() {
                    data[layout[name]] = r[i];
                }
                assert!(data.iter().all(|p| !p.is_null()));
                r[i] = self
                    .arena
                    .allocate(ObjectData::Product(type_index as _, data))
            }
            MakeSumTypeObject(i, type_name, variant_name, x) => {
                let type_index = self.symbol.sum_type_names[type_name];
                let variant = self.symbol.sum_types[type_index].variants[variant_name];
                r[i] = self
                    .arena
                    .allocate(ObjectData::Sum(type_index as _, variant, r[x]))
            }

            JumpIf(x, offset) => {
                let jumping = if *x == RegisterIndex::MAX {
                    true
                } else {
                    let condition = self.arena.view(r[x]);
                    let condition = unsafe { &(**condition).data };
                    let ObjectData::Sum(_, variant, _) = condition else {
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
                let x = r[x];
                let frame = self.frames.pop().unwrap();
                if !self.is_finished() {
                    let mut r = R(&mut self.frames);
                    r[&frame.return_register] = x;
                }
                return true;
            }
            Call(i, context_xs, name, argument_xs) => {
                let mut xs = context_xs.iter().map(|x| r[x]).collect::<Vec<_>>();
                let context = xs
                    .iter()
                    .map(|&x| match unsafe { &(**self.arena.view(x)).data } {
                        ObjectData::Product(type_index, _) => *type_index,
                        _ => todo!(),
                    })
                    .collect::<Box<_>>();
                xs.extend(argument_xs.iter().map(|x| r[x]));
                let index = self.symbol.function_dispatches[&(context, xs.len())][name];
                self.push_frame(index, &xs, *i);
                return true;
            }

            Inspect(x) => {
                // is that `view` guard dropped too early?
                let repr = match unsafe { &(**self.arena.view(r[x])).data } {
                    ObjectData::Vacant | ObjectData::Forwarded(_) => unreachable!(),

                    ObjectData::Integer(value) => format!("Integer {value}"),
                    ObjectData::String(value) => format!("String {value}"),
                    ObjectData::Product(type_index, _) => {
                        format!(
                            "(product) {}",
                            self.symbol.product_types[*type_index as usize].name
                        )
                    }
                    ObjectData::Sum(type_index, variant, _) => {
                        let meta = &self.symbol.sum_types[*type_index as usize];
                        format!(
                            "(sum) {}.{}",
                            meta.name, meta.variant_names[*variant as usize]
                        )
                    }
                    ObjectData::Any(value) => format!("(any) {}", value.type_name()),
                };
                println!(
                    "[{:>9.3?}] {:#x?} {repr}",
                    Instant::now() - self.instant_zero,
                    r[x]
                );
            }
            ProductObjectGet(i, x, name) => {
                let object = self.arena.view(r[x]);
                let object = unsafe { &(**object).data };
                let ObjectData::Product(type_index, data) = object else {
                    panic!()
                };
                let layout = &self.symbol.product_types[*type_index as usize].fields;
                r[i] = data[layout[name]]
            }
            ProductObjectSet(x, name, y) => {
                let object = self.arena.view_mut(r[x]);
                let object = unsafe { &mut (**object).data };
                let ObjectData::Product(type_index, data) = object else {
                    panic!()
                };
                let layout = &self.symbol.product_types[*type_index as usize].fields;
                data[layout[name]] = r[y]
            }
            Operator1(..) => unreachable!(),
            Operator2(i, op, x, y) => {
                use {crate::Operator2::*, ObjectData::*};
                let object = {
                    let (x, y) = (self.arena.view(r[x]), self.arena.view(r[y]));
                    match unsafe { (op, &(**x).data, &(**y).data) } {
                        (Add, Integer(x), Integer(y)) => Integer(*x + *y),
                        (Add, String(x), String(y)) => String(x.clone() + y),
                        _ => panic!(),
                    }
                };
                r[i] = self.arena.allocate(object)
            }

            MakeProductType(name, fields) => {
                let type_index = self.symbol.product_types.len();
                self.symbol
                    .product_type_names
                    .insert(name.clone(), type_index);
                self.symbol.product_types.push(ProductType {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .enumerate()
                        .map(|(i, name)| (name.clone(), i))
                        .collect(),
                });
            }
            MakeSumType(name, variants) => {
                let type_index = self.symbol.sum_types.len();
                self.symbol.sum_type_names.insert(name.clone(), type_index);
                self.symbol.sum_types.push(SumType {
                    name: name.clone(),
                    variants: variants
                        .iter()
                        .enumerate()
                        .map(|(i, name)| (name.clone(), i as _))
                        .collect(),
                    variant_names: variants.clone(),
                })
            }
            MakeFunction(..) => unreachable!(),
        }
        false
    }
}
