use std::{env::args, fs};

use shattuck::{eval::Machine, grammar::parse, Instruction};

fn main() {
    let instructions = parse(&fs::read_to_string(args().nth(1).unwrap()).unwrap());
    for (i, instruction) in instructions.iter().enumerate() {
        if let Instruction::MakeFunction(context_names, name, arity, instructions) = instruction {
            println!("{context_names:?}.{name}/{arity}:");
            for (i, instruction) in instructions.iter().enumerate() {
                println!("    {i:4} {instruction:?}")
            }
        } else {
            println!("{i:4} {instruction:?}")
        }
    }

    Machine::run_initial(instructions);
    Machine::run_initial(Box::new([
        Instruction::MakeLiteralObject(0, shattuck::InstructionLiteral::Integer(40)),
        Instruction::MakeLiteralObject(1, shattuck::InstructionLiteral::Integer(2)),
        Instruction::Operator2(0, shattuck::Operator2::Add, 0, 1),
        Instruction::Inspect(0),
        Instruction::Return(0),
    ]));
}
