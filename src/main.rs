use std::{env::args, fs};

use shattuck::{eval::Machine, grammar::parse};

fn main() {
    let mut module = parse(&fs::read_to_string(args().nth(1).unwrap()).unwrap());
    // for instruction in &mut *module.instructions {
    //     if let shattuck::Instruction::Effect(shattuck::instruction::Effect::MakeFunction(
    //         context_names,
    //         name,
    //         arity,
    //         module,
    //     )) = instruction
    //     {
    //         if &**name != "" {
    //             println!("{context_names:?}.{name}/{arity}:");
    //             for (i, instruction) in module.instructions.iter().enumerate() {
    //                 println!("  {i:6} {instruction:?}");
    //             }
    //         }
    //     }
    // }

    Machine::run_initial(module);
}
