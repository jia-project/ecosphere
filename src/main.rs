use std::{env::args, fs};

use shattuck::{eval::Machine, grammar::parse, optimize};

fn main() {
    let mut module = parse(&fs::read_to_string(args().nth(1).unwrap()).unwrap());
    for instruction in &mut *module.instructions {
        if let shattuck::Instruction::MakeFunction(context_names, name, arity, module) = instruction
        {
            if &**name == "hex" {
                println!("{context_names:?}.{name}/{arity}:");
                *module = optimize(module, *arity);
            }
        }
    }

    Machine::run_initial(module);
}
