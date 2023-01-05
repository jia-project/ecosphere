use shattuck::{eval::Machine, grammar::parse, Instruction};

const PROG: &str = r#"
make greetings_line(name) {
    "Hello, " + name + "!"
}

inspect greetings_line("cowsay");
"#;

fn main() {
    println!("{PROG}");
    let instructions = parse(PROG);
    for instruction in &*instructions {
        if let Instruction::MakeFunction(context_names, name, arity, instructions) = instruction {
            println!("{context_names:?}.{name}/{arity}:");
            for instruction in &**instructions {
                println!("  {instruction:?}")
            }
        } else {
            println!("{instruction:?}")
        }
    }

    Machine::run_initial(instructions);
}
