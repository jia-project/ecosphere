use pest::Parser as _;
use shattuck::{
    eval::Machine, parse::Rule, Instruction, InstructionLiteral, Operator2, Parser, RegisterIndex,
};

const PROG: &str = r#"
make greetings_line(name) {
    "Hello, " + name + "!"
}

inspect greetings_line("cowsay");
"#;

fn main() {
    let func_gl = Box::new([
        Instruction::MakeLiteralObject(1, InstructionLiteral::String(String::from("Hello, "))),
        Instruction::Operator2(0, Operator2::Add, 1, 0),
        Instruction::MakeLiteralObject(1, InstructionLiteral::String(String::from("!"))),
        Instruction::Operator2(0, Operator2::Add, 0, 1),
        Instruction::Return(0),
    ]);
    let func_name = || String::from("greetings_line");
    let instructions = Box::new([
        Instruction::MakeFunction(Box::new([]), func_name(), 1, func_gl),
        Instruction::MakeLiteralObject(0, InstructionLiteral::String(String::from("cowsay"))),
        Instruction::Call(0, Box::new([]), func_name(), Box::new([0])),
        Instruction::Inspect(0),
        Instruction::Return(RegisterIndex::MAX),
    ]);

    println!("{PROG}");
    dbg!(Parser::parse(Rule::Program, PROG).unwrap());

    Machine::run_initial(instructions);
}
