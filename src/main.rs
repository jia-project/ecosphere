use shattuck::{eval::Machine, grammar::parse};

const PROG: &str = r#"
make greetings_line(name) {
    "Hello, " + name + "!"
}

inspect greetings_line("cowsay");
"#;

fn main() {
    println!("{PROG}");
    let instructions = parse(PROG);
    println!("{instructions:?}");

    Machine::run_initial(instructions);
}
