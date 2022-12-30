const PROG: &str = r#"
make function greetings_line(name) {
    "Hello, " + name + "!"
}

inspect greetings_line("cowsay")
"#;

fn main() {
    println!("{PROG}");
}
