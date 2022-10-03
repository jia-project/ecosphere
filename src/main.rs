use cosphere::{
    vanilla::{op, spec},
    vm, Loader, Mem,
};

const INPUT: &str = r#"
fn main()
    trace(42);
    return nil;
end
"#;

fn main() {
    let mut loader = Loader::default();
    let mut mem = Mem::default();
    let mut op_context = op::Eval::install(&mut loader);
    spec::Context::new(INPUT, &mut loader).parse();
    loader.populate();
    vm::Context::new(&mut mem, &loader, &mut op_context).eval_call("main", &[]);
}
