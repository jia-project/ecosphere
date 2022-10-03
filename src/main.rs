use cosphere::{
    vanilla::{op, spec},
    vm, Loader, Mem,
};

const INPUT: &str = r#"
fn str(n Int)
    if n == 0 then return "0"; end

    let dict;
    mut dict = List();
    push(dict, "0");
    push(dict, "1");
    push(dict, "2");
    push(dict, "3");
    push(dict, "4");
    push(dict, "5");
    push(dict, "6");
    push(dict, "7");
    push(dict, "8");
    push(dict, "9");
    
    let s;
    mut s = "";
    while n != 0 then
        mut s = get(dict, n % 10) + s;
        mut n = n / 10;
    end
    return s;
end

struct Point
    x y
end

fn str(p Point)
    return "Point {" + str(x(p)) + ", " + str(y(p)) + "}";
end

fn main()
    trace(str(Point(42, 43)));
    return nil;
end
"#;

fn main() {
    let mut loader = Loader::default();
    let mut mem = Mem::default();
    let mut op_context = op::Eval::install(&mut loader);
    spec::Context::new(INPUT, &mut loader).parse();
    loader.populate();
    // loader.try_it();
    vm::Context::new(&mut mem, &loader, &mut op_context).eval_call("main", &[]);
}
