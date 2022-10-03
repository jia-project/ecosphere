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

fn str(l List)
    let s;
    mut s = "[";
    let sep;
    mut sep = "";
    let i;
    mut i = 0;
    let len;
    mut len = len(l);
    while i < len then
        mut s = s + sep + str(get(l, i));
        mut sep = ", ";
        mut i = i + 1;
    end
    mut s = s + "]";
    return s;
end

fn main()
    let l;
    mut l = List();
    push(l, 3);
    push(l, 14);
    push(l, 15);
    push(l, 92);
    push(l, 65);
    trace(str(l));
end
"#;

fn main() {
    let mut loader = Loader::default();
    let mut mem = Mem::default();
    op::Eval::install(&mut loader);

    spec::Context::new(INPUT, &mut loader).parse();
    loader.populate();

    // loader.try_it();
    vm::Context::new(&mut mem, &loader, &mut op::Eval::default()).eval_call("main", &[]);
}
