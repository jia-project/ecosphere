use cosphere::{
    vanilla::{op, spec},
    vm, Loader, Mem,
};

const INPUT: &str = r#"
fn ne(a, b) return not(a == b); end
fn le(a, b) return a < b or a == b; end
fn ge(a, b) return not(a < b); end
fn gt(a, b) return not(a <= b); end
fn sub(a, b) return a + -b; end
fn neg(a Result)
    if a then return false; else return true; end
end

fn str(n Int)
    if n == 0 then return "0"; end

    let dict = List();
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
    
    let s = "";
    while n != 0 then
        mut s = get(dict, n % 10) + s;
        mut n = n / 10;
    end
    return s;
end

fn str(l List)
    let s = "[";
    let sep = "";
    let i = 0;
    let len = len(l);
    while i < len then
        mut s = s + sep + str(get(l, i));
        mut sep = ", ";
        mut i = i + 1;
    end
    mut s = s + "]";
    return s;
end

fn sort(l)
    sort(l, 0, len(l));
    return nil;
end

fn sort(l, low, high)
    if low + 1 >= high then return nil; end
    let pivot = get(l, high - 1);
    let pivot_i = low - 1;
    let i = low;
    while i < high - 1 then
        if get(l, i) <= pivot then
            mut pivot_i = pivot_i + 1;
            if pivot_i != i then swap(l, pivot_i, i); end
        end
        mut i = i + 1;
    end
    mut pivot_i = pivot_i + 1;
    swap(l, pivot_i, high - 1);

    sort(l, low, pivot_i);
    sort(l, pivot_i + 1, high);
    return nil;
end

fn swap(l, i, j)
    let tmp = get(l, i);
    get_mut(l, i, get(l, j));
    get_mut(l, j, tmp);
    return nil;
end
 
fn main()
    let l = List();
    push(l, 3);
    push(l, 14);
    push(l, 15);
    push(l, 92);
    push(l, 65);
    trace(str(l));
    sort(l)
    trace(str(l));
    return nil;
end
"#;

fn main() {
    let mut loader = Loader::default();
    let mut mem = Mem::default();
    op::Eval::install(&mut loader);

    spec::Context::new(INPUT, &mut loader).parse();
    loader.populate();
    // loader.dump_blocks();

    let mut op = op::Eval::default();
    let mut vm = vm::Context::new(&mut mem, &loader, &mut op);
    vm.eval_call("main", &[]);
    while vm.eval_instr() {}
}
