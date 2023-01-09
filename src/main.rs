use shattuck::{eval::Machine, grammar::parse, Instruction};

const PROG: &str = r#"
make Run [
    * start
    * len
]

make Option [
    + None
    + Some
]

make merge_sort(v) {
    var MIN_RUN = 10;

    var len = v.length();
    # TODO insert sort for small list

    var runs = List.new();
    var end = len;
    while end > 0 {
        var start = end - 1;
        if start > 0 {
            start = start - 1;
            if v.get(start + 1) < v.get(start) {
                while start > 0 and v.get(start) < v.get(start - 1) {
                    start = start - 1;
                }
                reverse(v, start, end);
            } else {
                while start > 0 and not v.get(start) < v.get(start - 1) {
                    start = start - 1;
                }
            }
        }

        while start > 0 and end - start < MIN_RUN {
            start = start - 1;
            insert_head(v, start, end);
        }

        runs.push(Run[start = start, len = end - start]);
        end = start;

        while collapse(runs_start, runs_len) is Some r {
            var left = runs.get(r + 1);
            var right = runs.get(r);
            merge(v, left.start, right.start + right.len, left.len);
            runs.set(r, Run[start = left.start, len = left.len + right.len]);
            runs.remove(r + 1);
        }
    }

    assert runs.len() == 1;
    assert runs.get(0).start == 0;
    assert runs.get(0).len == len;
}
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
