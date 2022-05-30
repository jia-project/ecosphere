use std::{
    io::{stdout, Stdout, Write},
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker};

const TEXT: &str = r#"
func to_str(n is int) do
    let digit_table = basic.list()
    basic.list_push(digit_table, "0")
    basic.list_push(digit_table, "1")
    basic.list_push(digit_table, "2")
    basic.list_push(digit_table, "3")
    basic.list_push(digit_table, "4")
    basic.list_push(digit_table, "5")
    basic.list_push(digit_table, "6")
    basic.list_push(digit_table, "7")
    basic.list_push(digit_table, "8")
    basic.list_push(digit_table, "9")

    if n == 0 return "0"
    let s = ""
    while n != 0 do
        let old_s = s
        mut s = basic.str(basic.list_index(digit_table, n % 10))
        basic.str_push(s, old_s)
        mut n = n / 10
    end
    return s
end

func fib(n is int) do
    let i = 1
    let a = 1
    let b = 0
    while i < n do
        let c = a
        mut a = a + b
        mut b = c
        mut i = i + 1
    end
    return a
end

func main() do
    let s = basic.str("fib(10) = ")
    basic.str_push(s, .to_str(.fib(10)))
    basic.str_trace(s)
    return unit
end
"#;

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);
    basic::parse::Module::new("testbed", TEXT, &mut loader, &mem).load();

    let t0 = Instant::now();
    let (mut worker_list, collect) =
        Worker::new_group(1, mem, loader, || basic::Op::new(t0, TraceOut(stdout())));
    let worker = worker_list.pop().unwrap();
    let get_status = worker.spawn_main("testbed.main");
    worker.run_loop();

    println!("{:?}", get_status());
    collect.work();
}

struct TraceOut(Stdout);
impl Write for TraceOut {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().write_all(buf).map(|()| buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
