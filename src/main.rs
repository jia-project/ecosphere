use std::{
    io::{stdout, LineWriter, Stdout, Write},
    thread::spawn,
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker, TraceOut};

const TEXT: &str = r#"
func to_str(n is int) do
    let digit_table = basic.list()
    run basic.list_push(digit_table, "0")
    run basic.list_push(digit_table, "1")
    run basic.list_push(digit_table, "2")
    run basic.list_push(digit_table, "3")
    run basic.list_push(digit_table, "4")
    run basic.list_push(digit_table, "5")
    run basic.list_push(digit_table, "6")
    run basic.list_push(digit_table, "7")
    run basic.list_push(digit_table, "8")
    run basic.list_push(digit_table, "9")

    if n == 0 return "0"
    let s = ""
    while n != 0 do
        let old_s = s
        mut s = basic.str(basic.list_index(digit_table, n % 10))
        run basic.str_push(s, old_s)
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

func trace_hi(id is int) do
    let s = "hello from task "
    run basic.str_push(s, .to_str(id))
    run basic.str_trace(s)
    return _
end

func main() do
    let h1 = spawn .trace_hi(117)
    let h2 = spawn .trace_hi(418)
    wait h1
    wait h2
    run basic.str_trace("all done")
    return _
end
"#;

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);
    basic::parse::Module::new("testbed", TEXT, &mut loader, &mem).load();

    let t0 = Instant::now();
    let (mut worker_list, collect) = Worker::new_group(
        1,
        mem,
        loader,
        || basic::Op::new(),
        || LineWriter::new(TraceOut::new(SeqStdout(stdout()), t0)),
    );
    let collect = spawn(move || collect.run_loop());
    let worker = worker_list.pop().unwrap();
    let _ = worker.spawn_main("testbed.main");
    worker.run_loop();
    collect.join().unwrap();
}

struct SeqStdout(Stdout);
impl Write for SeqStdout {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().write_all(buf).map(|()| buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
