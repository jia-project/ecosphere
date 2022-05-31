use std::{
    io::{stdout, LineWriter, Write},
    thread::spawn,
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker, TraceOut};

const TEXT: &str = r#"
func to_str(n is int) do
    let digit_table = _.basic.list()
    run _.basic.list_push(digit_table, "0")
    run _.basic.list_push(digit_table, "1")
    run _.basic.list_push(digit_table, "2")
    run _.basic.list_push(digit_table, "3")
    run _.basic.list_push(digit_table, "4")
    run _.basic.list_push(digit_table, "5")
    run _.basic.list_push(digit_table, "6")
    run _.basic.list_push(digit_table, "7")
    run _.basic.list_push(digit_table, "8")
    run _.basic.list_push(digit_table, "9")

    if n == 0 return "0"
    let s = ""
    while n != 0 do
        let old_s = s
        mut s = _.basic.str(_.basic.list_index(digit_table, n % 10))
        run _.basic.str_push(s, old_s)
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
    let s = "[task "
    run _.basic.str_push(s, to_str(id))
    let i = 0
    while i < 10 do
        let s = _.basic.str(s)
        run _.basic.str_push(s, "] hi #")
        run _.basic.str_push(s, to_str(i))
        run _.basic.str_trace(s)
        mut i = i + 1
    end
    return _
end

func main() do
    let h1 = spawn trace_hi(117)
    let h2 = spawn trace_hi(418)
    wait h1
    wait h2
    run _.basic.str_trace("all done")
    return _
end
"#;

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);
    basic::parse::Module::new("testbed", TEXT, &mut loader, &mem).load();

    let t0 = Instant::now();
    let (worker_list, collect) = Worker::new_group(
        4,
        mem,
        loader,
        || basic::Op::new(),
        || LineWriter::new(TraceOut::new(LineWriter::new(SeqStdout), t0)),
    );
    let collect = spawn(move || collect.run_loop());
    let _ = worker_list[0].spawn_main("testbed.main");
    let worker_list: Vec<_> = worker_list
        .into_iter()
        .map(|mut worker| {
            spawn(move || {
                while !worker.run_loop() {
                    // TODO park
                }
            })
        })
        .collect();
    for worker in worker_list {
        worker.join().unwrap();
    }
    collect.join().unwrap();
}

struct SeqStdout;
impl Write for SeqStdout {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        stdout().lock().write_all(buf).map(|()| buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
