use std::{
    io::{stdout, LineWriter, Write},
    thread::spawn,
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker, TraceOut};

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);
    basic::parse::Module::new("lab", include_str!("lab.ecs"), &mut loader, &mem).load();

    let t0 = Instant::now();
    let (worker_list, collect) = Worker::new_group(
        4,
        mem,
        loader,
        || basic::Op::new(),
        // inner line writer: to trigger `SeqStdout` only once per line, so we
        // can antually have the per-line sync with it
        // outer line writer: to trigger `TraceOut` once per line, so we will
        // not have multiple timestamp on one line
        || LineWriter::new(TraceOut::new(LineWriter::new(SeqStdout), t0)),
    );
    let collect = spawn(move || collect.run_loop());
    let _ = worker_list[0].spawn_main("lab.main");
    let worker_list: Vec<_> = worker_list
        .into_iter()
        .map(|mut worker| spawn(move || worker.run_loop()))
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
