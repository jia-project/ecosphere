use std::{
    collections::hash_map::DefaultHasher,
    hash::Hasher,
    io::{stdout, LineWriter, Write},
    thread::spawn,
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker};

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);
    basic::parse::Module::new(
        "vanilla",
        include_str!("../src_ecs/vanilla.ecs"),
        &mut loader,
        &mem,
    )
    .load();
    basic::parse::Module::new(
        "vanilla.u64",
        include_str!("../src_ecs/vanilla.u64.ecs"),
        &mut loader,
        &mem,
    )
    .load();
    basic::parse::Module::new(
        "vanilla.siphash",
        include_str!("../src_ecs/vanilla.siphash.ecs"),
        &mut loader,
        &mem,
    )
    .load();
    basic::parse::Module::new(
        "prog",
        include_str!("../src_ecs/prog.ecs"),
        &mut loader,
        &mem,
    )
    .load();

    let t0 = Instant::now();
    let (worker_list, collect) = Worker::new_group(
        4,
        mem,
        loader,
        || basic::Op::new(),
        // line writer to trigger `SeqStdout` only once per line, so we can
        // actually have the per-line sync with it
        || LineWriter::new(SeqStdout),
        t0,
    );
    let collect = spawn(move || collect.run_loop());
    let _ = worker_list[0].spawn_main("prog.main");
    let worker_list: Vec<_> = worker_list
        .into_iter()
        .map(|mut worker| spawn(move || worker.run_loop()))
        .collect();
    for worker in worker_list {
        worker.join().unwrap();
    }
    collect.join().unwrap();

    let mut hasher = DefaultHasher::new();
    hasher.write("Hello, world!".as_bytes());
    println!("expected hash {:#x}", hasher.finish());
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
