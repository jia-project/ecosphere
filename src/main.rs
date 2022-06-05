use std::{
    collections::HashSet,
    fs::read_to_string,
    io::{stdout, LineWriter, Write},
    path::Path,
    thread::spawn,
    time::Instant,
};

use ecosphere::{basic, loader::Loader, mem::Mem, worker::Worker};

fn main() {
    let mem = Mem::default();
    let mut loader = Loader::default();
    basic::Op::load(&mut loader);

    let mut load_buffer = vec!["prog".to_string()];
    let mut loading_set: HashSet<_> = load_buffer.iter().cloned().collect();
    while let Some(module) = load_buffer.pop() {
        let source = read_to_string(Path::new(&format!("src_ecs/{module}.ecs"))).unwrap();
        let mut module = basic::parse::Module::new(&module, &source, &mut loader, &mem);
        module.load();
        for module in module.load_list {
            if !loading_set.contains(&module) {
                loading_set.insert(module.clone());
                load_buffer.push(module);
            }
        }
    }

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
