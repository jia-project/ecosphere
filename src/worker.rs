use std::{
    collections::{HashSet, VecDeque},
    io::{LineWriter, Write},
    iter,
    mem::replace,
    ops::Deref,
    sync::{
        atomic::Ordering::SeqCst,
        mpsc::{channel, Receiver, Sender},
        Arc, Condvar, Mutex,
    },
    time::Instant,
};

use crate::{
    interp::Interp,
    loader::Loader,
    mem::{Mem, Mutator, Obj, MEM_STAT},
    ObjCore, Operator, TraceOut,
};

pub struct Worker {
    mem: Arc<Mem>,
    loader: Arc<Loader>,
    operator: Box<dyn Operator + Send>,
    ready_queue: ReadyQueue,
    ready_signal: Arc<Condvar>,
    task_pool: TaskPool,
    trace_out: LineWriter<TraceOut<Box<dyn Write + Send>>>,
    wake_collect: Sender<()>,
}

pub struct CollectWorker {
    mem: Arc<Mem>,
    task_pool: TaskPool,
    loader: Arc<Loader>,
    trace_out: Box<dyn Write + Send>,
    wake: Receiver<()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TaskAddr(*mut Obj);
unsafe impl Send for TaskAddr {}

type ReadyQueue = Arc<Mutex<VecDeque<TaskHandle>>>;
type TaskPool = Arc<Mutex<HashSet<TaskAddr>>>;

struct Task {
    interp: Interp,
    // something useful like a uuid?
}

// reason to put task on heap instead of owning in shared/exclusive state of
// worker: don't want to keep a reference to task cross interp steps
unsafe impl ObjCore for Task {
    fn name(&self) -> &str {
        "intrinsic.Task"
    }

    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.interp.trace(mark);
    }
}

#[derive(Debug, Clone)]
struct TaskHandle {
    addr: TaskAddr,
    // probably cannot implement TaskHandle as a plain Prod object, with status
    // as a managed mutex, because this shared status has an unbounded lifetime,
    // it may even outlive `Mem` if a wake token has been kept forever
    status: Arc<Mutex<TaskStatus>>,
}

#[derive(Debug)]
pub enum TaskStatus {
    Running(Vec<WakeToken>),
    Finished(*mut Obj),
    Canceled,
}
unsafe impl Send for TaskStatus {}

unsafe impl ObjCore for TaskHandle {
    fn name(&self) -> &str {
        "intrinsic.TaskHandle"
    }
}

impl Worker {
    pub fn new_group<O: Operator + Send + 'static, W: Write + Send + 'static>(
        count: usize,
        mem: Mem,
        loader: Loader,
        make_operator: impl Fn() -> O,
        make_trace: impl Fn() -> W,
        t0: Instant,
    ) -> (Vec<Self>, CollectWorker) {
        let mem = Arc::new(mem);
        let loader = Arc::new(loader);
        let ready_queue = Arc::new(Mutex::new(VecDeque::new()));
        let ready_signal = Arc::new(Condvar::new());
        let task_pool = Arc::new(Mutex::new(HashSet::new()));
        let (wake_tx, wake_rx) = channel();
        let collect_worker = CollectWorker {
            mem: mem.clone(),
            task_pool: task_pool.clone(),
            loader: loader.clone(),
            trace_out: Box::new(make_trace()),
            wake: wake_rx,
        };
        (
            iter::repeat_with(move || Self {
                mem: mem.clone(),
                loader: loader.clone(),
                operator: Box::new(make_operator()),
                ready_queue: ready_queue.clone(),
                ready_signal: ready_signal.clone(),
                task_pool: task_pool.clone(),
                trace_out: LineWriter::new(TraceOut::new(Box::new(make_trace()), t0)),
                wake_collect: wake_tx.clone(),
            })
            .take(count)
            .collect(),
            collect_worker,
        )
    }

    fn drive_task(&mut self, handle: TaskHandle) {
        let res = loop {
            let mem = self.mem.mutator();
            // we don't have to keep this lock, because we are holding a mutator
            // task object must be alive as long as it has not been canceled
            // at the point mutator is acquired
            let status = &*handle.status.lock().unwrap();
            assert!(!matches!(status, TaskStatus::Finished(..)));
            if matches!(status, TaskStatus::Canceled) {
                break None;
            }

            let TaskAddr(task) = handle.addr;
            let mut task_obj = unsafe { mem.write(task) };
            let task: &mut Task = task_obj.downcast_mut().unwrap();

            if let Some(res) = task.interp.get_result() {
                drop(task_obj);
                break Some((res, mem));
            }

            let mut interface = WorkerInterface {
                handle: &handle,
                mem: &mem,
                is_paused: false,
                loader: &self.loader,
                task_pool: &self.task_pool,
                ready_queue: &self.ready_queue,
                ready_signal: &self.ready_signal,
                trace_out: &mut self.trace_out,
            };
            task.interp.step(&mut interface, &mut *self.operator);
            if interface.is_paused() {
                break None;
            }
            drop(interface);

            let load_factor = self.mem.load_factor();
            if load_factor > 1. {
                writeln!(self.trace_out, "heap overflow load factor {load_factor:.3}").unwrap();
            }
            if load_factor >= Mem::COLLECT_THRESHOLD {
                self.wake_collect.send(()).unwrap();
            }
        };
        if let Some((res, mem)) = res {
            let status = &mut *handle.status.lock().unwrap();
            // avoid overriding a concurrent canceling
            // ensure there's only running->finished/canceled transition
            if matches!(status, TaskStatus::Running(..)) {
                let notify_list = if let TaskStatus::Running(notify_list) =
                    replace(status, TaskStatus::Finished(res))
                {
                    notify_list
                } else {
                    unreachable!()
                };
                for notify in notify_list {
                    notify.resolve(&mem, None);
                }
                let res = self.task_pool.lock().unwrap().remove(&handle.addr);
                assert!(res);
            } else {
                assert!(matches!(status, TaskStatus::Canceled));
            }
        }
    }

    pub fn run_loop(&mut self) {
        loop {
            let mut ready_queue = self.ready_queue.lock().unwrap();
            if let Some(ready) = ready_queue.pop_front() {
                drop(ready_queue);
                self.drive_task(ready);
            } else if self.task_pool.lock().unwrap().is_empty() {
                self.ready_signal.notify_all();
                return;
            } else {
                // a little bit weird...
                drop(self.ready_signal.wait(ready_queue).unwrap());
            }
        }
    }

    fn spawn_internal(
        interp: Interp,
        mem: &Mutator<'_>,
        ready_queue: &ReadyQueue,
        task_pool: &TaskPool,
    ) -> TaskHandle {
        let task = TaskHandle {
            addr: TaskAddr(mem.make(Task { interp })),
            status: Arc::new(Mutex::new(TaskStatus::Running(Vec::new()))),
        };
        task_pool.lock().unwrap().insert(task.addr);
        ready_queue.lock().unwrap().push_back(task.clone());
        task
    }

    pub fn spawn_main(&self, name: &str) -> impl Fn() -> Option<*mut Obj> {
        let mut interp = Interp::default();
        interp.push_call(name, Vec::new(), &self.mem.mutator(), &self.loader);
        let status = Self::spawn_internal(
            interp,
            &self.mem.mutator(),
            &self.ready_queue,
            &self.task_pool,
        )
        .status;
        move || match *status.lock().unwrap() {
            TaskStatus::Running(..) => None,
            TaskStatus::Finished(res) => Some(res),
            TaskStatus::Canceled => unreachable!("top level task never be canceled"),
        }
    }
}

#[derive(Debug)]
pub struct WakeToken {
    handle: TaskHandle,
    ready_queue: ReadyQueue,
    ready_signal: Arc<Condvar>,
    resolved: bool,
}

impl WakeToken {
    pub fn resolve(mut self, mem: &Mutator<'_>, res: Option<*mut Obj>) -> bool {
        let status = &*self.handle.status.lock().unwrap();
        assert!(!matches!(status, TaskStatus::Finished(..)));
        if matches!(status, TaskStatus::Canceled) {
            return false;
        }
        let TaskAddr(task) = self.handle.addr;
        // we are holding a mutator, which means no sweeping can happen concurrently
        // so it is safe to access task object
        let mut task = unsafe { mem.write(task) };
        let task: &mut Task = task.downcast_mut().unwrap();
        task.interp.resume(res);
        self.ready_queue
            .lock()
            .unwrap()
            .push_back(self.handle.clone());
        self.ready_signal.notify_one();
        self.resolved = true;
        true
    }
}

impl Drop for WakeToken {
    fn drop(&mut self) {
        assert!(self.resolved);
    }
}

pub struct WorkerInterface<'a> {
    handle: &'a TaskHandle,
    is_paused: bool,
    pub mem: &'a Mutator<'a>,
    pub loader: &'a Loader,
    ready_queue: &'a ReadyQueue,
    ready_signal: &'a Arc<Condvar>,
    task_pool: &'a TaskPool,
    pub trace_out: &'a mut dyn Write,
}

impl WorkerInterface<'_> {
    pub fn spawn(&self, interp: Interp) -> *mut Obj {
        self.mem.make(Worker::spawn_internal(
            interp,
            &self.mem,
            self.ready_queue,
            self.task_pool,
        ))
    }

    pub fn pause(&mut self) -> WakeToken {
        assert!(!self.is_paused);
        self.is_paused = true;
        WakeToken {
            handle: self.handle.clone(),
            ready_queue: self.ready_queue.clone(),
            ready_signal: self.ready_signal.clone(),
            resolved: false,
        }
    }

    /// # Safety
    /// `handle` must be valid and alive.
    pub unsafe fn wait(&mut self, handle: *mut Obj) {
        let handle = self.mem.read(handle);
        let handle: &TaskHandle = handle.downcast_ref().unwrap();
        let status = &mut *handle.status.lock().unwrap();
        if let TaskStatus::Running(notify_list) = status {
            notify_list.push(self.pause());
        }
    }

    pub fn cancel(&self, handle: *mut Obj) -> bool {
        fn read<'a>(
            mem: &'a Mutator<'_>,
            handle: *mut Obj,
        ) -> impl Deref<Target = dyn ObjCore> + 'a {
            unsafe { mem.read(handle) }
        }
        let handle = read(&self.mem, handle);
        let handle: &TaskHandle = handle.downcast_ref().unwrap();
        let status = &mut *handle.status.lock().unwrap();
        if matches!(status, TaskStatus::Running(..)) {
            let notify_list =
                if let TaskStatus::Running(notify_list) = replace(status, TaskStatus::Canceled) {
                    notify_list
                } else {
                    unreachable!()
                };
            let res = self.task_pool.lock().unwrap().remove(&handle.addr);
            assert!(res);
            // assert task pool is not empty
            // there should be at least the current running task still there
            for notify in notify_list {
                notify.resolve(&self.mem, None);
            }
            true
        } else {
            false
        }
    }

    pub fn is_paused(&self) -> bool {
        self.is_paused
    }
}

impl CollectWorker {
    pub fn run_loop(mut self) {
        while let Ok(()) = self.wake.recv() {
            if self.mem.load_factor() < Mem::COLLECT_THRESHOLD {
                continue;
            }
            let mut collector = self.mem.collector();
            let collect_start = Instant::now();
            let mut preload_list = Vec::new();
            self.loader.trace(|obj| preload_list.push(obj));

            let cap = unsafe {
                collector.collect(
                    self.task_pool
                        .lock()
                        .unwrap()
                        .iter()
                        .map(|&TaskAddr(task)| task)
                        // simply chaining is safe because preload object should never
                        // duplicate with runtime task object
                        .chain(preload_list),
                )
            };
            writeln!(
                self.trace_out,
                "[collector] finish in {:8?} next threshold {:.2}MiB",
                Instant::now() - collect_start,
                cap as f32 / (1 << 20) as f32 * Mem::COLLECT_THRESHOLD
            )
            .unwrap();
        }
        writeln!(
            self.trace_out,
            "[collect worker] exit allocated {:.2}MiB load {:.3}",
            MEM_STAT.allocated.load(SeqCst) as f32 / (1 << 20) as f32,
            self.mem.load_factor()
        )
        .unwrap();
    }
}
