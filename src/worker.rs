use std::{
    collections::{HashSet, VecDeque},
    iter,
    sync::{Arc, Mutex},
};

use crate::{
    interp::Interp,
    loader::Loader,
    mem::{Mem, Mutator, Obj},
    ObjCore, Operator,
};

pub struct Worker {
    mem: Arc<Mem>,
    loader: Arc<Loader>,
    operator: Box<dyn Operator>,
    ready_queue: ReadyQueue,
    task_pool: TaskPool,
}

pub struct CollectWorker {
    mem: Arc<Mem>,
    task_pool: TaskPool,
}

type ReadyQueue = Arc<Mutex<VecDeque<TaskHandle>>>;
type TaskPool = Arc<Mutex<HashSet<*mut Obj>>>;

struct Task {
    interp: Interp,
    // something useful like a uuid?
}

impl ObjCore for Task {
    fn name(&self) -> &str {
        "intrinsic.Task"
    }

    fn trace(&self, mark: &mut dyn FnMut(*mut Obj)) {
        self.interp.trace(mark);
    }

    // TODO impl alloc size correctly to reflect Interp's on-heap allocation
}

#[derive(Debug, Clone)]
struct TaskHandle {
    addr: *mut Obj,
    // probably cannot implement TaskHandle as a plain Prod object, with status
    // as a managed mutex, because this shared status has an unbounded lifetime,
    // it may even outlive `Mem` if a wake token has been kept forever
    status: Arc<Mutex<TaskStatus>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    Running, // TODO notify list
    Finished(*mut Obj),
    Canceled,
}

impl ObjCore for TaskHandle {
    fn name(&self) -> &str {
        "intrinsic.TaskHandle"
    }
}

impl Worker {
    pub fn new_group<O: Operator + 'static>(
        count: usize,
        mem: Mem,
        loader: Loader,
        make_operator: impl Fn() -> O,
    ) -> (Vec<Self>, CollectWorker) {
        let mem = Arc::new(mem);
        let loader = Arc::new(loader);
        let ready_queue = Arc::new(Mutex::new(VecDeque::new()));
        let task_pool = Arc::new(Mutex::new(HashSet::new()));
        let collect_worker = CollectWorker {
            mem: mem.clone(),
            task_pool: task_pool.clone(),
        };
        (
            iter::repeat_with(move || Self {
                mem: mem.clone(),
                loader: loader.clone(),
                operator: Box::new(make_operator()),
                ready_queue: ready_queue.clone(),
                task_pool: task_pool.clone(),
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
            let status = *handle.status.lock().unwrap();
            assert!(!matches!(status, TaskStatus::Finished(..)));
            if status == TaskStatus::Canceled {
                break None;
            }

            let mut task = unsafe { mem.write(handle.addr) };
            let task: &mut Task = task.downcast_mut().unwrap();

            if let Some(res) = task.interp.get_result() {
                break Some(res);
            }

            let mut interface = WorkerInterface {
                handle: handle.clone(),
                ready_queue: self.ready_queue.clone(),
                task_pool: self.task_pool.clone(),
                mem,
                loader: &self.loader,
                is_paused: false,
            };
            task.interp.step(&mut interface, &mut *self.operator);
            if interface.is_paused() {
                break None;
            }
        };
        if let Some(res) = res {
            let mut status = handle.status.lock().unwrap();
            // avoid overriding a concurrent canceling
            // ensure there's only running->finished/canceled transition
            if *status == TaskStatus::Running {
                *status = TaskStatus::Finished(res);
                // TODO notify subscribers
                self.task_pool.lock().unwrap().remove(&handle.addr);
            }
        }
    }

    pub fn run_loop(mut self) {
        loop {
            if let Some(ready) = {
                let mut ready_queue = self.ready_queue.lock().unwrap();
                ready_queue.pop_front()
            } {
                self.drive_task(ready);
            } else if self.task_pool.lock().unwrap().is_empty() {
                return;
            } else {
                // park
            }
        }
    }

    fn spawn_internal(interp: Interp, mem: &Mutator<'_>, ready_queue: &ReadyQueue) -> TaskHandle {
        let task = TaskHandle {
            addr: mem.new(Task { interp }),
            status: Arc::new(Mutex::new(TaskStatus::Running)),
        };
        ready_queue.lock().unwrap().push_back(task.clone());
        task
    }

    pub fn spawn(&self, interp: Interp) -> impl Fn() -> TaskStatus {
        let status = Self::spawn_internal(interp, &self.mem.mutator(), &self.ready_queue).status;
        move || *status.lock().unwrap()
    }
}

pub struct WakeToken {
    handle: TaskHandle,
    ready_queue: ReadyQueue,
    resolved: bool,
}

impl WakeToken {
    pub fn resolve(mut self, mem: Mutator<'_>, res: *mut Obj) -> bool {
        let status = *self.handle.status.lock().unwrap();
        assert_ne!(status, TaskStatus::Running);
        if status == TaskStatus::Canceled {
            return false;
        }
        // we are holding a mutator, which means no sweeping can happen concurrently
        // so it is safe to access task object
        let mut task = unsafe { mem.write(self.handle.addr) };
        let task: &mut Task = task.downcast_mut().unwrap();
        task.interp.resume(res);
        self.ready_queue
            .lock()
            .unwrap()
            .push_back(self.handle.clone());
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
    handle: TaskHandle,
    ready_queue: ReadyQueue,
    task_pool: TaskPool,
    is_paused: bool,
    pub mem: Mutator<'a>,
    pub loader: &'a Loader,
}

impl WorkerInterface<'_> {
    pub fn spawn(&self, interp: Interp) -> *mut Obj {
        self.mem
            .new(Worker::spawn_internal(interp, &self.mem, &self.ready_queue))
    }

    pub fn pause(&mut self) -> WakeToken {
        assert!(!self.is_paused);
        self.is_paused = true;
        WakeToken {
            handle: self.handle.clone(),
            ready_queue: self.ready_queue.clone(),
            resolved: false,
        }
    }

    pub fn cancel(&self, handle: *mut Obj) -> bool {
        let handle = unsafe { self.mem.read(handle) };
        let handle: &TaskHandle = handle.downcast_ref().unwrap();
        let mut status = handle.status.lock().unwrap();
        if *status == TaskStatus::Running {
            *status = TaskStatus::Canceled;
            self.task_pool.lock().unwrap().remove(&handle.addr);
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
    pub fn work(&self) {
        let mut collector = self.mem.collector();
        unsafe { collector.collect(self.task_pool.lock().unwrap().iter().copied()) };
    }
}
