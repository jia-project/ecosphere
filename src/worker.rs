use std::{
    collections::HashMap,
    iter,
    sync::{
        atomic::{AtomicU32, Ordering::SeqCst},
        Arc, Mutex,
    },
};

use crate::{
    interp::{Interp, InterpStatus},
    loader::Loader,
    mem::{Mem, Obj},
    Op,
};

type TaskId = u32;
type WakeId = u32;
type PauseTable = Arc<Mutex<HashMap<(TaskId, WakeId), Task>>>;
type ReadyQueue = Arc<Mutex<Vec<Ready>>>;

pub struct Worker<O: Op> {
    mem: Arc<Mem>,
    loader: Arc<Loader>,
    context: O::Worker,
    ready_queue: ReadyQueue,
    pause_table: PauseTable,
    task_id: Arc<AtomicU32>,
}

struct Ready {
    id: TaskId,
    wake: WakeId,
    task: Task,
}

struct Task {
    interp: Interp,
    handle: *mut Obj,
}

impl<O: Op> Worker<O> {
    pub fn repeat(
        mem: Mem,
        loader: Loader,
        make_context: impl Fn() -> O::Worker,
    ) -> impl Iterator<Item = Self> {
        let mem = Arc::new(mem);
        let loader = Arc::new(loader);
        let ready_queue = Arc::new(Mutex::new(Vec::new()));
        let pause_table = Arc::new(Mutex::new(HashMap::new()));
        let task_id = Arc::new(AtomicU32::new(0));
        iter::repeat_with(move || Self {
            mem: mem.clone(),
            loader: loader.clone(),
            context: make_context(),
            ready_queue: ready_queue.clone(),
            pause_table: pause_table.clone(),
            task_id: task_id.clone(),
        })
    }

    unsafe fn drive_task(&mut self, ready: Ready) {
        let Ready { mut task, id, wake } = ready;
        assert_eq!(task.interp.get_status(), InterpStatus::Running);
        let wake_token = WakeToken {
            id: wake,
            task: id,
            pause_table: self.pause_table.clone(),
            ready_queue: self.ready_queue.clone(),
        };
        let interface = WorkerInterface {
            wake_token,
            ready_queue: self.ready_queue.clone(),
            task_id: self.task_id.clone(),
        };
        while task.interp.get_status() == InterpStatus::Running {
            // TODO check handle status whether canceled
            task.interp.step::<O>(
                self.mem.mutator(),
                &self.loader,
                &mut self.context,
                &interface,
            );
        }
        if task.interp.get_status() == InterpStatus::Paused {
            self.pause_table.lock().unwrap().insert((id, wake), task);
        } else {
            // TODO record result into handle
        }
    }

    pub unsafe fn run_loop(&mut self) {
        loop {
            if let Some(ready) = {
                let mut ready_queue = self.ready_queue.lock().unwrap();
                ready_queue.pop()
            } {
                self.drive_task(ready);
            } else {
                // park
            }
        }
    }
}

#[derive(Clone)]
pub struct WakeToken {
    id: WakeId,
    task: TaskId,
    pause_table: PauseTable,
    ready_queue: ReadyQueue,
}

impl WakeToken {
    pub fn resolve(self, res: *mut Obj) -> bool {
        if let Some(mut task) = self
            .pause_table
            .lock()
            .unwrap()
            .remove(&(self.task, self.id))
        {
            task.interp.resume(res);
            self.ready_queue.lock().unwrap().push(Ready {
                id: self.task,
                wake: self.id + 1,
                task,
            });
            true
        } else {
            false
        }
    }
}

pub struct WorkerInterface {
    pub wake_token: WakeToken,
    ready_queue: ReadyQueue,
    task_id: Arc<AtomicU32>,
}

impl WorkerInterface {
    pub fn spawn(&self, interp: Interp, handle: *mut Obj) -> TaskId {
        let id = self.task_id.fetch_add(1, SeqCst);
        let ready = Ready {
            id,
            wake: 0,
            task: Task { interp, handle },
        };
        self.ready_queue.lock().unwrap().push(ready);
        id
    }
}
