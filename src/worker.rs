use std::{
    collections::HashMap,
    iter,
    sync::{
        atomic::{AtomicU32, Ordering::SeqCst},
        Arc, Mutex,
    },
};

use crate::{
    interp::Interp,
    loader::Loader,
    mem::{Mem, Obj},
    Op,
};

type TaskId = u32;
type PauseTable = Arc<Mutex<HashMap<TaskId, Task>>>;
type ReadyList = Arc<Mutex<Vec<Task>>>;

pub struct Worker<O: Op> {
    mem: Arc<Mem>,
    loader: Arc<Loader>,
    context: O::Worker,
    ready_list: ReadyList,
    pause_table: PauseTable,
    task_id: Arc<AtomicU32>,
}

struct Task {
    id: TaskId,
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
            ready_list: ready_queue.clone(),
            pause_table: pause_table.clone(),
            task_id: task_id.clone(),
        })
    }

    unsafe fn drive_task(&mut self, mut task: Task) {
        assert!(task.interp.get_result().is_none());
        let interface = WorkerInterface {
            task_id: task.id,
            ready_list: self.ready_list.clone(),
            pause_table: self.pause_table.clone(),
            global_id: self.task_id.clone(),
        };

        // TODO not canceled
        while task.interp.get_result().is_none() {
            task.interp.step::<O>(
                self.mem.mutator(),
                &self.loader,
                &mut self.context,
                &interface,
            );
        }
        // TODO if result available wake subscriber
    }

    pub unsafe fn run_loop(&mut self) {
        loop {
            if let Some(ready) = {
                let mut ready_queue = self.ready_list.lock().unwrap();
                ready_queue.pop()
            } {
                self.drive_task(ready);
            } else {
                // park
            }
        }
    }
}

pub struct WakeToken {
    task: TaskId,
    pause_table: PauseTable,
    ready_list: ReadyList,
    resolved: bool,
}

impl WakeToken {
    pub fn resolve(mut self, res: *mut Obj) {
        let mut task = self.pause_table.lock().unwrap().remove(&self.task).unwrap();
        task.interp.resume(res);
        self.ready_list.lock().unwrap().push(task);
        self.resolved = true;
    }
}

impl Drop for WakeToken {
    fn drop(&mut self) {
        assert!(self.resolved);
    }
}

pub struct WorkerInterface {
    task_id: TaskId,
    ready_list: ReadyList,
    pause_table: PauseTable,
    global_id: Arc<AtomicU32>,
}

impl WorkerInterface {
    pub fn spawn(&self, interp: Interp, handle: *mut Obj) -> TaskId {
        let id = self.global_id.fetch_add(1, SeqCst);
        let task = Task { id, interp, handle };
        self.ready_list.lock().unwrap().push(task);
        id
    }

    pub fn pause(&self) -> WakeToken {
        // TODO change to paused state
        WakeToken {
            task: self.task_id,
            pause_table: self.pause_table.clone(),
            ready_list: self.ready_list.clone(),
            resolved: false,
        }
    }
}
