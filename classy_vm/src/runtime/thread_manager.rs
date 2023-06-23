use thiserror::Error;

use std::collections::HashMap;

#[cfg(not(loom))]
use std::{
    sync::{
        atomic::{self, AtomicBool},
        Arc, Condvar, Mutex,
    },
    thread::{self, ThreadId},
};

#[cfg(loom)]
use loom::{
    sync::{
        atomic::{self, AtomicBool},
        Arc, Condvar, Mutex,
    },
    thread::{self, ThreadId},
};

use crate::mem::ptr::NonNullPtr;

use super::class::frame::Frame;

struct SyncBarrier {
    should_wait: Mutex<bool>,
    cvar: Condvar,
}

impl SyncBarrier {
    pub fn new() -> Self {
        Self {
            should_wait: Mutex::new(false),
            cvar: Condvar::new(),
        }
    }

    pub fn wait(&self) {
        let mut should_wait = self.should_wait.lock().unwrap();
        while *should_wait {
            should_wait = self.cvar.wait(should_wait).unwrap();
        }
    }

    pub fn lock(&self) {
        let mut should_wait = self.should_wait.lock().unwrap();
        *should_wait = true;
    }

    pub fn release(&self) {
        let mut should_wait = self.should_wait.lock().unwrap();
        *should_wait = false;
        self.cvar.notify_all();
    }
}

pub struct ThreadGcData {
    pub stack: Vec<NonNullPtr<Frame>>,
    pub handle: Option<thread::JoinHandle<()>>,
}

unsafe impl Send for ThreadGcData {}

struct ThreadsState {
    count: usize,
    stop_for_gc: bool,
    threads: HashMap<ThreadId, ThreadGcData>,
    parked_threads: usize,
}

pub struct ThreadManager {
    should_sync: AtomicBool,
    sync_barrier: SyncBarrier,
    gc_thread_wait_for_all_threads: SyncBarrier,
    state: Mutex<ThreadsState>,
}

impl ThreadManager {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            should_sync: false.into(),
            sync_barrier: SyncBarrier::new(),
            gc_thread_wait_for_all_threads: SyncBarrier::new(),
            state: Mutex::new(ThreadsState {
                count: 0,
                stop_for_gc: false,
                threads: HashMap::new(),
                parked_threads: 0,
            }),
        })
    }

    #[cfg(not(loom))]
    pub fn new_thread<Task>(self: &Arc<Self>, vm_thread: Task) -> Result<(), StoppedForGc>
    where
        Task: FnOnce() + Send + 'static,
    {
        let mut state = self.state.lock().unwrap();
        if state.stop_for_gc {
            return Err(StoppedForGc);
        }
        state.count += 1;
        let this = self.clone();
        let t = thread::spawn(move || {
            vm_thread();
            while let Err(StoppedForGc) = this.cleanup_thread() {
                this.stop_for_gc().unwrap();
            }
        });

        println!(
            "New thread with id {:?} count is {}",
            t.thread().id(),
            state.count
        );
        state.threads.insert(
            t.thread().id(),
            ThreadGcData {
                handle: Some(t),
                stack: Vec::new(),
            },
        );
        Ok(())
    }

    /// Register thread with the id to the thread manager.
    /// This thread is then responsible for cleaning itself up when it dies or
    /// stopping in case of a gc request.
    ///
    /// This method should be used if one wants their non-runtime thread to
    /// be tracked for the purpose of gc or vm shutdown.
    pub fn manually_register_thread(
        self: &Arc<Self>,
        id: std::thread::ThreadId,
    ) -> Result<(), StoppedForGc> {
        let mut state = self.state.lock().unwrap();
        if state.stop_for_gc {
            return Err(StoppedForGc);
        }
        state.count += 1;
        println!("Registering thread with id {id:?} count is {}", state.count);
        state.threads.insert(
            id,
            ThreadGcData {
                stack: Vec::new(),
                handle: None,
            },
        );
        Ok(())
    }

    pub fn current_threads_count(&self) -> usize {
        let state = self.state.lock().unwrap();
        state.count
    }

    pub fn cleanup_thread(&self) -> Result<(), StoppedForGc> {
        let id = thread::current().id();
        let mut state = self.state.lock().unwrap();
        if state.stop_for_gc {
            Err(StoppedForGc)
        } else {
            state.count -= 1;
            println!("Cleaning up thread {id:?} the count is {}", state.count);
            state.threads.remove(&id);
            Ok(())
        }
    }

    pub fn request_stop_for_gc(&self) -> Result<usize, StopRequetError> {
        let mut state = self.state.lock().unwrap();
        if state.stop_for_gc {
            return Err(StopRequetError::AlreadyRequested);
        }
        if state.count == 0 {
            return Err(StopRequetError::NoThreadsToStop);
        }
        state.stop_for_gc = true;
        self.sync_barrier.lock();
        self.gc_thread_wait_for_all_threads.lock();
        self.should_sync.store(true, atomic::Ordering::SeqCst);
        Ok(state.count)
    }

    pub fn release_stopped_threads(&self) {
        let mut state = self.state.lock().unwrap();
        assert!(state.stop_for_gc);
        state.stop_for_gc = false;
        state.parked_threads = 0;
        self.should_sync.store(false, atomic::Ordering::SeqCst);
        self.sync_barrier.release();
    }

    pub fn stop_for_gc(&self) -> Result<(), SyncFailed> {
        let mut state = self.state.lock().unwrap();
        if !state.stop_for_gc {
            return Err(SyncFailed::NoSyncRequested);
        }
        state.parked_threads += 1;
        if state.parked_threads == state.count {
            // grab the sync barrier lock
            let mut should_wait = self.sync_barrier.should_wait.lock().unwrap();
            drop(state);
            // allow gc to work, even if it finishes work before we check the
            // condition we hold the lock so it needs to wait for our wait before
            // it can release us.
            self.gc_thread_wait_for_all_threads.release();
            while *should_wait {
                should_wait = self.sync_barrier.cvar.wait(should_wait).unwrap();
            }
            Ok(())
        } else {
            let mut should_wait = self.sync_barrier.should_wait.lock().unwrap();
            // drop state barrier so other threads can sync
            drop(state);
            while *should_wait {
                should_wait = self.sync_barrier.cvar.wait(should_wait).unwrap();
            }
            Ok(())
        }
    }

    pub fn should_stop_thread_for_gc(&self) -> bool {
        self.should_sync.load(atomic::Ordering::SeqCst)
    }

    pub fn wait_for_all_threads_stopped_from_unmanaged_thread(&self) {
        self.gc_thread_wait_for_all_threads.wait()
    }

    pub fn wait_for_all_threads_stopped(&self) -> Result<(), SyncFailed> {
        {
            let mut state = self.state.lock().unwrap();
            if !state.stop_for_gc {
                return Err(SyncFailed::NoSyncRequested);
            }
            state.parked_threads += 1;
            if state.parked_threads == state.count {
                return Ok(());
            }
        }
        self.wait_for_all_threads_stopped_from_unmanaged_thread();
        Ok(())
    }

    pub fn update_gc_data(&self, thread_id: ThreadId, stack: Vec<NonNullPtr<Frame>>) {
        let mut state = self.state.lock().unwrap();
        let data = state.threads.get_mut(&thread_id).unwrap();
        data.stack = stack;
    }

    pub fn get_gc_data(&self) -> HashMap<ThreadId, Vec<NonNullPtr<Frame>>> {
        let state = self.state.lock().unwrap();
        state
            .threads
            .iter()
            .map(|(id, data)| (*id, data.stack.clone()))
            .collect()
    }

    pub fn get_gc_data_for_thread(&self, thread_id: ThreadId) -> Vec<NonNullPtr<Frame>> {
        let state = self.state.lock().unwrap();
        state.threads.get(&thread_id).unwrap().stack.clone()
    }
}

#[cfg(loom)]
impl ThreadManager {
    pub fn new_thread<Task>(this: &Arc<Self>, vm_thread: Task) -> Result<(), StoppedForGc>
    where
        Task: FnOnce() + Send + 'static,
    {
        let mut state = this.state.lock().unwrap();
        if state.stop_for_gc {
            return Err(StoppedForGc);
        }
        state.count += 1;
        let this = this.clone();
        let t = thread::spawn(move || {
            vm_thread();
            while let Err(StoppedForGc) = this.cleanup_thread() {
                this.stop_for_gc().unwrap();
            }
        });
        state.threads.insert(t.thread().id(), t);
        Ok(())
    }
}

#[derive(Error, Debug)]
#[error("Thread creation has been blocked")]
pub struct StoppedForGc;

#[derive(Error, Debug)]
pub enum SyncFailed {
    #[error("No thread synchronisation has been requested")]
    NoSyncRequested,
}

#[derive(Error, Debug)]
#[error("Cannot request gc stop if there are no running threads")]
pub enum StopRequetError {
    NoThreadsToStop,
    AlreadyRequested,
}

#[cfg(test)]
mod test {

    use super::*;

    fn wait_for_all_threads(tm: Arc<ThreadManager>) {
        while tm.current_threads_count() > 0 {
            std::thread::yield_now();
        }
    }

    #[test]
    fn requesting_stop_for_zero_threads_returns_error() {
        let tm = ThreadManager::new();
        assert!(tm.request_stop_for_gc().is_err());
    }

    #[test]
    fn thread_manager_can_synchronise_gc_thread_waiting_for_one_thread() {
        let tm = ThreadManager::new();
        let worker_tm = tm.clone();
        println!("New thread");
        tm.new_thread(move || {
            println!("Thread1 waiting for should stop for gc");
            while !worker_tm.should_stop_thread_for_gc() {
                std::thread::yield_now();
            }
            println!("Thread1 should stop for gc, syncing");
            worker_tm.stop_for_gc().unwrap();
            println!("Thread1 Synced");
        })
        .unwrap();
        println!("Creating gc thread");
        let gc_tm = tm.clone();
        let gc_thread = thread::spawn(move || {
            println!("GCThread: Waiting until number of threads is 1");
            while gc_tm.current_threads_count() != 1 {
                std::thread::yield_now();
            }
            println!("GCThread: Requesting stop");
            gc_tm.request_stop_for_gc().unwrap();
            println!("GCThread: Wait for all threads stopped");
            gc_tm.wait_for_all_threads_stopped_from_unmanaged_thread();
            println!("GCThread: realeasing waiting threads");
            gc_tm.release_stopped_threads();
            println!("GCThread: Done");
        });
        println!("Waiting for gc thread");
        gc_thread.join().unwrap();
        println!("Waiting for worker thread");
        wait_for_all_threads(tm);
    }

    // Cannot execute on miri, blocks indefinetely
    #[cfg(not(miri))]
    #[test]
    fn thread_manager_can_synchronise_multiple_gc_threads() {
        let tm = ThreadManager::new();
        println!("New thread");
        for i in 0..3 {
            let i = i;
            let worker_tm = tm.clone();
            tm.new_thread(move || {
                println!("Thread {i} waiting for should stop for gc");
                while !worker_tm.should_stop_thread_for_gc() {
                    std::thread::yield_now();
                }
                println!("Thread {i} should stop for gc, syncing");
                worker_tm.stop_for_gc().unwrap();
                println!("Thread {i} Synced");
            })
            .unwrap();
        }
        println!("Creating gc thread");
        let gc_tm = tm.clone();
        let gc_thread = thread::spawn(move || {
            println!("GCThread: Waiting until number of threads is 3");
            while gc_tm.current_threads_count() != 3 {
                std::thread::yield_now();
            }
            println!("GCThread: Requesting stop");
            gc_tm.request_stop_for_gc().unwrap();
            println!("GCThread: Wait for all threads stopped");
            gc_tm.wait_for_all_threads_stopped_from_unmanaged_thread();
            println!("GCThread: releasing waiting threads");
            gc_tm.release_stopped_threads();
            println!("GCThread: Done");
        });
        println!("Waiting for gc thread");
        gc_thread.join().unwrap();
        println!("Waiting for worker thread");
        wait_for_all_threads(tm);
    }
}
