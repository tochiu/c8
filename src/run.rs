use crate::{
    config::{C8Config, GOOD_INSTRUCTION_FREQUENCY_DIFF, OKAY_INSTRUCTION_FREQUENCY_DIFF},
    dbg::core::Debugger,
    vm::{
        core::{VMEvent, VM},
        interp::InterpreterError,
        prog::Program,
    },
};

use crossterm::style::Stylize;

use std::{
    fmt::Display,
    ops::DerefMut,
    sync::{
        mpsc::{channel, Receiver, Sender, TryRecvError},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

pub type C8 = (VM, Option<Debugger>);
pub type C8Lock = Arc<Mutex<C8>>;

pub type RunResult = Result<RunAnalytics, InterpreterError>;
pub type RunControlResult = Result<(), &'static str>;

pub struct Runner {
    config: C8Config,

    c8: Arc<Mutex<C8>>,

    thread_handle: JoinHandle<RunResult>,
    thread_continue_sender: Sender<bool>,

    vm_event_sender: Sender<VMEvent>,
}

impl Runner {
    pub fn c8(&self) -> C8Lock {
        Arc::clone(&self.c8)
    }

    pub fn config(&self) -> &C8Config {
        &self.config
    }

    pub fn vm_event_sender(&self) -> Sender<VMEvent> {
        self.vm_event_sender.clone()
    }

    pub fn pause(&mut self) -> RunControlResult {
        self.send_vm_can_continue(false)
    }

    pub fn resume(&mut self) -> RunControlResult {
        self.send_vm_can_continue(true)
    }

    pub fn is_finished(&self) -> bool {
        self.thread_handle.is_finished()
    }

    pub fn spawn(config: C8Config, program: Program) -> Self {
        let (vm_event_sender, vm_event_receiver) = channel::<VMEvent>();
        let (thread_continue_sender, thread_continue_receiver) = channel::<bool>();

        let program_name = program.name.clone();

        let c8 = Arc::new(Mutex::new({
            let vm = VM::new(program, vm_event_receiver);
            let dbg = if config.debugging {
                Some(Debugger::new(&vm))
            } else {
                None
            };

            (vm, dbg)
        }));

        let thread_handle = {
            let c8 = Arc::clone(&c8);
            thread::spawn(move || -> RunResult {
                // this thread updates state the interpreter relies on,
                // calls the next instruction with said state,
                // and handles the output of said instruction

                let mut timer_instant = Instant::now();
                let mut interval = Interval::new(
                    "interp",
                    Duration::from_secs_f64(1.0 / config.instruction_frequency as f64),
                    Duration::from_millis(8),
                    IntervalAccuracy::High,
                );

                let mut continuation = RunContinuation {
                    cont: false,
                    recv: thread_continue_receiver,
                };

                let mut runtime_start = Instant::now();
                let mut runtime_burst_duration = Duration::ZERO;
                let mut runtime_duration = Duration::ZERO;
                let mut instructions_executed = 0;
                let mut just_resumed = false;

                loop {
                    // vm runner step
                    let mut _guard = c8.lock().unwrap();
                    let (vm, maybe_dbg) = _guard.deref_mut();

                    if continuation.try_cont() {
                        // we can step synchronously

                        if just_resumed {
                            just_resumed = false;
                            vm.clear_events();
                        }

                        let time_elapsed = timer_instant.elapsed().as_secs_f32();
                        timer_instant = Instant::now();
                        vm.time_step = time_elapsed;

                        // dbg.step will never return an Err variant because if the vm steps into an invalid
                        // state then the debugger step will return false which will pause the thread

                        // because dbg is what continues the thread it is assumed that it will never
                        // send the signal to resume the thread from an invalid state.. if it ever resumes
                        // then it must be from a rolled-back state

                        let step_cont = if let Some(dbg) = maybe_dbg {
                            dbg.step(vm)
                        } else {
                            vm.handle_inputs();
                            vm.step()?;
                            true
                        };

                        drop(_guard);

                        continuation.try_cont();
                        continuation.cont = continuation.cont && step_cont;

                        if continuation.try_cont() {
                            interval.sleep();
                            instructions_executed += 1;
                            runtime_burst_duration = runtime_start.elapsed();
                            continue;
                        }
                    } else {
                        drop(_guard);
                    }

                    runtime_duration = runtime_duration.saturating_add(runtime_burst_duration);

                    // we yield until either we can continue or we must exit

                    if continuation.can_cont() {
                        just_resumed = true;
                        runtime_burst_duration = Duration::ZERO;
                        runtime_start = Instant::now();
                        timer_instant = Instant::now();
                        interval.reset();
                    } else {
                        return Ok(RunAnalytics {
                            runtime_duration,
                            instructions_executed,
                            target_ips: config.instruction_frequency,
                            program_name,
                        });
                    }
                }
            })
        };

        Runner {
            config,
            c8,
            thread_handle,
            vm_event_sender,
            thread_continue_sender,
        }
    }

    pub fn exit(self) -> RunResult {
        let Runner {
            thread_continue_sender,
            thread_handle,
            ..
        } = self;
        drop(thread_continue_sender); // vm thread should detect sender was dropped (we are the only one) and exit thread if still alive
        thread_handle.join().expect("Runner exited without result")
    }

    fn send_vm_can_continue(&mut self, can_continue: bool) -> RunControlResult {
        if self.is_finished() {
            return Err("VM has already exited");
        }
        self.thread_continue_sender
            .send(can_continue)
            .map_err(|_| "Unable to send VM continue signal")
    }
}

struct RunContinuation {
    cont: bool,
    recv: Receiver<bool>,
}

impl RunContinuation {
    fn try_cont(&mut self) -> bool {
        loop {
            self.cont = self.recv.try_iter().last().unwrap_or(self.cont);
            match self.recv.try_recv() {
                Ok(can) => self.cont = can,
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    self.cont = false;
                    break;
                }
            }
        }

        self.cont
    }

    // can yield ( 30ms resolution )
    fn can_cont(&mut self) -> bool {
        loop {
            self.cont = self.recv.try_iter().last().unwrap_or(self.cont);
            match self.recv.try_recv() {
                Ok(can) => self.cont = can,
                Err(TryRecvError::Empty) => {
                    if self.cont {
                        break;
                    } else {
                        // not using recv() because thats a busy-wait and
                        // we aren't certain a receive will follow soon enough
                        thread::sleep(Duration::from_millis(30));
                    }
                }
                Err(TryRecvError::Disconnected) => {
                    self.cont = false;
                    break;
                }
            }
        }

        self.cont
    }
}

pub struct RunAnalytics {
    runtime_duration: Duration,
    instructions_executed: u64,
    target_ips: u32,
    program_name: String,
}

impl Display for RunAnalytics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ips = self.instructions_executed as f64 / self.runtime_duration.as_secs_f64();
        let ips_diff = (ips - self.target_ips as f64) / self.target_ips as f64 * 100.0;
        let color_ips_diff = if ips_diff.abs() > OKAY_INSTRUCTION_FREQUENCY_DIFF {
            Stylize::red
        } else if ips_diff.abs() > GOOD_INSTRUCTION_FREQUENCY_DIFF {
            Stylize::yellow
        } else {
            Stylize::green
        };
        writeln!(
            f,
            "{} \"{}\" runtime",
            format!("Analyzing").green().bold(),
            self.program_name
        )?;
        writeln!(
            f,
            "    {} Runner: {:.3}s",
            format!("|").blue().bold(),
            self.runtime_duration.as_secs_f64()
        )?;
        write!(
            f,
            "    {} Runner continuously executed {:.2} inst/sec",
            format!("=").blue().bold(),
            if ips.is_finite() { ips } else { 0.0 }
        )?;

        if ips.is_finite() {
            write!(
                f,
                " ( {} from {} inst/sec target )",
                color_ips_diff(format!(
                    "{}{:.2}%",
                    if ips_diff >= 0.0 { "+" } else { "" },
                    ips_diff
                ))
                .bold(),
                self.target_ips
            )?;
        }

        Ok(())
    }
}

/*
 * Intervals keep state about the time elapsed since the previous sleep call
 * and compensate for it in the next sleep call so the starting time of logic
 * running between each sleep will be spaced apart by the desired interval
 *
 * Because sleep fundamentally cannot execute for exactly the amount requested and logic between each sleep will elapse for a nonzero duration
 * we cut the duration overslept and the execution time of the logic from the original sleep duration to try and resynchronize
 *
 * Deviations from the target interval that require more than one interval to compensate for will not be compensated for but instead forgotten
 * For example if we execute for 0.8 seconds but are supposed to execute within 0.2 second intervals at a time then
 * we will not try to compensate for the extra 0.6 seconds across multiple intervals and instead refuse to sleep in the next interval (0.2 - 0.6 <= 0)
 * and forget the remaining time to cut out
 *
 * This behavior exists because recovering from time deviations bigger than the target interval will lead to subsequent intervals being much
 * smaller than our target interval when what we are optimizing for is minimal absolute deviation
 *
 * I don't really think this construct is necessary but I thought I might need it
 * Anyway having this gives me peace of mind and also I already wrote it and could probably use it somewhere else later
 */

#[derive(PartialEq, Eq)]
pub enum IntervalAccuracy {
    Default,
    High,
}

pub struct Interval {
    // interval name
    #[allow(dead_code)]
    name: &'static str,

    // desired duration of interval
    interval: Duration,

    // max_quantum is the maximum duration we can go without sleeping before being forced to sleep
    // compensation can lead to no sleeping if oversleeping and/or executing logic between sleeps is longer than the desired interval
    // allowing this to occur unchecked can lead to deadlock since other threads could potentially be starved of execution time
    max_quantum: Duration,

    // instant last sleep was complete
    task_start: Instant,

    // how much we overslept by
    oversleep_duration: Duration,

    // how long weve gone since an actual sleep
    quantum_duration: Duration,

    accuracy: IntervalAccuracy,
}

impl Interval {
    pub fn new(
        name: &'static str,
        interval: Duration,
        max_quantum: Duration,
        accuracy: IntervalAccuracy,
    ) -> Self {
        Interval {
            name,
            interval,
            max_quantum,
            task_start: Instant::now(),
            oversleep_duration: Duration::ZERO,
            quantum_duration: Duration::ZERO,
            accuracy,
        }
    }

    pub fn reset(&mut self) {
        self.task_start = Instant::now();
        self.oversleep_duration = Duration::ZERO;
        self.quantum_duration = Duration::ZERO;
    }

    pub fn sleep(&mut self) {
        let task_duration = self.task_start.elapsed(); // time since the end of our last sleep

        // update our quantum duration
        self.quantum_duration += task_duration;

        // we kept track of how much we overslept last time so this and our task duration combined is our overshoot
        // we must subtract from the original to stay on schedule
        let mut sleep_duration = self
            .interval
            .saturating_sub(task_duration)
            .saturating_sub(self.oversleep_duration);

        // if our compensation leads to no sleeping and we havent exceeded our max quantum then we dont sleep
        if sleep_duration.is_zero() && self.quantum_duration < self.max_quantum {
            self.oversleep_duration = Duration::ZERO;
        } else {
            // otherwise if were not sleeping but we are past our max quantum we must sleep (right now its hardcoded to a constant 1ns)
            if sleep_duration.is_zero() && self.quantum_duration >= self.max_quantum {
                sleep_duration = Duration::from_nanos(1);
            }

            let now = Instant::now();

            if self.accuracy == IntervalAccuracy::High {
                spin_sleep::sleep(sleep_duration);
            } else {
                thread::sleep(sleep_duration);
            }

            // store how much we overslept by and reset our quantum duration since we slept for a nonzero duration
            self.oversleep_duration = now.elapsed().saturating_sub(sleep_duration);
            self.quantum_duration = Duration::ZERO;
        }

        // log::trace!(
        //     "name: {}, task: {} us, sleep: {} us, oversleep: {} us",
        //     self.name,
        //     task_duration.as_micros(),
        //     sleep_duration.as_micros(),
        //     self.oversleep_duration.as_micros()
        // );

        // update task start to now since sleep is done
        self.task_start = Instant::now();
    }
}
