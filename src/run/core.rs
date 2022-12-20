use super::{
    input::Key,
    rom::Rom,
    vm::{VMEvent, VM},
};

use crate::{dbg::core::Debugger, render::spawn_render_thread};

use anyhow::Result;
use crossterm::{
    event::{
        poll, read, Event, KeyCode as CrosstermKey, KeyEventKind,
        KeyModifiers as CrosstermKeyModifiers,
    },
    style::Stylize,
};
use device_query::DeviceQuery;

use std::{
    collections::{BTreeMap, HashSet},
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

pub type RunResult = Result<RunAnalytics, String>;
pub type RunControlResult = Result<(), &'static str>;

pub const GOOD_INSTRUCTION_FREQUENCY_DIFF: f64 = 1.0;
pub const OKAY_INSTRUCTION_FREQUENCY_DIFF: f64 = 10.0;

pub fn spawn_run_threads(rom: Rom, init_hz: u16) -> (JoinHandle<()>, JoinHandle<RunResult>) {
    // runner
    let rom_config = rom.config.clone();
    let mut runner = Runner::spawn(rom, init_hz);

    // render
    let (render_sender, render_thread) = spawn_render_thread(runner.c8(), rom_config.clone());

    // main thread
    let c8 = runner.c8();
    let vm_event_sender = runner.vm_event_sender();

    let main_thread = thread::spawn(move || -> RunResult {
        let device_state = device_query::DeviceState::new();
        let mut last_keys = HashSet::new();

        // start runner
        if !rom_config.debugging {
            runner.resume().expect("Unable to resume runner");
        }

        loop {
            // event loop
            let terminal_event_received =
                poll(Duration::from_millis(15)).expect("Unable to poll for terminal events");

            if runner.is_finished() {
                return runner.exit();
            }

            if terminal_event_received {
                let event = read().expect("Unable to read terminal event");
                let mut sink_vm_events = false;

                if rom_config.debugging {
                    let mut _guard = c8.lock().expect("Unable to lock c8");
                    let (vm, Some(dbg)) = _guard.deref_mut() else {
                        unreachable!("Debug runs should contain a debugger");
                    };

                    sink_vm_events = sink_vm_events || dbg.is_active();

                    // TODO: handle errors
                    if dbg.handle_input_event(event.clone(), &mut runner, vm) {
                        render_sender.send(()).expect("Unable to send render event");
                    }

                    sink_vm_events = sink_vm_events || dbg.is_active();
                }

                match event {
                    Event::Resize(_, _) => {
                        render_sender.send(()).expect("Unable to send render event");
                    }
                    Event::FocusGained => {
                        if !sink_vm_events {
                            vm_event_sender
                                .send(VMEvent::Focus)
                                .expect("Unable to send VM focus event");
                        }
                    }
                    Event::FocusLost => {
                        if !sink_vm_events {
                            vm_event_sender
                                .send(VMEvent::Unfocus)
                                .expect("Unable to send VM unfocus event");
                        }
                    }
                    Event::Key(key_event) => {
                        // Esc or Crtl+C interrupt handler
                        if (key_event.code == CrosstermKey::Esc && !sink_vm_events) // Esc is an exit if debugger isnt sinking keys
                            || key_event.modifiers.contains(CrosstermKeyModifiers::CONTROL) // Ctrl+C is a hard exit
                                && (key_event.code == CrosstermKey::Char('c')
                                    || key_event.code == CrosstermKey::Char('C'))
                        {
                            // exit virtual machine
                            return runner.exit();
                        } else if !sink_vm_events {
                            // kinda expecting a crossterm key event to mean renderer is in focus
                            if let KeyEventKind::Repeat | KeyEventKind::Press = key_event.kind {
                                if let Ok(key) = Key::try_from(key_event.code) {
                                    vm_event_sender
                                        .send(VMEvent::FocusingKeyDown(key))
                                        .expect("Unable to send VM focusing key down event");
                                }
                            }
                        }
                    }
                    _ => (),
                };
            }

            // execute device query step
            let keys = HashSet::from_iter(
                device_state
                    .get_keys()
                    .into_iter()
                    .filter_map(|keycode| Key::try_from(keycode).ok()),
            );

            for &key in keys.difference(&last_keys) {
                vm_event_sender
                    .send(VMEvent::KeyDown(key))
                    .expect("Unable to send VM key down event");
            }

            for &key in last_keys.difference(&keys) {
                vm_event_sender
                    .send(VMEvent::KeyUp(key))
                    .expect("Unable to send VM key up event");
            }

            last_keys = keys;

            // TODO attach event listener to logger instead of polling to update
            if rom_config.logging {
                render_sender.send(()).expect("Unable to send render event");
            }
        }
    });

    (render_thread, main_thread)
}

fn update_frequency_stats(
    stats: &mut BTreeMap<u16, (Duration, u64)>,
    freq: u16,
    duration: Duration,
    instructions: u64,
) {
    if instructions == 0 {
        return;
    }

    let (total_duration, count) = stats.entry(freq).or_insert((Duration::ZERO, 0));
    *total_duration = total_duration.saturating_add(duration);
    *count += instructions;
}

pub struct Runner {
    c8: Arc<Mutex<C8>>,

    thread_handle: JoinHandle<RunResult>,
    thread_continue_sender: Sender<bool>,
    thread_frequency_sender: Sender<u16>,

    vm_event_sender: Sender<VMEvent>,
}

impl Runner {
    pub fn c8(&self) -> C8Lock {
        Arc::clone(&self.c8)
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

    pub fn set_instruction_frequency(&mut self, frequency: u16) -> Result<(), &'static str> {
        self.thread_frequency_sender
            .send(frequency)
            .map_err(|_| "Failed to send instruction frequency to vm thread")
    }

    pub fn spawn(rom: Rom, init_hz: u16) -> Self {
        let (vm_event_sender, vm_event_receiver) = channel::<VMEvent>();
        let (thread_continue_sender, thread_continue_receiver) = channel::<bool>();
        let (thread_frequency_sender, thread_frequency_receiver) = channel::<u16>();

        let rom_config = rom.config.clone();

        let c8 = Arc::new(Mutex::new({
            let vm = VM::new(rom, vm_event_receiver);
            let dbg = if rom_config.debugging {
                Some(Debugger::new(&vm, init_hz))
            } else {
                None
            };

            (vm, dbg)
        }));

        let mut frequency = init_hz;
        let mut frequency_stats: BTreeMap<u16, (Duration, u64)> = BTreeMap::new();

        let thread_handle = {
            let c8 = Arc::clone(&c8);
            thread::spawn(move || -> RunResult {
                // this thread updates state the interpreter relies on,
                // calls the next instruction with said state,
                // and handles the output of said instruction

                let mut timer_instant = Instant::now();
                let mut interval = Interval::new(
                    "interp",
                    Duration::from_secs_f64(1.0 / frequency as f64),
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
                    let mut _guard = c8.lock().expect("Failed to lock C8 for main loop");
                    let (vm, maybe_dbg) = _guard.deref_mut();

                    let mut step_can_continue = false;

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

                        step_can_continue = if let Some(dbg) = maybe_dbg {
                            dbg.step(vm)
                        } else {
                            vm.step()?
                        };

                        drop(_guard);

                        continuation.try_cont();
                        continuation.cont = continuation.cont && step_can_continue;

                        if continuation.cont {
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

                    if !(step_can_continue && !rom_config.debugging) && continuation.can_cont() {
                        just_resumed = true;
                        runtime_burst_duration = Duration::ZERO;
                        runtime_start = Instant::now();
                        timer_instant = Instant::now();
                        if let Some(freq) = thread_frequency_receiver.try_iter().last() {
                            update_frequency_stats(
                                &mut frequency_stats,
                                frequency,
                                runtime_duration,
                                instructions_executed,
                            );
                            interval.interval = Duration::from_secs_f64(1.0 / freq as f64);
                            frequency = freq;
                            runtime_duration = Duration::ZERO;
                            instructions_executed = 0;
                        }
                        interval.reset();
                    } else {
                        update_frequency_stats(
                            &mut frequency_stats,
                            frequency,
                            runtime_duration,
                            instructions_executed,
                        );
                        return Ok(RunAnalytics {
                            frequency_stats,
                            rom_name: rom_config.name,
                        });
                    }
                }
            })
        };

        Runner {
            c8,
            thread_handle,
            vm_event_sender,
            thread_continue_sender,
            thread_frequency_sender,
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
    frequency_stats: BTreeMap<u16, (Duration, u64)>,
    rom_name: String,
}

impl Display for RunAnalytics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} \"{}\" runtime",
            format!("Analyzing").green().bold(),
            self.rom_name
        )?;

        for (&target_ips, &(runtime_duration, instructions_executed)) in self.frequency_stats.iter()
        {
            let ips = instructions_executed as f64 / runtime_duration.as_secs_f64();
            let ips_diff = (ips - target_ips as f64) / target_ips as f64 * 100.0;
            let color_ips_diff = if ips_diff.abs() > OKAY_INSTRUCTION_FREQUENCY_DIFF {
                Stylize::red
            } else if ips_diff.abs() > GOOD_INSTRUCTION_FREQUENCY_DIFF {
                Stylize::yellow
            } else {
                Stylize::green
            };

            writeln!(
                f,
                "\n    {} Runner ({:#04}Hz): {:.3}s",
                format!("|").blue().bold(),
                target_ips,
                runtime_duration.as_secs_f64()
            )?;
            write!(
                f,
                "    {} Runner continuously executed at {:#07.2}Hz",
                format!("=").blue().bold(),
                if ips.is_finite() { ips } else { 0.0 }
            )?;

            if ips.is_finite() {
                write!(
                    f,
                    " ( {} from {:#04}Hz target )",
                    color_ips_diff(format!(
                        "{}{:.2}%",
                        if ips_diff >= 0.0 { "+" } else { "" },
                        ips_diff
                    ))
                    .bold(),
                    target_ips
                )?;
            }
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
