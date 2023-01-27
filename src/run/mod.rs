pub mod audio;
pub mod disp;
pub mod input;
pub mod instruct;
pub mod interp;
pub mod mem;
pub mod rom;
pub mod vm;

use {
    input::Key,
    rom::Rom,
    vm::{VMEvent, VM},
};

use crate::{
    dbg::Debugger,
    render::{spawn_render_thread, TARGET_FRAME_DURATION},
};

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
        mpsc::{channel, Receiver, RecvTimeoutError, Sender, TryRecvError},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use self::audio::AudioController;

pub type C8 = (VM, Option<Debugger>);
pub type C8Lock = Arc<Mutex<C8>>;

pub type RunResult = Result<RunAnalytics, String>;
pub type RunControlResult = Result<(), &'static str>;

pub const GOOD_EXECUTION_FREQUENCY_PERCENT_DIFF: f64 = 1.0;
pub const OKAY_EXECUTION_FREQUENCY_PERCENT_DIFF: f64 = 10.0;

pub fn spawn_run_threads(
    rom: Rom,
    initial_target_execution_frequency: u32,
    audio_controller: AudioController,
) -> (JoinHandle<RunResult>, JoinHandle<()>) {
    // runner
    let rom_config = rom.config.clone();
    let mut runner = Runner::spawn(rom, initial_target_execution_frequency, audio_controller);

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
                            match key_event.code {
                                CrosstermKey::Char('-') => {
                                    vm_event_sender.send(VMEvent::VolumeChange(false)).ok();
                                }
                                CrosstermKey::Char('=') => {
                                    vm_event_sender.send(VMEvent::VolumeChange(true)).ok();
                                }
                                _ => {
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

    (main_thread, render_thread)
}

pub struct Runner {
    c8: Arc<Mutex<C8>>,

    thread_handle: JoinHandle<RunResult>,
    thread_continue_sender: Sender<bool>,
    thread_frequency_sender: Sender<u32>,

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

    pub fn set_execution_frequency(&mut self, frequency: u32) -> Result<(), &'static str> {
        self.thread_frequency_sender
            .send(frequency)
            .map_err(|_| "Failed to send instruction frequency to vm thread")
    }

    pub fn spawn(
        rom: Rom,
        initial_target_execution_frequency: u32,
        audio_controller: AudioController,
    ) -> Self {
        let target_frame_duration_seconds: f64 = TARGET_FRAME_DURATION.as_secs_f64();

        let (vm_event_sender, vm_event_receiver) = channel::<VMEvent>();
        let (thread_continue_sender, thread_continue_receiver) = channel::<bool>();
        let (thread_frequency_sender, thread_frequency_receiver) = channel::<u32>();

        let rom_config = rom.config.clone();

        let c8 = Arc::new(Mutex::new({
            let vm = VM::new(rom, vm_event_receiver, audio_controller);
            let dbg = if rom_config.debugging {
                Some(Debugger::new(&vm, initial_target_execution_frequency))
            } else {
                None
            };

            (vm, dbg)
        }));

        let mut cycles_per_frame = (target_frame_duration_seconds
            * initial_target_execution_frequency as f64)
            .round()
            .max(1.0) as u32;
        let mut frequency_stats: BTreeMap<u32, (Duration, u64)> = BTreeMap::new();

        let thread_handle = {
            let c8 = Arc::clone(&c8);
            thread::spawn(move || -> RunResult {
                // this thread updates state the interpreter relies on,
                // calls the next instruction with said state,
                // and handles the output of said instruction

                let thread_start = Instant::now();

                let mut continuation = RunContinuation {
                    cont: false,
                    recv: thread_continue_receiver,
                };

                let mut total_simulated_time = 0.0;

                let mut freq_duration = Duration::ZERO;
                let mut freq_instructions_executed = 0_u64;

                let mut burst_start = Instant::now();
                let mut burst_elapsed = Duration::ZERO;
                let mut burst_just_started = true;

                let mut frame_start = Instant::now();

                // 1 frame of work
                loop {
                    // vm runner step
                    let mut _guard = c8.lock().expect("Failed to lock C8 for main loop");
                    let (vm, maybe_dbg) = _guard.deref_mut();

                    let mut step_can_continue = false;

                    if continuation.try_cont() {
                        // we can step synchronously

                        if burst_just_started {
                            burst_just_started = false;
                            vm.clear_events();
                            vm.resume_audio();
                            frame_start = Instant::now();
                        }

                        vm.update_audio();

                        let time_per_cycle =
                            target_frame_duration_seconds / cycles_per_frame as f64;

                        vm.time_step = time_per_cycle;

                        let now = Instant::now();
                        if let Some(dbg) = maybe_dbg {
                            step_can_continue = dbg.step(vm, cycles_per_frame as usize);
                        } else {
                            step_can_continue = vm.flush_and_stepn(cycles_per_frame)?
                        }

                        let elapsed = now.elapsed();

                        if step_can_continue {
                            log::trace!(
                                "Completed {} cycles in {} us",
                                cycles_per_frame,
                                elapsed.as_micros()
                            );
                        }

                        vm.update_audio();

                        continuation.try_cont();
                        continuation.cont &= step_can_continue;

                        if continuation.cont {
                            drop(_guard);

                            frame_start = frame_start
                                .checked_add(TARGET_FRAME_DURATION)
                                .expect("Could not calculate next frame start");
                            let sleep_start = Instant::now();
                            let sleep_duration = frame_start.saturating_duration_since(sleep_start);
                            spin_sleep::sleep(sleep_duration);

                            if sleep_duration.is_zero() {
                                log::warn!(
                                    "Overran frame budget by {} us! Skipping sleep and starting next frame immediately", 
                                    sleep_start.duration_since(frame_start).as_micros()
                                );
                                frame_start = sleep_start;
                            } else {
                                log::trace!(
                                    "Overslept remaining frame budget by {} us",
                                    Instant::now().duration_since(frame_start).as_micros()
                                );
                            }

                            freq_instructions_executed += cycles_per_frame as u64;
                            total_simulated_time += time_per_cycle * cycles_per_frame as f64;
                            burst_elapsed = burst_start.elapsed();
                            continue;
                        }
                    }

                    vm.pause_audio();

                    drop(_guard);

                    freq_duration = freq_duration.saturating_add(burst_elapsed);

                    // we yield until either we can continue or we must exit

                    if !(step_can_continue && !rom_config.debugging) && continuation.can_cont() {
                        if let Some(freq) = thread_frequency_receiver.try_iter().last() {
                            update_frequency_stats(
                                &mut frequency_stats,
                                (cycles_per_frame as f64 / target_frame_duration_seconds).round()
                                    as u32,
                                freq_duration,
                                freq_instructions_executed,
                            );
                            cycles_per_frame = (target_frame_duration_seconds * freq as f64)
                                .round()
                                .max(1.0) as u32;
                            freq_duration = Duration::ZERO;
                            freq_instructions_executed = 0;
                        }

                        burst_just_started = true;
                        burst_elapsed = Duration::ZERO;
                        burst_start = Instant::now();
                    } else {
                        update_frequency_stats(
                            &mut frequency_stats,
                            (cycles_per_frame as f64 / target_frame_duration_seconds).round()
                                as u32,
                            freq_duration,
                            freq_instructions_executed,
                        );

                        return Ok(RunAnalytics {
                            frequency_stats,
                            up_time: thread_start.elapsed(),
                            simulated_time: total_simulated_time,
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
        // vm thread should detect sender was dropped (we are the only one) and exit thread if still alive
        drop(thread_continue_sender); 
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
            match self.recv.recv_timeout(Duration::from_millis(30)) {
                Ok(can) => self.cont = can,
                Err(RecvTimeoutError::Timeout) => {
                    if self.cont {
                        break;
                    }
                }
                Err(RecvTimeoutError::Disconnected) => {
                    self.cont = false;
                    break;
                }
            }
        }

        self.cont
    }
}

pub struct RunAnalytics {
    frequency_stats: BTreeMap<u32, (Duration, u64)>,
    up_time: Duration,
    simulated_time: f64,
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
            let color_ips_diff = if ips_diff.abs() > OKAY_EXECUTION_FREQUENCY_PERCENT_DIFF {
                Stylize::red
            } else if ips_diff.abs() > GOOD_EXECUTION_FREQUENCY_PERCENT_DIFF {
                Stylize::yellow
            } else {
                Stylize::green
            };

            write!(f, "\n    {}", format!("|").blue().bold())?;

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

            write!(f, "\n    {}", format!("|").blue().bold())?;

            write!(
                f,
                "\n    {}  Simulated Time: {:.3}s",
                format!("=").blue().bold(),
                //target_ips,
                self.simulated_time
            )?;

            write!(
                f,
                "\n    {} C8 Program Time: {:.3}s",
                format!("=").blue().bold(),
                //target_ips,
                self.up_time.as_secs_f64()
            )?;
        }

        Ok(())
    }
}

fn update_frequency_stats(
    stats: &mut BTreeMap<u32, (Duration, u64)>,
    freq: u32,
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
