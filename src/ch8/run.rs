use super::{
    audio::AudioController,
    rom::Rom,
    stats::C8Stats,
    vm::{VMEvent, VM, VM_FRAME_DURATION, VM_FRAME_RATE},
};

use crate::dbg::Debugger;

use anyhow::Result;

use std::{
    ops::DerefMut,
    sync::{
        mpsc::{channel, Receiver, RecvTimeoutError, Sender, TryRecvError},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

pub type C8 = (VM, Option<Debugger>);
pub type C8Lock = Arc<Mutex<C8>>;

pub type RunResult = Result<C8Stats, String>;
pub type RunControlResult = Result<(), &'static str>;

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
        mut initial_target_execution_frequency: u32,
        audio_controller: AudioController,
    ) -> Self {
        let target_frame_duration_seconds: f64 = VM_FRAME_DURATION.as_secs_f64();

        // TODO: change from execution frequency to cycles per second
        initial_target_execution_frequency -= initial_target_execution_frequency % VM_FRAME_RATE;
        initial_target_execution_frequency = initial_target_execution_frequency.max(VM_FRAME_RATE);

        let (vm_event_sender, vm_event_receiver) = channel::<VMEvent>();
        let (thread_continue_sender, thread_continue_receiver) = channel::<bool>();
        let (thread_frequency_sender, thread_frequency_receiver) = channel::<u32>();

        let rom_config = rom.config.clone();

        let mut cycles_per_frame = initial_target_execution_frequency / VM_FRAME_RATE;
        let mut stats = C8Stats::new(rom_config.name);

        let c8 = Arc::new(Mutex::new({
            let vm = VM::new(rom, vm_event_receiver, audio_controller, cycles_per_frame);
            let dbg = if rom_config.debugging {
                Some(Debugger::new(&vm, initial_target_execution_frequency))
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
                        vm.set_cycles_per_frame(cycles_per_frame);

                        let now = Instant::now();
                        if let Some(dbg) = maybe_dbg {
                            step_can_continue = dbg.step(vm, cycles_per_frame as usize);
                        } else {
                            step_can_continue =
                                vm.flush_external_input_and_stepn(cycles_per_frame)?
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
                                .checked_add(VM_FRAME_DURATION)
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
                            total_simulated_time += 1.0 / VM_FRAME_RATE as f64;
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
                            stats.update_frequency_stats(
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
                        stats.update_frequency_stats(
                            (cycles_per_frame as f64 / target_frame_duration_seconds).round()
                                as u32,
                            freq_duration,
                            freq_instructions_executed,
                        );

                        stats.up_time = thread_start.elapsed();
                        stats.simulated_time = total_simulated_time;

                        return Ok(stats);
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

    // can yield
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
