use super::{
    input::{Key, Keyboard},
    interp::{Interpreter, InterpreterError, InterpreterRequest},
    prog::Program,
    disp::DisplayBuffer
};

use crate::{
    util::{Interval, IntervalAccuracy},
    dbg::core::Debugger, 
    config::{C8VMConfig, GOOD_IPS_DIFF, OKAY_IPS_DIFF}
};

use crossterm::style::Stylize;

use std::{
    cell::Cell,
    fmt::Display,
    any::Any,
    sync::{
        mpsc::{channel, Receiver, Sender, TryRecvError},
        Arc, Mutex,
    },
    thread,
    time::{Duration, Instant}, ops::DerefMut,
};

pub type VMWare = Arc<Mutex<(VM, Option<Debugger>)>>;
pub type VMRunResult = Result<VMRunAnalytics, VMRunError>;

// TODO: maybe support sound (right now the sound timer does nothing external)

pub struct VM {
    interp: Interpreter,

    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO

    display: bool,
    keyboard: Keyboard,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f64,
    delay_timer: f64,

    timer_frequency: u32,
}

impl VM {
    pub fn interpreter(&self) -> &Interpreter {
        &self.interp
    }

    pub fn keyboard(&self) -> &Keyboard {
        &self.keyboard
    }

    pub fn keyboard_mut(&mut self) -> &mut Keyboard {
        &mut self.keyboard
    }

    pub fn sound_timer(&self) -> f64 {
        self.sound_timer
    }

    pub fn delay_timer(&self) -> f64 {
        self.delay_timer
    }

    pub fn queue_events(&mut self) {
        self.event_queue.extend(self.event.try_iter());
    }

    pub fn clear_events(&mut self) {
        for _ in self.event.try_iter() {
            continue; // no-op
        }
    }

    pub fn clear_event_queue(&mut self) {
        self.queue_events();
        self.event_queue.clear();
    }

    pub fn drain_event_queue(&mut self) {
        self.queue_events();
        for event in self.event_queue.drain(..) {
            log::debug!("Processing Event {:?}", event);
            match event {
                VMEvent::KeyUp(key) => self.keyboard.handle_key_up(key),
                VMEvent::KeyDown(key) => self.keyboard.handle_key_down(key),
                VMEvent::Focus => self.keyboard.handle_focus(),
                VMEvent::Unfocus => self.keyboard.handle_unfocus(),
                VMEvent::FocusingKeyDown(key) => self.keyboard.handle_focusing_key_down(key),
            }
        }
    }

    pub fn extract_new_frame(&mut self) -> Option<DisplayBuffer> {
        if self.display {
            self.display = false;
            Some(self.interp.output.display)
        } else {
            None
        }
    }

    pub fn step(&mut self, time_elapsed: f64) -> Result<(), VMRunError> {
        self.drain_event_queue();

        // update timers and clamp between the bounds of a byte because that is the data type
        self.sound_timer = (self.sound_timer - time_elapsed * self.timer_frequency as f64)
            .clamp(u8::MIN as f64, u8::MAX as f64);
        self.delay_timer = (self.delay_timer - time_elapsed * self.timer_frequency as f64)
            .clamp(u8::MIN as f64, u8::MAX as f64);

        // update interpreter input
        let mut input = self.interp.input_mut();

        self.keyboard.flush(input); // the keyboard will write its state to the input
        input.delay_timer = self.delay_timer.ceil() as u8; // delay timer is ceiled to the nearest 8-bit number

        // interpret next instruction
        let output = self.interp.step()?;

        // handle any external request by the executed instruction
        if let Some(request) = output.request {
            match request {
                InterpreterRequest::Display => self.display = true,
                InterpreterRequest::SetDelayTimer(time) => self.delay_timer = time as f64,
                InterpreterRequest::SetSoundTimer(time) => self.sound_timer = time as f64,
            }
        }

        Ok(())
    }
}

pub struct VMRunner {

    config: C8VMConfig,

    vmware: VMWare,

    vm_event_sender: Sender<VMEvent>,
    vm_continue_sender: Sender<bool>,
    vm_result_receiver: Receiver<VMRunResult>,
    vm_result: Cell<Option<VMRunResult>>,
}

impl VMRunner {
    pub fn ware(&self) -> VMWare {
        Arc::clone(&self.vmware)
    }

    pub fn config(&self) -> &C8VMConfig {
        &self.config
    }

    pub fn vm_event_sender(&self) -> Sender<VMEvent> {
        self.vm_event_sender.clone()
    }

    pub fn pause(&mut self) -> Result<(), VMControlError> {
        self.can_continue(false)
    }

    pub fn resume(&mut self) -> Result<(), VMControlError> {
        self.can_continue(true)
    }

    pub fn is_finished(&self) -> bool {
        let vm_result = self.vm_result.take();
        if vm_result.is_some() {
            self.vm_result.set(vm_result); // oh ok we just checking so lets just.. put it back in lol
            return true;
        } else if let Ok(vm_result) = self.vm_result_receiver.try_recv() {
            self.vm_result.set(Some(vm_result));
            return true;
        } else {
            return false;
        }
    }

    pub fn spawn(config: C8VMConfig, program: Program) -> Self {
        let (vm_event_sender, vm_event_receiver) = channel::<VMEvent>();
        let (vm_result_sender, vm_result_receiver) = channel::<VMRunResult>();
        let (vm_continue_sender, vm_continue_receiver) = channel::<bool>();

        let program_name = program.name.clone();

        let vmware = Arc::new(Mutex::new({
            let vm = VM {
                interp: Interpreter::from(program),

                event: vm_event_receiver,
                event_queue: Vec::new(),

                // Virtualized IO
                display: false,
                keyboard: Keyboard::default(),

                // these precise external timers will be mapped
                // to a byte range when executing interpreter instructions
                sound_timer: 0.0,
                delay_timer: 0.0,

                timer_frequency: config.timer_frequency,
            };
            let dbg = if config.debugging {
                Some(Debugger::init(&vm))
            } else {
                None
            };

            (vm, dbg)
        }));

        let handle = {
            let vmware = Arc::clone(&vmware);
            thread::spawn(move || -> VMRunResult {
                // this thread updates state the interpreter relies on,
                // calls the next instruction with said state,
                // and handles the output of said instruction

                let mut timer_instant = Instant::now();
                let mut interval = Interval::new(
                    "interp",
                    Duration::from_secs_f64(1.0 / config.instruction_frequency as f64),
                    Duration::from_millis(8),
                    IntervalAccuracy::High
                );

                let mut continuation = VMRunContinuation {
                    cont: false,
                    recv: vm_continue_receiver,
                };

                let mut runtime_start = Instant::now();
                let mut runtime_burst_duration = Duration::ZERO;
                let mut runtime_duration = Duration::ZERO;
                let mut interpreter_duration = Duration::ZERO;
                let mut instructions_executed = 0;
                let mut just_resumed = false;

                loop {
                    let mut _guard = vmware.lock().unwrap();
                    let (vm, maybe_dbg) = _guard.deref_mut();

                    if continuation.try_cont() { // we can step synchronously

                        if just_resumed {
                            just_resumed = false;
                            vm.clear_events();
                        }
                        
                        let time_elapsed = timer_instant.elapsed().as_secs_f64();
                        timer_instant = Instant::now();
                        
                        vm.step(time_elapsed)?;

                        let duration = timer_instant.elapsed();

                        continuation.try_cont();
                        continuation.cont = continuation.cont && maybe_dbg.as_mut().map_or(true, |dbg| dbg.step(vm));

                        drop(_guard);

                        if continuation.try_cont() {
                            interval.sleep();
                            instructions_executed += 1;
                            interpreter_duration = interpreter_duration.saturating_add(duration);
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
                        return Ok(VMRunAnalytics {
                            runtime_duration,
                            interpreter_duration,
                            instructions_executed,
                            target_ips: config.instruction_frequency,
                            program_name,
                        });
                    }
                }
            })
        };

        thread::spawn(move || {
            vm_result_sender
                .send(handle.join().map_or_else(
                    |vm_panic_err| Err(VMRunError::ThreadPanic(vm_panic_err)),
                    |vm_result| vm_result,
                ))
                .unwrap_or_default();
        });

        VMRunner {
            config,
            vmware,
            vm_event_sender,
            vm_result_receiver,
            vm_result: Cell::new(None),
            vm_continue_sender,
        }
    }

    pub fn exit(self) -> VMRunResult {
        let VMRunner {
            vm_continue_sender, ..
        } = self;
        drop(vm_continue_sender); // thread should detect senders were dropped (we are the only one) and exit thread if still alive
        self.vm_result.take().unwrap_or_else(|| {
            self.vm_result_receiver
                .recv()
                .unwrap_or_else(|_| unreachable!("recv must return a value"))
        })
    }

    fn can_continue(&mut self, can_continue: bool) -> Result<(), VMControlError> {
        if self.vm_result.get_mut().is_some() {
            return Err(VMControlError::AlreadyDone);
        }

        self.vm_continue_sender
            .send(can_continue)
            .map_err(|_| VMControlError::ThreadNotResponding)
    }
}

#[derive(Debug)]
pub enum VMEvent {
    KeyUp(Key),
    KeyDown(Key),
    Focus,
    Unfocus,
    FocusingKeyDown(Key),
}

#[derive(Debug)]
pub enum VMRunError {
    Interpreter(InterpreterError),
    ThreadPanic(Box<dyn Any + Send + 'static>),
}

impl From<InterpreterError> for VMRunError {
    fn from(err: InterpreterError) -> Self {
        VMRunError::Interpreter(err)
    }
}

pub struct VMRunAnalytics {
    runtime_duration: Duration,
    interpreter_duration: Duration,
    instructions_executed: u64,
    target_ips: u32,
    program_name: String,
}

impl Display for VMRunAnalytics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ips = self.instructions_executed as f64 / self.runtime_duration.as_secs_f64();
        let ips_diff = (ips - self.target_ips as f64) / self.target_ips as f64 * 100.0;
        let color_ips_diff = if ips_diff.abs() > OKAY_IPS_DIFF {
            Stylize::red
        } else if ips_diff.abs() > GOOD_IPS_DIFF {
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
            "    {} Interp: {:.3}s",
            format!("|").blue().bold(),
            self.interpreter_duration.as_secs_f64()
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

#[derive(Debug)]
pub enum VMControlError {
    AlreadyDone,
    ThreadNotResponding,
}

struct VMRunContinuation {
    cont: bool,
    recv: Receiver<bool>,
}

impl VMRunContinuation {
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