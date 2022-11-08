extern crate log;

mod disp;
mod input;
mod interp;
mod prog;
mod disass;

use input::{Key, Keyboard};
use disp::{Display, Terminal};
use prog::{Program, ProgramKind};
use interp::{Interpreter, InterpreterInput, InterpreterRequest};
use disass::Disassembly;

use device_query::DeviceEvents;
use crossterm::event::{
    poll, read, Event, KeyCode as CrosstermKey, KeyModifiers as CrosstermKeyModifiers,
};
use log::LevelFilter;


use std::{
    io,
    ops::DerefMut,
    sync::{Arc, Mutex},
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

const INSTRUCTION_FREQUENCY: u32 = 2000;
const TIMER_FREQUENCY: u32 = 60;

#[derive(Default)]
struct CHIP8VM {

    active: bool,

    interp: Interpreter,
    interp_input: InterpreterInput, 

    // Virtualized IO

    display: Display,
    keyboard: Keyboard,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f64,
    delay_timer: f64,
}

impl CHIP8VM {
    fn exit(&mut self) {
        self.active = false;
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // arg parsing

    let mut args = std::env::args().skip(1).collect::<Vec<_>>();

    // parse arguments for a log level
    let logger_level = if let Some(i) = args.iter().position(|arg| arg == "--log") {
        let mut level_parsed = true;
        let level = match args
            .iter()
            .nth(i + 1)
            .map(|s| s.to_ascii_lowercase())
            .as_ref()
            .map(String::as_str)
        {
            Some("trace") => LevelFilter::Trace,
            Some("debug") => LevelFilter::Debug,
            Some("info") => LevelFilter::Info,
            Some("warn") => LevelFilter::Warn,
            Some("error") => LevelFilter::Error,
            Some("off") => LevelFilter::Off,
            _ => {
                level_parsed = false;
                LevelFilter::Info
            }
        };

        if level_parsed {
            args.remove(i + 1);
        }

        args.remove(i);
        level
    } else {
        LevelFilter::Off
    };

    // parse arguments for a CHIP48 or COSMACVIP interpreter flag
    let program_kind: ProgramKind = if let Some(i) = args.iter().position(|arg| arg == "--kind")
    {
        let kind = match args
            .iter()
            .nth(i + 1)
            .map(|s| s.to_ascii_lowercase())
            .as_ref()
            .map(String::as_str)
        {
            Some("cosmacvip") => ProgramKind::COSMACVIP,
            Some("chip48") => ProgramKind::CHIP48,
            Some("common") => ProgramKind::COMMON,
            _ => Err("--kind must be followed by COSMACVIP, CHIP48, or COMMON")?,
        };

        args.remove(i + 1);
        args.remove(i);

        kind
    } else {
        Default::default()
    };

    // parse arguments for dissassembly
    let disassemble: bool = if let Some(i) = args.iter().position(|arg| arg == "--disass") {
        args.remove(i);
        true
    } else {
        false
    };

    let program_name = args.first().ok_or("expected program name")?;
    let program = Program::read(format!("roms/{}.ch8", program_name), program_kind)?;

    // dissassemble if requested
    if disassemble {
        if let Some(level) = logger_level.to_level() {
            simple_logger::init_with_level(level).unwrap();
        }

        log::info!("Dissassembling \"{}\"", program_name);

        print!("{}", Disassembly::from(program));

        log::info!("Dissassembly complete");

        return Ok(());
    }

    // initialize tui logger
    if logger_level != LevelFilter::Off {
        tui_logger::init_logger(logger_level).unwrap();
        tui_logger::set_default_level(logger_level);
    }

    // thread-safe virtual machine
    let vm = Arc::new(Mutex::new(CHIP8VM {
        active: true,
        interp: Interpreter::from(program),
        ..Default::default()
    }));

    // terminal struct for drawing the display
    let mut terminal = Terminal::setup(
        format!(" CHIP8 Virtual Machine ({}) ", program_name),
        logger_level != LevelFilter::Off,
    )?;

    let mut handles: Vec<JoinHandle<Result<(), std::io::Error>>> = vec![];

    {
        // this thread updates state the interpreter relies on, 
        // calls the next instruction with said state, 
        // and handles the output of said instruction

        let vm = Arc::clone(&vm);
        handles.push(thread::spawn(move || -> Result<(), io::Error> {
            let mut timer_instant = Instant::now();

            let mut interval = Interval::new(
                "interp",
                Duration::from_secs_f64(1.0 / INSTRUCTION_FREQUENCY as f64),
                Duration::from_millis(8),
            );

            loop {
                {
                    
                    let mut vm_guard = vm.lock().unwrap();
                    let vm = vm_guard.deref_mut();

                    if !vm.active {
                        return Ok(());
                    }

                    // update timer using time elapsed since last update
                    let elapsed = timer_instant.elapsed().as_secs_f64();
                    timer_instant = Instant::now();

                    // TODO: maybe support sound (right now the sound timer does nothing external)

                    // update timers and clamp between the bounds of a byte because that is the data type
                    vm.sound_timer = (vm.sound_timer - elapsed * TIMER_FREQUENCY as f64).clamp(u8::MIN as f64, u8::MAX as f64);
                    vm.delay_timer = (vm.delay_timer - elapsed * TIMER_FREQUENCY as f64).clamp(u8::MIN as f64, u8::MAX as f64);

                    // update interpreter input

                    let input = &mut vm.interp_input;

                    vm.keyboard.flush(input);  // the keyboard will write its state to the input
                    input.delay_timer = vm.delay_timer.ceil() as u8; // delay timer is ceiled to the nearest 8-bit number

                    // interpret next instruction
                    let output = vm.interp.step(input);

                    // handle the any external request by the executed instruction
                    if let Some(request) = output.request {
                        match request {
                            InterpreterRequest::Display => vm.display.update(&output.display),
                            InterpreterRequest::SetDelayTimer(time) => vm.delay_timer = time as f64,
                            InterpreterRequest::SetSoundTimer(time) => vm.sound_timer = time as f64,
                        }
                    }

                    // if logging is enabled then we continuously refresh the terminal to keep log output up-to-date
                    if logger_level != LevelFilter::Off {
                        vm.display.refresh();
                    }

                    // clear ephemeral interpreter input
                    vm.interp_input.just_pressed_key = None;
                    vm.interp_input.just_released_key = None;
                }

                interval.sleep();
            }
        }));
    }

    {
        // terminal render thread

        let vm = Arc::clone(&vm);
        handles.push(thread::spawn(move || -> Result<(), io::Error> {

            let mut interval = Interval::new(
                "render",
                Duration::from_millis(16),
                Duration::from_millis(16),
            );

            loop {
                {
                    let mut vm = vm.lock().unwrap();

                    if vm.active {
                        if let Some(buf) = vm.display.extract_new_frame() {
                            // receiving a display buffer means we must draw
                            // drawing is expensive and should run in parallel with the rest of the vm so lets drop here
                            drop(vm);
                            terminal.draw(&buf)?;
                        }
                    } else {
                        // inactive vm means stop what we are doing
                        drop(vm);
                        terminal.exit()?;
                        return Ok(());
                    };
                }

                interval.sleep();
            }
        }));
    }

    {
        // terminal event handler thread

        let vm = Arc::clone(&vm);

        handles.push(thread::spawn(move || -> Result<(), io::Error> {

            let device_state = device_query::DeviceState::new();

            let _guard_key_down = { // listen for key down events
                let vm = Arc::clone(&vm);
                device_state.on_key_down(move |key| {
                    if let Ok(key) = Key::try_from(*key) {
                        vm.lock().unwrap().keyboard.handle_key_down(key);
                    }
                })
            };

            let _guard_key_up = { // listen for key up events
                let vm = Arc::clone(&vm);
                device_state.on_key_up(move |key| {
                    if let Ok(key) = Key::try_from(*key) {
                        vm.lock().unwrap().keyboard.handle_key_up(key);
                    }
                })
            };

            loop { // poll for important events specific to the terminal that device_qeury could not comprehend
                if poll(Duration::from_millis(100))? {
                    match read()? {
                        Event::Resize(_, _) => vm.lock().unwrap().display.refresh(),
                        Event::FocusGained => vm.lock().unwrap().keyboard.handle_focus(),
                        Event::FocusLost => vm.lock().unwrap().keyboard.handle_unfocus(),
                        Event::Key(key_event) => { // Esc or Crtl+C interrupt handler
                            if key_event.code == CrosstermKey::Esc
                                || key_event.modifiers.contains(CrosstermKeyModifiers::CONTROL)
                                    && (key_event.code == CrosstermKey::Char('c')
                                        || key_event.code == CrosstermKey::Char('C'))
                            {
                                // exit virtual machine
                                vm.lock().unwrap().exit();
                                return Ok(());
                            } else {
                                // kinda expecting a crossterm key event to mean terminal is in focus
                                // pretty sure device state executing first sinks a key input
                                vm.lock().unwrap().keyboard.handle_crossterm_poke(key_event);
                            }
                        }
                        _ => (),
                    }
                }
            }
        }))
    }

    // wait for all threads
    for handler in handles {
        handler.join().unwrap()?;
    }

    Ok(())
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
}

impl Interval {
    fn new(name: &'static str, interval: Duration, max_quantum: Duration) -> Self {
        Interval {
            name,
            interval,
            max_quantum,
            task_start: Instant::now(),
            oversleep_duration: Duration::ZERO,
            quantum_duration: Duration::ZERO,
        }
    }

    fn sleep(&mut self) {
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
            // otherwise if were not sleeping but we are past our max quantum we must sleep (right now its hardcoded to a constant 1ms)
            if sleep_duration.is_zero() && self.quantum_duration >= self.max_quantum {
                sleep_duration = Duration::from_millis(1);
            }

            let now = Instant::now();

            spin_sleep::sleep(sleep_duration); // sleep

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
