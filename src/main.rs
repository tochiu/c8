extern crate log;

mod disp;
mod input;
mod interp;
mod prog;
mod disass;
mod vm;
mod util;
mod debug;

use input::Key;
use disp::{Terminal, RenderEvent, EMPTY_DISPLAY};
use prog::{Program, ProgramKind};
use disass::Disassembly;

use device_query::DeviceEvents;
use crossterm::{event::{
    poll, read, Event, KeyCode as CrosstermKey, KeyModifiers as CrosstermKeyModifiers, KeyEventKind,
}, style::Stylize};
use log::LevelFilter;
use util::{Interval, IntervalAccuracy};
use vm::{VMRunner, VMRunConfig, VMEvent, VMRunResult};

use std::{
    io,
    sync::{Mutex, mpsc::{channel, TryRecvError}, Arc},
    thread,
    time::Duration,
};

use crate::debug::Debugger;

// TODO: maybe support sound (right now the sound timer does nothing external)

const INSTRUCTION_FREQUENCY: u32 = 2000;
const TIMER_FREQUENCY: u32 = 60;

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
            Some("chip8") => ProgramKind::CHIP8,
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

    let should_debug: bool = if let Some(i) = args.iter().position(|arg| arg == "--debug") {
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
            simple_logger::init_with_level(level)?;
        }

        log::info!("Dissassembling \"{}\"", program_name);
        let disassembly = Disassembly::from(program);
        log::info!("Dissassembly complete");
        print!("{}", disassembly);
    } else {
        // initialize tui logger
        if logger_level != LevelFilter::Off {
            tui_logger::init_logger(logger_level)?;
            tui_logger::set_default_level(logger_level);
        }

        // virtual machine runner
        let mut vm_runner = VMRunner::spawn(VMRunConfig {
            instruction_frequency: INSTRUCTION_FREQUENCY,
            timer_frequency: TIMER_FREQUENCY,
        }, program);

        // debugger
        let maybe_dbg = if should_debug {
            Some(Arc::new(Mutex::new(Debugger::new())))
        } else {
            None
        };

        let (render_sender, render_thread) = { // terminal render thread

            let vm = vm_runner.vm();
            let mut terminal = Terminal::setup(
                format!(" {} Virtual Machine ({}) ", program_kind, program_name),
                logger_level != LevelFilter::Off,
                maybe_dbg.clone()
            )?;

            let (render_sender, render_receiver) = channel::<RenderEvent>();

            (render_sender, thread::spawn(move || -> Result<(), io::Error> {
                let mut buf = EMPTY_DISPLAY;

                let mut interval = Interval::new(
                    "render",
                    Duration::from_millis(16),
                    Duration::from_millis(16),
                    IntervalAccuracy::Default
                );

                loop {
                    let mut rerender = false;
                    for event in render_receiver.try_iter() {
                        match event {
                            RenderEvent::Refresh => rerender = true
                        }
                    }

                    if let Err(TryRecvError::Disconnected) = render_receiver.try_recv() {
                        terminal.exit()?;
                        return Ok(())
                    }

                    if let Some(new_buf) = vm.lock().unwrap().extract_new_frame() {
                        buf = new_buf;
                        rerender = true;
                    }
                    
                    if rerender {
                        terminal.draw(&buf)?;
                    }

                    interval.sleep();
                }
            }))
        };

        let main_thread = { // main thread

            let vm_event_sender = vm_runner.vm_event_sender();

            thread::spawn(move || -> VMRunResult {

                let device_state = device_query::DeviceState::new();
                
                let _guard_key_down = {
                    let vm_event_sender = Mutex::new(vm_event_sender.clone());
                    device_state.on_key_down(move |key| {
                        if let Ok(key) = Key::try_from(*key) {
                            vm_event_sender.lock().unwrap().send(VMEvent::KeyDown(key)).unwrap_or_default()
                        }
                    })
                };

                let _guard_key_up = {
                    let vm_event_sender = Mutex::new(vm_event_sender.clone());
                    device_state.on_key_up(move |key| {
                        if let Ok(key) = Key::try_from(*key) {
                            vm_event_sender.lock().unwrap().send(VMEvent::KeyUp(key)).unwrap_or_default()
                        }
                    })
                };

                // start vm
                if !should_debug {
                    vm_runner.resume().unwrap();
                }

                loop { // event loop

                    let mut rerender = false;

                    if poll(Duration::from_millis(50)).unwrap() {
                        if let Some(event) = read().ok() {
                            let mut sink_key_event = false;
                            if let Some(dbg) = maybe_dbg.as_ref() {
                                let mut dbg = dbg.lock().unwrap();
                                sink_key_event = sink_key_event || dbg.active;
                                rerender = dbg.handle_input_event(event.clone(), &mut vm_runner);
                                sink_key_event = sink_key_event || dbg.active;
                            }

                            match event {
                                Event::Resize(_, _) => {
                                    render_sender.send(RenderEvent::Refresh).ok();
                                },
                                Event::FocusGained => {
                                    vm_event_sender.send(VMEvent::Focus).ok();
                                },
                                Event::FocusLost => {
                                    vm_event_sender.send(VMEvent::Unfocus).ok();
                                },
                                Event::Key(key_event) => { // Esc or Crtl+C interrupt handler
                                    if (key_event.code == CrosstermKey::Esc && !sink_key_event)
                                        || key_event.modifiers.contains(CrosstermKeyModifiers::CONTROL)
                                            && (key_event.code == CrosstermKey::Char('c')
                                                || key_event.code == CrosstermKey::Char('C'))
                                    {
                                        // exit virtual machine
                                        return vm_runner.exit()
                                    } else if !sink_key_event {
                                        // kinda expecting a crossterm key event to mean terminal is in focus
                                        // pretty sure device state executing first sinks a key input
                                        if let KeyEventKind::Repeat | KeyEventKind::Press = key_event.kind {
                                            if let Ok(key) = Key::try_from(key_event.code) {
                                                vm_event_sender.send(VMEvent::FocusingKeyDown(key)).ok();
                                            }
                                        }
                                    }
                                },
                                _ => (),
                            };
                        }
                    }

                    // TODO attach event listener to logger instead of polling to update
                    if logger_level != LevelFilter::Off {
                        rerender = true;
                    }

                    if rerender {
                        render_sender.send(RenderEvent::Refresh).ok();
                    }

                    // TODO: we should check state and exit if panic here maybe
                }
            })
        };

        // wait for threads
        render_thread.join().unwrap()?;
        println!("\n  {} for {} thread", format!("Waiting").green().bold(), program_kind);
        println!("{}", main_thread.join().unwrap().unwrap());
    }

    Ok(())
}