extern crate log;

mod disp;
mod input;
mod interp;
mod prog;
mod disass;
mod vm;
mod util;

use input::Key;
use disp::{Terminal, RenderEvent, EMPTY_DISPLAY};
use prog::{Program, ProgramKind};
use disass::Disassembly;

use device_query::DeviceEvents;
use crossterm::event::{
    poll, read, Event, KeyCode as CrosstermKey, KeyModifiers as CrosstermKeyModifiers, KeyEventKind,
};
use log::LevelFilter;
use util::Interval;
use vm::{VMRunner, VMRunConfig, VMEvent};


use std::{
    io,
    sync::{Mutex, mpsc::{channel, TryRecvError}},
    thread::{self, JoinHandle},
    time::Duration,
};

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

    let mut handles: Vec<JoinHandle<Result<(), std::io::Error>>> = vec![];
    let (render_sender, render_receiver) = channel::<RenderEvent>();

    // thread-safe virtual machine runner
    let mut vm_runner = VMRunner::spawn(VMRunConfig {
        instruction_frequency: 2000,
        timer_frequency: 60,
        display_sender: render_sender.clone()
    }, program);

    { // terminal render thread

        // terminal struct for drawing the display
        let mut terminal = Terminal::setup(
            format!(" CHIP8 Virtual Machine ({}) ", program_name),
            logger_level != LevelFilter::Off,
        )?;

        handles.push(thread::spawn(move || -> Result<(), io::Error> {
            let mut buf = EMPTY_DISPLAY;

            let mut interval = Interval::new(
                "render",
                Duration::from_millis(16),
                Duration::from_millis(16),
            );

            loop {
                let mut rerender = false;
                for event in render_receiver.try_iter() {
                    match event {
                        RenderEvent::Display(new_buf) => {
                            buf = new_buf;
                            rerender = true;
                        },
                        RenderEvent::Refresh => rerender = true
                    }
                }

                if let Err(TryRecvError::Disconnected) = render_receiver.try_recv() {
                    terminal.exit()?;
                    return Ok(())
                } 
                
                if rerender {
                    terminal.draw(&buf)?;
                }
                interval.sleep();
            }
        }));
    }

    { // main thread

        let vm_event_sender = vm_runner.clone_event_sender();

        handles.push(thread::spawn(move || -> Result<(), io::Error> {

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
            vm_runner.resume().unwrap();

            loop { // poll for important events specific to the terminal that device_qeury could not comprehend
                if poll(Duration::from_millis(100))? {
                    match read()? {
                        Event::Resize(_, _) => render_sender.send(RenderEvent::Refresh).unwrap_or_default(),
                        Event::FocusGained => vm_event_sender.send(VMEvent::Focus).unwrap_or_default(),
                        Event::FocusLost => vm_event_sender.send(VMEvent::Unfocus).unwrap_or_default(),
                        Event::Key(key_event) => { // Esc or Crtl+C interrupt handler
                            if key_event.code == CrosstermKey::Esc
                                || key_event.modifiers.contains(CrosstermKeyModifiers::CONTROL)
                                    && (key_event.code == CrosstermKey::Char('c')
                                        || key_event.code == CrosstermKey::Char('C'))
                            {
                                // exit virtual machine
                                vm_runner.exit().unwrap();
                                return Ok(());
                            } else {
                                // kinda expecting a crossterm key event to mean terminal is in focus
                                // pretty sure device state executing first sinks a key input
                                if key_event.kind == KeyEventKind::Press {
                                    if let Ok(key) = Key::try_from(key_event.code) {
                                        vm_event_sender.send(VMEvent::FocusingKeyDown(key)).unwrap_or_default();
                                    }
                                }
                            }
                        }
                        _ => (),
                    }
                }

                // update log if its on
                if logger_level != LevelFilter::Off {
                    render_sender.send(RenderEvent::Refresh).unwrap_or_default();
                }

                // TODO: we should check state and exit if panic here
            }
        }))
    }

    // wait for all threads
    for handler in handles {
        handler.join().unwrap()?;
    }

    Ok(())
}