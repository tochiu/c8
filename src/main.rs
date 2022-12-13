extern crate log;

mod config;
mod dbg;
mod disass;
mod render;
mod run;

use {
    disass::Disassembler,
    render::spawn_render_thread,
    run::{
        core::{RunResult, Runner},
        vm::VMEvent,
        input::Key,
        prog::{Program, ProgramKind},
    },
};

use config::C8Config;
use crossterm::{
    event::{
        poll, read, Event, KeyCode as CrosstermKey, KeyEventKind,
        KeyModifiers as CrosstermKeyModifiers,
    },
    style::Stylize,
};
use device_query::DeviceQuery;
use log::LevelFilter;
use anyhow::{Result, Context, bail};

use std::{collections::HashSet, ops::DerefMut, thread, time::Duration};

fn main() -> Result<()> {
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
    let program_kind: ProgramKind = if let Some(i) = args.iter().position(|arg| arg == "--kind") {
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
            _ => bail!("--kind must be followed by COSMACVIP, CHIP48, or COMMON"),
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

    let check: bool = if let Some(i) = args.iter().position(|arg| arg == "--check") {
        args.remove(i);
        true
    } else {
        false
    };

    let debugging: bool = if let Some(i) = args.iter().position(|arg| arg == "--debug") {
        args.remove(i);
        true
    } else {
        false
    };

    let program_name = args.first().context("Expected program name")?;//.ok_or("lol")?;// .with_context(|| format!("Expected program name"))?; //.ok_or("expected program name")?;
    let program = Program::read(format!("roms/{}.ch8", program_name), program_kind)?;

    let config = C8Config {
        title: format!(" {} Virtual Machine ({}) ", program_kind, program_name),
        logging: logger_level != LevelFilter::Off,
        debugging,
        ..Default::default()
    };

    // dissassemble if requested
    if disassemble || check {
        if let Some(level) = logger_level.to_level() {
            simple_logger::init_with_level(level)?;
        }

        let mut disass = Disassembler::from(program);
        disass.run();
        if check {
            disass.write_stacktraces(&mut std::io::stdout())?;
        }
        if disassemble {
            print!("{}", disass);
        }
    } else {
        // initialize tui logger
        if logger_level != LevelFilter::Off {
            tui_logger::init_logger(logger_level)?;
            tui_logger::set_default_level(logger_level);
        }

        // virtual machine runner
        let mut runner = Runner::spawn(config.clone(), program);

        // wait
        println!(
            "\n  {} for {} thread",
            format!("Waiting").green().bold(),
            program_kind
        );

        let (render_sender, render_thread) = spawn_render_thread(runner.c8(), config.clone());

        let main_thread = {
            // main thread
            let c8 = runner.c8();
            let vm_event_sender = runner.vm_event_sender();

            thread::spawn(move || -> RunResult {
                let device_state = device_query::DeviceState::new();
                let mut last_keys = HashSet::new();

                // start vm
                if !debugging {
                    runner
                        .resume()
                        .expect("Unable to resume runner");
                }

                loop {
                    // event loop
                    let terminal_event_received = poll(Duration::from_millis(15))
                        .expect("Unable to poll for terminal events");
                    
                    if runner.is_finished() {
                        return runner.exit();
                    }

                    if terminal_event_received {
                        let event = read().expect("Unable to read terminal event");
                        let mut sink_vm_events = false;

                        if debugging {
                            let mut _guard = c8.lock().expect("Unable to lock c8");
                            let (vm, Some(dbg)) = _guard.deref_mut() else {
                                unreachable!("Debug runs should contain a debugger");
                            };

                            sink_vm_events = sink_vm_events || dbg.is_active();

                            // TODO: handle errors
                            if dbg.handle_input_event(event.clone(), &mut runner, vm) {
                                render_sender.send(())
                                    .expect("Unable to send render event");
                            }

                            sink_vm_events = sink_vm_events || dbg.is_active();
                        }

                        match event {
                            Event::Resize(_, _) => {
                                render_sender.send(())
                                    .expect("Unable to send render event");
                            }
                            Event::FocusGained => {
                                if !sink_vm_events {
                                    vm_event_sender.send(VMEvent::Focus)
                                        .expect("Unable to send VM focus event");
                                }
                            }
                            Event::FocusLost => {
                                if !sink_vm_events {
                                    vm_event_sender.send(VMEvent::Unfocus).
                                        expect("Unable to send VM unfocus event");
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
                                    if let KeyEventKind::Repeat | KeyEventKind::Press =
                                        key_event.kind
                                    {
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
                        vm_event_sender.send(VMEvent::KeyDown(key))
                            .expect("Unable to send VM key down event");
                    }

                    for &key in last_keys.difference(&keys) {
                        vm_event_sender.send(VMEvent::KeyUp(key))
                            .expect("Unable to send VM key up event");
                    }

                    last_keys = keys;

                    // TODO attach event listener to logger instead of polling to update
                    if logger_level != LevelFilter::Off {
                        render_sender.send(())
                            .expect("Unable to send render event");
                    }
                }
            })
        };

        // wait for threads
        render_thread.join().expect("Failed to join render thread");
        match main_thread.join().expect("Failed to join main thread") {
            Ok(analytics) => {
                println!("{}", analytics);
            }
            Err(err) => {
                println!(
                    "\n    {} {}",
                    format!("Error").red().bold(),
                    err
                );
            }
        }
    }

    Ok(())
}
