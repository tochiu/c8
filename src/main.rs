extern crate log;

mod asm;
mod ch8;
mod cli;
mod dbg;
mod render;
mod run;

use {
    asm::Disassembler,
    ch8::rom::Rom,
    cli::{Cli, CliCommand},
    render::panic_cleanup_terminal,
    run::spawn_run_thread,
};

use anyhow::Result;
use clap::Parser;
use crossterm::style::Stylize;

use std::io::stdout;

use crate::{ch8::{
    audio::spawn_audio_stream,
    vm::{VM_FRAME_RATE, VM}, run::Runner,
}, dbg::Debugger, render::spawn_render_thread};

fn main() -> Result<()> {
    match Cli::parse().command {
        CliCommand::Check { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm = Disassembler::from(Rom::read(
                path,
                kind.map(cli::KindOption::to_kind),
                None
            )?);
            disasm.run();
            disasm.write_issue_traces(&mut stdout())?;
        }
        CliCommand::Dasm { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm = Disassembler::from(Rom::read(
                path,
                kind.map(cli::KindOption::to_kind),
                None
            )?);
            disasm.run();
            print!("{}", disasm);
        }
        CliCommand::Run {
            path,
            debug,
            hz,
            cpf,
            log,
            kind,
        } => {
            let rom = Rom::read(path, kind.map(cli::KindOption::to_kind), None)?;
            let kind = rom.config.kind;
            let cpf = cpf.or(hz.map(|hz| hz / VM_FRAME_RATE)).unwrap_or(kind.default_cycles_per_frame());
            let logging = log.is_some();
            
            if let Some(level) = log {
                tui_logger::init_logger(level.to_level_filter())?;
                tui_logger::set_default_level(level.to_level_filter());
            }

            // preempt wait thread message
            println!(
                "\n  {} for {} thread",
                format!("Waiting").green().bold(),
                kind
            );

            // override panic hook to cleanup terminal before panic
            let default_panic_hook = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |panic_info| {
                if let Err(cleanup_err) = panic_cleanup_terminal() {
                    eprintln!("Failed to cleanup terminal after panic: {}", cleanup_err);
                } else {
                    eprintln!("");
                }
                default_panic_hook(panic_info);
            }));

            // audio stream
            let (_audio_stream, audio_controller) = spawn_audio_stream();

            // vm and optional debugger
            let vm = VM::new(rom, cpf, audio_controller);
            let dbg = if debug {
                Some(Debugger::new(&vm, cpf * VM_FRAME_RATE))
            } else {
                None
            };

            // vm runner
            let runner = Runner::new(vm, dbg);

            // spawn render thread
            let (render_controller, render_thread) = spawn_render_thread(runner.c8(), logging);

            // spawn run thread
            let run_thread = spawn_run_thread(runner, render_controller, debug, logging);

            // wait for threads
            render_thread
                .join()
                .expect("Failed to join render thread");
            match run_thread.join().expect("Failed to join run thread") {
                Ok(analytics) => println!("{}", analytics),
                Err(err) => println!("\n    {} {}", format!("Error").red().bold(), err),
            }
        }
    }

    Ok(())
}
