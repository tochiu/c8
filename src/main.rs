extern crate log;

mod asm;
mod cli;
mod dbg;
mod render;
mod run;
mod set;

use {
    asm::Disassembler,
    cli::{Cli, CliCommand},
    render::panic_cleanup_terminal,
    run::{rom::Rom, spawn_run_threads},
};

use anyhow::Result;
use clap::Parser;
use crossterm::style::Stylize;

use std::io::stdout;

use crate::run::audio::spawn_audio_stream;

fn main() -> Result<()> {
    match Cli::parse().command {
        CliCommand::Check { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm =
                Disassembler::from(Rom::read(path, kind.map(cli::KindOption::to_kind), log.is_some(), false)?);
            disasm.run();
            disasm.write_issue_traces(&mut stdout())?;
        }
        CliCommand::Dasm { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm =
                Disassembler::from(Rom::read(path, kind.map(cli::KindOption::to_kind), log.is_some(), false)?);
            disasm.run();
            print!("{}", disasm);
        }
        CliCommand::Run {
            path,
            debug,
            hz,
            log,
            kind,
        } => {
            let rom = Rom::read(path, kind.map(cli::KindOption::to_kind), log.is_some(), debug)?;
            let kind = rom.config.kind;

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

            // spawn audio stream
            let (_audio_stream, audio_controller) = spawn_audio_stream();

            // spawn run threads
            let (run_main_thread, run_render_thread) = spawn_run_threads(rom, hz, audio_controller);

            // wait for threads
            run_render_thread
                .join()
                .expect("Failed to join render thread");
            match run_main_thread.join().expect("Failed to join main thread") {
                Ok(analytics) => println!("{}", analytics),
                Err(err) => println!("\n    {} {}", format!("Error").red().bold(), err),
            }
        }
    }

    Ok(())
}
