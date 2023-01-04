extern crate log;

mod asm;
mod cli;
mod dbg;
mod render;
mod run;

use {
    asm::Disassembler,
    cli::{Cli, CliCommand},
    render::panic_cleanup_terminal,
    run::{core::spawn_run_threads, rom::Rom},
};

use anyhow::Result;
use clap::Parser;
use crossterm::style::Stylize;

use std::io::stdout;

fn main() -> Result<()> {
    match Cli::parse().command {
        CliCommand::Check { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm =
                Disassembler::from(Rom::read(path, kind.to_kind(), log.is_some(), false)?);
            disasm.run();
            disasm.write_issue_traces(&mut stdout())?;
        }
        CliCommand::Dasm { path, log, kind } => {
            if let Some(level) = log {
                simple_logger::init_with_level(level.to_level())?;
            }

            let mut disasm =
                Disassembler::from(Rom::read(path, kind.to_kind(), log.is_some(), false)?);
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
            let kind = kind.to_kind();
            let rom = Rom::read(path, kind, log.is_some(), debug)?;

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

            let default_panic_hook = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |panic_info| {
                if let Err(cleanup_err) = panic_cleanup_terminal() {
                    eprintln!("Failed to cleanup terminal after panic: {}", cleanup_err);
                } else {
                    eprintln!("");
                }
                default_panic_hook(panic_info);
            }));

            // spawn run threads
            let (run_main_thread, run_render_thread, run_sound_thread) = spawn_run_threads(rom, hz);

            // wait for threads
            run_sound_thread
                .join()
                .expect("Failed to join sound thread");
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
