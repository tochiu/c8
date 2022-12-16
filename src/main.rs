extern crate log;

mod asm;
mod cli;
mod config;
mod dbg;
mod render;
mod run;

use {
    asm::Disassembler,
    cli::{Cli, CliCommand},
    config::C8Config,
    run::{core::spawn_run_threads, prog::Program},
};

use anyhow::Result;
use clap::Parser;
use crossterm::style::Stylize;
use log::LevelFilter;

use std::io::stdout;

use crate::render::panic_cleanup_terminal;

fn main() -> Result<()> {
    match Cli::parse().command {
        CliCommand::Check { path, log } => {
            if let Some(level) = log.to_level_filter().to_level() {
                simple_logger::init_with_level(level)?;
            }

            let mut disasm = Disassembler::from(Program::read(path)?);
            disasm.run();
            disasm.write_issue_traces(&mut stdout())?;
        }
        CliCommand::Dasm { path, log } => {
            if let Some(level) = log.to_level_filter().to_level() {
                simple_logger::init_with_level(level)?;
            }

            let mut disasm = Disassembler::from(Program::read(path)?);
            disasm.run();
            print!("{}", disasm);
        }
        CliCommand::Run { path, debug, hz, log } => {
            let logger_level = log.to_level_filter();
            let program = Program::read(path)?;

            if logger_level != LevelFilter::Off {
                tui_logger::init_logger(logger_level)?;
                tui_logger::set_default_level(logger_level);
            }

            // config
            let config = C8Config {
                title: format!(" {} Virtual Machine ({}) ", program.kind, program.name),
                logging: logger_level != LevelFilter::Off,
                instruction_frequency: hz,
                debugging: debug,
                ..Default::default()
            };

            // preempt wait thread message
            println!(
                "\n  {} for {} thread",
                format!("Waiting").green().bold(),
                program.kind
            );

            let default_panic_hook = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |panic_info| {
                if let Err(cleanup_err) = panic_cleanup_terminal() {
                    eprintln!("Failed to cleanup terminal after panic: {}", cleanup_err);
                }
                default_panic_hook(panic_info);
            }));

            // spawn run threads
            let (run_render_thread, run_main_thread) = spawn_run_threads(program, config);

            // wait for threads
            run_render_thread.join().expect("Failed to join render thread");
            match run_main_thread.join().expect("Failed to join run thread") {
                Ok(analytics) => println!("{}", analytics),
                Err(err) => println!("\n    {} {}", format!("Error").red().bold(), err),
            }
        }
    }

    Ok(())
}
