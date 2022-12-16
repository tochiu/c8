use crate::config::DEFAULT_INSTRUCTION_FREQUENCY;

use clap::{Parser, Subcommand};
use log::LevelFilter;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
/// C8: CHIP-8 toolkit complete with a virtual machine, debugger, and disassembler.
/// Supports the modern CHIP-8 specification with SCHIP and XO-CHIP support coming soon.
pub struct Cli {
    #[command(subcommand)]
    pub command: CliCommand,
}

#[derive(clap::ValueEnum, Clone, Copy)]
pub enum LogLevel {
    Off,
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl LogLevel {
    pub fn to_level_filter(self) -> LevelFilter {
        match self {
            LogLevel::Trace => LevelFilter::Trace,
            LogLevel::Debug => LevelFilter::Debug,
            LogLevel::Info => LevelFilter::Info,
            LogLevel::Warn => LevelFilter::Warn,
            LogLevel::Error => LevelFilter::Error,
            LogLevel::Off => LevelFilter::Off,
        }
    }
}

#[derive(Subcommand)]
pub enum CliCommand {
    /// C8 CHECK: Statically checks a CHIP-8 ROM for issues
    Check {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL", default_value_t=LogLevel::Off)]
        log: LogLevel,
    },

    /// C8 DASM: Disassembles a CHIP-8 ROM
    Dasm {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL", default_value_t=LogLevel::Off)]
        log: LogLevel,
    },

    /// C8 RUN: Loads a CHIP-8 ROM and runs it
    Run {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Runs the ROM in debug mode
        #[arg(short, long)]
        debug: bool,

        /// Sets the instructions executed per second
        #[arg(long, default_value_t = DEFAULT_INSTRUCTION_FREQUENCY)]
        hz: u16,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL", default_value_t=LogLevel::Off)]
        log: LogLevel,
    },
}
