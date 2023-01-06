use crate::run::rom::RomKind;

use clap::{Parser, Subcommand, ValueEnum};
use log::{Level, LevelFilter};
use std::path::PathBuf;

const DEFAULT_EXECUTION_FREQUENCY: u32 = 2000;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
/// C8: CHIP-8 toolkit complete with a virtual machine, debugger, and disassembler.
/// Supports the modern CHIP-8 specification with SCHIP and XO-CHIP support coming soon.
pub struct Cli {
    #[command(subcommand)]
    pub command: CliCommand,
}

#[derive(ValueEnum, Clone, Copy)]
pub enum KindOption {
    #[clap(aliases = &["c8"])]
    CHIP8,

    #[clap(aliases = &["sc"])]
    SCHIP,

    #[clap(aliases = &["vip"])]
    COSMACVIP,

    #[clap(aliases = &["xo"])]
    XOCHIP,
}

impl KindOption {
    pub fn to_kind(self) -> RomKind {
        match self {
            KindOption::CHIP8 => RomKind::CHIP8,
            KindOption::SCHIP => RomKind::SCHIP,
            KindOption::COSMACVIP => RomKind::COSMACVIP,
            KindOption::XOCHIP => RomKind::XOCHIP,
        }
    }
}

#[derive(ValueEnum, Clone, Copy)]
pub enum LogLevelOption {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl LogLevelOption {
    pub fn to_level(self) -> Level {
        match self {
            LogLevelOption::Trace => Level::Trace,
            LogLevelOption::Debug => Level::Debug,
            LogLevelOption::Info => Level::Info,
            LogLevelOption::Warn => Level::Warn,
            LogLevelOption::Error => Level::Error,
        }
    }

    pub fn to_level_filter(self) -> LevelFilter {
        match self {
            LogLevelOption::Trace => LevelFilter::Trace,
            LogLevelOption::Debug => LevelFilter::Debug,
            LogLevelOption::Info => LevelFilter::Info,
            LogLevelOption::Warn => LevelFilter::Warn,
            LogLevelOption::Error => LevelFilter::Error,
        }
    }
}

#[derive(Subcommand)]
pub enum CliCommand {
    /// Statically checks a CHIP-8 ROM for potential issues
    Check {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL")]
        log: Option<LogLevelOption>,

        /// Sets the ROM kind
        #[arg(long, value_enum)]
        kind: Option<KindOption>,
    },

    /// Disassembles a CHIP-8 ROM
    Dasm {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL")]
        log: Option<LogLevelOption>,

        /// Sets the ROM kind
        #[arg(long, value_enum)]
        kind: Option<KindOption>,
    },

    /// Loads a CHIP-8 ROM and runs it
    Run {
        /// Path of the ROM to load
        #[arg(value_name = "ROM")]
        path: PathBuf,

        /// Runs the ROM in debug mode
        #[arg(short, long)]
        debug: bool,

        /// Sets the instructions executed per second
        #[arg(long, default_value_t = DEFAULT_EXECUTION_FREQUENCY)]
        hz: u32,

        /// Enable logging
        #[arg(short, long, value_enum, value_name = "LEVEL")]
        log: Option<LogLevelOption>,

        /// Sets the ROM kind
        #[arg(long, value_enum)]
        kind: Option<KindOption>,
    },
}
