use crate::run::input::Key;

use clap::{Parser, Subcommand, ValueEnum};

use std::{num::ParseIntError, path::PathBuf, str::FromStr};

fn parse_key(value: &str) -> Result<Key, &'static str> {
    if value.starts_with("0x") {
        u8::from_str_radix(&value[2..], 16)
            .map_err(|_| "")
            .and_then(Key::try_from)
    } else {
        Key::try_from(value)
    }
    .map_err(|_| "Key must be <QUERTY KEY> or 0x<CHIP-8 KEY>")
}

pub fn parse_addr(arg: &str) -> Result<u16, ParseIntError> {
    if arg.starts_with("0x") {
        u16::from_str_radix(arg.trim_start_matches("0x"), 16)
    } else {
        u16::from_str_radix(arg, 10)
    }
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[clap(name = "", no_binary_name = true)]
pub struct DebugCli {
    #[command(subcommand)]
    pub command: DebugCliCommand,
}

#[derive(Subcommand, Clone)] // TODO
pub enum KeyCommand {
    /// Execute a key up
    #[clap(visible_aliases = &["u"])]
    Up {
        #[arg(value_name = "KEY")]
        #[arg(value_parser = parse_key)]
        key: Key,
    },

    /// Execute a key down
    #[clap(visible_aliases = &["d"])]
    Down {
        #[arg(value_name = "KEY")]
        #[arg(value_parser = parse_key)]
        key: Key,
    },

    /// Execute a key up, down, and up sequence
    #[clap(visible_aliases = &["p"])]
    Press {
        #[arg(value_name = "KEY")]
        #[arg(value_parser = parse_key)]
        key: Key,
    },

    /// Switch keyboard interface to QWERTY or CHIP-8
    #[clap(visible_aliases = &["s"])]
    Switch,
}

#[derive(Subcommand, Clone)]
pub enum ClearCommand {
    /// Clear keyboard state
    #[clap(visible_aliases = &["k", "keys"])]
    Keyboard,

    /// Clear a breakpoint address
    #[clap(visible_aliases = &["b"])]
    Break {
        #[arg(value_name = "ADDRESS", value_parser = parse_addr)]
        breakpoint: u16,
    },

    /// Clear a watchpoint pointer, register, or address
    #[clap(visible_aliases = &["w"])]
    Watch { watchpoint: WatchOption },

    /// Clear all subcommand
    All {
        #[command(subcommand)]
        what: WatchBreakOption,
    },
}

#[derive(Subcommand, Clone)]
pub enum WatchBreakOption {
    #[clap(visible_aliases = &["b"])]
    Break,

    #[clap(visible_aliases = &["w"])]
    Watch,
}

#[derive(Subcommand, Clone)]
pub enum ShowHideOption {
    /// Program display output
    #[clap(visible_aliases = &["d", "disp", "vm"])]
    Display,

    /// Current memory state of the program
    #[clap(visible_aliases = &["m", "mem"])]
    Memory,

    /// Memory but without address collapsing around instructions and both graphics and instruction descriptions visible
    Verbose,
}

#[derive(Subcommand, Clone)]
pub enum DumpOption {
    /// Write memory state
    #[clap(visible_aliases = &["m", "mem"])]
    Memory {
        #[arg(value_name = "FILE PATH")]
        path: PathBuf,
    },
}

#[derive(Clone)]
pub enum GotoOption {
    SemanticLocation(SemanticLocation),
    Pointer(Pointer),
    Address(u16),
}

impl FromStr for GotoOption {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "start" => Ok(Self::SemanticLocation(SemanticLocation::Start)),
            "end" => Ok(Self::SemanticLocation(SemanticLocation::End)),
            "pc" => Ok(Self::Pointer(Pointer::Pc)),
            "i" => Ok(Self::Pointer(Pointer::I)),
            _ => parse_addr(value)
                .map(|addr| Self::Address(addr))
                .map_err(|_| {
                    "Location must be \"start\", \"end\", \"pc\", \"i\", or a valid address"
                }),
        }
    }
}

#[derive(Clone)]
pub enum WatchOption {
    Register(Register),
    Pointer(Pointer),
    Address(u16),
}

impl FromStr for WatchOption {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "pc" => Ok(Self::Pointer(Pointer::Pc)),
            "i" => Ok(Self::Pointer(Pointer::I)),
            _ => {
                if value.starts_with('v') {
                    Register::from_str(value, false)
                        .map(|reg| Self::Register(reg))
                        .map_err(|_| {
                            "Watchpoint must be \"pc\", \"i\", a valid register, or a valid address"
                        })
                } else {
                    parse_addr(value)
                        .map(|addr| Self::Address(addr))
                        .map_err(|_| {
                            "Watchpoint must be \"pc\", \"i\", a valid register, or a valid address"
                        })
                }
            }
        }
    }
}

#[derive(ValueEnum, Clone, Copy)]
pub enum SemanticLocation {
    Start,
    End,
}

#[derive(ValueEnum, Clone, Copy)]
pub enum Pointer {
    Pc,
    I,
}

#[derive(ValueEnum, Clone, Copy)]
pub enum Register {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    #[clap(aliases = &["v10"])]
    Va,
    #[clap(aliases = &["v11"])]
    Vb,
    #[clap(aliases = &["v12"])]
    Vc,
    #[clap(aliases = &["v13"])]
    Vd,
    #[clap(aliases = &["v14"])]
    Ve,
    #[clap(aliases = &["v15"])]
    Vf,
}

impl Register {
    pub fn to_index(self) -> u8 {
        match self {
            Self::V0 => 0,
            Self::V1 => 1,
            Self::V2 => 2,
            Self::V3 => 3,
            Self::V4 => 4,
            Self::V5 => 5,
            Self::V6 => 6,
            Self::V7 => 7,
            Self::V8 => 8,
            Self::V9 => 9,
            Self::Va => 10,
            Self::Vb => 11,
            Self::Vc => 12,
            Self::Vd => 13,
            Self::Ve => 14,
            Self::Vf => 15,
        }
    }
}

#[derive(Subcommand)]
pub enum DebugCliCommand {
    /// Continue running the program until the next breakpoint, watchpoint or error
    #[clap(visible_aliases = &["c", "cont"])]
    Continue,

    /// Run the next N (default = 1) instructions of the program
    #[clap(visible_aliases = &["s"])]
    Step {
        #[arg(value_name = "AMOUNT", default_value_t = 1)]
        amount: usize,
    },

    /// Set the instructions executed per second of the program
    #[clap(visible_aliases = &["hz", "ips", "rate", "freq", "frequency"])]
    Hertz {
        #[arg(value_name = "FREQUENCY")]
        hertz: u16,
    },

    /// Record the program history as it executes
    #[clap(visible_aliases = &["r", "rec"])]
    Record,

    /// Stop recording and clear program history
    Stop,

    /// Redo the next N (default = 1) instructions within the program history
    #[clap(visible_aliases = &["ff", "fast-forward", ">>"])]
    Redo {
        #[arg(value_name = "STEP SIZE", default_value_t = 1)]
        amount: usize,
    },

    /// Undo the last N (default = 1) instructions within the program history
    #[clap(visible_aliases = &["rw", "rewind", "<<"])]
    Undo {
        #[arg(value_name = "STEP SIZE", default_value_t = 1)]
        amount: usize,
    },

    /// Navigate the program history view
    #[clap(visible_aliases = &["hist"])]
    History,

    /// Navigate the memory view
    #[clap(visible_aliases = &["m", "mem"])]
    Memory,

    /// Go to a location in memory
    #[clap(visible_aliases = &["g"])]
    Goto {
        /// [possible values: start, end, pc, i, <ADDRESS>]
        location: GotoOption,
    },

    /// Follow a pointer in memory
    #[clap(visible_aliases = &["f"])]
    Follow { pointer: Pointer },

    /// Unfollow the followed pointer
    #[clap(visible_aliases = &["uf"])]
    Unfollow,

    /// Set a breakpoint at an address
    #[clap(visible_aliases = &["b"])]
    Break {
        #[arg(value_name = "ADDRESS", value_parser = parse_addr)]
        address: u16,
    },

    /// Watch a register, pointer, or address for change
    #[clap(visible_aliases = &["w"])]
    Watch { watchpoint: WatchOption },

    /// Execute show subcommand
    Show {
        #[command(subcommand)]
        view: ShowHideOption,
    },

    /// Execute hide subcommand
    Hide {
        #[command(subcommand)]
        view: ShowHideOption,
    },

    /// Execute info subcommand
    #[clap(visible_aliases = &["i"])]
    Info {
        #[command(subcommand)]
        what: WatchBreakOption,
    },

    /// Execute keyboard subcommand
    #[clap(visible_aliases = &["k"])]
    Key {
        #[command(subcommand)]
        command: KeyCommand,
    },

    /// Execute clear subcommand
    #[clap(visible_aliases = &["clr"])]
    Clear {
        #[command(subcommand)]
        command: ClearCommand,
    },

    /// Execute dump subcommand
    #[clap(visible_aliases = &["d"])]
    Dump {
        #[command(subcommand)]
        what: DumpOption,
    },
}