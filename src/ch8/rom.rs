use super::{
    interp::PROGRAM_STARTING_ADDRESS,
    mem::{DEFAULT_PROGRAM_MEMORY_SIZE, XOCHIP_PROGRAM_MEMORY_SIZE},
};

use crate::asm::Disassembler;

use std::{ffi::OsStr, fmt::Display, fs::read, io, path::Path};

#[derive(Clone)]
pub struct RomConfig {
    pub name: String,
    pub kind: RomKind
    //pub quirks: RomQuirks,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RomKind {
    COSMACVIP,
    CHIP8,
    SCHIP,
    XOCHIP,
}

impl RomKind {
    pub fn max_size(self) -> usize {
        if self == RomKind::XOCHIP {
            XOCHIP_PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS as usize
        } else {
            DEFAULT_PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS as usize
        }
    }

    pub fn default_cycles_per_frame(self) -> u32 {
        match self {
            Self::COSMACVIP => 10,
            Self::CHIP8 => 10,
            Self::SCHIP => 30,
            Self::XOCHIP => 1000,
        }
    }
}

impl Display for RomKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CHIP8 => write!(f, "CHIP8"),
            Self::COSMACVIP => write!(f, "CHIP8 (COSMAC VIP)"),
            Self::SCHIP => write!(f, "SCHIP"),
            Self::XOCHIP => write!(f, "X0-CHIP"),
        }
    }
}

#[derive(Clone)]
pub struct Rom {
    pub config: RomConfig,
    pub data: Vec<u8>,
}

impl Rom {
    pub fn read<P: AsRef<Path>>(
        path: P,
        kind: Option<RomKind>,
    ) -> io::Result<Rom> {
        let data = read(path.as_ref())?;
        let kind =
            kind.unwrap_or_else(|| match path.as_ref().extension().and_then(OsStr::to_str) {
                Some("sc8") => RomKind::SCHIP,
                Some("xo8") => RomKind::XOCHIP,
                _ => {
                    if data.len() > DEFAULT_PROGRAM_MEMORY_SIZE {
                        RomKind::XOCHIP
                    } else {
                        let mut dasm = Disassembler::from(Rom {
                            config: RomConfig {
                                name: String::new(),
                                kind: RomKind::CHIP8
                            },
                            data: data.clone(),
                        });

                        dasm.run();

                        let suggested_rom_kind = dasm.suggested_rom_kind();
                        while suggested_rom_kind != dasm.rom.config.kind {
                            dasm.rom.config.kind = suggested_rom_kind;
                            dasm.reset();
                            dasm.run();
                        }

                        suggested_rom_kind
                    }
                }
            });

        let rom = Rom {
            config: RomConfig {
                name: path
                    .as_ref()
                    .file_stem()
                    .and_then(OsStr::to_str)
                    .unwrap_or("Untitled")
                    .into(),
                kind,
            },
            data,
        };

        let max_rom_size = rom.config.kind.max_size();

        if rom.data.len() < 2 {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("ROM size ({}B) is below minimum size (2B)", rom.data.len()),
            ))
        } else if rom.data.len() > max_rom_size as usize {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "ROM size ({}B) exceeds maximum size ({}B)",
                    rom.data.len(),
                    max_rom_size
                ),
            ))
        } else {
            Ok(rom)
        }
    }
}
