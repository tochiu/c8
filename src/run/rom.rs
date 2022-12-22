use std::{ffi::OsStr, fmt::Display, fs::read, io, path::Path};

use super::interp::{PROGRAM_MEMORY_SIZE, PROGRAM_STARTING_ADDRESS};

pub const MAX_ROM_SIZE: u16 = PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS;

#[derive(Clone)]
pub struct RomConfig {
    pub name: String,
    pub kind: RomKind,
    pub logging: bool,
    pub debugging: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RomKind {
    CHIP8,
    COSMACVIP,
    SCHIP,
}

impl Display for RomKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CHIP8 => write!(f, "CHIP8"),
            Self::COSMACVIP => write!(f, "CHIP8 (COSMAC VIP)"),
            Self::SCHIP => write!(f, "SCHIP"),
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
        kind: RomKind,
        logging: bool,
        debugging: bool,
    ) -> io::Result<Rom> {
        let rom = Rom {
            config: RomConfig {
                name: path
                    .as_ref()
                    .file_stem()
                    .and_then(OsStr::to_str)
                    .unwrap_or("Untitled")
                    .into(),
                kind,
                logging,
                debugging,
            },
            data: read(path)?,
        };

        if rom.data.len() < 2 {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("ROM size ({}B) is below minimum size (2B)", rom.data.len()),
            ))
        } else if rom.data.len() > MAX_ROM_SIZE as usize {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "ROM size ({}B) exceeds maximum size ({}B)",
                    rom.data.len(),
                    MAX_ROM_SIZE
                ),
            ))
        } else {
            Ok(rom)
        }
    }
}
