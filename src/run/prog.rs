use std::{ffi::OsStr, fmt::Display, fs::read, io, path::Path};

pub const PROGRAM_STARTING_ADDRESS: u16 = 0x200;
pub const PROGRAM_MEMORY_SIZE: u16 = 4096;

pub const PROGRAM_MAX_SIZE: u16 = PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS;

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub enum ProgramKind {
    #[default]
    CHIP8,
    CHIP48,
    COSMACVIP,
}

impl Display for ProgramKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CHIP8 => write!(f, "CHIP8"),
            Self::CHIP48 => write!(f, "CHIP48"),
            Self::COSMACVIP => write!(f, "CHIP8 (COSMAC VIP)"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Program {
    pub name: String,
    pub kind: ProgramKind,
    pub data: Vec<u8>,
}

impl Program {
    pub fn read<P: AsRef<Path>>(path: P) -> io::Result<Program> {
        let program = Program {
            kind: match path.as_ref().extension().and_then(OsStr::to_str) {
                Some("ch8" | "c8") => ProgramKind::CHIP8,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Invalid file extension (expected .ch8 or .c8)",
                    ))
                }
            },
            name: path
                .as_ref()
                .file_stem()
                .and_then(OsStr::to_str)
                .unwrap_or("Untitled")
                .into(),
            data: read(path)?,
        };

        if program.data.len() < 2 {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Program size ({}B) is below minimum size (2B)",
                    program.data.len()
                ),
            ))
        } else if program.data.len() > PROGRAM_MAX_SIZE as usize {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Program size ({}B) exceeds maximum size ({}B)",
                    program.data.len(),
                    PROGRAM_MAX_SIZE
                ),
            ))
        } else {
            Ok(program)
        }
    }
}
