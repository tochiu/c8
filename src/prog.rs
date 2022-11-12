use crate::interp::{Instruction, InstructionParameters};

use std::{ffi::OsStr, fs::read, io, path::Path, fmt::Display};

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
            Self::COSMACVIP => write!(f, "CHIP8 (COSMAC VIP)")
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
    pub fn read<P: AsRef<Path>>(path: P, kind: ProgramKind) -> io::Result<Program> {
        let program = Program {
            kind,
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
                    "program size ({}B) is below minimum size (2B)",
                    program.data.len()
                ),
            ))
        } else if program.data.len() > PROGRAM_MAX_SIZE as usize {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "program size ({}B) exceeds maximum size ({}B)",
                    program.data.len(),
                    PROGRAM_MAX_SIZE
                ),
            ))
        } else {
            Ok(program)
        }
    }

    pub fn instruction_parameters(&self) -> impl Iterator<Item = InstructionParameters> + '_ {
        self.data
            .windows(2)
            .map(|slice| InstructionParameters::from([slice[0], slice[1]]))
    }

    pub fn instructions(&self) -> impl Iterator<Item = Option<Instruction>> + '_ {
        self.instruction_parameters()
            .map(|params| Instruction::try_from(params).ok())
    }
}
