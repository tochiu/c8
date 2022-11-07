use std::{fs::read, io, path::Path};

use crate::interp::{Instruction, InstructionParameters};

pub const PROGRAM_STARTING_ADDRESS: u16 = 0x200;
pub const PROGRAM_MEMORY_SIZE: u16 = 4096;

#[derive(Debug, PartialEq, Eq, Default)]
pub enum ProgramKind {
    #[default]
    COMMON,
    CHIP48,
    COSMACVIP,
    // SUPERCHIP,
}

#[derive(Default, Debug)]
pub struct Program {
    pub kind: ProgramKind,
    pub data: Vec<u8>,
}

impl Program {
    pub fn read<P: AsRef<Path>>(path: P, kind: ProgramKind) -> io::Result<Program> {
        Ok(Program {
            kind,
            data: read(path)?,
        })
    }

    pub fn instruction_parameters(&self) -> impl Iterator<Item = InstructionParameters> + '_ {
        self.data
            .chunks_exact(2)
            .map(|chunk| InstructionParameters::from([chunk[0], chunk[1]]))
    }

    pub fn instructions(&self) -> impl Iterator<Item = Option<Instruction>> + '_ {
        self.instruction_parameters()
            .map(|params| Instruction::try_from(params).ok())
    }
}