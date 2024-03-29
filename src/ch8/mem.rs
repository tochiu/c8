use super::{
    instruct::{Instruction, InstructionParameters},
    interp::PROGRAM_STARTING_ADDRESS,
    rom::{Rom, RomKind},
};

use std::{ops::Range, slice::Windows};

pub const MEM_ACCESS_DRAW_FLAG: u8 = 0b1;
pub const MEM_ACCESS_READ_FLAG: u8 = 0b10;
pub const MEM_ACCESS_WRITE_FLAG: u8 = 0b100;
pub const MEM_ACCESS_EXEC_FLAG: u8 = 0b1000;

pub fn extract_access_flags(flag: u8) -> (bool, bool, bool, bool) {
    (
        flag & MEM_ACCESS_DRAW_FLAG == MEM_ACCESS_DRAW_FLAG,
        flag & MEM_ACCESS_READ_FLAG == MEM_ACCESS_READ_FLAG,
        flag & MEM_ACCESS_WRITE_FLAG == MEM_ACCESS_WRITE_FLAG,
        flag & MEM_ACCESS_EXEC_FLAG == MEM_ACCESS_EXEC_FLAG,
    )
}

pub const FONT_STARTING_ADDRESS: u16 = 0x50; // store font in memory from 0x50 to 0x9F inclusive
pub const FONT_CHAR_DATA_SIZE: u8 = 5;
pub const FONT: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

pub const BIG_FONT_STARTING_ADDRESS: u16 = 0xA0; // store big font in memory from 0xA0 to 0xFF inclusive
pub const BIG_FONT_CHAR_DATA_SIZE: u8 = 10;
pub const BIG_FONT: [u8; 100] = [
    0x3C, 0x7E, 0xE7, 0xC3, 0xC3, 0xC3, 0xC3, 0xE7, 0x7E, 0x3C, // 0
    0x18, 0x38, 0x58, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3C, // 1
    0x3E, 0x7F, 0xC3, 0x06, 0x0C, 0x18, 0x30, 0x60, 0xFF, 0xFF, // 2
    0x3C, 0x7E, 0xC3, 0x03, 0x0E, 0x0E, 0x03, 0xC3, 0x7E, 0x3C, // 3
    0x06, 0x0E, 0x1E, 0x36, 0x66, 0xC6, 0xFF, 0xFF, 0x06, 0x06, // 4
    0xFF, 0xFF, 0xC0, 0xC0, 0xFC, 0xFE, 0x03, 0xC3, 0x7E, 0x3C, // 5
    0x3E, 0x7C, 0xE0, 0xC0, 0xFC, 0xFE, 0xC3, 0xC3, 0x7E, 0x3C, // 6
    0xFF, 0xFF, 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x60, 0x60, // 7
    0x3C, 0x7E, 0xC3, 0xC3, 0x7E, 0x7E, 0xC3, 0xC3, 0x7E, 0x3C, // 8
    0x3C, 0x7E, 0xC3, 0xC3, 0x7F, 0x3F, 0x03, 0x03, 0x3E, 0x7C, // 9
];

pub const DEFAULT_PROGRAM_MEMORY_SIZE: usize = 4096;
pub const XOCHIP_PROGRAM_MEMORY_SIZE: usize = 65536;

pub trait MemoryRef {
    fn address_add(&self, lhs: u16, rhs: u16) -> u16
    where
        Self: AsRef<[u8]>,
    {
        lhs.overflowing_add(rhs).0 & (self.as_ref().len() - 1) as u16
    }

    fn address_sub(&self, lhs: u16, rhs: u16) -> u16
    where
        Self: AsRef<[u8]>,
    {
        lhs.overflowing_sub(rhs).0 & (self.as_ref().len() - 1) as u16
    }

    fn affected_instruction_range(&self, address: u16, size: u16) -> (Range<usize>, Range<usize>)
    where
        Self: AsRef<[u8]>,
    {
        let memory = self.as_ref();
        let range_start =
            self.address_sub(address as u16, Instruction::MAX_INSTRUCTION_SIZE - 1) as usize;
        let range_end =
            range_start + size as usize + Instruction::MAX_INSTRUCTION_SIZE as usize - 1;

        if range_end > memory.len() {
            let range_overflow = range_end - memory.len();
            let left_range = range_start..memory.len();
            let right_range = 0..range_overflow;
            (left_range, right_range)
        } else {
            (range_start..range_end, 0..0)
        }
    }

    fn export(&self, address: u16, dst: &mut [u8])
    where
        Self: AsRef<[u8]>,
    {
        let memory = self.as_ref();
        let export_start = address as usize % memory.len();
        let export_end = export_start + dst.len();

        if export_end > memory.len() {
            let export_overflow = export_end - memory.len();
            let (left_dst, right_dst) = dst.split_at_mut(memory.len() - export_start);
            left_dst.copy_from_slice(&memory[export_start..]);
            right_dst.copy_from_slice(&memory[..export_overflow]);
        } else {
            dst.copy_from_slice(&memory[export_start..export_end]);
        }
    }

    fn instruction_parameters(&self) -> MemoryInstructionParametersIterator
    where
        Self: AsRef<[u8]>,
    {
        MemoryInstructionParametersIterator::new(self.as_ref())
    }
}

pub trait MemoryMut {
    fn import(&mut self, src: &[u8], address: u16)
    where
        Self: AsMut<[u8]>,
    {
        let memory = self.as_mut();
        let import_start = address as usize % memory.len();
        let import_end = import_start + src.len();

        if import_end > memory.len() {
            let import_overflow = import_end - memory.len();
            let (left_src, right_src) = src.split_at(memory.len() - import_start);
            memory[import_start..].copy_from_slice(left_src);
            memory[..import_overflow].copy_from_slice(right_src);
        } else {
            memory[import_start..import_end].copy_from_slice(src);
        }
    }
}

impl<T> MemoryRef for T where T: AsRef<[u8]> + ?Sized {}
impl<T> MemoryMut for T where T: AsMut<[u8]> + ?Sized {}

pub struct MemoryInstructionParametersIterator<'a> {
    window: Windows<'a, u8>,
    edge: MemoryInstructionBytesIterator<'a>,
}

impl<'a> MemoryInstructionParametersIterator<'a> {
    pub fn new(memory: &'a [u8]) -> Self {
        Self {
            window: memory.windows(4),
            edge: MemoryInstructionBytesIterator::new(memory, memory.len().saturating_sub(3)),
        }
    }
}

impl<'a> Iterator for MemoryInstructionParametersIterator<'_> {
    type Item = InstructionParameters;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(slice) = self.window.next() {
            Some(InstructionParameters::from(slice))
        } else {
            Some(InstructionParameters::new(self.edge.next()?))
        }
    }
}

pub struct MemoryInstructionBytesIterator<'a> {
    memory: &'a [u8],
    index: usize,
    buffer: [u8; 4],
}

impl<'a> MemoryInstructionBytesIterator<'a> {
    pub fn new(memory: &'a [u8], index: usize) -> Self {
        Self {
            memory,
            index,
            buffer: [
                0,
                memory[(index + 0) % memory.len()],
                memory[(index + 1) % memory.len()],
                memory[(index + 2) % memory.len()],
            ],
        }
    }
}

impl<'a> Iterator for MemoryInstructionBytesIterator<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.memory.len() {
            return None;
        }

        self.buffer.copy_within(1.., 0);
        self.buffer[3] = self.memory[(self.index + 3) % self.memory.len()];

        self.index += 1;

        Some(u32::from_be_bytes(self.buffer))
    }
}

pub fn allocate_memory(rom: &Rom) -> Vec<u8> {
    let mut memory = vec![
        0;
        if rom.config.kind == RomKind::XOCHIP {
            XOCHIP_PROGRAM_MEMORY_SIZE
        } else {
            DEFAULT_PROGRAM_MEMORY_SIZE
        }
    ];

    memory.import(&rom.data, PROGRAM_STARTING_ADDRESS);
    memory.import(&FONT, FONT_STARTING_ADDRESS);
    if rom.config.kind >= RomKind::SCHIP {
        memory.import(&BIG_FONT, BIG_FONT_STARTING_ADDRESS);
    }

    memory
}
