use crate::asm::write_inst_dasm;

use super::rom::{RomConfig, RomKind};

pub fn decode_op(bits: u32) -> u8 {
    ((bits & 0xF0000000) >> 4 * 7) as u8
}

pub fn decode_x(bits: u32) -> u8 {
    ((bits & 0x0F000000) >> 4 * 6) as u8
}

pub fn decode_y(bits: u32) -> u8 {
    ((bits & 0x00F00000) >> 4 * 5) as u8
}

pub fn decode_n(bits: u32) -> u8 {
    ((bits & 0x000F0000) >> 4 * 4) as u8
}

pub fn decode_nn(bits: u32) -> u8 {
    ((bits & 0x00FF0000) >> 4 * 4) as u8
}

pub fn decode_nnn(bits: u32) -> u16 {
    ((bits & 0x0FFF0000) >> 4 * 4) as u16
}

pub fn decode_nnnn(bits: u32) -> u16 {
    ((bits & 0x0000FFFF) >> 4 * 0) as u16
}

pub enum InstructionDecodeError {
    UnknownInstruction {
        parameters: InstructionParameters,
    },
    IncompatibleRomKind {
        parameters: InstructionParameters,
        instruction: Instruction,
        expected_rom_kind: RomKind,
        actual_rom_kind: RomKind,
    },
}

impl std::fmt::Display for InstructionDecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InstructionDecodeError::UnknownInstruction { parameters } => {
                write!(f, "Unknown instruction: {}", parameters)
            }
            InstructionDecodeError::IncompatibleRomKind {
                parameters,
                instruction,
                expected_rom_kind,
                actual_rom_kind,
            } => {
                let mut message = String::new();
                let mut comment = String::new();
                write_inst_dasm(
                    &instruction,
                    RomConfig {
                        kind: *expected_rom_kind,
                        quirks: expected_rom_kind.default_rom_quirks(),
                    },
                    &mut message,
                    &mut comment,
                )
                .ok();
                write!(
                    f,
                    "{:04X} a.k.a. \"{}\" ({}) is at least a {} instruction but ROM is {}",
                    parameters.significant_bytes(instruction.size()),
                    message,
                    comment,
                    expected_rom_kind,
                    actual_rom_kind
                )
            }
        }
    }
}

// Takes 16 bits (instruction size) and decomposes it into its parts
#[derive(Clone, Copy, Debug)]
pub struct InstructionParameters {
    pub bits: u32,
    pub op: u8,
    pub x: u8,
    pub y: u8,
    pub n: u8,
    pub nn: u8,
    pub nnn: u16,
    pub nnnn: u16,
}

impl From<&[u8]> for InstructionParameters {
    fn from(bytes: &[u8]) -> Self {
        InstructionParameters::new(
            (bytes[0] as u32) << 8 * 3
                | (bytes[1] as u32) << 8 * 2
                | (bytes[2] as u32) << 8 * 1
                | (bytes[3] as u32) << 8 * 0,
        )
    }
}

impl From<[u8; 4]> for InstructionParameters {
    fn from(bytes: [u8; 4]) -> Self {
        InstructionParameters::new(u32::from_be_bytes(bytes))
    }
}

impl InstructionParameters {
    pub fn new(bits: u32) -> Self {
        InstructionParameters {
            bits,
            op: decode_op(bits),
            x: decode_x(bits),
            y: decode_y(bits),
            n: decode_n(bits),
            nn: decode_nn(bits),
            nnn: decode_nnn(bits),
            nnnn: decode_nnnn(bits),
        }
    }

    pub fn significant_bytes(&self, bytes: u16) -> u32 {
        self.bits >> (32 - 8 * bytes)
    }

    pub fn default_significant_bytes(&self) -> u32 {
        self.significant_bytes(2)
    }

    pub fn try_decode(&self, kind: RomKind) -> Result<Instruction, InstructionDecodeError> {
        Instruction::try_from_u32(self.bits, kind)
    }
}

impl std::fmt::Display for InstructionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:04X} (op = {:#X?}, x = {:?}, y = {:?}, n = {:?}, nn = {:?}, nnn = {:?}, nnnn = {:?})",
            self.default_significant_bytes(), self.op, self.x, self.y, self.n, self.nn, self.nnn, self.nnnn
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Exit,
    Jump(u16),
    JumpWithOffset(u16, u8),
    CallSubroutine(u16),
    SubroutineReturn,
    SkipIfEqualsConstant(u8, u8),
    SkipIfNotEqualsConstant(u8, u8),
    SkipIfEquals(u8, u8),
    SkipIfNotEquals(u8, u8),
    SkipIfKeyDown(u8),
    SkipIfKeyNotDown(u8),
    WaitForKey(u8),
    SetConstant(u8, u8),
    AddConstant(u8, u8),
    Set(u8, u8),
    Or(u8, u8),
    And(u8, u8),
    Xor(u8, u8),
    Add(u8, u8),
    Sub(u8, u8, bool),
    Shift(u8, u8, bool),
    GetDelayTimer(u8),
    SetDelayTimer(u8),
    SetSoundTimer(u8),
    SetIndex(u16),
    SetIndexToLong(u16),
    SetIndexToHexChar(u8),
    SetIndexToBigHexChar(u8),
    AddToIndex(u8),
    Load(u8),
    Store(u8),
    LoadRange(u8, u8),
    StoreRange(u8, u8),
    LoadFlags(u8),
    StoreFlags(u8),
    StoreBinaryCodedDecimal(u8),
    GenerateRandom(u8, u8),
    SetPlane(u8),
    Draw(u8, u8, u8),
    ScrollUp(u8),
    ScrollDown(u8),
    ScrollLeft,
    ScrollRight,
    LowResolution,
    HighResolution,
    ClearScreen,
    LoadAudio,
    SetPitch(u8),
}

impl Instruction {
    pub const MAX_INSTRUCTION_SIZE: u16 = 4;
    pub fn size(&self) -> u16 {
        if let &Instruction::SetIndexToLong(_) = self {
            4
        } else {
            2
        }
    }

    pub fn size_or_default(instruction: &Option<Instruction>) -> u16 {
        instruction.as_ref().map_or(2, Instruction::size)
    }

    pub fn try_from_u32(bits: u32, kind: RomKind) -> Result<Instruction, InstructionDecodeError> {
        let op = decode_op(bits);
        let x = decode_x(bits);
        let y = decode_y(bits);
        let n = decode_n(bits);

        let instruction = match (op, x, y, n) {
            (0x0, 0x0, 0xE, 0x0) => Instruction::ClearScreen,
            (0x0, 0x0, 0xE, 0xE) => Instruction::SubroutineReturn,
            (0x0, 0x0, 0xC, __n) => Instruction::ScrollDown(n),
            (0x0, 0x0, 0xD, __n) => Instruction::ScrollUp(n),
            (0x0, 0x0, 0xF, 0xB) => Instruction::ScrollRight,
            (0x0, 0x0, 0xF, 0xC) => Instruction::ScrollLeft,
            (0x0, 0x0, 0xF, 0xD) => Instruction::Exit,
            (0x0, 0x0, 0xF, 0xE) => Instruction::LowResolution,
            (0x0, 0x0, 0xF, 0xF) => Instruction::HighResolution,
            (0x1, __x, __y, __n) => Instruction::Jump(decode_nnn(bits)),
            (0x2, __x, __y, __n) => Instruction::CallSubroutine(decode_nnn(bits)),
            (0x3, __x, __y, __n) => Instruction::SkipIfEqualsConstant(x, decode_nn(bits)),
            (0x4, __x, __y, __n) => Instruction::SkipIfNotEqualsConstant(x, decode_nn(bits)),
            (0x5, __x, __y, 0x0) => Instruction::SkipIfEquals(x, y),
            (0x5, __x, __y, 0x2) => Instruction::StoreRange(x, y),
            (0x5, __x, __y, 0x3) => Instruction::LoadRange(x, y),
            (0x6, __x, __y, __n) => Instruction::SetConstant(x, decode_nn(bits)),
            (0x7, __x, __y, __n) => Instruction::AddConstant(x, decode_nn(bits)),
            (0x8, __x, __y, 0x0) => Instruction::Set(x, y),
            (0x8, __x, __y, 0x1) => Instruction::Or(x, y),
            (0x8, __x, __y, 0x2) => Instruction::And(x, y),
            (0x8, __x, __y, 0x3) => Instruction::Xor(x, y),
            (0x8, __x, __y, 0x4) => Instruction::Add(x, y),
            (0x8, __x, __y, 0x5) => Instruction::Sub(x, y, true),
            (0x8, __x, __y, 0x6) => Instruction::Shift(x, y, true),
            (0x8, __x, __y, 0x7) => Instruction::Sub(x, y, false),
            (0x8, __x, __y, 0xE) => Instruction::Shift(x, y, false),
            (0x9, __x, __y, 0x0) => Instruction::SkipIfNotEquals(x, y),
            (0xA, __x, __y, __n) => Instruction::SetIndex(decode_nnn(bits)),
            (0xB, __x, __y, __n) => Instruction::JumpWithOffset(decode_nnn(bits), x),
            (0xC, __x, __y, __n) => Instruction::GenerateRandom(x, decode_nn(bits)),
            (0xD, __x, __y, __n) => Instruction::Draw(x, y, n),
            (0xE, __x, 0x9, 0xE) => Instruction::SkipIfKeyDown(x),
            (0xE, __x, 0xA, 0x1) => Instruction::SkipIfKeyNotDown(x),
            (0xF, 0x0, 0x0, 0x0) => Instruction::SetIndexToLong(decode_nnnn(bits)),
            (0xF, __x, 0x0, 0x1) => Instruction::SetPlane(x),
            (0xF, 0x0, 0x0, 0x2) => Instruction::LoadAudio,
            (0xF, __x, 0x0, 0x7) => Instruction::GetDelayTimer(x),
            (0xF, __x, 0x0, 0xA) => Instruction::WaitForKey(x),
            (0xF, __x, 0x1, 0x5) => Instruction::SetDelayTimer(x),
            (0xF, __x, 0x1, 0x8) => Instruction::SetSoundTimer(x),
            (0xF, __x, 0x1, 0xE) => Instruction::AddToIndex(x),
            (0xF, __x, 0x2, 0x9) => Instruction::SetIndexToHexChar(x),
            (0xF, __x, 0x3, 0x0) => Instruction::SetIndexToBigHexChar(x),
            (0xF, __x, 0x3, 0x3) => Instruction::StoreBinaryCodedDecimal(x),
            (0xF, __x, 0x3, 0xA) => Instruction::SetPitch(x),
            (0xF, __x, 0x5, 0x5) => Instruction::Store(x),
            (0xF, __x, 0x6, 0x5) => Instruction::Load(x),
            (0xF, __x, 0x7, 0x5) => Instruction::StoreFlags(x),
            (0xF, __x, 0x8, 0x5) => Instruction::LoadFlags(x),
            _ => {
                return Err(InstructionDecodeError::UnknownInstruction {
                    parameters: InstructionParameters::new(bits),
                })
            }
        };

        match instruction {
            Instruction::Exit
            | Instruction::LowResolution
            | Instruction::HighResolution
            | Instruction::ScrollDown(_)
            | Instruction::ScrollRight
            | Instruction::ScrollLeft
            | Instruction::SetIndexToBigHexChar(_) => {
                if kind < RomKind::SCHIP {
                    return Err(InstructionDecodeError::IncompatibleRomKind {
                        instruction,
                        parameters: InstructionParameters::new(bits),
                        expected_rom_kind: RomKind::SCHIP,
                        actual_rom_kind: kind,
                    });
                }
            }
            Instruction::ScrollUp(_)
            | Instruction::LoadAudio
            | Instruction::SetPitch(_)
            | Instruction::LoadRange(_, _)
            | Instruction::StoreRange(_, _)
            | Instruction::SetIndexToLong(_)
            | Instruction::SetPlane(_) => {
                if kind < RomKind::XOCHIP {
                    return Err(InstructionDecodeError::IncompatibleRomKind {
                        instruction,
                        parameters: InstructionParameters::new(bits),
                        expected_rom_kind: RomKind::XOCHIP,
                        actual_rom_kind: kind,
                    });
                }
            }
            Instruction::LoadFlags(vx) | Instruction::StoreFlags(vx) => {
                if kind < RomKind::SCHIP {
                    return Err(InstructionDecodeError::IncompatibleRomKind {
                        instruction,
                        parameters: InstructionParameters::new(bits),
                        expected_rom_kind: RomKind::SCHIP,
                        actual_rom_kind: kind,
                    });
                } else if vx > 0x7 && kind < RomKind::XOCHIP {
                    return Err(InstructionDecodeError::IncompatibleRomKind {
                        instruction,
                        parameters: InstructionParameters::new(bits),
                        expected_rom_kind: RomKind::XOCHIP,
                        actual_rom_kind: kind,
                    });
                }
            }
            _ => (),
        };

        Ok(instruction)
    }
}
