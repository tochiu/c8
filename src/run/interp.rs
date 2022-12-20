use super::{
    disp::{DisplayMode, Display},
    input::Key,
    rom::{Rom, RomKind}
};

use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

pub const VFLAG: usize = 15;

pub const PROGRAM_STARTING_ADDRESS: u16 = 0x200;
pub const PROGRAM_MEMORY_SIZE: u16 = 4096;

const FONT_STARTING_ADDRESS: u16 = 0x50; // store font in memory from 0x50 to 0x9F inclusive
const FONT_CHAR_DATA_SIZE: u8 = 5;
const FONT: [u8; 80] = [
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

const BIG_FONT_STARTING_ADDRESS: u16 = 0xA0; // store big font in memory from 0xA0 to 0xFF inclusive
const BIG_FONT_CHAR_DATA_SIZE: u8 = 10;
const BIG_FONT: [u8; 100] = [
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

// Takes 16 bits (instruction size) and decomposes it into its parts
#[derive(Clone, Copy, Debug)]
pub struct InstructionParameters {
    pub bits: u16,
    pub op: u8,
    pub x: u8,
    pub y: u8,
    pub n: u8,
    pub nn: u8,
    pub nnn: u16,
}

impl From<u16> for InstructionParameters {
    fn from(bits: u16) -> Self {
        InstructionParameters {
            bits,
            op: ((bits & 0xF000) >> 4 * 3) as u8,
            x: ((bits & 0x0F00) >> 4 * 2) as u8,
            y: ((bits & 0x00F0) >> 4 * 1) as u8,
            n: ((bits & 0x000F) >> 4 * 0) as u8,
            nn: ((bits & 0x00FF) >> 4 * 0) as u8,
            nnn: ((bits & 0x0FFF) >> 4 * 0) as u16,
        }
    }
}

impl From<[u8; 2]> for InstructionParameters {
    fn from(bytes: [u8; 2]) -> Self {
        InstructionParameters::from((bytes[0] as u16) << 8 | bytes[1] as u16)
    }
}

impl InstructionParameters {
    pub fn try_decode(&self, kind: RomKind) -> Result<Instruction, String> {
        let (op, x, y, n, nn, nnn) = (
            self.op, self.x, self.y, self.n, self.nn, self.nnn,
        );

        match op {
            0x0 => match nnn {
                0x0E0 => Ok(Instruction::ClearScreen),
                0x0EE => Ok(Instruction::SubroutineReturn),
                _ => {
                    match y {
                        0x00C => Ok(Instruction::ScrollDown(n)),
                        _ => match nnn {
                            0x0FB => Ok(Instruction::ScrollRight),
                            0x0FC => Ok(Instruction::ScrollLeft),
                            0x0FD => Ok(Instruction::Exit),
                            0x0FE => Ok(Instruction::LowResolution),
                            0x0FF => Ok(Instruction::HighResolution),
                            _ => Err(format!("unable to decode instruction {}", self))
                        }
                    }.and_then(|instruction| {
                        if kind == RomKind::SCHIP {
                            Ok(instruction)
                        } else {
                            Err(format!("{:?} is a {} instruction but program is {}", instruction, RomKind::SCHIP, kind))
                        }
                    })
                },
            },
            0x1 => Ok(Instruction::Jump(nnn)),
            0x2 => Ok(Instruction::CallSubroutine(nnn)),
            0x3 => Ok(Instruction::SkipIfEqualsConstant(x, nn)),
            0x4 => Ok(Instruction::SkipIfNotEqualsConstant(x, nn)),
            0x5 => Ok(Instruction::SkipIfEquals(x, y)),
            0x6 => Ok(Instruction::SetConstant(x, nn)),
            0x7 => Ok(Instruction::AddConstant(x, nn)),
            0x8 => match n {
                0x0 => Ok(Instruction::Set(x, y)),
                0x1 => Ok(Instruction::Or(x, y)),
                0x2 => Ok(Instruction::And(x, y)),
                0x3 => Ok(Instruction::Xor(x, y)),
                0x4 => Ok(Instruction::Add(x, y)),
                0x5 => Ok(Instruction::Sub(x, y, true)),
                0x6 => Ok(Instruction::Shift(x, y, true)),
                0x7 => Ok(Instruction::Sub(x, y, false)),
                0xE => Ok(Instruction::Shift(x, y, false)),
                _ => Err(format!("unable to decode instruction {}", self)),
            },
            0x9 => match n {
                0x0 => Ok(Instruction::SkipIfNotEquals(x, y)),
                _ => Err(format!("unable to decode instruction {}", self)),
            },
            0xA => Ok(Instruction::SetIndex(nnn)),
            0xB => Ok(Instruction::JumpWithOffset(nnn, x)),
            0xC => Ok(Instruction::GenerateRandom(x, nn)),
            0xD => Ok(Instruction::Display(x, y, n)),
            0xE => match nn {
                0x9E => Ok(Instruction::SkipIfKeyDown(x)),
                0xA1 => Ok(Instruction::SkipIfKeyNotDown(x)),
                _ => Err(format!("unable to decode instruction {}", self)),
            },
            0xF => match nn {
                0x07 => Ok(Instruction::GetDelayTimer(x)),
                0x15 => Ok(Instruction::SetDelayTimer(x)),
                0x18 => Ok(Instruction::SetSoundTimer(x)),
                0x1E => Ok(Instruction::AddToIndex(x)),
                0x0A => Ok(Instruction::GetKey(x)),
                0x29 => Ok(Instruction::SetIndexToHexChar(x)),
                0x33 => Ok(Instruction::StoreDecimal(x)),
                0x55 => Ok(Instruction::Store(x)),
                0x65 => Ok(Instruction::Load(x)),
                _ => match nn {
                    0x30 => Ok(Instruction::SetIndexToBigHexChar(x)),
                    0x75 => Ok(Instruction::StoreFlags(x)),
                    0x85 => Ok(Instruction::LoadFlags(x)),
                    _ => Err(format!("unable to decode instruction {}", self)),
                }.and_then(|instruction| {
                    if kind == RomKind::SCHIP {
                        Ok(instruction)
                    } else {
                        Err(format!("{:?} is a {} instruction but program is {}", instruction, RomKind::SCHIP, kind))
                    }
                }),
            },
            _ => Err(format!("unable to decode instruction {}", self)),
        }
    }
}

impl std::fmt::Display for InstructionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#06X} (op = {:#X?}, x = {:?}, y = {:?}, n = {:?}, nn = {:?}, nnn = {:?})",
            self.bits, self.op, self.x, self.y, self.n, self.nn, self.nnn
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
    GetKey(u8),
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
    SetIndexToHexChar(u8),
    SetIndexToBigHexChar(u8),
    AddToIndex(u8),
    Load(u8),
    Store(u8),
    LoadFlags(u8),
    StoreFlags(u8),
    StoreDecimal(u8),
    GenerateRandom(u8, u8),
    Display(u8, u8, u8),
    ScrollDown(u8),
    ScrollLeft,
    ScrollRight,
    LowResolution,
    HighResolution,
    ClearScreen,
}

// State the interpreter pulls from IO is stored here
#[derive(Debug, Default)]
pub struct InterpreterInput {
    pub delay_timer: u8,

    pub down_keys: u16,
    pub just_pressed_key: Option<u8>,
    pub just_released_key: Option<u8>,
}

// Response body so IO know how to proceed
pub struct InterpreterOutput {
    pub display: Display,
    pub awaiting_input: bool,

    pub request: Option<InterpreterRequest>,
}

// Interpreter IO Request
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InterpreterRequest {
    Display,
    SetDelayTimer(u8),
    SetSoundTimer(u8),
}

pub type InterpreterMemory = [u8; PROGRAM_MEMORY_SIZE as usize];

#[derive(Eq, PartialEq)]
pub enum PartialInterpreterStatePayload {
    Rng(StdRng),
    Display(Display),
    Flags([u8; 8]),
}

#[derive(PartialEq, Eq)]
pub struct InterpreterHistoryFragment {
    pub instruction: Option<Instruction>,
    pub pc: u16,
    pub return_address: u16,
    pub index: u16,
    pub index_memory: [u8; 32],
    pub registers: [u8; 16],
    pub payload: Option<Box<PartialInterpreterStatePayload>>,
}

impl From<&Interpreter> for InterpreterHistoryFragment {
    fn from(interp: &Interpreter) -> Self {
        let mut index_memory = [0; 32];
        let index = interp.index as usize;
        if index < interp.memory.len() {
            let n = (index + 32).min(interp.memory.len()) - index;
            index_memory[..n].copy_from_slice(&interp.memory[index..index + n]);
        }

        let instruction = interp.try_fetch_decode().ok();

        InterpreterHistoryFragment {
            payload: instruction.and_then(|instruction| match instruction {
                Instruction::GenerateRandom(_, _) => Some(Box::new(PartialInterpreterStatePayload::Rng(interp.rng.clone()))),
                Instruction::ClearScreen | Instruction::ScrollDown(_) | Instruction::ScrollLeft | Instruction::ScrollRight | Instruction::LowResolution | Instruction::HighResolution => Some(Box::new(PartialInterpreterStatePayload::Display(interp.output.display.clone()))),
                Instruction::StoreFlags(_) => Some(Box::new(PartialInterpreterStatePayload::Flags(interp.flags))),
                _ => Some(Box::new(PartialInterpreterStatePayload::Display(interp.output.display.clone()))),
            }),

            pc: interp.pc,
            instruction,
            return_address: interp.stack.last().cloned().unwrap_or_default(),
            index: interp.index,
            index_memory,
            registers: interp.registers,
        }
    }
}

impl InterpreterHistoryFragment {
    pub(super) fn are_get_key_forms(&self, rhs: &Self) -> bool {
        if let Some(&Instruction::GetKey(_)) = self.instruction.as_ref() {
            self == rhs
        } else {
            false
        }
    }

    pub(super) fn does_modify_display(&self) -> bool {
        match self.instruction {
            Some(Instruction::ClearScreen | Instruction::ScrollDown(_) | Instruction::ScrollLeft | Instruction::ScrollRight | Instruction::LowResolution | Instruction::HighResolution | Instruction::Display(_, _, _)) => true,
            _ => false,
        }
    }
}

pub struct Interpreter {
    pub memory: InterpreterMemory,
    pub pc: u16,
    pub index: u16,
    pub stack: Vec<u16>,
    pub flags: [u8; 8],
    pub registers: [u8; 16],
    pub input: InterpreterInput,
    pub output: InterpreterOutput,
    pub rom: Rom,
    pub rng: StdRng,
}

impl<'a> From<Rom> for Interpreter {
    fn from(rom: Rom) -> Self {
        Interpreter {
            memory: Self::alloc(&rom),
            rom,
            pc: PROGRAM_STARTING_ADDRESS,
            index: 0,
            stack: Vec::with_capacity(16),
            flags: [0; 8],
            registers: [0; 16],
            rng: StdRng::from_entropy(),
            input: Default::default(),
            output: InterpreterOutput {
                display: Default::default(),
                awaiting_input: false,
                request: None,
            },
        }
    }
}

impl Interpreter {
    pub fn instruction_parameters(
        binary: &[u8],
    ) -> impl Iterator<Item = InstructionParameters> + '_ {
        binary
            .windows(2)
            .map(|slice| InstructionParameters::from([slice[0], slice[1]]))
    }

    pub fn alloc(rom: &Rom) -> InterpreterMemory {
        let mut memory = [0; PROGRAM_MEMORY_SIZE as usize];

        memory[FONT_STARTING_ADDRESS as usize..FONT_STARTING_ADDRESS as usize + FONT.len()]
            .copy_from_slice(&FONT);

        memory[BIG_FONT_STARTING_ADDRESS as usize..BIG_FONT_STARTING_ADDRESS as usize + BIG_FONT.len()]
            .copy_from_slice(&BIG_FONT);

        memory[PROGRAM_STARTING_ADDRESS as usize
            ..PROGRAM_STARTING_ADDRESS as usize + rom.data.len()]
            .copy_from_slice(&rom.data);

        memory
    }

    pub fn checked_addr_add(&self, addr: u16, amt: u16) -> Option<u16> {
        let (result_addr, result_overflow) = addr.overflowing_add(amt);
        if (addr as usize) < self.memory.len()
            && (result_addr as usize) < self.memory.len()
            && !result_overflow
        {
            Some(result_addr)
        } else {
            None
        }
    }

    pub fn undo(&mut self, prior_state: &InterpreterHistoryFragment) {
        self.pc = prior_state.pc;
        self.index = prior_state.index;
        self.registers = prior_state.registers;

        let index = self.index as usize;
        let n = (index + 32).min(self.memory.len()) - index;

        self.memory[index..index + n].copy_from_slice(&prior_state.index_memory);
        let Some(inst) = prior_state.instruction.as_ref() else {
            unreachable!("Cannot undo to a state without an instruction")
        };
        log::trace!("Undoing instruction: {:?}", inst);

        match inst {
            Instruction::CallSubroutine(_) => {
                self.stack.pop();
            }
            Instruction::SubroutineReturn => {
                self.stack.push(prior_state.return_address);
            }
            Instruction::Display(vx, vy, height) => {
                if self.exec_display_instruction(*vx, *vy, *height).is_err() {
                    unreachable!("Cannot undo to a state with an invalid display instruction");
                }
                self.registers[VFLAG] = prior_state.registers[VFLAG];
            }
            _ => (),
        }

        if let Some(payload) = prior_state.payload.as_deref() {
            match payload {
                PartialInterpreterStatePayload::Display(display) => {
                    self.output.display = display.clone();
                }
                PartialInterpreterStatePayload::Rng(rng) => {
                    self.rng = rng.clone();
                }
                PartialInterpreterStatePayload::Flags(flags) => {
                    self.flags = *flags;
                }
            }
        }
    }

    // interpret the next instruction
    pub fn step(&mut self) -> Result<bool, String> {
        // clear output request
        self.output.request = None;

        // fetch + decode
        match self.try_fetch_decode() {
            Ok(inst) => {
                log::trace!("Instruction {:#05X?} {:?} ", self.pc, inst);
                self.pc += 2;

                // execute instruction
                match self.exec(inst) {
                    Ok(should_continue) => {
                        if !should_continue {
                            self.pc -= 2; // revert program counter
                        }

                        Ok(should_continue)
                    }
                    Err(e) => {
                        self.pc -= 2; // revert program counter
                        Err(e)
                    }
                }
            }
            Err(e) => Err(format!("Decode at {:#05X?} failed: {}", self.pc, e)),
        }
    }

    pub fn pick_key<'a, 'b, T: TryInto<Key>>(
        &'a self,
        key_down: &'b Option<T>,
        key_up: &'b Option<T>,
    ) -> &'b Option<T> {
        match self.rom.config.kind {
            RomKind::COSMACVIP => key_up,
            _ => key_down,
        }
    }

    pub fn try_fetch_decode(&self) -> Result<Instruction, String> {
        if (self.pc as usize) < self.memory.len() - 1 {
            InstructionParameters::from([
                self.memory[self.pc as usize],
                self.memory[self.pc as usize + 1],
            ]).try_decode(self.rom.config.kind)
        } else {
            Err(format!(
                "Fetch failed: Program counter address is out of bounds ({:#05X?})",
                self.pc
            ))
        }
    }

    fn exec(&mut self, inst: Instruction) -> Result<bool, String> {
        match inst {
            Instruction::Exit => {
                return Ok(false)
            }
            Instruction::Jump(pc) => self.pc = pc,
            Instruction::JumpWithOffset(address, vx) => {
                let offset = if self.rom.config.kind == RomKind::CHIP48 {
                    self.registers[vx as usize] as u16
                } else {
                    self.registers[0] as u16
                };

                if (offset as usize) < self.memory.len().saturating_sub(address as usize) {
                    self.pc = address + offset;
                } else {
                    return Err(format!(
                        "Jump with offset failed: adresss {:#05X?} with offset {:#04X?} ({}) is out of bounds",
                        address, offset, offset
                    ));
                }
            }
            Instruction::CallSubroutine(pc) => {
                self.stack.push(self.pc);
                self.pc = pc;
            }
            Instruction::SubroutineReturn => {
                self.pc = self
                    .stack
                    .pop()
                    .expect("Could not return from subroutine because stack is empty")
            }
            Instruction::SkipIfEqualsConstant(vx, value) => {
                if self.registers[vx as usize] == value {
                    self.pc += 2
                }
            }
            Instruction::SkipIfNotEqualsConstant(vx, value) => {
                if self.registers[vx as usize] != value {
                    self.pc += 2
                }
            }
            Instruction::SkipIfEquals(vx, vy) => {
                if self.registers[vx as usize] == self.registers[vy as usize] {
                    self.pc += 2
                }
            }
            Instruction::SkipIfNotEquals(vx, vy) => {
                if self.registers[vx as usize] != self.registers[vy as usize] {
                    self.pc += 2
                }
            }
            Instruction::SkipIfKeyDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 1 {
                    self.pc += 2
                }
            }
            Instruction::SkipIfKeyNotDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 0 {
                    self.pc += 2
                }
            }
            Instruction::GetKey(vx) => {
                if let Some(key_code) =
                    self.pick_key(&self.input.just_pressed_key, &self.input.just_released_key)
                {
                    self.registers[vx as usize] = *key_code;
                } else {
                    self.pc -= 2;
                }
            }
            Instruction::SetConstant(vx, value) => self.registers[vx as usize] = value,
            Instruction::AddConstant(vx, change) => {
                self.registers[vx as usize] = self.registers[vx as usize].overflowing_add(change).0
            }
            Instruction::Set(vx, vy) => self.registers[vx as usize] = self.registers[vy as usize],
            Instruction::Or(vx, vy) => self.registers[vx as usize] |= self.registers[vy as usize],
            Instruction::And(vx, vy) => self.registers[vx as usize] &= self.registers[vy as usize],
            Instruction::Xor(vx, vy) => self.registers[vx as usize] ^= self.registers[vy as usize],
            Instruction::Add(vx, vy) => {
                let (value, overflowed) =
                    self.registers[vx as usize].overflowing_add(self.registers[vy as usize]);
                self.registers[vx as usize] = value;
                self.registers[VFLAG] = overflowed as u8;
            }
            Instruction::Sub(vx, vy, vx_minus_vy) => {
                let (value, overflowed) = if vx_minus_vy {
                    self.registers[vx as usize].overflowing_sub(self.registers[vy as usize])
                } else {
                    self.registers[vy as usize].overflowing_sub(self.registers[vx as usize])
                };

                self.registers[vx as usize] = value;
                self.registers[VFLAG] = !overflowed as u8; // vf is 0 on overflow instead of 1 like add
            }
            Instruction::Shift(vx, vy, right) => {
                let bits = match self.rom.config.kind {
                    RomKind::COSMACVIP => self.registers[vy as usize],
                    _ => self.registers[vx as usize],
                };

                if right {
                    self.registers[VFLAG] = bits & 1;
                    self.registers[vx as usize] = bits >> 1;
                } else {
                    self.registers[VFLAG] = bits.reverse_bits() & 1;
                    self.registers[vx as usize] = bits << 1;
                }
            }
            Instruction::GetDelayTimer(vx) => self.registers[vx as usize] = self.input.delay_timer,
            Instruction::SetDelayTimer(vx) => {
                self.output.request = Some(InterpreterRequest::SetDelayTimer(
                    self.registers[vx as usize],
                ))
            }
            Instruction::SetSoundTimer(vx) => {
                self.output.request = Some(InterpreterRequest::SetSoundTimer(
                    self.registers[vx as usize],
                ))
            }
            Instruction::SetIndex(index) => self.index = index,
            Instruction::SetIndexToHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0xF {
                    return Err(format!("Failed to set index: hex char \"{:X}\" does not exist", c));
                }

                self.index = FONT_STARTING_ADDRESS
                    + (FONT_CHAR_DATA_SIZE as u16 * c as u16)
            }
            Instruction::SetIndexToBigHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0x9 {
                    return Err(format!("Failed to set index: big hex char \"{:X}\" does not exist", c));
                }

                self.index = BIG_FONT_STARTING_ADDRESS
                    + (BIG_FONT_CHAR_DATA_SIZE as u16 * c as u16)
            }
            Instruction::AddToIndex(vx) => {
                self.index = self
                    .index
                    .overflowing_add(self.registers[vx as usize] as u16)
                    .0;
            }
            Instruction::Load(vx) => {
                if let Some(addr) = self.checked_addr_add(self.index, vx as u16) {
                    self.registers[..=vx as usize]
                        .copy_from_slice(&self.memory[self.index as usize..=addr as usize]);
                    if self.rom.config.kind == RomKind::COSMACVIP {
                        self.index = addr.overflowing_add(1).0;
                    }
                } else {
                    return Err(format!(
                        "Failed to load bytes from memory: out of bounds read ({} byte{} from i = {:#05X?})", 
                        vx + 1, 
                        if vx > 0 { "s" } else { "" }, 
                        self.index
                    ));
                }
            }
            Instruction::Store(vx) => {
                if let Some(addr) = self.checked_addr_add(self.index, vx as u16) {
                    self.memory[self.index as usize..=addr as usize]
                        .copy_from_slice(&self.registers[..=vx as usize]);
                    if self.rom.config.kind == RomKind::COSMACVIP {
                        self.index = addr.overflowing_add(1).0;
                    }
                } else {
                    return Err(format!(
                        "Failed to write bytes to memory: out of bounds write ({} byte{} from i = {:#05X?})", 
                        vx + 1, 
                        if vx > 0 { "s" } else { "" }, 
                        self.index
                    ));
                }
            }
            Instruction::LoadFlags(vx) => {
                if (vx as usize) < self.flags.len() {
                    self.registers[..=vx as usize].copy_from_slice(&self.flags[..=vx as usize]);
                } else {
                    return Err(format!(
                        "Failed to load bytes from flags: out of bounds read ({} byte{} from flags)", 
                        vx + 1, 
                        if vx > 0 { "s" } else { "" }
                    ));
                }
            }
            Instruction::StoreFlags(vx) => {
                if (vx as usize) < self.flags.len() {
                    self.flags[..=vx as usize].copy_from_slice(&self.registers[..=vx as usize]);
                } else {
                    return Err(format!(
                        "Failed to write bytes to flags: out of bounds write ({} byte{} from flags)", 
                        vx + 1, 
                        if vx > 0 { "s" } else { "" }
                    ));
                }
            }
            Instruction::StoreDecimal(vx) => {
                if let Some(addr) = self.checked_addr_add(self.index, 2) {
                    let number = self.registers[vx as usize];
                    for (i, val) in self.memory[self.index as usize..=addr as usize]
                        .iter_mut()
                        .rev()
                        .enumerate()
                    {
                        *val = number / 10u8.pow(i as u32) % 10;
                    }
                } else {
                    return Err(format!("Failed to write decimal to memory: out of bounds write (3 bytes from i = {:#05X?})", self.index));
                }
            }
            Instruction::GenerateRandom(vx, bound) => {
                self.registers[vx as usize] = (self.rng.next_u32() & bound as u32) as u8;
            }
            Instruction::Display(vx, vy, height) => {
                self.exec_display_instruction(vx, vy, height)?;
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::ScrollDown(n) => {
                self.output.display.scroll_down(n as usize);
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::ScrollLeft => {
                self.output.display.scroll_left();
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::ScrollRight => {
                self.output.display.scroll_right();
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::LowResolution => {
                self.output.display.set_mode(DisplayMode::LowResolution);
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::HighResolution => {
                self.output.display.set_mode(DisplayMode::HighResolution);
                self.output.request = Some(InterpreterRequest::Display);
            }
            Instruction::ClearScreen => {
                self.output.display.clear();
                self.output.request = Some(InterpreterRequest::Display);
            }
        }

        Ok(true)
    }

    fn exec_display_instruction(&mut self, vx: u8, vy: u8, height: u8) -> Result<(), String> {
        let (bytes_per_row, height) = if self.rom.config.kind == RomKind::SCHIP && height == 0 { (2, 16) } else { (1, height) };
        let required_bytes = height * bytes_per_row;

        if self
            .checked_addr_add(self.index, required_bytes.saturating_sub(1) as u16)
            .is_none()
        {
            return Err(format!(
                "Failed to display: sprite out of bounds read ({} byte{} from i = {:#05X?})", 
                required_bytes, 
                if required_bytes == 1 { "" } else { "s" }, 
                self.index
            ));
        }

        self.registers[VFLAG] = self.output.display.draw(
            &self.memory[self.index as usize..],
            self.registers[vx as usize],
            self.registers[vy as usize],
            height,
            bytes_per_row == 2,
        ) as u8;

        Ok(())
    }
}
