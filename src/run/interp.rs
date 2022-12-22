use super::{
    disp::{DisplayMode, Display},
    input::Key,
    rom::{Rom, RomKind}, 
    mem::{MEM_ACCESS_EXEC_FLAG, MEM_ACCESS_READ_FLAG, MEM_ACCESS_WRITE_FLAG, MEM_ACCESS_DRAW_FLAG}
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

pub trait MemoryRef {
    fn add_addresses(&self, lhs: u16, rhs: u16) -> u16;
    fn export(&self, address: u16, dst: &mut [u8]);
}

pub trait MemoryMut {
    fn import(&mut self, src: &[u8], address: u16);
}

impl<T> MemoryRef for T where T: AsRef<[u8]> + ?Sized {
    fn add_addresses(&self, lhs: u16, rhs: u16) -> u16 {
        lhs.overflowing_add(rhs).0 % self.as_ref().len() as u16
    }

    fn export(&self, address: u16, dst: &mut [u8]) {
        let memory = self.as_ref();
        let address = address as usize % memory.len();

        let pivot = dst.len().min(address.abs_diff(memory.len()));
        let (dst0, dst1) = dst.split_at_mut(pivot);

        let src0 = &memory[address..address + dst0.len()];
        let src1 = &memory[..dst1.len()];

        dst0.copy_from_slice(src0);
        dst1.copy_from_slice(src1);
    }
}

impl<T> MemoryMut for T where T: AsMut<[u8]> + ?Sized {
    fn import(&mut self, src: &[u8], address: u16) {
        let memory = self.as_mut();
        let address = address as usize % memory.len();

        let pivot = src.len().min(address.abs_diff(memory.len()));
        let (src0, src1) = src.split_at(pivot);

        memory[address..address + src0.len()].copy_from_slice(src0);
        memory[..src1.len()].copy_from_slice(src1);
    }
}

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

impl InstructionParameters {
    pub fn from_bytes(byte0: u8, byte1: u8) -> Self {
        InstructionParameters::from((byte0 as u16) << 8 | byte1 as u16)
    }
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

    pub vertical_blank: bool,

    pub down_keys: u16,
    pub just_pressed_key: Option<u8>,
    pub just_released_key: Option<u8>,
}

// Response body so IO know how to proceed
pub struct InterpreterOutput {
    pub display: Display,
    pub waiting: bool,

    pub request: Option<InterpreterRequest>,
}

// Interpreter IO Request
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InterpreterRequest {
    Display,
    SetDelayTimer(u8),
    SetSoundTimer(u8),
}

#[derive(Eq, PartialEq, Debug)]
pub enum InterpreterHistoryFragmentExtra {
    WillGenerateRandom {
        rng: Box<StdRng>,
    },
    WillDrawEntireDisplay {
        display: Box<Display>,
    },
    WillLoadFromMemory {
        index_access_flag_slice: [u8; 32]
    },
    WillStoreInMemory {
        index_memory: [u8; 16],
        index_access_flag_slice: [u8; 16]
    },
    WillStoreInFlags {
        flags: [u8; 8],
    },
    WillReturnFromSubroutine {
        return_address: u16,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub struct InterpreterHistoryFragment {
    pub instruction: Option<Instruction>,
    pub pc: u16,
    pub pc_access_flags: u8,
    pub index: u16,
    pub registers: [u8; 16],
    pub extra: Option<Box<InterpreterHistoryFragmentExtra>>,
}

impl InterpreterHistoryFragment {
    // TODO RESTORE MEMORY ACCESS FLAGS
    pub fn log_diff(&self, other: &Self) {
        if self.instruction != other.instruction {
            log::debug!("Instruction difference: {:?} -> {:?}", self.instruction, other.instruction);
        }
        if self.pc != other.pc {
            log::debug!("PC difference: {:?} -> {:?}", self.pc, other.pc);
        }
        if self.pc_access_flags != other.pc_access_flags {
            log::debug!("PC access flags difference: {:?} -> {:?}", self.pc_access_flags, other.pc_access_flags);
        }
        if self.index != other.index {
            log::debug!("Index difference: {:?} -> {:?}", self.index, other.index);
        }
        if self.registers != other.registers {
            log::debug!("Registers difference: {:?} -> {:?}", self.registers, other.registers);
        }
        if self.extra != other.extra {
            log::debug!("Payload difference: {:?} -> {:?}", self.extra, other.extra);
        }
    }
}

pub struct Interpreter {
    pub memory: [u8; PROGRAM_MEMORY_SIZE as usize],
    pub memory_access_flags: Vec<u8>,
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

impl From<Rom> for Interpreter {
    fn from(rom: Rom) -> Self {
        Interpreter {
            memory: Self::alloc(&rom),
            memory_access_flags: if rom.config.debugging {
                vec![0; PROGRAM_MEMORY_SIZE as usize]
            } else {
                vec![]
            },
            
            pc: PROGRAM_STARTING_ADDRESS,
            index: 0,
            stack: Vec::with_capacity(16),
            flags: [0; 8],
            registers: [0; 16],
            rom,
            rng: StdRng::from_entropy(),
            input: Default::default(),
            output: InterpreterOutput {
                display: Default::default(),
                waiting: false,
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
            .map(|slice| InstructionParameters::from_bytes(slice[0], slice[1]))
            .chain(
                std::iter::once(InstructionParameters::from_bytes(
                    *binary.last().expect("Binary shouldn't be empty!"), 
                    *binary.first().expect("Binary shouldn't be empty!")
                ))
            )
    }

    pub fn alloc(rom: &Rom) -> [u8; PROGRAM_MEMORY_SIZE as usize] {
        let mut memory = [0; PROGRAM_MEMORY_SIZE as usize];
        memory.import(&rom.data, PROGRAM_STARTING_ADDRESS);
        memory.import(&FONT, FONT_STARTING_ADDRESS);
        if rom.config.kind == RomKind::SCHIP {
            memory.import(&BIG_FONT, BIG_FONT_STARTING_ADDRESS);
        }
        
        memory
    }

    // TODO: this needs to be removed since all chip8 specifications wait for the key up in the Get Key (FX0A) instruction
    pub fn pick_key<'a, 'b, T: TryInto<Key>>(
        &'a self,
        _: &'b Option<T>,
        key_up: &'b Option<T>,
    ) -> &'b Option<T> {
        key_up
    }

    pub fn undo(&mut self, prior_state: &InterpreterHistoryFragment) {
        self.pc = prior_state.pc;
        self.index = prior_state.index;
        self.registers = prior_state.registers;

        log::debug!("Restoring memory access flags: {:?} -> {:?}", self.memory_access_flags[self.pc as usize], prior_state.pc_access_flags);
        self.memory_access_flags[self.pc as usize] = prior_state.pc_access_flags;

        let Some(instruction) = prior_state.instruction.as_ref() else {
            unreachable!("Cannot undo to a state without an instruction")
        };

        log::trace!("Undoing instruction: {:?}", instruction);

        match instruction {
            Instruction::CallSubroutine(_) => {
                self.stack.pop();
            }
            Instruction::Display(vx, vy, height) => {
                self.exec_display_instruction(*vx, *vy, *height);
                self.registers[VFLAG] = prior_state.registers[VFLAG];
            }
            _ => (),
        }

        let Some(extra) = prior_state.extra.as_deref() else {
            return
        };

        match extra {
            InterpreterHistoryFragmentExtra::WillGenerateRandom { 
                rng 
            } => {
                self.rng = rng.as_ref().clone();
            },
            InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { 
                display 
            } => {
                self.output.display = display.as_ref().clone();
            },
            InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                index_access_flag_slice
            } => {
                self.memory_access_flags.import(index_access_flag_slice, self.index);
            },
            InterpreterHistoryFragmentExtra::WillStoreInMemory {
                index_memory,
                index_access_flag_slice
            } => {
                self.memory.import(index_memory, self.index);
                self.memory_access_flags.import(index_access_flag_slice, self.index);
            },
            InterpreterHistoryFragmentExtra::WillStoreInFlags {
                flags
            } => {
                self.flags = *flags;
            },
            InterpreterHistoryFragmentExtra::WillReturnFromSubroutine {
                return_address
            } => {
                self.stack.push(*return_address);
            },
        }
    }

    pub fn to_history_fragment(&self) -> InterpreterHistoryFragment {
        let mut index_memory = [0; 32];
        self.memory.export(self.index, &mut index_memory);

        let instruction = self.try_fetch_decode().ok();
        let extra = instruction.and_then(|instruction| match instruction {
            Instruction::GenerateRandom(_, _) => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillGenerateRandom {
                    rng: Box::new(self.rng.clone()),
                }
            )),

            Instruction::ClearScreen 
            | Instruction::ScrollDown(_) 
            | Instruction::ScrollLeft 
            | Instruction::ScrollRight 
            | Instruction::LowResolution 
            | Instruction::HighResolution => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { 
                    display: Box::new(self.output.display.clone()) 
                }
            )),

            Instruction::Load(_) | Instruction::Display(_, _, _) => Some(Box::new({
                let mut index_access_flag_slice = [0; 32];
                self.memory_access_flags.export(self.index, &mut index_access_flag_slice);
                InterpreterHistoryFragmentExtra::WillLoadFromMemory { 
                    index_access_flag_slice
                }
            })),

            Instruction::Store(_) | Instruction::StoreDecimal(_) => Some(Box::new({
                let mut index_memory = [0; 16];
                let mut index_access_flag_slice = [0; 16];
                self.memory.export(self.index, &mut index_memory);
                self.memory_access_flags.export(self.index, &mut index_access_flag_slice);
                InterpreterHistoryFragmentExtra::WillStoreInMemory { 
                    index_memory,
                    index_access_flag_slice
                }
            })),

            Instruction::StoreFlags(_) => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillStoreInFlags { 
                    flags: self.flags,
                }
            )),

            Instruction::SubroutineReturn => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillReturnFromSubroutine { 
                    return_address: self.stack.last().cloned().unwrap_or_default(),
                }
            )),
            
            _ => None,
        });

        InterpreterHistoryFragment {
            pc: self.pc,
            pc_access_flags: self.memory_access_flags[self.pc as usize],
            instruction,
            index: self.index,
            registers: self.registers,
            extra,
        }
    }

    pub fn update_memory_access_flags(&mut self, executed_fragment: &InterpreterHistoryFragment) {
        self.memory_access_flags[executed_fragment.pc as usize] |= MEM_ACCESS_EXEC_FLAG;

        let Some(instruction) = executed_fragment.instruction else {
            return
        };

        match instruction {

            Instruction::Load(vx) => {
                let mut workspace = [0; 16];
                let buf = &mut workspace[0..=vx as usize];
                self.memory_access_flags.export(executed_fragment.index, buf);
                buf.iter_mut().for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags.import(buf, executed_fragment.index);
            },

            Instruction::Display(_, _, height) => {
                let sprite_bytes = if self.rom.config.kind == RomKind::SCHIP && height == 0 {
                    32
                } else {
                    height as usize
                };

                let mut workspace = [0; 32];
                let buf = &mut workspace[0..sprite_bytes];
                self.memory_access_flags.export(executed_fragment.index, buf);
                buf.iter_mut().for_each(|flags| *flags |= MEM_ACCESS_DRAW_FLAG);
                self.memory_access_flags.import(buf, executed_fragment.index);
            },

            Instruction::Store(vx) => {
                let mut workspace = [0; 16];
                let buf = &mut workspace[0..=vx as usize];
                self.memory_access_flags.export(executed_fragment.index, buf);
                buf.iter_mut().for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags.import(buf, executed_fragment.index);
            },

            Instruction::StoreDecimal(_) => {
                let mut buf = [0; 3];
                self.memory_access_flags.export(executed_fragment.index, &mut buf);
                buf.iter_mut().for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags.import(&buf, executed_fragment.index);
            }
            
            _ => (),
        }
    }

    // interpret the next instruction
    pub fn step(&mut self) -> Result<bool, String> {
        // clear ephemeral output
        self.output.request = None;
        self.output.waiting = false;

        // fetch + decode
        match self.try_fetch_decode() {
            Ok(instruction) => {
                log::trace!("Instruction {:#05X?} {:?} ", self.pc, instruction);
                let prior_pc = self.pc;

                self.pc = self.memory.add_addresses(self.pc, 2);

                // execute instruction
                let result = self.exec(instruction);

                // revert if execution failed or if execution shouldnt continue or if the interpreter is waiting
                if !result.as_ref().unwrap_or(&false) || self.output.waiting {
                    self.pc = prior_pc;
                }

                result
            }
            Err(e) => Err(format!("Decode at {:#05X?} failed: {}", self.pc, e)),
        }
    }

    pub fn fetch(&self) -> InstructionParameters {
        let mut bytes = [0; 2];
        self.memory.export(self.pc, &mut bytes);
        InstructionParameters::from_bytes(bytes[0], bytes[1])
    }
    
    pub fn try_fetch_decode(&self) -> Result<Instruction, String> {
        self.fetch().try_decode(self.rom.config.kind)
    }

    fn exec(&mut self, inst: Instruction) -> Result<bool, String> {
        match inst {

            Instruction::Exit => {
                return Ok(false)
            }

            Instruction::Jump(address) => self.pc = self.memory.add_addresses(address, 0),

            Instruction::JumpWithOffset(address, vx) => {
                let offset = if self.rom.config.kind == RomKind::SCHIP {
                    self.registers[vx as usize] as u16
                } else {
                    self.registers[0] as u16
                };

                self.pc = self.memory.add_addresses(address, offset);
            }

            Instruction::CallSubroutine(address) => {
                self.stack.push(self.pc);
                self.pc = self.memory.add_addresses(address, 0);
            }

            Instruction::SubroutineReturn => {
                self.pc = self
                    .stack
                    .pop()
                    .expect("Could not return from subroutine because stack is empty")
            }

            Instruction::SkipIfEqualsConstant(vx, value) => {
                if self.registers[vx as usize] == value {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::SkipIfNotEqualsConstant(vx, value) => {
                if self.registers[vx as usize] != value {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::SkipIfEquals(vx, vy) => {
                if self.registers[vx as usize] == self.registers[vy as usize] {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::SkipIfNotEquals(vx, vy) => {
                if self.registers[vx as usize] != self.registers[vy as usize] {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::SkipIfKeyDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 1 {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::SkipIfKeyNotDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 0 {
                    self.pc = self.memory.add_addresses(self.pc, 2);
                }
            }

            Instruction::GetKey(vx) => {
                if let Some(key_code) =
                    self.pick_key(&self.input.just_pressed_key, &self.input.just_released_key)
                {
                    self.registers[vx as usize] = *key_code;
                } else {
                    self.output.waiting = true;
                }
            }

            Instruction::SetConstant(vx, value) => self.registers[vx as usize] = value,

            Instruction::AddConstant(vx, change) => {
                self.registers[vx as usize] = self.registers[vx as usize].overflowing_add(change).0
            }

            Instruction::Set(vx, vy) => self.registers[vx as usize] = self.registers[vy as usize],

            Instruction::Or(vx, vy) => {
                self.registers[vx as usize] |= self.registers[vy as usize];
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.registers[VFLAG] = 0;
                }
            },

            Instruction::And(vx, vy) => {
                self.registers[vx as usize] &= self.registers[vy as usize];
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.registers[VFLAG] = 0;
                }
            },

            Instruction::Xor(vx, vy) => {
                self.registers[vx as usize] ^= self.registers[vy as usize];
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.registers[VFLAG] = 0;
                }
            },

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
                    self.registers[vx as usize] = bits >> 1;
                    self.registers[VFLAG] = bits & 1;
                } else {
                    self.registers[vx as usize] = bits << 1;
                    self.registers[VFLAG] = bits.reverse_bits() & 1;
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

                self.index = self.memory.add_addresses(FONT_STARTING_ADDRESS, FONT_CHAR_DATA_SIZE as u16 * c as u16);
            }

            Instruction::SetIndexToBigHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0x9 {
                    return Err(format!("Failed to set index: big hex char \"{:X}\" does not exist", c));
                }

                self.index = self.memory.add_addresses(BIG_FONT_STARTING_ADDRESS, BIG_FONT_CHAR_DATA_SIZE as u16 * c as u16);
            }

            Instruction::AddToIndex(vx) => {
                self.index = self.memory.add_addresses(self.index, self.registers[vx as usize] as u16);
            }

            Instruction::Load(vx) => {
                self.memory.export(self.index, &mut self.registers[..=vx as usize]);
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.index = self.memory.add_addresses(self.index, vx as u16 + 1);
                }
            }

            Instruction::Store(vx) => {
                self.memory.import(&self.registers[..=vx as usize], self.index);
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.index = self.memory.add_addresses(self.index, vx as u16 + 1);
                }
            }

            Instruction::LoadFlags(vx) => {
                if (vx as usize) >= self.flags.len() {
                    return Err(format!("Failed to read from flags: flags {}..={} does not exist", self.flags.len(), vx));
                }
                self.flags.export(0, &mut self.registers[..=vx as usize]);
            }

            Instruction::StoreFlags(vx) => {
                if (vx as usize) >= self.flags.len() {
                    return Err(format!("Failed to write to flags: flags {}..={} does not exist", self.flags.len(), vx));
                }
                self.flags.import(&self.registers[..=vx as usize], 0);
            }

            Instruction::StoreDecimal(vx) => {
                let mut bcd = [0, 0, 0];
                let decimal = self.registers[vx as usize];
                bcd
                    .iter_mut()
                    .rev()
                    .enumerate()
                    .for_each(|(i, val)| *val = decimal / 10u8.pow(i as u32) % 10);
                self.memory.import(&bcd, self.index);
            }

            Instruction::GenerateRandom(vx, bound) => {
                self.registers[vx as usize] = (self.rng.next_u32() & bound as u32) as u8;
            }

            Instruction::Display(vx, vy, height) => {
                if self.rom.config.kind == RomKind::COSMACVIP && !self.input.vertical_blank {
                    self.output.waiting = true;
                } else {
                    self.exec_display_instruction(vx, vy, height);
                    self.output.request = Some(InterpreterRequest::Display);
                }
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

    fn exec_display_instruction(&mut self, vx: u8, vy: u8, height: u8) {
        let (bytes_per_row, height) = if self.rom.config.kind == RomKind::SCHIP && height == 0 { 
            (2, 16) 
        } else {
            (1, height)
        };

        let mut sprite = [0; 32];
        self.memory.export(self.index, &mut sprite[..(height * bytes_per_row) as usize]);

        self.registers[VFLAG] = self.output.display.draw(
            &sprite,
            self.registers[vx as usize] as u16,
            self.registers[vy as usize] as u16,
            height as u16,
            bytes_per_row == 2,
        ) as u8;
    }
}
