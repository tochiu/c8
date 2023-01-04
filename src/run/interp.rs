use crate::asm::write_inst_asm;

use super::{
    audio::{Audio, AUDIO_BUFFER_SIZE_BYTES},
    disp::{Display, DisplayMode},
    input::Key,
    mem::*,
    rom::{Rom, RomKind},
};

use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

pub const VFLAG: usize = 15;

pub const PROGRAM_STARTING_ADDRESS: u16 = 0x200;

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
            op  : ((bits & 0xF0000000) >> 4 * 7) as u8,
            x   : ((bits & 0x0F000000) >> 4 * 6) as u8,
            y   : ((bits & 0x00F00000) >> 4 * 5) as u8,
            n   : ((bits & 0x000F0000) >> 4 * 4) as u8,
            nn  : ((bits & 0x00FF0000) >> 4 * 4) as u8,
            nnn : ((bits & 0x0FFF0000) >> 4 * 4) as u16,
            nnnn: ((bits & 0x0000FFFF) >> 4 * 0) as u16,
        }
    }

    pub fn significant_bytes(&self, bytes: u16) -> u32 {
        self.bits >> (32 - 8*bytes)
    }

    pub fn default_significant_bytes(&self) -> u32 {
        self.significant_bytes(2)
    }

    pub fn try_decode(&self, kind: RomKind) -> Result<Instruction, String> {
        let InstructionParameters {
            op,
            x,
            y,
            n,
            nn,
            nnn,
            nnnn,
            ..
        } = *self;

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
            (0x1, __x, __y, __n) => Instruction::Jump(nnn),
            (0x2, __x, __y, __n) => Instruction::CallSubroutine(nnn),
            (0x3, __x, __y, __n) => Instruction::SkipIfEqualsConstant(x, nn),
            (0x4, __x, __y, __n) => Instruction::SkipIfNotEqualsConstant(x, nn),
            (0x5, __x, __y, 0x0) => Instruction::SkipIfEquals(x, y),
            (0x5, __x, __y, 0x2) => Instruction::StoreRange(x, y),
            (0x5, __x, __y, 0x3) => Instruction::LoadRange(x, y),
            (0x6, __x, __y, __n) => Instruction::SetConstant(x, nn),
            (0x7, __x, __y, __n) => Instruction::AddConstant(x, nn),
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
            (0xA, __x, __y, __n) => Instruction::SetIndex(nnn),
            (0xB, __x, __y, __n) => Instruction::JumpWithOffset(nnn, x),
            (0xC, __x, __y, __n) => Instruction::GenerateRandom(x, nn),
            (0xD, __x, __y, __n) => Instruction::Draw(x, y, n),
            (0xF, 0x0, 0x0, 0x0) => Instruction::SetIndexToLong(nnnn),
            (0xE, __x, 0x9, 0xE) => Instruction::SkipIfKeyDown(x),
            (0xE, __x, 0xA, 0x1) => Instruction::SkipIfKeyNotDown(x),
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
            _ => return Err(format!("Unable to decode instruction {}", self)),
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
                    return Err(self.compatibility_issue_msg(instruction, RomKind::SCHIP, kind));
                }
            }
            Instruction::ScrollUp(_) 
            | Instruction::LoadAudio
            | Instruction::SetPitch(_)
            | Instruction::LoadRange(_, _)
            | Instruction::StoreRange(_, _) 
            | Instruction::SetIndexToLong(_) => {
                if kind < RomKind::XOCHIP {
                    return Err(self.compatibility_issue_msg(instruction, RomKind::XOCHIP, kind));
                }
            }
            Instruction::LoadFlags(vx) | Instruction::StoreFlags(vx) => {
                if kind < RomKind::SCHIP {
                    return Err(self.compatibility_issue_msg(instruction, RomKind::SCHIP, kind));
                } else if vx > 0x7 && kind < RomKind::XOCHIP {
                    return Err(self.compatibility_issue_msg(instruction, RomKind::XOCHIP, kind));
                }
            }
            _ => (),
        };

        Ok(instruction)
    }

    fn compatibility_issue_msg(
        &self,
        instruction: Instruction,
        expected_kind: RomKind,
        actual_kind: RomKind,
    ) -> String {
        let mut message = String::new();
        let mut comment = String::new();
        write_inst_asm(&instruction, expected_kind, &mut message, &mut comment).ok();
        format!(
            "{:04X} a.k.a. \"{}\" ({}) is at least a {} instruction but ROM is {}",
            self.significant_bytes(instruction.size()), message, comment, expected_kind, actual_kind
        )
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

// Interpreter IO Request
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum InterpreterOutput {
    Display,
    SetDelayTimer(u8),
    SetSoundTimer(u8),
    UpdateAudioPitch,
    UpdateAudioBuffer,
}

#[derive(Eq, PartialEq, Debug)]
pub enum InterpreterHistoryFragmentExtra {
    WillGenerateRandom {
        prior_rng: Box<StdRng>,
    },
    WillDrawEntireDisplay {
        prior_display: Box<Display>,
    },
    WillLoadFromMemory {
        prior_index_access_flag_slice: [u8; 32],
    },
    WillStoreInMemory {
        prior_index_memory: [u8; 16],
        prior_index_access_flag_slice: [u8; 16],
    },
    WillStoreInFlags {
        prior_flags: [u8; 16],
    },
    WillReturnFromSubroutine {
        prior_return_address: u16,
    },
    WillSetPitch {
        prior_pitch: u8,
    },
    WillLoadAudio {
        prior_buffer: [u8; AUDIO_BUFFER_SIZE_BYTES],
        prior_index_access_flag_slice: [u8; 16],
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
    pub fn log_diff(&self, other: &Self) {
        if self.instruction != other.instruction {
            log::debug!(
                "Instruction difference: {:?} -> {:?}",
                self.instruction,
                other.instruction
            );
        }
        if self.pc != other.pc {
            log::debug!("PC difference: {:?} -> {:?}", self.pc, other.pc);
        }
        if self.pc_access_flags != other.pc_access_flags {
            log::debug!(
                "PC access flags difference: {:?} -> {:?}",
                self.pc_access_flags,
                other.pc_access_flags
            );
        }
        if self.index != other.index {
            log::debug!("Index difference: {:?} -> {:?}", self.index, other.index);
        }
        if self.registers != other.registers {
            log::debug!(
                "Registers difference: {:?} -> {:?}",
                self.registers,
                other.registers
            );
        }
        if self.extra != other.extra {
            log::debug!("Payload difference: {:?} -> {:?}", self.extra, other.extra);
        }
    }
}

pub struct Interpreter {
    pub memory: Vec<u8>,
    pub memory_access_flags: Vec<u8>,
    pub pc: u16,
    pub index: u16,
    pub stack: Vec<u16>,
    pub flags: [u8; 16],
    pub registers: [u8; 16],
    pub rom: Rom,
    pub rng: StdRng,
    pub display: Display,
    pub waiting: bool,
    pub audio: Audio,
    pub input: InterpreterInput,
    pub output: Option<InterpreterOutput>,
}

impl From<Rom> for Interpreter {
    fn from(rom: Rom) -> Self {
        let memory = allocate_memory(&rom);
        Interpreter {
            memory_access_flags: if rom.config.debugging {
                vec![0; memory.len()]
            } else {
                vec![]
            },
            memory,
            pc: PROGRAM_STARTING_ADDRESS,
            index: 0,
            stack: Vec::with_capacity(16),
            flags: [0; 16],
            registers: [0; 16],
            rng: StdRng::from_entropy(),
            display: Default::default(),
            waiting: false,
            audio: Audio::from(rom.config.kind),
            input: Default::default(),
            output: None,
            rom,
        }
    }
}

impl Interpreter {
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

        log::debug!(
            "Restoring memory access flags: {:?} -> {:?}",
            self.memory_access_flags[self.pc as usize],
            prior_state.pc_access_flags
        );
        self.memory_access_flags[self.pc as usize] = prior_state.pc_access_flags;

        let Some(instruction) = prior_state.instruction.as_ref() else {
            unreachable!("Cannot undo to a state without an instruction")
        };

        log::trace!("Undoing instruction: {:?}", instruction);

        match instruction {
            Instruction::CallSubroutine(_) => {
                self.stack.pop();
            }
            Instruction::Draw(vx, vy, height) => {
                self.exec_display_instruction(*vx, *vy, *height);
                self.registers[VFLAG] = prior_state.registers[VFLAG];
            }
            _ => (),
        }

        let Some(extra) = prior_state.extra.as_deref() else {
            return
        };

        match extra {
            InterpreterHistoryFragmentExtra::WillGenerateRandom { prior_rng } => {
                self.rng = prior_rng.as_ref().clone();
            }

            InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { prior_display } => {
                self.display = prior_display.as_ref().clone();
            }

            InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                prior_index_access_flag_slice,
            } => {
                self.memory_access_flags
                    .import(prior_index_access_flag_slice, self.index);
            }

            InterpreterHistoryFragmentExtra::WillStoreInMemory {
                prior_index_memory,
                prior_index_access_flag_slice: index_access_flag_slice,
            } => {
                self.memory.import(prior_index_memory, self.index);
                self.memory_access_flags
                    .import(index_access_flag_slice, self.index);
            }

            InterpreterHistoryFragmentExtra::WillStoreInFlags { prior_flags } => {
                self.flags = *prior_flags;
            }

            InterpreterHistoryFragmentExtra::WillReturnFromSubroutine {
                prior_return_address,
            } => {
                self.stack.push(*prior_return_address);
            }

            InterpreterHistoryFragmentExtra::WillLoadAudio {
                prior_buffer,
                prior_index_access_flag_slice,
            } => {
                self.audio.buffer = *prior_buffer;
                self.memory_access_flags
                    .import(prior_index_access_flag_slice, self.index);
            }

            InterpreterHistoryFragmentExtra::WillSetPitch { prior_pitch } => {
                self.audio.pitch = *prior_pitch;
            }
        }
    }

    pub fn to_history_fragment(&self) -> InterpreterHistoryFragment {
        let mut index_memory = [0; 32];
        self.memory.export(self.index, &mut index_memory);

        let instruction = self.try_fetch_decode().ok();
        let extra = instruction.and_then(|instruction| match instruction {
            Instruction::GenerateRandom(_, _) => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillGenerateRandom {
                    prior_rng: Box::new(self.rng.clone()),
                },
            )),

            Instruction::ClearScreen
            | Instruction::ScrollUp(_)
            | Instruction::ScrollDown(_)
            | Instruction::ScrollLeft
            | Instruction::ScrollRight
            | Instruction::LowResolution
            | Instruction::HighResolution => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillDrawEntireDisplay {
                    prior_display: Box::new(self.display.clone()),
                },
            )),

            Instruction::Load(_) | Instruction::LoadRange(_, _) | Instruction::Draw(_, _, _) => {
                Some(Box::new({
                    let mut index_access_flag_slice = [0; 32];
                    self.memory_access_flags
                        .export(self.index, &mut index_access_flag_slice);
                    InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                        prior_index_access_flag_slice: index_access_flag_slice,
                    }
                }))
            }

            Instruction::Store(_)
            | Instruction::StoreRange(_, _)
            | Instruction::StoreBinaryCodedDecimal(_) => Some(Box::new({
                let mut index_memory = [0; 16];
                let mut index_access_flag_slice = [0; 16];
                self.memory.export(self.index, &mut index_memory);
                self.memory_access_flags
                    .export(self.index, &mut index_access_flag_slice);
                InterpreterHistoryFragmentExtra::WillStoreInMemory {
                    prior_index_memory: index_memory,
                    prior_index_access_flag_slice: index_access_flag_slice,
                }
            })),

            Instruction::StoreFlags(_) => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillStoreInFlags {
                    prior_flags: self.flags,
                },
            )),

            Instruction::SubroutineReturn => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillReturnFromSubroutine {
                    prior_return_address: self.stack.last().cloned().unwrap_or_default(),
                },
            )),

            Instruction::LoadAudio => Some(Box::new({
                let mut index_access_flag_slice = [0; 16];
                self.memory_access_flags
                    .export(self.index, &mut index_access_flag_slice);
                InterpreterHistoryFragmentExtra::WillLoadAudio {
                    prior_buffer: self.audio.buffer,
                    prior_index_access_flag_slice: index_access_flag_slice,
                }
            })),

            Instruction::SetPitch(_) => {
                Some(Box::new(InterpreterHistoryFragmentExtra::WillSetPitch {
                    prior_pitch: self.audio.pitch,
                }))
            }

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
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::LoadRange(mut vstart, mut vend) => {
                if vstart > vend {
                    std::mem::swap(&mut vstart, &mut vend);
                }
                let mut workspace = [0; 16];
                let buf = &mut workspace[vstart as usize..=vend as usize];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::Draw(_, _, height) => {
                let sprite_bytes = if self.rom.config.kind == RomKind::SCHIP && height == 0 {
                    32
                } else {
                    height as usize
                };

                let mut workspace = [0; 32];
                let buf = &mut workspace[0..sprite_bytes];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_DRAW_FLAG | MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::Store(vx) => {
                let mut workspace = [0; 16];
                let buf = &mut workspace[0..=vx as usize];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::StoreRange(mut vstart, mut vend) => {
                if vstart > vend {
                    std::mem::swap(&mut vstart, &mut vend);
                }
                let mut workspace = [0; 16];
                let buf = &mut workspace[vstart as usize..=vend as usize];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::LoadAudio => {
                let mut buf = [0; 16];
                self.memory_access_flags
                    .export(executed_fragment.index, &mut buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(&buf, executed_fragment.index);
            }

            Instruction::StoreBinaryCodedDecimal(_) => {
                let mut buf = [0; 3];
                self.memory_access_flags
                    .export(executed_fragment.index, &mut buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags
                    .import(&buf, executed_fragment.index);
            }

            _ => (),
        }
    }

    // interpret the next instruction
    pub fn step(&mut self) -> Result<bool, String> {
        // clear ephemeral output
        self.output = None;
        self.waiting = false;

        // fetch + decode
        match self.try_fetch_decode() {
            Ok(instruction) => {
                log::trace!("Instruction {:#05X?} {:?} ", self.pc, instruction);
                let prior_pc = self.pc;

                self.pc = self
                    .memory
                    .address_add(self.pc, instruction.size());

                // execute instruction
                let result = self.exec(instruction);

                // revert if execution failed or if execution shouldnt continue or if the interpreter is waiting
                if !result.as_ref().unwrap_or(&false) || self.waiting {
                    self.pc = prior_pc;
                }

                result
            }
            Err(e) => Err(format!("Decode at {:#05X?} failed: {}", self.pc, e)),
        }
    }

    pub fn fetch(&self) -> InstructionParameters {
        let mut bytes = [0; 4];
        self.memory.export(self.pc, &mut bytes);
        InstructionParameters::from(bytes)
    }

    pub fn try_fetch_decode(&self) -> Result<Instruction, String> {
        self.fetch().try_decode(self.rom.config.kind)
    }

    fn exec(&mut self, inst: Instruction) -> Result<bool, String> {
        let mut skip_next_instruction = false;

        match inst {
            Instruction::Exit => return Ok(false),

            Instruction::Jump(address) => self.pc = self.memory.address_add(address, 0),

            Instruction::JumpWithOffset(address, vx) => {
                let offset = if self.rom.config.kind == RomKind::SCHIP {
                    self.registers[vx as usize] as u16
                } else {
                    self.registers[0] as u16
                };

                self.pc = self.memory.address_add(address, offset);
            }

            Instruction::CallSubroutine(address) => {
                self.stack.push(self.pc);
                self.pc = self.memory.address_add(address, 0);
            }

            Instruction::SubroutineReturn => {
                self.pc = self
                    .stack
                    .pop()
                    .expect("Could not return from subroutine because stack is empty")
            }

            Instruction::SkipIfEqualsConstant(vx, value) => {
                if self.registers[vx as usize] == value {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfNotEqualsConstant(vx, value) => {
                if self.registers[vx as usize] != value {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfEquals(vx, vy) => {
                if self.registers[vx as usize] == self.registers[vy as usize] {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfNotEquals(vx, vy) => {
                if self.registers[vx as usize] != self.registers[vy as usize] {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfKeyDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 1 {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfKeyNotDown(vx) => {
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 0 {
                    skip_next_instruction = true
                }
            }

            Instruction::WaitForKey(vx) => {
                if let Some(key_code) =
                    self.pick_key(&self.input.just_pressed_key, &self.input.just_released_key)
                {
                    self.registers[vx as usize] = *key_code;
                } else {
                    self.waiting = true;
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
            }

            Instruction::And(vx, vy) => {
                self.registers[vx as usize] &= self.registers[vy as usize];
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.registers[VFLAG] = 0;
                }
            }

            Instruction::Xor(vx, vy) => {
                self.registers[vx as usize] ^= self.registers[vy as usize];
                if self.rom.config.kind == RomKind::COSMACVIP {
                    self.registers[VFLAG] = 0;
                }
            }

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
                    RomKind::COSMACVIP | RomKind::XOCHIP => self.registers[vy as usize],
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
                self.output = Some(InterpreterOutput::SetDelayTimer(
                    self.registers[vx as usize],
                ))
            }

            Instruction::SetSoundTimer(vx) => {
                self.output = Some(InterpreterOutput::SetSoundTimer(
                    self.registers[vx as usize],
                ))
            }

            Instruction::SetIndex(address) => self.index = address,

            Instruction::SetIndexToLong(address) => self.index = address,

            Instruction::SetIndexToHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0xF {
                    return Err(format!(
                        "Failed to set index: hex char \"{:X}\" does not exist",
                        c
                    ));
                }

                self.index = self
                    .memory
                    .address_add(FONT_STARTING_ADDRESS, FONT_CHAR_DATA_SIZE as u16 * c as u16);
            }

            Instruction::SetIndexToBigHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0x9 {
                    return Err(format!(
                        "Failed to set index: big hex char \"{:X}\" does not exist",
                        c
                    ));
                }

                self.index = self.memory.address_add(
                    BIG_FONT_STARTING_ADDRESS,
                    BIG_FONT_CHAR_DATA_SIZE as u16 * c as u16,
                );
            }

            Instruction::AddToIndex(vx) => {
                self.index = self
                    .memory
                    .address_add(self.index, self.registers[vx as usize] as u16);
            }

            Instruction::Load(vx) => {
                self.memory
                    .export(self.index, &mut self.registers[..=vx as usize]);
                if let RomKind::COSMACVIP | RomKind::XOCHIP = self.rom.config.kind {
                    self.index = self.memory.address_add(self.index, vx as u16 + 1);
                }
            }

            Instruction::Store(vx) => {
                self.memory
                    .import(&self.registers[..=vx as usize], self.index);
                if let RomKind::COSMACVIP | RomKind::XOCHIP = self.rom.config.kind {
                    self.index = self.memory.address_add(self.index, vx as u16 + 1);
                }
            }

            Instruction::LoadRange(mut vstart, mut vend) => {
                let reverse = vstart > vend;
                if reverse {
                    std::mem::swap(&mut vstart, &mut vend);
                }
                let buf = &mut self.registers[vstart as usize..=vend as usize];
                self.memory.export(self.index, buf);
                if reverse {
                    buf.reverse();
                }
            }

            Instruction::StoreRange(mut vstart, mut vend) => {
                let reverse = vstart > vend;
                if reverse {
                    std::mem::swap(&mut vstart, &mut vend);
                }
                let buf = &mut self.registers[vstart as usize..=vend as usize];
                if reverse {
                    buf.reverse();
                }
                self.memory.import(buf, self.index);
                if reverse {
                    buf.reverse();
                }
            }

            Instruction::LoadFlags(vx) => {
                self.flags.export(0, &mut self.registers[..=vx as usize]);
            }

            Instruction::StoreFlags(vx) => {
                self.flags.import(&self.registers[..=vx as usize], 0);
            }

            Instruction::StoreBinaryCodedDecimal(vx) => {
                let mut bcd = [0, 0, 0];
                let decimal = self.registers[vx as usize];
                bcd.iter_mut()
                    .rev()
                    .enumerate()
                    .for_each(|(i, val)| *val = decimal / 10u8.pow(i as u32) % 10);
                self.memory.import(&bcd, self.index);
            }

            Instruction::GenerateRandom(vx, bound) => {
                self.registers[vx as usize] = (self.rng.next_u32() & bound as u32) as u8;
            }

            Instruction::Draw(vx, vy, height) => {
                if self.rom.config.kind == RomKind::COSMACVIP && !self.input.vertical_blank {
                    self.waiting = true;
                } else {
                    self.exec_display_instruction(vx, vy, height);
                    self.output = Some(InterpreterOutput::Display);
                }
            }

            Instruction::ScrollUp(n) => {
                self.display.scroll_up(n as usize);
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::ScrollDown(n) => {
                self.display.scroll_down(n as usize);
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::ScrollLeft => {
                self.display.scroll_left();
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::ScrollRight => {
                self.display.scroll_right();
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::LowResolution => {
                self.display.set_mode(DisplayMode::LowResolution);
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::HighResolution => {
                self.display.set_mode(DisplayMode::HighResolution);
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::ClearScreen => {
                self.display.clear();
                self.output = Some(InterpreterOutput::Display);
            }

            Instruction::LoadAudio => {
                self.memory.export(self.index, &mut self.audio.buffer);
                self.output = Some(InterpreterOutput::UpdateAudioBuffer);
            }

            Instruction::SetPitch(vx) => {
                self.audio.pitch = self.registers[vx as usize];
                self.output = Some(InterpreterOutput::UpdateAudioPitch);
            }
        }

        if skip_next_instruction {
            self.pc = self.memory.address_add(
                self.pc,
                Instruction::size_or_default(&self.try_fetch_decode().ok()),
            );
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
        self.memory
            .export(self.index, &mut sprite[..(height * bytes_per_row) as usize]);

        self.registers[VFLAG] = self.display.draw(
            &sprite,
            self.registers[vx as usize] as u16,
            self.registers[vy as usize] as u16,
            height as u16,
            bytes_per_row == 2,
        ) as u8;
    }
}
