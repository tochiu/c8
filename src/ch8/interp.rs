use super::{
    audio::{Audio, AUDIO_BUFFER_SIZE_BYTES},
    disp::{Display, DisplayBuffer, DisplayMode},
    input::Key,
    instruct::{Instruction, InstructionParameters},
    mem::*,
    rom::{Rom, RomKind},
};

use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

pub const VFLAG: usize = 15;

pub const PROGRAM_STARTING_ADDRESS: u16 = 0x200;
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
    WillSetPlane {
        prior_selected_plane_bitflags: u8,
    },
    WillDrawEntireDisplay {
        prior_display_buffers: [Option<Box<DisplayBuffer>>; 4],
    },
    WillLoadFromMemory {
        prior_index_access_flags: Vec<u8>,
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
    pub memory_last_address: u16,
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
    instruction: Option<(Instruction, u16)>,
    workspace: [u8; 128],
    prefetch: Vec<Option<(Instruction, u16)>>,
    error: String,
    valid: bool,
}

impl From<Rom> for Interpreter {
    fn from(rom: Rom) -> Self {
        let memory = allocate_memory(&rom);
        let memory_last_address = (memory.len() - 1) as u16;
        let prefetch = memory
            .instruction_parameters()
            .map(|params| {
                params
                    .try_decode(rom.config.kind)
                    .ok()
                    .map(|inst| (inst, inst.size()))
            })
            .collect();
        let mut interp = Interpreter {
            memory_access_flags: if rom.config.debugging {
                vec![0; memory.len()]
            } else {
                vec![]
            },
            memory_last_address,
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
            instruction: None,
            workspace: [0; 128],
            error: String::new(),
            valid: true,
            prefetch,
            rom,
        };

        interp.fetch_decode();
        interp
    }
}

impl Interpreter {
    pub fn instruction(&self) -> Option<Instruction> {
        self.instruction.map(|(inst, _)| inst)
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
        let Some(instruction) = prior_state.instruction.as_ref() else {
            unreachable!("Cannot undo to a state without an instruction")
        };

        self.pc = prior_state.pc;
        self.index = prior_state.index;
        self.registers = prior_state.registers;
        self.instruction = Some((*instruction, instruction.size()));

        log::debug!(
            "Restoring memory access flags: {:?} -> {:?}",
            self.memory_access_flags[self.pc as usize],
            prior_state.pc_access_flags
        );
        self.memory_access_flags[self.pc as usize] = prior_state.pc_access_flags;

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

            InterpreterHistoryFragmentExtra::WillDrawEntireDisplay {
                prior_display_buffers,
            } => {
                for (plane, maybe_prior_plane) in self
                    .display
                    .planes
                    .iter_mut()
                    .zip(prior_display_buffers.into_iter())
                {
                    let Some(prior_plane) = maybe_prior_plane.as_deref() else {
                        continue
                    };

                    *plane = *prior_plane;
                }
            }

            InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                prior_index_access_flags,
            } => {
                self.memory_access_flags
                    .import(&prior_index_access_flags, self.index);
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

            InterpreterHistoryFragmentExtra::WillSetPlane {
                prior_selected_plane_bitflags,
            } => {
                self.display.selected_plane_bitflags = *prior_selected_plane_bitflags;
            }
        }
    }

    pub fn to_history_fragment(&self) -> InterpreterHistoryFragment {
        let instruction = self.instruction();
        let extra = instruction.and_then(|instruction| match instruction {
            Instruction::GenerateRandom(_, _) => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillGenerateRandom {
                    prior_rng: Box::new(self.rng.clone()),
                },
            )),

            Instruction::SetPlane(_) => {
                Some(Box::new(InterpreterHistoryFragmentExtra::WillSetPlane {
                    prior_selected_plane_bitflags: self.display.selected_plane_bitflags,
                }))
            }

            Instruction::ClearScreen
            | Instruction::ScrollUp(_)
            | Instruction::ScrollDown(_)
            | Instruction::ScrollLeft
            | Instruction::ScrollRight
            | Instruction::LowResolution
            | Instruction::HighResolution => Some(Box::new(
                InterpreterHistoryFragmentExtra::WillDrawEntireDisplay {
                    prior_display_buffers: [0, 1, 2, 3].map(|i| {
                        if self.display.selected_plane_bitflags >> i == 1 {
                            Some(Box::new(self.display.planes[i]))
                        } else {
                            None
                        }
                    }),
                },
            )),

            Instruction::Load(vx) => Some(Box::new({
                let mut prior_index_access_flags = vec![0; vx as usize + 1];
                self.memory_access_flags
                    .export(self.index, &mut prior_index_access_flags);
                InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                    prior_index_access_flags,
                }
            })),

            Instruction::LoadRange(vstart, vend) => Some(Box::new({
                let mut prior_index_access_flags = vec![0; vstart.abs_diff(vend) as usize + 1];
                self.memory_access_flags
                    .export(self.index, &mut prior_index_access_flags);
                InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                    prior_index_access_flags,
                }
            })),

            Instruction::Draw(_, _, n) => Some(Box::new({
                let mut prior_index_access_flags = vec![0; self.get_sprite_draw_info(n).2];
                self.memory_access_flags
                    .export(self.index, &mut prior_index_access_flags);
                InterpreterHistoryFragmentExtra::WillLoadFromMemory {
                    prior_index_access_flags,
                }
            })),

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
                let buf = &mut self.workspace[0..=vx as usize];
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
                let buf = &mut self.workspace[vstart as usize..=vend as usize];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::Draw(_, _, n) => {
                let total_bytes = self.get_sprite_draw_info(n).2;
                let buf = &mut self.workspace[..total_bytes];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_DRAW_FLAG | MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::Store(vx) => {
                let buf = &mut self.workspace[0..=vx as usize];
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

                let buf = &mut self.workspace[vstart as usize..=vend as usize];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags
                    .import(buf, executed_fragment.index);
            }

            Instruction::StoreBinaryCodedDecimal(_) => {
                let buf = &mut self.workspace[..3];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_WRITE_FLAG);
                self.memory_access_flags
                    .import(&buf, executed_fragment.index);
            }

            Instruction::LoadAudio => {
                let buf = &mut self.workspace[..16];
                self.memory_access_flags
                    .export(executed_fragment.index, buf);
                buf.iter_mut()
                    .for_each(|flags| *flags |= MEM_ACCESS_READ_FLAG);
                self.memory_access_flags
                    .import(&buf, executed_fragment.index);
            }

            _ => (),
        }
    }

    // interpret the current instruction
    #[inline(always)]
    pub fn step(&mut self) -> bool {
        let Some((instruction, instruction_size)) = self.instruction else {
            self.valid = false;
            self.error = format!("Decode at {:#05X?} failed: {}", self.pc, self.error);
            return false;
        };

        let prior_pc = self.pc;

        // advance pc
        self.pc = self.pc.overflowing_add(instruction_size).0 & self.memory_last_address;

        // execute instruction

        // revert if execution failed or if execution shouldnt continue or if the interpreter is waiting
        if !self.exec(instruction) || self.waiting {
            self.pc = prior_pc;
            self.instruction = Some((instruction, instruction_size));
            false
        } else {
            self.fetch_decode();
            true
        }
    }

    pub fn stop_result(&self) -> Result<bool, String> {
        if self.valid {
            Ok(false)
        } else {
            Err(self.error.clone())
        }
    }

    fn fetch_decode(&mut self) {
        self.instruction = self.prefetch[self.pc as usize];
        if self.instruction.is_some() {
            return;
        }

        match InstructionParameters::try_decode_from_u32(
            u32::from_be_bytes([
                self.memory[(self.pc as usize + 0) % self.memory.len()],
                self.memory[(self.pc as usize + 1) % self.memory.len()],
                self.memory[(self.pc as usize + 2) % self.memory.len()],
                self.memory[(self.pc as usize + 3) % self.memory.len()],
            ]),
            self.rom.config.kind,
        ) {
            Ok(instruction) => {
                self.instruction = Some((instruction, instruction.size()));
                self.prefetch[self.pc as usize] = self.instruction;
            }
            Err(e) => {
                self.instruction = None;
                self.error = e;
            }
        }
    }

    #[inline(always)]
    fn exec(&mut self, inst: Instruction) -> bool {
        let mut skip_next_instruction = false;

        match inst {
            Instruction::Exit => {
                self.valid = true;
                return false;
            }

            Instruction::Jump(address) => self.pc = address & self.memory_last_address,

            Instruction::JumpWithOffset(address, vx) => {
                let offset = if self.rom.config.kind == RomKind::SCHIP {
                    self.registers[vx as usize] as u16
                } else {
                    self.registers[0] as u16
                };

                self.pc = address.overflowing_add(offset).0 & self.memory_last_address;
                //self.memory.address_add(address, offset);
            }

            Instruction::CallSubroutine(address) => {
                self.stack.push(self.pc);
                self.pc = address & self.memory_last_address;
            }

            Instruction::SubroutineReturn => {
                let Some(pc) = self.stack.pop() else {
                    self.valid = false;
                    self.error = "Could not return from subroutine because stack is empty".to_string();
                    return false
                };

                self.pc = pc;
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
                let key = self.registers[vx as usize];
                if key <= 0xF && self.input.down_keys >> key & 1 == 1 {
                    skip_next_instruction = true
                }
            }

            Instruction::SkipIfKeyNotDown(vx) => {
                let key = self.registers[vx as usize];
                if key > 0xF || self.input.down_keys >> key & 1 == 0 {
                    skip_next_instruction = true
                }
            }

            Instruction::WaitForKey(vx) => {
                if let Some(key_code) = self.input.just_released_key {
                    self.registers[vx as usize] = key_code;
                    self.waiting = false;
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

            Instruction::SetIndex(address) => self.index = address & self.memory_last_address,

            Instruction::SetIndexToLong(address) => self.index = address & self.memory_last_address,

            Instruction::SetIndexToHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0xF {
                    self.valid = false;
                    self.error =
                        format!("Failed to set index: hex char \"{:X}\" does not exist", c);
                    return false;
                }

                self.index = FONT_STARTING_ADDRESS + FONT_CHAR_DATA_SIZE as u16 * c as u16;
            }

            Instruction::SetIndexToBigHexChar(vx) => {
                let c = self.registers[vx as usize];
                if c > 0x9 {
                    self.valid = false;
                    self.error = format!(
                        "Failed to set index: big hex char \"{:X}\" does not exist",
                        c
                    );
                    return false;
                }

                self.index = BIG_FONT_STARTING_ADDRESS + BIG_FONT_CHAR_DATA_SIZE as u16 * c as u16;
            }

            Instruction::AddToIndex(vx) => {
                self.index = self
                    .index
                    .overflowing_add(self.registers[vx as usize] as u16)
                    .0
                    & self.memory_last_address;
            }

            Instruction::Load(vx) => {
                self.memory
                    .export(self.index, &mut self.registers[..=vx as usize]);
                if let RomKind::COSMACVIP | RomKind::XOCHIP = self.rom.config.kind {
                    self.index =
                        self.index.overflowing_add(vx as u16 + 1).0 & self.memory_last_address;
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

            Instruction::Store(vx) => {
                self.memory
                    .import(&self.registers[..=vx as usize], self.index);

                let (prefetch_range0, prefetch_range1) = self
                    .memory
                    .affected_instruction_range(self.index, vx as u16 + 1);
                self.prefetch[prefetch_range0].fill(None);
                self.prefetch[prefetch_range1].fill(None);

                if let RomKind::COSMACVIP | RomKind::XOCHIP = self.rom.config.kind {
                    self.index =
                        self.index.overflowing_add(vx as u16 + 1).0 & self.memory_last_address;
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

                let (prefetch_range0, prefetch_range1) = self
                    .memory
                    .affected_instruction_range(self.index, buf.len() as u16);
                self.prefetch[prefetch_range0].fill(None);
                self.prefetch[prefetch_range1].fill(None);
            }

            Instruction::LoadFlags(vx) => {
                self.flags.export(0, &mut self.registers[..=vx as usize]);
            }

            Instruction::StoreFlags(vx) => {
                self.flags.import(&self.registers[..=vx as usize], 0);
            }

            Instruction::StoreBinaryCodedDecimal(vx) => {
                let decimal = self.registers[vx as usize];
                self.workspace[..3]
                    .iter_mut()
                    .rev()
                    .enumerate()
                    .for_each(|(i, val)| *val = decimal / 10u8.pow(i as u32) % 10);
                self.memory.import(&self.workspace[..3], self.index);

                let (prefetch_range0, prefetch_range1) =
                    self.memory.affected_instruction_range(self.index, 3);
                self.prefetch[prefetch_range0].fill(None);
                self.prefetch[prefetch_range1].fill(None);
            }

            Instruction::GenerateRandom(vx, bound) => {
                self.registers[vx as usize] = (self.rng.next_u32() & bound as u32) as u8;
            }

            Instruction::SetPlane(flags) => {
                self.display.selected_plane_bitflags = flags;
            }

            Instruction::Draw(vx, vy, height) => {
                if self.rom.config.kind == RomKind::COSMACVIP && !self.input.vertical_blank {
                    self.waiting = true;
                } else {
                    self.waiting = false;
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
            // NOTE: fetch decode is kinda expensive so check specifically for F000
            self.pc = self
                .pc
                .overflowing_add(
                    if self.rom.config.kind == RomKind::XOCHIP
                        && self.memory[self.pc as usize] == 0xF0
                        && self.memory[(self.pc as usize + 1) % self.memory.len()] == 0x00
                    {
                        4
                    } else {
                        2
                    },
                )
                .0
                & self.memory_last_address;
        }

        true
    }

    fn exec_display_instruction(&mut self, vx: u8, vy: u8, n: u8) {
        let (bytes_per_row, height, total_bytes) = self.get_sprite_draw_info(n);

        self.memory
            .export(self.index, &mut self.workspace[..total_bytes]);

        self.registers[VFLAG] = self.display.draw(
            &self.workspace,
            self.registers[vx as usize] as u16,
            self.registers[vy as usize] as u16,
            height,
            bytes_per_row,
            self.rom.config.kind == RomKind::XOCHIP,
        ) as u8;
    }

    // (bytes per row, rows per plane, total bytes to read)
    fn get_sprite_draw_info(&self, n: u8) -> (usize, usize, usize) {
        if self.rom.config.kind >= RomKind::SCHIP && n == 0 {
            (
                2,
                16,
                32 * self.display.selected_plane_bitflags.count_ones() as usize,
            )
        } else {
            (
                1,
                n as usize,
                n as usize * self.display.selected_plane_bitflags.count_ones() as usize,
            )
        }
    }
}
