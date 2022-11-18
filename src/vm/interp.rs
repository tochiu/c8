use super::disp::{DisplayBuffer, DISPLAY_HEIGHT, DISPLAY_WIDTH};
use super::prog::{Program, ProgramKind, PROGRAM_STARTING_ADDRESS, PROGRAM_MEMORY_SIZE};

use std::fmt::Display;

pub const VFLAG: usize = 15;

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

// Takes a 16 bit number (instruction size) and decomposes it into its parts
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
            op:  ((bits & 0xF000) >> 4 * 3) as u8,
            x:   ((bits & 0x0F00) >> 4 * 2) as u8,
            y:   ((bits & 0x00F0) >> 4 * 1) as u8,
            n:   ((bits & 0x000F) >> 4 * 0) as u8,
            nn:  ((bits & 0x00FF) >> 4 * 0) as u8,
            nnn: ((bits & 0x0FFF) >> 4 * 0) as u16,
        }
    }
}

impl From<[u8; 2]> for InstructionParameters {
    fn from(bytes: [u8; 2]) -> Self {
        InstructionParameters::from((bytes[0] as u16) << 8 | bytes[1] as u16)
    }
}

impl Display for InstructionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#06X} (op = {:#X?}, x = {:?}, y = {:?}, n = {:?}, nn = {:?}, nnn = {:?})",
            self.bits, self.op, self.x, self.y, self.n, self.nn, self.nnn
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    ClearScreen,
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
    AddToIndex(u8),
    Load(u8),
    Store(u8),
    StoreDecimal(u8),
    GenerateRandom(u8, u8),
    Display(u8, u8, u8),
}

impl TryFrom<InstructionParameters> for Instruction {
    type Error = String;
    fn try_from(params: InstructionParameters) -> Result<Self, Self::Error> {
        let (op, x, y, n, nn, nnn) = (
            params.op, params.x, params.y, params.n, params.nn, params.nnn,
        );

        match op {
            0x0 => match nnn {
                0x0E0 => Ok(Self::ClearScreen),
                0x0EE => Ok(Self::SubroutineReturn),
                _ => Err(format!("unable to decode instruction {}", params)),
            },
            0x1 => Ok(Self::Jump(nnn)),
            0x2 => Ok(Self::CallSubroutine(nnn)),
            0x3 => Ok(Self::SkipIfEqualsConstant(x, nn)),
            0x4 => Ok(Self::SkipIfNotEqualsConstant(x, nn)),
            0x5 => Ok(Self::SkipIfEquals(x, y)),
            0x6 => Ok(Self::SetConstant(x, nn)),
            0x7 => Ok(Self::AddConstant(x, nn)),
            0x8 => match n {
                0x0 => Ok(Self::Set(x, y)),
                0x1 => Ok(Self::Or(x, y)),
                0x2 => Ok(Self::And(x, y)),
                0x3 => Ok(Self::Xor(x, y)),
                0x4 => Ok(Self::Add(x, y)),
                0x5 => Ok(Self::Sub(x, y, true)),
                0x6 => Ok(Self::Shift(x, y, true)),
                0x7 => Ok(Self::Sub(x, y, false)),
                0xE => Ok(Self::Shift(x, y, false)),
                _ => Err(format!("unable to decode instruction {}", params)),
            },
            0x9 => match n {
                0x0 => Ok(Self::SkipIfNotEquals(x, y)),
                _ => Err(format!("unable to decode instruction {}", params)),
            },
            0xA => Ok(Self::SetIndex(nnn)),
            0xB => Ok(Self::JumpWithOffset(nnn, x)),
            0xC => Ok(Self::GenerateRandom(x, nn)),
            0xD => Ok(Self::Display(x, y, n)),
            0xE => match nn {
                0x9E => Ok(Self::SkipIfKeyDown(x)),
                0xA1 => Ok(Self::SkipIfKeyNotDown(x)),
                _ => Err(format!("unable to decode instruction {}", params)),
            },
            0xF => match nn {
                0x07 => Ok(Self::GetDelayTimer(x)),
                0x15 => Ok(Self::SetDelayTimer(x)),
                0x18 => Ok(Self::SetSoundTimer(x)),
                0x1E => Ok(Self::AddToIndex(x)),
                0x0A => Ok(Self::GetKey(x)),
                0x29 => Ok(Self::SetIndexToHexChar(x)),
                0x33 => Ok(Self::StoreDecimal(x)),
                0x55 => Ok(Self::Store(x)),
                0x65 => Ok(Self::Load(x)),
                _ => Err(format!("unable to decode instruction {}", params)),
            },
            _ => Err(format!("unable to decode instruction {}", params)),
        }
    }
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
#[derive(Debug)]
pub struct InterpreterOutput {
    pub display: DisplayBuffer,
    pub awaiting_input: bool,

    pub request: Option<InterpreterRequest>,
}

// Interpreter IO Request
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterRequest {
    Display,
    SetDelayTimer(u8),
    SetSoundTimer(u8),
}

#[derive(Debug)]
pub enum InterpreterError {
    BadInstruction(String)
}

#[derive(Debug)]
pub struct Interpreter {
    pub memory: [u8; PROGRAM_MEMORY_SIZE as usize],
    pub pc: u16,
    pub index: u16,
    pub stack: Vec<u16>,
    pub registers: [u8; 16],
    pub input: InterpreterInput,
    pub output: InterpreterOutput,
    pub program: Program
}

impl<'a> From<Program> for Interpreter {
    fn from(program: Program) -> Self {
        let mut memory = [0; PROGRAM_MEMORY_SIZE as usize];

        memory[FONT_STARTING_ADDRESS as usize..FONT_STARTING_ADDRESS as usize + FONT.len()]
            .copy_from_slice(&FONT);
        
        memory[PROGRAM_STARTING_ADDRESS as usize
            ..PROGRAM_STARTING_ADDRESS as usize + program.data.len()]
            .copy_from_slice(&program.data);
        
        Interpreter {
            program,
            memory,
            pc: PROGRAM_STARTING_ADDRESS,
            index: 0,
            stack: Vec::new(),
            registers: [0; 16],
            input: Default::default(),
            output: InterpreterOutput {
                display: [0; DISPLAY_HEIGHT as usize],
                awaiting_input: false,
                request: None,
            },
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::from(Program::default())
    }
}

impl Interpreter {

    pub fn input_mut(&mut self) -> &mut InterpreterInput {
        &mut self.input
    }

    // interpret the next instruction
    pub fn step(&mut self) -> Result<&InterpreterOutput, InterpreterError> {
        // clear output request
        self.output.request = None;

        // fetch + decode
        let inst = Instruction::try_from(self.fetch())
            .map_err(|str| InterpreterError::BadInstruction(str))?;

        log::trace!("instruction {:#05X?} {:?} ", self.pc, inst);
        
        self.pc += 2;

        // exec instruction
        Ok(self.exec(inst))
    }

    pub fn fetch(&self) -> InstructionParameters {
        InstructionParameters::from([self.memory[self.pc as usize], self.memory[self.pc as usize + 1]])
    }
    
    pub fn exec(&mut self, inst: Instruction) -> &InterpreterOutput {
        match inst {
            Instruction::ClearScreen => {
                self.output.display.fill(0);
                self.output.request = Some(InterpreterRequest::Display);
            }

            Instruction::Jump(pc) => self.pc = pc,

            Instruction::JumpWithOffset(address, vx) => {
                if self.program.kind == ProgramKind::CHIP48 {
                    self.pc = address + self.registers[vx as usize] as u16;
                } else {
                    self.pc = address + self.registers[0] as u16;
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
                    .expect("could not return from subroutine because stack is empty")
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
                // log::debug!("skip if {:?} key up", Key::try_from(self.registers[vx as usize]));
                if self.input.down_keys >> self.registers[vx as usize] & 1 == 0 {
                    self.pc += 2
                }
            }

            Instruction::GetKey(vx) => {
                if let Some(key_code) = 
                    match self.program.kind {
                        ProgramKind::COSMACVIP => self.input.just_released_key,
                        _ => self.input.just_pressed_key,
                    } 
                {
                    self.registers[vx as usize] = key_code;
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
                let bits = match self.program.kind {
                    ProgramKind::COSMACVIP => self.registers[vy as usize],
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
                self.index = FONT_STARTING_ADDRESS
                    + (FONT_CHAR_DATA_SIZE as u16 * self.registers[vx as usize] as u16)
            }

            // TODO: maybe make optional behavior (register vf set on overflow) configurable
            Instruction::AddToIndex(vx) => {
                self.index = (self.index as u32 + self.registers[vx as usize] as u32) as u16
                    % PROGRAM_MEMORY_SIZE as u16
            }

            Instruction::Load(vx) => {
                self.registers[..=vx as usize].copy_from_slice(
                    &self.memory[self.index as usize..=self.index as usize + vx as usize],
                );
                if self.program.kind == ProgramKind::COSMACVIP {
                    self.index += vx as u16 + 1;
                }
            }

            Instruction::Store(vx) => {
                self.memory[self.index as usize..=self.index as usize + vx as usize]
                    .copy_from_slice(&self.registers[..=vx as usize]);
                if self.program.kind == ProgramKind::COSMACVIP {
                    self.index += vx as u16 + 1;
                }
            }

            Instruction::StoreDecimal(vx) => {
                let number = self.registers[vx as usize];
                for i in 0..3 {
                    self.memory[self.index as usize + i] = number / 10u8.pow(2 - i as u32) % 10;
                }
            }

            Instruction::GenerateRandom(vx, bound) => {
                self.registers[vx as usize] = rand::random::<u8>() & bound
            }

            Instruction::Display(vx, vy, height) => {
                let pos_x = self.registers[vx as usize] % DISPLAY_WIDTH;
                let pos_y = self.registers[vy as usize] % DISPLAY_HEIGHT;

                self.registers[VFLAG] = 0;

                for (y, sprite_row) in (0..height.min(DISPLAY_HEIGHT - pos_y)) // row indicies that aren't clipped by the display height
                    .into_iter()
                    .map(|row_i| { // mapping row index to 2-tuple of...
                        (
                            // y-coord of row index in display buffer
                            (pos_y + row_i) as usize, 

                            // 8-bit row data expanded to 64 bits by padding (64 - 8) zeros to the right and then shifting everything to the right by pos_x amount
                            (self.memory[self.index as usize + row_i as usize] as u64)
                                << (u64::BITS - u8::BITS)
                                >> pos_x,
                        )
                    })
                {
                    let display_row = self.output.display[y];
                    if display_row & sprite_row != 0 {
                        // if any 2 bits are both 1 then we need to set register VF (VFLAG) to 1
                        self.registers[VFLAG] = 1;
                    }

                    self.output.display[y] = display_row ^ sprite_row; // bitwise xor with the current display row
                }

                self.output.request = Some(InterpreterRequest::Display);
            }
        }

        &self.output
    }
}
