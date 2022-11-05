use crate::disp::{DisplayBuffer, DISPLAY_HEIGHT, DISPLAY_WIDTH};
use std::{fmt::Display, fs::read, io, path::Path};

const PROGRAM_STARTING_ADDRESS: u16 = 0x200;

const VFLAG: usize = 15;

const MEMORY_SIZE: usize = 4096;

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

#[derive(Clone, Copy, Debug)]
struct InstructionParameters {
    bits: u16,
    op: u8,
    x: u8,
    y: u8,
    n: u8,
    nn: u8,
    nnn: u16,
}

impl InstructionParameters {
    fn new(bits: u16) -> Self {
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

impl Display for InstructionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:X?} (op = {:X?}, x = {:?}, y = {:?}, n = {:?}, nn = {:?}, nnn = {:?})",
            self.bits, self.op, self.x, self.y, self.n, self.nn, self.nnn
        )
    }
}

#[derive(Debug)]
enum Instruction {
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

impl Instruction {
    fn decode(params: InstructionParameters) -> Self {
        let (op, x, y, n, nn, nnn) = (
            params.op, params.x, params.y, params.n, params.nn, params.nnn,
        );

        match op {
            0x0 => match nnn {
                0x0E0 => Self::ClearScreen,
                0x0EE => Self::SubroutineReturn,
                _ => panic!("unable to decode instruction {}", params),
            },
            0x1 => Self::Jump(nnn),
            0x2 => Self::CallSubroutine(nnn),
            0x3 => Self::SkipIfEqualsConstant(x, nn),
            0x4 => Self::SkipIfNotEqualsConstant(x, nn),
            0x5 => Self::SkipIfEquals(x, y),
            0x6 => Self::SetConstant(x, nn),
            0x7 => Self::AddConstant(x, nn),
            0x8 => match n {
                0x0 => Self::Set(x, y),
                0x1 => Self::Or(x, y),
                0x2 => Self::And(x, y),
                0x3 => Self::Xor(x, y),
                0x4 => Self::Add(x, y),
                0x5 => Self::Sub(x, y, true),
                0x6 => Self::Shift(x, y, true),
                0x7 => Self::Sub(x, y, false),
                0xE => Self::Shift(x, y, false),
                _ => panic!("unable to decode instruction {}", params),
            },
            0x9 => match n {
                0x0 => Self::SkipIfNotEquals(x, y),
                _ => panic!("unable to decode instruction {}", params),
            },
            0xA => Self::SetIndex(nnn),
            0xB => Self::JumpWithOffset(nnn, x),
            0xC => Self::GenerateRandom(x, nn),
            0xD => Self::Display(x, y, n),
            0xE => match nn {
                0x9E => Self::SkipIfKeyDown(x),
                0xA1 => Self::SkipIfKeyNotDown(x),
                _ => panic!("unable to decode instruction {}", params),
            },
            0xF => match nn {
                0x07 => Self::GetDelayTimer(x),
                0x15 => Self::SetDelayTimer(x),
                0x18 => Self::SetSoundTimer(x),
                0x1E => Self::AddToIndex(x),
                0x0A => Self::GetKey(x),
                0x29 => Self::SetIndexToHexChar(x),
                0x33 => Self::StoreDecimal(x),
                0x55 => Self::Store(x),
                0x65 => Self::Load(x),
                _ => panic!("unable to decode instruction {}", params),
            },
            _ => panic!("unable to decode instruction {}", params),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
#[allow(dead_code)]
pub enum InterpreterKind {
    #[default]
    CHIP48,
    COSMACVIP,
    // SUPERCHIP,
}

#[derive(Debug, Default)]
pub struct InterpreterInput {
    pub delay_timer: u8,

    pub pressed_keys: u16,
    pub just_pressed_key: Option<u8>,
    pub just_released_key: Option<u8>,
}

#[derive(Debug)]
pub struct InterpreterOutput {
    pub display: DisplayBuffer,
    pub awaiting_input: bool,

    pub request: Option<InterpreterRequest>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpreterRequest {
    Display,
    SetDelayTimer(u8),
    SetSoundTimer(u8),
}

#[derive(Debug)]
pub struct Interpreter {
    kind: InterpreterKind,
    memory: [u8; MEMORY_SIZE],
    pc: u16,
    index: u16,
    stack: Vec<u16>,
    registers: [u8; 16],
    output: InterpreterOutput,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut memory = [0; MEMORY_SIZE];
        memory[FONT_STARTING_ADDRESS as usize..FONT_STARTING_ADDRESS as usize + FONT.len()]
            .copy_from_slice(&FONT);
        Interpreter {
            kind: Default::default(),
            memory,
            pc: 0,
            index: 0,
            stack: Vec::new(),
            registers: [0; 16],
            output: InterpreterOutput {
                display: [0; DISPLAY_HEIGHT as usize],
                awaiting_input: false,
                request: None,
            },
        }
    }
}

impl Interpreter {
    pub fn from_program<P: AsRef<Path>>(path: P, kind: InterpreterKind) -> io::Result<Interpreter> {
        let mut interp = Interpreter {
            kind,
            ..Default::default()
        };
        interp.load_program(path)?;
        Ok(interp)
    }

    pub fn load_program<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let program = read(path)?;
        self.memory[PROGRAM_STARTING_ADDRESS as usize
            ..PROGRAM_STARTING_ADDRESS as usize + program.len()]
            .copy_from_slice(&program);
        self.pc = PROGRAM_STARTING_ADDRESS;
        Ok(())
    }

    pub fn step(&mut self, input: &InterpreterInput) -> &InterpreterOutput {
        // clear output request
        self.output.request = None;

        // fetch + decode
        let inst = Instruction::decode(InstructionParameters::new(
            (self.memory[self.pc as usize] as u16) << 8 | self.memory[self.pc as usize + 1] as u16, // assuming big-endianness (MSB first)
        ));

        self.pc += 2;

        log::trace!("instruction {:?} ", inst);

        // execute
        match inst {
            Instruction::ClearScreen => {
                log::info!("clearing screen");

                self.output.display.fill(0);
                self.output.request = Some(InterpreterRequest::Display);
            }

            Instruction::Jump(pc) => self.pc = pc,

            Instruction::JumpWithOffset(address, vx) => {
                if self.kind == InterpreterKind::COSMACVIP {
                    self.pc = address + self.registers[0] as u16;
                } else {
                    self.pc = address + self.registers[vx as usize] as u16;
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
                if input.pressed_keys >> self.registers[vx as usize] & 1 == 1 {
                    self.pc += 2
                }
            }

            Instruction::SkipIfKeyNotDown(vx) => {
                if input.pressed_keys >> self.registers[vx as usize] & 1 == 0 {
                    self.pc += 2
                }
            }

            Instruction::GetKey(vx) => {
                if let Some(key_code) = match self.kind {
                    InterpreterKind::COSMACVIP => input.just_released_key,
                    _ => input.just_pressed_key,
                } {
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
                self.registers[VFLAG] = !overflowed as u8;
            }

            // ambiguous!
            Instruction::Shift(vx, vy, right) => {
                let bits = match self.kind {
                    InterpreterKind::COSMACVIP => self.registers[vy as usize],
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

            Instruction::GetDelayTimer(vx) => self.registers[vx as usize] = input.delay_timer,

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
                    % MEMORY_SIZE as u16
            }

            Instruction::Load(vx) => {
                self.registers[..=vx as usize].copy_from_slice(
                    &self.memory[self.index as usize..=self.index as usize + vx as usize],
                );
                if self.kind == InterpreterKind::COSMACVIP {
                    self.index += vx as u16 + 1;
                }
            }

            Instruction::Store(vx) => {
                self.memory[self.index as usize..=self.index as usize + vx as usize]
                    .copy_from_slice(&self.registers[..=vx as usize]);
                if self.kind == InterpreterKind::COSMACVIP {
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

                log::info!("drawing sprite @ (x = {:?}, y = {:?})", pos_x, pos_y);

                self.registers[VFLAG] = 0;

                for (y, sprite_row) in (0..height.min(DISPLAY_HEIGHT - pos_y)) // row indicies that aren't clipped by the display height
                    .into_iter()
                    .map(|row_i| { // mapping row index to 2-tuple of...
                        (
                            // y-coord of row index in display buffer
                            (pos_y + row_i) as usize, 

                            // 8-bit data of row expanded to 64 bits by padding (64 - 8) zeros to the right and then shifting everything to the right by pos_x amount
                            (self.memory[self.index as usize + row_i as usize] as u64)
                                << (u64::BITS - u8::BITS)
                                >> pos_x,
                        )
                    })
                {
                    let display_row = self.output.display[y];
                    if display_row & sprite_row != 0 {
                        // if any 2 bits were both a 1 then we need to set register VF (VFLAG) to 1
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
