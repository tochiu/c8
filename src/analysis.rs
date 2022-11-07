use std::{collections::HashSet, fmt::Display};

use crate::{interp::{Instruction, Interpreter, VFLAG}, prog::{PROGRAM_STARTING_ADDRESS, PROGRAM_MEMORY_SIZE, Program, ProgramKind}};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct SimulationState {
    registers: [Option<u8>; 16],
    index: usize
}

fn addr_index(addr: u16) -> Option<usize> {
    if addr % 2 == 0 && addr >= PROGRAM_STARTING_ADDRESS && addr < PROGRAM_MEMORY_SIZE {
        Some((addr - PROGRAM_STARTING_ADDRESS) as usize/2)
    } else {
        None
    }
}

pub struct Analysis {
    instructions: Vec<Option<Instruction>>,
    interp: Interpreter,
    touch: Vec<bool>
}

impl From<Program> for Analysis {
    fn from(program: Program) -> Self {
        let instructions: Vec<_> = program.instructions().collect();
        let interp = program.into();
        let touch = vec![false; instructions.len()];

        let mut analysis = Analysis { instructions, interp, touch };
        analysis.eval(SimulationState { registers: [None; 16], index: 0 }, &mut HashSet::new());
        analysis
    }
}

impl Analysis {
    fn simulate(&mut self, inst: Instruction, state: &SimulationState) -> [u8; 16] {
        for (i, register) in self.interp.registers_mut().iter_mut().enumerate() {
            if let Some(value) = state.registers[i] {
                *register = value;
            }
        }

        self.interp.exec(inst, &Default::default());
        return *self.interp.registers_mut();
    }

    fn eval(&mut self, mut state: SimulationState, memstate: &mut HashSet<SimulationState>) {
        if let Some(Some(instruction)) = self.instructions.get(state.index) {
            // memoize
            if memstate.contains(&state) {
                return
            }
            memstate.insert(state);

            // touch
            self.touch[state.index] = true;

            // traverse
            match *instruction {
                Instruction::Jump(addr) => {
                    if let Some(index) = addr_index(addr) {
                        state.index = index;
                        println!("eval {:?} with state {:?}", instruction, state);
                        self.eval(state, memstate);
                    }
                    return; // this is a discontinuity so return
                },
                // SIDE NOTE:
                // could consider possible address jump range but thats just gonna cause false-positive and idk seems very cringe
                // well if v0 is well defined which it probably should be for any sane binary with you know.. constant jumps then we should be fine
                // it is NOT good practice for variable jumps to be encoded in the vx register but rather split into a conditonal with separate jumps
                // esp since jump addresses the whole memory space and doesnt need to be segmented
                Instruction::JumpWithOffset(addr, vx) => { // analysis pit
                    let vx = if self.interp.program.kind == ProgramKind::CHIP48 { vx } else { 0 }; // chip48 is cringe
                    if let Some(index) = state.registers[vx as usize].and_then(|offset| addr_index(addr + offset as u16)) {
                        state.index = index;
                        println!("eval {:?} with state {:?}", instruction, state);
                        self.eval(state, memstate);
                    }
                    return; // this is a discontinuity so return
                },
                Instruction::CallSubroutine(addr) => {
                    if let Some(index) = addr_index(addr) {
                        println!("eval {:?} with state {:?}", instruction, state);
                        self.eval(SimulationState { index, registers: state.registers }, memstate);
                    }
                },
                Instruction::SubroutineReturn => return, // this doesnt call an eval the caller just continues when this returns
                Instruction::SkipIfEqualsConstant(_, _)
                    | Instruction::SkipIfNotEqualsConstant(_, _)
                    | Instruction::SkipIfEquals(_, _)
                    | Instruction::SkipIfNotEquals(_, _)
                    | Instruction::SkipIfKeyDown(_)
                    | Instruction::SkipIfKeyNotDown(_) 
                    => 
                {
                    // evaluate skip branch (after this match statement the normal branch is evaluated)
                    state.index += 2;
                    self.eval(state, memstate);
                    state.index -= 2;
                },
                Instruction::GetKey(vx) => {
                    state.registers[vx as usize] = None;
                },
                Instruction::SetConstant(vx, value) => {
                    state.registers[vx as usize] = Some(value);
                },
                Instruction::AddConstant(vx, value) => {
                    state.registers[vx as usize] = state.registers[vx as usize].map(|x| x.overflowing_add(value).0);
                },
                Instruction::Set(vx, vy) => {
                    state.registers[vx as usize] = state.registers[vy as usize];
                },
                Instruction::Or(vx, vy) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        state.registers[vx as usize] = Some(self.simulate(Instruction::Or(vx, vy), &state)[vx as usize]);
                    } else {
                        state.registers[vx as usize] = None;
                    }
                },
                Instruction::And(vx, vy) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        state.registers[vx as usize] = Some(self.simulate(Instruction::And(vx, vy), &state)[vx as usize]);
                    } else {
                        state.registers[vx as usize] = None;
                    }
                },
                Instruction::Xor(vx, vy) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        state.registers[vx as usize] = Some(self.simulate(Instruction::Xor(vx, vy), &state)[vx as usize]);
                    } else {
                        state.registers[vx as usize] = None;
                        state.registers[VFLAG] = None;
                    }
                },
                Instruction::Add(vx, vy) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        let registers = self.simulate(Instruction::Add(vx, vy), &state);
                        state.registers[vx as usize] = Some(registers[vx as usize]);
                        state.registers[VFLAG] = Some(registers[VFLAG]);
                    } else {
                        state.registers[vx as usize] = None;
                        state.registers[VFLAG] = None;
                    }
                },
                Instruction::Sub(vx, vy, vx_minus_vy) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        let registers = self.simulate(Instruction::Sub(vx, vy, vx_minus_vy), &state);
                        state.registers[vx as usize] = Some(registers[vx as usize]);
                        state.registers[VFLAG] = Some(registers[VFLAG]);
                    } else {
                        state.registers[vx as usize] = None;
                        state.registers[VFLAG] = None;
                    }
                },
                Instruction::Shift(vx, vy, right) => {
                    if state.registers[vx as usize].is_some() && state.registers[vy as usize].is_some() {
                        let registers = self.simulate(Instruction::Shift(vx, vy, right), &state);
                        state.registers[vx as usize] = Some(registers[vx as usize]);
                        state.registers[VFLAG] = Some(registers[VFLAG]);
                    } else {
                        state.registers[vx as usize] = None;
                        state.registers[VFLAG] = None;
                    }
                },
                Instruction::GetDelayTimer(vx) => {
                    state.registers[vx as usize] = None;
                },

                // instructions that dont contribute to analysis because they dont change registers
                // perhaps later down the line i will include other parts of the interpreter in the analysis
                Instruction::ClearScreen
                    | Instruction::SetDelayTimer(_)
                    | Instruction::SetSoundTimer(_)
                    | Instruction::SetIndex(_)
                    | Instruction::SetIndexToHexChar(_)
                    | Instruction::AddToIndex(_)
                    | Instruction::Store(_)
                    | Instruction::StoreDecimal(_)
                    | Instruction::Display(_, _, _)
                    => (),
                
                Instruction::Load(vx) => {
                    state.registers[0..=vx as usize].fill(None);
                },
                Instruction::GenerateRandom(vx, _) => {
                    state.registers[vx as usize] = None;
                }
            }

            // we made it this far so the instruction doesn't introduce a discontinuity
            // evaluate next instruction
            state.index += 1;
            self.eval(state, memstate);
        }
    }
}

impl Display for Analysis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let asm = self.instructions.iter().map(|maybe_inst| -> Result<(String, String), std::fmt::Error> {
            let mut content = String::new();
            let mut comments = String::new();
            
            if let Some(inst) = maybe_inst {
                write_inst_asm(inst, &mut content, &mut comments)?;
            }

            Ok((content, comments))
        }).collect::<Vec<_>>();

        let mut content_columns = asm
            .iter()
            .filter_map(|maybe_inst| maybe_inst.as_ref().ok().map(|(content, _)| content.len()))
            .max()
            .unwrap_or_default();
        content_columns += 4 - (content_columns + 2) % 4;

        for (i, params) in self.interp.program.instruction_parameters().enumerate() {
            let addr = PROGRAM_STARTING_ADDRESS as usize + 2*i;
            let addr_symbol = if self.touch[i] {"X"} else if self.instructions[i].is_some() {"?"} else {" "};
            let (content, comment) = asm[i].as_ref().map_err(|_| std::fmt::Error)?;

            write!(f, "{:#05X}: |{}| {:#06X}", addr, addr_symbol, params.bits)?;

            if addr_symbol == "X" {
                if !content.is_empty() {
                    write!(f, " {}", content)?;
                }
                if !comment.is_empty() {
                    write!(f, "{}# {}", " ".repeat(content_columns - content.len()), comment)?;
                }
                writeln!(f, "")?;
            } else {
                let byte0 = (params.bits >> 8) as u8;
                let byte1 = (params.bits & u8::MAX as u16) as u8;
                let mut graphic0 = String::with_capacity(16);
                let mut graphic1 = String::with_capacity(16);

                //write_byte_str((params.bits >> 8) as u8, 1, &mut byte0)?;
                //graphic0.push_str(" 2X GRAPHIC ");
                write_byte_str(byte0, 2, &mut graphic0)?;
                //write_byte_str((params.bits & u8::MAX as u16) as u8, 1, &mut byte1)?;
                //graphic1.push_str(" 2X GRAPHIC ");
                write_byte_str(byte1 , 2, &mut graphic1)?;
                
                writeln!(f, "{}# {:08b} 2X GRAHPHIC {}", " ".repeat(content_columns + 1), byte0, graphic0)?;
                writeln!(f, "{:#05X}: |{}|{}# {:08b} 2X GRAHPHIC {}", addr + 1, addr_symbol, " ".repeat(content_columns + 8), byte1, graphic1)?;

                //write!(f, "{}# ")
                //write!(f, "{}# {}", " ".repeat(content_columns - content.len()), comment)?;
            }
        }

        Ok(())
    }
}

fn write_byte_str(byte: u8, repeat: usize, f: &mut impl std::fmt::Write) -> std::fmt::Result {
    for filled in (0..8).rev().map(|i| byte >> i & 1 == 1) {
        write!(f, "{}", if filled { "@".repeat(repeat) } else { ".".repeat(repeat) })?;
    }

    Ok(())
}

fn write_inst_asm(inst: &Instruction, f: &mut impl std::fmt::Write, c: &mut impl std::fmt::Write) -> std::fmt::Result {
    match inst {
        Instruction::ClearScreen => {
            write!(f, "clear")?;
            write!(c, "clear display")
        },

        // side effect of discontinuity instructions having no comments is it highlights a clear break in execution 
        Instruction::Jump(addr) => write!(f, "jump  {:#05X}", addr),
        Instruction::JumpWithOffset(addr, _) => write!(f, "jumpo {:#05X}", addr),
        Instruction::CallSubroutine(addr) => write!(f, "call  {:#05X}", addr), 
        Instruction::SubroutineReturn => write!(f, "return"),

        Instruction::SkipIfEqualsConstant(vx, value) => {
            write!(f, "seq   $v{} {}", vx, value)?;
            write!(c, "skip next if $v{} == {}", vx, value)
        },
        Instruction::SkipIfNotEqualsConstant(vx, value) => {
            write!(f, "sne   $v{} {}", vx, value)?;
            write!(c, "skip next if $v{} != {}", vx, value)
        },
        Instruction::SkipIfEquals(vx, vy) => {
            write!(f, "seq   $v{} $v{}", vx, vy)?;
            write!(c, "skip next if $v{} == $v{}", vx, vy)
        },
        Instruction::SkipIfNotEquals(vx, vy) => {
            write!(f, "sne   $v{} $v{}", vx, vy)?;
            write!(c, "skip next if $v{} != $v{}", vx, vy)
        },
        Instruction::SkipIfKeyDown(vx) => {
            write!(f, "skd   $v{}", vx)?;
            write!(c, "skip next if $v{} key is down", vx)
        },
        Instruction::SkipIfKeyNotDown(vx) => {
            write!(f, "sku   $v{}", vx)?;
            write!(c, "skip next if $v{} key is up", vx)
        },
        Instruction::GetKey(vx) => {
            write!(f, "lnkp  $v{}", vx)?;
            write!(c, "$v{} = load next key press", vx)
        },
        Instruction::SetConstant(vx, value) => {
            write!(f, "mov   $v{} {}", vx, value)?;
            write!(c, "$v{} = {}", vx, value)
        },
        Instruction::AddConstant(vx, value) => {
            write!(f, "add   $v{} {}", vx, value)?;
            write!(c, "$v{} += {}", vx, value)
        },
        Instruction::Set(vx, vy) => {
            write!(f, "mov   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} = $v{}", vx, vy)
        },
        Instruction::Or(vx, vy) => {
            write!(f, "or    $v{} $v{}", vx, vy)?;
            write!(c, "$v{} |= $v{}", vx, vy)
        },
        Instruction::And(vx, vy) => {
            write!(f, "and   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} &= $v{}", vx, vy)
        },
        Instruction::Xor(vx, vy) => {
            write!(f, "xor   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} ^= $v{}", vx, vy)
        },
        Instruction::Add(vx, vy) => {
            write!(f, "add   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} += $v{}", vx, vy)
        },
        Instruction::Sub(vx, vy, vx_minus_vy) => {
            if *vx_minus_vy {
                write!(f, "sub   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} -= $v{}", vx, vy)
            } else {
                write!(f, "subn  $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} - $v{}", vx, vy, vx)
            }
        },
        Instruction::Shift(vx, vy, right) => {
            if *right {
                write!(f, "srl   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} >> 1", vx, vy)
            } else {
                write!(f, "sll   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} << 1", vx, vy)
            }
        },
        Instruction::GetDelayTimer(vx) => {
            write!(f, "ldly  $v{}", vx)?;
            write!(c, "$v{} = delay timer", vx)
        },
        Instruction::SetDelayTimer(vx) => {
            write!(f, "sdly  $v{}", vx)?;
            write!(c, "delay timer = $v{}", vx)
        },
        Instruction::SetSoundTimer(vx) => {
            write!(f, "sound $v{}", vx)?;
            write!(c, "sound timer = $v{}", vx)
        },
        Instruction::SetIndex(value) => {
            write!(f, "iset  {:#05X}", value)?;
            write!(c, "index = {:#05X}", value)
        },
        Instruction::SetIndexToHexChar(vx) => {
            write!(f, "ichr  $v{}", vx)?;
            write!(c, "index = sprite address of hexadecimal char $v{}", vx)
        },
        Instruction::AddToIndex(value) => {
            write!(f, "iadd  {}", value)?;
            write!(c, "index += {} ({:#05X})", value, value)
        },
        Instruction::Load(vx) => {
            write!(f, "load  {}", vx + 1)?;
            write!(c, "load {} byte(s) into $v(0..={})", vx + 1, vx)
        },
        Instruction::Store(vx) => {
            write!(f, "save  {}", vx + 1)?;
            write!(c, "save {} byte(s) from $v(0..={})", vx + 1, vx)
        },
        Instruction::StoreDecimal(vx) => {
            write!(f, "sbcd  $v{}", vx)?;
            write!(c, "save binary-coded decimal $v{}", vx)
        },
        Instruction::GenerateRandom(vx, bound) => {
            write!(f, "rand  $v{} {}", vx, bound)?;
            write!(c, "$v{} = rand in range [0, {}]", vx, bound)
        },
        Instruction::Display(vx, vy, height) => {
            write!(f, "disp  $v{} $v{} {}", vx, vy, height)?;
            write!(c, "display {} rows of sprite at screen pos ($v{}, $v{})", height, vx, vy)
        }
    }
}