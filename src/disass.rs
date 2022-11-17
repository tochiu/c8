use crate::vm::{
    interp::Instruction,
    prog::{Program, PROGRAM_STARTING_ADDRESS},
};

use std::fmt::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum InstructionTag {
    Not,
    Parsable,
    Reachable,
    Likely,
    Proven,
}

#[derive(Clone, Copy, Debug)]
pub struct DisassemblyPath {
    index: usize,
    tag: InstructionTag,
    depth: usize,
}

impl DisassemblyPath {
    fn fork(&self, index: usize) -> Self {
        DisassemblyPath {
            index,
            tag: self.tag,
            depth: self.depth,
        }
    }

    fn tag(mut self, tag: InstructionTag) -> Self {
        self.tag = tag;
        self
    }

    fn subroutine(mut self) -> Self {
        self.depth += 1;
        self
    }
}

pub struct Disassembly {
    instructions: Vec<Option<Instruction>>,
    program: Program,
    tags: Vec<InstructionTag>,
}

impl From<Program> for Disassembly {
    fn from(program: Program) -> Self {
        let instructions: Vec<_> = program.instructions().collect();
        let tags = instructions
            .iter()
            .map(|maybe_inst| {
                if maybe_inst.is_some() {
                    InstructionTag::Parsable
                } else {
                    InstructionTag::Not
                }
            })
            .collect();

        let mut disass = Disassembly {
            instructions,
            program,
            tags,
        };
        disass.eval(DisassemblyPath {
            index: 0,
            tag: InstructionTag::Proven,
            depth: 0,
        });
        disass
    }
}

impl Disassembly {
    fn addr_index(&self, addr: u16) -> Option<usize> {
        if addr >= PROGRAM_STARTING_ADDRESS
            && addr < PROGRAM_STARTING_ADDRESS + self.program.data.len() as u16
        {
            Some((addr - PROGRAM_STARTING_ADDRESS) as usize)
        } else {
            None
        }
    }

    fn index_addr(&self, index: usize) -> u16 {
        PROGRAM_STARTING_ADDRESS + index as u16
    }

    fn eval(&mut self, state: DisassemblyPath) -> bool {
        if let Some(Some(instruction)) = self.instructions.get(state.index) {
            let tag_old = self.tags[state.index];
            if tag_old >= state.tag {
                log::trace!(
                    "path {:#05X?} is already good!",
                    self.index_addr(state.index)
                );
                return true;
            }

            // update tag
            self.tags[state.index] = state.tag;

            log::trace!("traversing path {:#05X?}", self.index_addr(state.index));

            // traverse
            let path_is_valid = match *instruction {
                Instruction::Jump(addr) => {
                    if let Some(index) = self.addr_index(addr) {
                        self.eval(state.fork(index))
                    } else {
                        if state.tag == InstructionTag::Proven {
                            log::error!("Attempt to jump to instruction {:#05X}", addr);
                        }
                        false
                    }
                }

                // mark reachable jumps & then traverse the path of each reachable jump with the likely tag
                // should stick if the path intersects a similar or better path
                // (this instruction is what is singe-handedly making this disassembler nontrivial)
                Instruction::JumpWithOffset(addr, _) => {
                    let maybe_lbound = self.addr_index(addr);
                    let maybe_rbound = self.addr_index(addr + 255);

                    if maybe_lbound.or(maybe_rbound).is_some() {
                        log::info!(
                            "Jump with offset from {:#05X} to {:#05X} + [0, 256) found",
                            self.index_addr(state.index),
                            addr
                        );

                        let lbound = maybe_lbound.unwrap_or(0);
                        let rbound = maybe_rbound.unwrap_or(self.tags.len().saturating_sub(1));
                        for kind in self.tags[lbound..=rbound].iter_mut() {
                            if *kind == InstructionTag::Parsable {
                                *kind = InstructionTag::Reachable;
                            }
                        }

                        for i in lbound..=rbound {
                            self.eval(state.fork(i).tag(InstructionTag::Likely));
                        }

                        true
                    } else {
                        if state.tag == InstructionTag::Proven {
                            log::error!("Attempt to jump from {:#05X} to {:#05X} + [0, 256)", self.index_addr(state.index), addr);
                        } else {
                            log::warn!("Potential attempt to jump from {:#05X} to {:#05X} + [0, 256)", self.index_addr(state.index), addr);
                        }
                        false
                    }
                }

                Instruction::CallSubroutine(addr) => {
                    if let Some(index) = self.addr_index(addr) {
                        self.eval(state.fork(index).subroutine())
                            && self.eval(state.fork(state.index + 2))
                    } else {
                        if state.tag == InstructionTag::Proven {
                            log::error!("Attempt to call subroutine {:#05X}", addr);
                        }
                        false
                    }
                }

                Instruction::SubroutineReturn => {
                    if state.depth > 0 {
                        true
                    } else {
                        if state.tag == InstructionTag::Proven {
                            log::error!(
                                "Attempted return at {:#05X} when stack is empty", 
                                self.index_addr(state.index)
                            );
                        }
                        false
                    }
                },
                Instruction::SkipIfEqualsConstant(_, _)
                | Instruction::SkipIfNotEqualsConstant(_, _)
                | Instruction::SkipIfEquals(_, _)
                | Instruction::SkipIfNotEquals(_, _)
                | Instruction::SkipIfKeyDown(_)
                | Instruction::SkipIfKeyNotDown(_) => {
                    // evaluate branches (after this match statement the normal branch is evaluated)
                    // one quirk i notice is that only 1 of the skip branches must be valid for the branch to be valid
                    // this seems sensible to me for disassembly but i could be wrong (im probably wrong)
                    let false_path_is_valid = self.eval(state.fork(state.index + 2));
                    let true_path_is_valid = self.eval(state.fork(state.index + 4));
                    false_path_is_valid || true_path_is_valid
                }

                // if we made it this far then the instruction doesn't introduce a discontinuity so evaluate the next
                _ => self.eval(state.fork(state.index + 2)),
            };

            log::trace!(
                "path {:#05X?} is being marked as {}, bubbling up!",
                self.index_addr(state.index),
                if path_is_valid { "good" } else { "bad" }
            );

            if !path_is_valid {
                // invalid path means we must revert
                self.tags[state.index] = tag_old;
                log::trace!("invalid path!");
                return false;
            }

            true
        } else {
            log::trace!(
                "path {:#05X?} is not parsable, bubbling!",
                self.index_addr(state.index)
            );

            if state.tag == InstructionTag::Proven {
                log::error!(
                    "Could not decode proven instruction at {:#05X} (Binary could be faulty or instruction is dynamically loaded)", 
                    self.index_addr(state.index)
                );
            }

            false
        }
    }
}

impl Display for Disassembly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let asm = self
            .instructions
            .iter()
            .map(|maybe_inst| -> Result<(String, String), std::fmt::Error> {
                let mut content = String::new();
                let mut comments = String::new();

                if let Some(inst) = maybe_inst {
                    write_inst_asm(inst, &mut content, &mut comments)?;
                }

                Ok((content, comments))
            })
            .collect::<Vec<_>>();

        let mut content_columns = asm
            .iter()
            .filter_map(|maybe_inst| maybe_inst.as_ref().ok().map(|(content, _)| content.len()))
            .max()
            .unwrap_or_default();
        content_columns += 4 - (content_columns + 2) % 4;

        for (i, params) in self.program.instruction_parameters().enumerate() {
            if self.tags[i] < InstructionTag::Proven
                && self
                    .tags
                    .get(i - 1)
                    .map_or(false, |&tag| tag >= InstructionTag::Proven)
            {
                continue;
            }

            let addr = PROGRAM_STARTING_ADDRESS as usize + i;
            let addr_symbol = match self.tags[i] {
                InstructionTag::Not => ' ',
                InstructionTag::Parsable => '?',
                InstructionTag::Reachable => '*',
                InstructionTag::Likely => 'O',
                InstructionTag::Proven => match self.instructions[i] {
                    Some(Instruction::JumpWithOffset(_, _)) => '!',
                    _ => 'X',
                },
            };

            let (content, comment) = asm[i].as_ref().map_err(|_| std::fmt::Error)?;

            write!(f, "{:#05X}: |{}| ", addr, addr_symbol)?;

            if self.tags[i] >= InstructionTag::Parsable {
                write!(f, "{:#06X}", params.bits)?;
            } else {
                write!(f, "{}", " ".repeat(6))?;
            }

            if self.tags[i] >= InstructionTag::Proven {
                if !content.is_empty() {
                    write!(f, " {}", content)?;
                }
                if !comment.is_empty() {
                    write!(
                        f,
                        "{}# {}",
                        " ".repeat(content_columns - content.len()),
                        comment
                    )?;
                }
                writeln!(f, "")?;
            } else {
                let byte0 = (params.bits >> 8) as u8;
                let byte1 = (params.bits & u8::MAX as u16) as u8;
                let mut graphic0 = String::with_capacity(16);
                let mut graphic1 = String::with_capacity(16);

                write_byte_str(byte0, 2, &mut graphic0)?;
                write_byte_str(byte1, 2, &mut graphic1)?;

                let mut offset = 0;

                if self.tags[i] >= InstructionTag::Likely && !content.is_empty() {
                    write!(f, " {}", content)?;
                    offset = content.len() + 1;
                }

                writeln!(
                    f,
                    "{}# {:08b} 2X GRAHPHIC {}",
                    " ".repeat(content_columns + 1 - offset),
                    byte0,
                    graphic0
                )?;

                // writeln!(
                //     f,
                //     "       | |{}# {:08b} 2X GRAHPHIC {}",
                //     " ".repeat(content_columns + 8),
                //     byte1,
                //     graphic1
                // )?;
            }
        }

        Ok(())
    }
}

fn write_byte_str(byte: u8, bit_width: usize, f: &mut impl std::fmt::Write) -> std::fmt::Result {
    for filled in (0..8).rev().map(|i| byte >> i & 1 == 1) {
        write!(
            f,
            "{}",
            if filled {
                "@".repeat(bit_width)
            } else {
                ".".repeat(bit_width)
            }
        )?;
    }

    Ok(())
}

fn write_inst_asm(
    inst: &Instruction,
    f: &mut impl std::fmt::Write,
    c: &mut impl std::fmt::Write,
) -> std::fmt::Result {
    match inst {
        Instruction::ClearScreen => {
            write!(f, "clear")?;
            write!(c, "clear display")
        }

        // side effect of discontinuity instructions having no comments is it highlights a clear break in execution
        Instruction::Jump(addr) => write!(f, "jump  {:#05X}", addr),
        Instruction::JumpWithOffset(addr, _) => write!(f, "jumpo {:#05X}", addr),
        Instruction::CallSubroutine(addr) => write!(f, "call  {:#05X}", addr),
        Instruction::SubroutineReturn => write!(f, "ret"),

        Instruction::SkipIfEqualsConstant(vx, value) => {
            write!(f, "seq   v{:x} {}", vx, value)?;
            write!(c, "skip next if v{:x} == {}", vx, value)
        }
        Instruction::SkipIfNotEqualsConstant(vx, value) => {
            write!(f, "sne   v{:x} {}", vx, value)?;
            write!(c, "skip next if v{:x} != {}", vx, value)
        }
        Instruction::SkipIfEquals(vx, vy) => {
            write!(f, "seq   v{:x} v{:x}", vx, vy)?;
            write!(c, "skip next if v{:x} == v{:x}", vx, vy)
        }
        Instruction::SkipIfNotEquals(vx, vy) => {
            write!(f, "sne   v{:x} v{:x}", vx, vy)?;
            write!(c, "skip next if v{:x} != v{:x}", vx, vy)
        }
        Instruction::SkipIfKeyDown(vx) => {
            write!(f, "skd   v{:x}", vx)?;
            write!(c, "skip next if v{:x} key is down", vx)
        }
        Instruction::SkipIfKeyNotDown(vx) => {
            write!(f, "sku   v{:x}", vx)?;
            write!(c, "skip next if v{:x} key is up", vx)
        }
        Instruction::GetKey(vx) => {
            write!(f, "lnkp  v{:x}", vx)?;
            write!(c, "v{:x} = load next key press", vx)
        }
        Instruction::SetConstant(vx, value) => {
            write!(f, "mov   v{:x} {}", vx, value)?;
            write!(c, "v{:x} = {}", vx, value)
        }
        Instruction::AddConstant(vx, value) => {
            write!(f, "add   v{:x} {}", vx, value)?;
            write!(c, "v{:x} += {}", vx, value)
        }
        Instruction::Set(vx, vy) => {
            write!(f, "mov   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} = v{:x}", vx, vy)
        }
        Instruction::Or(vx, vy) => {
            write!(f, "or    v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} |= v{:x}", vx, vy)
        }
        Instruction::And(vx, vy) => {
            write!(f, "and   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} &= v{:x}", vx, vy)
        }
        Instruction::Xor(vx, vy) => {
            write!(f, "xor   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} ^= v{:x}", vx, vy)
        }
        Instruction::Add(vx, vy) => {
            write!(f, "add   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} += v{:x}", vx, vy)
        }
        Instruction::Sub(vx, vy, vx_minus_vy) => {
            if *vx_minus_vy {
                write!(f, "sub   v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} -= v{:x}", vx, vy)
            } else {
                write!(f, "subn  v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} - v{:x}", vx, vy, vx)
            }
        }
        Instruction::Shift(vx, vy, right) => {
            if *right {
                write!(f, "srl   v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} >> 1", vx, vy)
            } else {
                write!(f, "sll   v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} << 1", vx, vy)
            }
        }
        Instruction::GetDelayTimer(vx) => {
            write!(f, "ldly  v{:x}", vx)?;
            write!(c, "v{:x} = delay timer", vx)
        }
        Instruction::SetDelayTimer(vx) => {
            write!(f, "sdly  v{:x}", vx)?;
            write!(c, "delay timer = v{:x}", vx)
        }
        Instruction::SetSoundTimer(vx) => {
            write!(f, "sound v{:x}", vx)?;
            write!(c, "sound timer = v{:x}", vx)
        }
        Instruction::SetIndex(value) => {
            write!(f, "iset  {:#05X}", value)?;
            write!(c, "index = {:#05X}", value)
        }
        Instruction::SetIndexToHexChar(vx) => {
            write!(f, "ichr  v{:x}", vx)?;
            write!(c, "index = sprite address of hexadecimal char v{:x}", vx)
        }
        Instruction::AddToIndex(value) => {
            write!(f, "iadd  {}", value)?;
            write!(c, "index += {} ({:#05X})", value, value)
        }
        Instruction::Load(vx) => {
            write!(f, "load  {}", vx + 1)?;
            write!(c, "load {} byte(s) into v(0..={})", vx + 1, vx)
        }
        Instruction::Store(vx) => {
            write!(f, "save  {}", vx + 1)?;
            write!(c, "save {} byte(s) from v(0..={})", vx + 1, vx)
        }
        Instruction::StoreDecimal(vx) => {
            write!(f, "sbcd  v{:x}", vx)?;
            write!(c, "save binary-coded decimal v{:x}", vx)
        }
        Instruction::GenerateRandom(vx, bound) => {
            write!(f, "rand  v{:x} {}", vx, bound)?;
            write!(c, "v{:x} = rand in range [0, {}]", vx, bound)
        }
        Instruction::Display(vx, vy, height) => {
            write!(f, "disp  v{:x} v{:x} {}", vx, vy, height)?;
            write!(
                c,
                "display {} row{} of sprite at screen pos (v{:x}, v{:x})",
                height,
                if *height == 1 { "" } else { "s" },
                vx,
                vy
            )
        }
    }
}
