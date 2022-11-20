use crate::vm::{
    interp::{Instruction, InstructionParameters, Interpreter, InterpreterMemory},
    prog::{Program, PROGRAM_MEMORY_SIZE, PROGRAM_STARTING_ADDRESS},
};

use std::fmt::{Display, Write};

const INSTRUCTION_COLUMNS: usize = 36;

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
    addr: u16,
    tag: InstructionTag,
    depth: usize,
}

impl DisassemblyPath {
    fn fork(&self, addr: u16) -> Self {
        DisassemblyPath {
            addr,
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
    instruction_params: Vec<InstructionParameters>,
    instructions: Vec<Option<Instruction>>,
    program: Program,
    memory: InterpreterMemory,
    tags: Vec<InstructionTag>,
}

impl From<Program> for Disassembly {
    fn from(program: Program) -> Self {
        let memory = Interpreter::alloc(&program);

        let mut instruction_params = Interpreter::instruction_parameters(&memory).collect::<Vec<_>>();
        let mut instructions = instruction_params
            .iter()
            .cloned()
            .map(Instruction::try_from)
            .map(Result::ok)
            .collect::<Vec<_>>();
        let mut tags = instructions
            .iter()
            .cloned()
            .map(|maybe_inst| {
                if maybe_inst.is_some() {
                    InstructionTag::Parsable
                } else {
                    InstructionTag::Not
                }
            })
            .collect::<Vec<_>>();

        // the last byte isn't processed (since instructions are 16 bits) so we must insert it manually
        instruction_params.push(InstructionParameters::from(
            (memory.last().cloned().unwrap_or_default() as u16) << 8,
        ));
        instructions.push(None);
        tags.push(InstructionTag::Not);

        let mut disass = Disassembly {
            instruction_params,
            instructions,
            program,
            memory,
            tags,
        };

        disass.eval(DisassemblyPath {
            addr: PROGRAM_STARTING_ADDRESS,
            tag: InstructionTag::Proven,
            depth: 0,
        });

        disass
    }
}

impl Disassembly {
    fn validate_jump_addr(addr: u16) -> bool {
        addr <= PROGRAM_MEMORY_SIZE - 2
    }

    fn eval(&mut self, state: DisassemblyPath) -> bool {
        if let Some(Some(instruction)) = self.instructions.get(state.addr as usize) {
            let tag_old = self.tags[state.addr as usize];
            if tag_old >= state.tag {
                log::trace!("path {:#05X?} is already good!", state.addr);
                return true;
            }

            // update tag
            self.tags[state.addr as usize] = state.tag;

            log::trace!("traversing path {:#05X?}", state.addr);

            // traverse
            let path_is_valid = match *instruction {
                Instruction::Jump(addr) => {
                    if Disassembly::validate_jump_addr(addr) {
                        self.eval(state.fork(addr))
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
                    let maybe_lbound = if Disassembly::validate_jump_addr(addr) {
                        Some(addr as usize)
                    } else {
                        None
                    };
                    let maybe_rbound = if Disassembly::validate_jump_addr(addr + 255) {
                        Some(addr as usize + 255)
                    } else {
                        None
                    };

                    if maybe_lbound.or(maybe_rbound).is_some() {
                        log::info!(
                            "Jump with offset from {:#05X} to {:#05X} + [0, 256) found",
                            state.addr,
                            addr
                        );

                        let lbound = maybe_lbound.unwrap_or(0);
                        let rbound = maybe_rbound.unwrap_or(PROGRAM_MEMORY_SIZE as usize - 2);
                        for kind in self.tags[lbound..=rbound].iter_mut() {
                            if *kind == InstructionTag::Parsable {
                                *kind = InstructionTag::Reachable;
                            }
                        }

                        for i in lbound..=rbound {
                            self.eval(state.fork(i as u16).tag(InstructionTag::Likely));
                        }

                        true
                    } else {
                        if state.tag == InstructionTag::Proven {
                            log::error!(
                                "Attempt to jump from {:#05X} to {:#05X} + [0, 256)",
                                state.addr,
                                addr
                            );
                        } else {
                            log::warn!(
                                "Potential attempt to jump from {:#05X} to {:#05X} + [0, 256)",
                                state.addr,
                                addr
                            );
                        }
                        false
                    }
                }

                Instruction::CallSubroutine(addr) => {
                    if Disassembly::validate_jump_addr(addr) {
                        self.eval(state.fork(addr).subroutine())
                            && self.eval(state.fork(state.addr + 2))
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
                                state.addr
                            );
                        }
                        false
                    }
                }

                Instruction::SkipIfEqualsConstant(_, _)
                | Instruction::SkipIfNotEqualsConstant(_, _)
                | Instruction::SkipIfEquals(_, _)
                | Instruction::SkipIfNotEquals(_, _)
                | Instruction::SkipIfKeyDown(_)
                | Instruction::SkipIfKeyNotDown(_) => {
                    // evaluate branches (after this match statement the normal branch is evaluated)
                    // one quirk i notice is that only 1 of the skip branches must be valid for the branch to be valid
                    // this seems sensible to me for disassembly but i could be wrong (im probably wrong)
                    let false_path_is_valid = self.eval(state.fork(state.addr + 2));
                    let true_path_is_valid = self.eval(state.fork(state.addr + 4));
                    false_path_is_valid || true_path_is_valid
                }

                // if we made it this far then the instruction doesn't introduce a discontinuity so evaluate the next
                _ => self.eval(state.fork(state.addr + 2)),
            };

            log::trace!(
                "path {:#05X?} is being marked as {}, bubbling up!",
                state.addr,
                if path_is_valid { "good" } else { "bad" }
            );

            if !path_is_valid {
                // invalid path means we must revert
                self.tags[state.addr as usize] = tag_old;
                log::trace!("invalid path!");
                return false;
            }

            true
        } else {
            log::trace!("path {:#05X?} is not parsable, bubbling!", state.addr);

            if state.tag == InstructionTag::Proven {
                log::error!(
                    "Could not decode proven instruction at {:#05X} (Binary could be faulty or instruction is dynamically loaded)", 
                    state.addr
                );
            }

            false
        }
    }

    fn write_addr_disass(
        &self,
        addr: u16,
        a: &mut impl std::fmt::Write,
        i: &mut impl std::fmt::Write,
        c: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        let index = addr as usize;
        let params = self.instruction_params[index];
        let instruction = self.instructions[index];
        let tag = self.tags[index];

        let addr_symbol = match tag {
            InstructionTag::Not => ' ',
            InstructionTag::Parsable => '?',
            InstructionTag::Reachable => '*',
            InstructionTag::Likely => 'O',
            InstructionTag::Proven => {
                if let Some(Instruction::JumpWithOffset(_, _)) = instruction {
                    '!'
                } else {
                    'X'
                }
            }
        };

        // address + tag symbol
        write!(a, "{:#05X}: |{}|", addr, addr_symbol)?;

        // instruction if parsable
        if tag >= InstructionTag::Parsable {
            write!(a, " {:#06X}", params.bits)?;
        }

        if let Some(instruction) = instruction.as_ref() {
            write_inst_asm(instruction, i, c)?;
        }

        Ok(())
    }
}

impl Display for Disassembly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bin_header = String::new();
        let mut asm_content = String::new();
        let mut asm_comment = String::new();

        for (addr, byte, tag) in self.tags[PROGRAM_STARTING_ADDRESS as usize..]
            .iter()
            .take(self.program.data.len())
            .enumerate()
            .map(|(i, tag)| (PROGRAM_STARTING_ADDRESS + i as u16, self.memory[PROGRAM_STARTING_ADDRESS as usize + i], *tag))
            .filter(|(addr, _, tag)| {
                *tag >= InstructionTag::Proven
                    || !self
                        .tags
                        .get(*addr as usize - 1)
                        .map_or(false, |&tag| tag >= InstructionTag::Proven)
            })
        {
            bin_header.clear();
            asm_content.clear();
            asm_comment.clear();

            self.write_addr_disass(addr, &mut bin_header, &mut asm_content, &mut asm_comment)?;

            let show_bin_comment = tag <= InstructionTag::Likely;
            let show_asm_content = tag >= InstructionTag::Likely;
            let show_asm_comment = show_asm_content && asm_comment.len() > 0;

            let mut content_length = 0;

            f.write_str(&bin_header)?;
            content_length += bin_header.len();

            if show_asm_content {
                f.write_char(' ')?;
                f.write_str(&asm_content)?;
                content_length += asm_content.len() + 1;
            }

            if show_bin_comment || show_asm_comment {
                write!(f, "{}# ", " ".repeat(INSTRUCTION_COLUMNS.saturating_sub(content_length)))?;
            }

            if show_bin_comment {
                write!(f, "{:08b} 2X GRAHPHIC ", byte)?;
                write_byte_str(f, byte, 2)?;
            } else if show_asm_comment {
                f.write_str(&asm_comment)?;
            }

            f.write_char('\n')?;
        }

        Ok(())
    }
}

pub fn  write_byte_str(
    f: &mut impl std::fmt::Write,
    byte: u8,
    bit_width: usize,
) -> std::fmt::Result {
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

pub fn write_inst_asm(
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
            write!(
                c,
                "load {} byte{} into v(0..={})",
                vx + 1,
                if *vx == 0 { "" } else { " " },
                vx
            )
        }
        Instruction::Store(vx) => {
            write!(f, "save  {}", vx + 1)?;
            write!(
                c,
                "save {} byte{} from v(0..={})",
                vx + 1,
                if *vx == 0 { "" } else { " " },
                vx
            )
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
