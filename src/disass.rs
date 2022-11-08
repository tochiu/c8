use crate::{
    interp::Instruction,
    prog::{Program, PROGRAM_MEMORY_SIZE, PROGRAM_STARTING_ADDRESS},
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
        if addr % 2 == 0
            && addr >= PROGRAM_STARTING_ADDRESS
            && addr < PROGRAM_MEMORY_SIZE
            && addr < PROGRAM_STARTING_ADDRESS + self.program.data.len() as u16
        {
            Some((addr - PROGRAM_STARTING_ADDRESS) as usize / 2)
        } else {
            None
        }
    }

    fn index_addr(&self, index: usize) -> u16 {
        PROGRAM_STARTING_ADDRESS + 2 * index as u16
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
                        false
                    }
                }

                // mark reachable jumps & then traverse the path of each reachable jump with the likely tag
                // should stick if the path intersects a similar or better path
                // (this instruction is what is singe-handedly making this disassembler nontrivial)
                Instruction::JumpWithOffset(addr, _) => {
                    if let Some(index) = self.addr_index(addr) {
                        let bound = (index + 128).min(self.tags.len());
                        for kind in self.tags[index..bound].iter_mut() {
                            if *kind == InstructionTag::Parsable {
                                *kind = InstructionTag::Reachable;
                            }
                        }

                        for i in index..bound {
                            self.eval(state.fork(i).tag(InstructionTag::Likely));
                        }

                        true
                    } else {
                        false
                    }
                }

                Instruction::CallSubroutine(addr) => {
                    if let Some(index) = self.addr_index(addr) {
                        self.eval(state.fork(index).subroutine()) && self.eval(state.fork(state.index + 1))
                    } else {
                        false
                    }
                }

                Instruction::SubroutineReturn => state.depth > 0,
                Instruction::SkipIfEqualsConstant(_, _)
                | Instruction::SkipIfNotEqualsConstant(_, _)
                | Instruction::SkipIfEquals(_, _)
                | Instruction::SkipIfNotEquals(_, _)
                | Instruction::SkipIfKeyDown(_)
                | Instruction::SkipIfKeyNotDown(_) => {
                    // evaluate branches (after this match statement the normal branch is evaluated)
                    // one quirk i notice is that only 1 of the skip branches must be valid for the branch to be valid
                    // this seems sensible to me for disassembly but i could be wrong (im probably wrong)
                    let false_path_is_valid = self.eval(state.fork(state.index + 1));
                    let true_path_is_valid = self.eval(state.fork(state.index + 2));
                    false_path_is_valid || true_path_is_valid
                }

                // if we made it this far then the instruction doesn't introduce a discontinuity
                // so evaluate the next
                _ => self.eval(state.fork(state.index + 1)),
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

            return true;
        }

        log::trace!(
            "path {:#05X?} is not parsable, bubbling!",
            self.index_addr(state.index)
        );

        return false;
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
            let addr = PROGRAM_STARTING_ADDRESS as usize + 2 * i;
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

            write!(f, "{:#05X}: |{}| {:#06X}", addr, addr_symbol, params.bits)?;

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
                writeln!(
                    f,
                    "{:#05X}: | |{}# {:08b} 2X GRAHPHIC {}",
                    addr + 1,
                    " ".repeat(content_columns + 8),
                    byte1,
                    graphic1
                )?;
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
            write!(f, "seq   $v{} {}", vx, value)?;
            write!(c, "skip next if $v{} == {}", vx, value)
        }
        Instruction::SkipIfNotEqualsConstant(vx, value) => {
            write!(f, "sne   $v{} {}", vx, value)?;
            write!(c, "skip next if $v{} != {}", vx, value)
        }
        Instruction::SkipIfEquals(vx, vy) => {
            write!(f, "seq   $v{} $v{}", vx, vy)?;
            write!(c, "skip next if $v{} == $v{}", vx, vy)
        }
        Instruction::SkipIfNotEquals(vx, vy) => {
            write!(f, "sne   $v{} $v{}", vx, vy)?;
            write!(c, "skip next if $v{} != $v{}", vx, vy)
        }
        Instruction::SkipIfKeyDown(vx) => {
            write!(f, "skd   $v{}", vx)?;
            write!(c, "skip next if $v{} key is down", vx)
        }
        Instruction::SkipIfKeyNotDown(vx) => {
            write!(f, "sku   $v{}", vx)?;
            write!(c, "skip next if $v{} key is up", vx)
        }
        Instruction::GetKey(vx) => {
            write!(f, "lnkp  $v{}", vx)?;
            write!(c, "$v{} = load next key press", vx)
        }
        Instruction::SetConstant(vx, value) => {
            write!(f, "mov   $v{} {}", vx, value)?;
            write!(c, "$v{} = {}", vx, value)
        }
        Instruction::AddConstant(vx, value) => {
            write!(f, "add   $v{} {}", vx, value)?;
            write!(c, "$v{} += {}", vx, value)
        }
        Instruction::Set(vx, vy) => {
            write!(f, "mov   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} = $v{}", vx, vy)
        }
        Instruction::Or(vx, vy) => {
            write!(f, "or    $v{} $v{}", vx, vy)?;
            write!(c, "$v{} |= $v{}", vx, vy)
        }
        Instruction::And(vx, vy) => {
            write!(f, "and   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} &= $v{}", vx, vy)
        }
        Instruction::Xor(vx, vy) => {
            write!(f, "xor   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} ^= $v{}", vx, vy)
        }
        Instruction::Add(vx, vy) => {
            write!(f, "add   $v{} $v{}", vx, vy)?;
            write!(c, "$v{} += $v{}", vx, vy)
        }
        Instruction::Sub(vx, vy, vx_minus_vy) => {
            if *vx_minus_vy {
                write!(f, "sub   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} -= $v{}", vx, vy)
            } else {
                write!(f, "subn  $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} - $v{}", vx, vy, vx)
            }
        }
        Instruction::Shift(vx, vy, right) => {
            if *right {
                write!(f, "srl   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} >> 1", vx, vy)
            } else {
                write!(f, "sll   $v{} $v{}", vx, vy)?;
                write!(c, "$v{} = $v{} << 1", vx, vy)
            }
        }
        Instruction::GetDelayTimer(vx) => {
            write!(f, "ldly  $v{}", vx)?;
            write!(c, "$v{} = delay timer", vx)
        }
        Instruction::SetDelayTimer(vx) => {
            write!(f, "sdly  $v{}", vx)?;
            write!(c, "delay timer = $v{}", vx)
        }
        Instruction::SetSoundTimer(vx) => {
            write!(f, "sound $v{}", vx)?;
            write!(c, "sound timer = $v{}", vx)
        }
        Instruction::SetIndex(value) => {
            write!(f, "iset  {:#05X}", value)?;
            write!(c, "index = {:#05X}", value)
        }
        Instruction::SetIndexToHexChar(vx) => {
            write!(f, "ichr  $v{}", vx)?;
            write!(c, "index = sprite address of hexadecimal char $v{}", vx)
        }
        Instruction::AddToIndex(value) => {
            write!(f, "iadd  {}", value)?;
            write!(c, "index += {} ({:#05X})", value, value)
        }
        Instruction::Load(vx) => {
            write!(f, "load  {}", vx + 1)?;
            write!(c, "load {} byte(s) into $v(0..={})", vx + 1, vx)
        }
        Instruction::Store(vx) => {
            write!(f, "save  {}", vx + 1)?;
            write!(c, "save {} byte(s) from $v(0..={})", vx + 1, vx)
        }
        Instruction::StoreDecimal(vx) => {
            write!(f, "sbcd  $v{}", vx)?;
            write!(c, "save binary-coded decimal $v{}", vx)
        }
        Instruction::GenerateRandom(vx, bound) => {
            write!(f, "rand  $v{} {}", vx, bound)?;
            write!(c, "$v{} = rand in range [0, {}]", vx, bound)
        }
        Instruction::Display(vx, vy, height) => {
            write!(f, "disp  $v{} $v{} {}", vx, vy, height)?;
            write!(
                c,
                "display {} row{} of sprite at screen pos ($v{}, $v{})",
                height,
                if *height == 1 { "" } else { "s" },
                vx,
                vy
            )
        }
    }
}
