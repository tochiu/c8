use crate::run::{
    interp::{
        Instruction, InstructionParameters, Interpreter,
        PROGRAM_STARTING_ADDRESS, PROGRAM_MEMORY_SIZE, MemoryRef,
    },
    rom::{Rom, RomKind},
};

use std::{
    fmt::{Display, Write},
    time::Instant,
};

pub const INSTRUCTION_COLUMNS: usize = 30;
pub const INSTRUCTION_MAX_LENGTH: usize = 13;
pub const ADDRESS_COMMENT_TOKEN: &'static str = "#";

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum InstructionTag {
    Not,       // Instruction that cannot be parsed
    Parsable,  // Instruction that can be parsed
    Reachable, // Parsable instruction that is reachable by at least one execution path
    Valid, // Reachable instruction whose subsequent execution contains at least one path that leads to a valid or better instruction
    Proven, // Valid instruction that can be reached by at least one static execution path (path w/o jump with offset)
}

impl InstructionTag {
    fn to_symbol(self) -> char {
        match self {
            InstructionTag::Not => ' ',
            InstructionTag::Parsable => '?',
            InstructionTag::Reachable => '*',
            InstructionTag::Valid => 'O',
            InstructionTag::Proven => 'X',
        }
    }
}

pub struct DisassemblyPath<'a> {
    addr: u16,
    addr_max: u16,
    tag: InstructionTag,
    depth: u8,
    trace: &'a mut Vec<TraceEntry>,
    trace_pushed: bool,
}

impl<'a> DisassemblyPath<'_> {
    fn fork(&'a mut self, addr: u16, offset: u16) -> DisassemblyPath<'a> {
        DisassemblyPath {
            addr: addr.overflowing_add(offset).0 & self.addr_max,
            addr_max: self.addr_max,
            tag: self.tag,
            depth: self.depth,
            trace: self.trace,
            trace_pushed: false,
        }
    }

    fn fork_offset(&'a mut self, offset: u16) -> DisassemblyPath<'a> {
        self.fork(self.addr, offset)
    }

    fn tag(mut self, tag: InstructionTag) -> Self {
        self.tag = tag;
        self
    }

    fn subroutine(mut self) -> Self {
        self.depth += 1;
        self
    }

    fn save_trace<S: Into<String>>(&self, disasm: &mut Disassembler, message: S) {
        if self.tag < InstructionTag::Proven {
            return;
        }

        disasm.traces.push(Trace {
            tag: self.tag,
            message: message.into(),
            entries: self.trace.clone(),
        });
    }

    fn enter_trace(&mut self, instruction: Option<Instruction>, payload: Option<u8>) {
        self.trace_pushed = true;
        self.trace.push(TraceEntry {
            addr: self.addr,
            instruction,
            payload,
        });
    }

    fn leave_trace(&mut self) {
        if self.trace_pushed {
            self.trace.pop();
            self.trace_pushed = false;
        }
    }
}

pub struct Disassembler {
    pub instruction_params: Vec<InstructionParameters>,
    pub instructions: Vec<Option<Instruction>>,
    pub rom: Rom,
    pub memory: [u8; PROGRAM_MEMORY_SIZE as usize],
    pub tags: Vec<InstructionTag>,
    pub traces: Vec<Trace>,
}

pub struct Trace {
    tag: InstructionTag,
    message: String,
    entries: Vec<TraceEntry>,
}

#[derive(Clone)]
struct TraceEntry {
    addr: u16,
    instruction: Option<Instruction>,
    payload: Option<u8>,
}

impl From<Rom> for Disassembler {
    fn from(rom: Rom) -> Self {
        let memory = Interpreter::alloc(&rom);

        let instruction_params =
            Interpreter::instruction_parameters(&memory).collect::<Vec<_>>();
        let instructions = instruction_params
            .iter()
            .map(|params| params.try_decode(rom.config.kind).ok())
            .collect::<Vec<_>>();
        let tags = instructions
            .iter()
            .map(|maybe_inst| {
                if maybe_inst.is_some() {
                    InstructionTag::Parsable
                } else {
                    InstructionTag::Not
                }
            })
            .collect::<Vec<_>>();

        Disassembler {
            instruction_params,
            instructions,
            rom,
            memory,
            tags,
            traces: Vec::new(),
        }
    }
}

impl Disassembler {
    // it is assumed that bytes > 0 and bytes <= 16 
    pub fn update(&mut self, interp: &Interpreter, index: u16, bytes: u16) {
        let memory = interp.memory;
        let mut disasm_required = false;

        let mut instruction_bytes = [0; 2];

        for i in 0..bytes {
            let addr = memory.add_addresses(index, i);
            memory.export(addr, &mut instruction_bytes);

            let instruction_param_bits = ((instruction_bytes[0] as u16) << 8) | instruction_bytes[1] as u16;
            if instruction_param_bits == self.instruction_params[addr as usize].bits {
                continue;
            }

            log::debug!("Updating instruction at {:#05X} ({:04X})", addr, instruction_param_bits);

            let instruction_params = InstructionParameters::from(instruction_param_bits);
            let instruction = instruction_params.try_decode(interp.rom.config.kind).ok();
            
            let old_tag = self.tags[addr as usize];
            
            disasm_required = disasm_required
                || old_tag > InstructionTag::Parsable
                || old_tag < InstructionTag::Parsable && instruction.is_some();
            
            self.memory[addr as usize] = instruction_bytes[0];
            self.instruction_params[addr as usize] = instruction_params;
            self.instructions[addr as usize] = instruction;
            self.tags[addr as usize] = if instruction.is_some() {
                InstructionTag::Parsable
            } else {
                InstructionTag::Not
            };
        }

        if disasm_required {
            // reset tags that are >=parsable back to parsable before rerunning disassembler
            self.tags.iter_mut().for_each(|tag| *tag = (*tag).min(InstructionTag::Parsable));
            self.traces.clear();
            self.run();
        }
    }

    pub fn run(&mut self) {
        self.run_from(PROGRAM_STARTING_ADDRESS, InstructionTag::Proven, 0);
    }

    pub fn run_from(&mut self, addr: u16, tag: InstructionTag, depth: u8) {
        log::info!(
            "Disassembling \"{}\" starting at {:#05X}",
            &self.rom.config.name,
            addr
        );
        let mut trace_buffer = Vec::new();
        let now = Instant::now();
        self.eval(DisassemblyPath {
            addr,
            addr_max: (self.memory.len() - 1) as u16,
            tag,
            depth,
            trace: &mut trace_buffer,
            trace_pushed: false,
        });
        let elapsed = now.elapsed().as_micros();
        log::info!(
            "Disassembled \"{}\" in {} us",
            &self.rom.config.name,
            elapsed
        );
    }

    fn eval(&mut self, mut path: DisassemblyPath) -> bool {
        let Some(instruction) = self.instructions[path.addr as usize] else {

            // Early exit if we are not able to decode the instruction

            path.enter_trace(None, None);
            path.save_trace(self, "Unable to decode instruction");
            path.leave_trace();

            log::trace!("path {:#05X?} is not parsable, backtracking!", path.addr);

            if path.tag == InstructionTag::Proven {
                log::error!(
                    "Could not decode proven instruction at {:#05X} (Binary could be faulty or instruction is dynamically loaded)", 
                    path.addr
                );
            }

            return false
        };

        // check if path is already validated with a better tag or if it is already invalid
        
        let current_tag = self.tags[path.addr as usize];
        if current_tag >= path.tag {
            log::trace!("path {:#05X?} is already good, backtracking!", path.addr);
            return true;
        } else if current_tag == InstructionTag::Reachable {
            // reachable tag => instruction is reachable but path is invalid
            log::trace!("path {:#05X?} is already invalid, backtracking!", path.addr);
            return false;
        }

        // update tag
        self.tags[path.addr as usize] = path.tag;

        log::trace!("traversing path {:#05X?}", path.addr);

        // traverse

        let path_is_valid = match instruction {
            Instruction::Exit => true,

            Instruction::Jump(addr) => {
                path.enter_trace(Some(instruction), None);
                let path_is_valid = self.eval(path.fork(addr, 0));
                path.leave_trace();
                path_is_valid
            }

            // mark reachable jumps & then traverse the path of each reachable jump with the likely tag
            // should stick if the path intersects a similar or better path
            // (this instruction is what is singe-handedly making this disassembler nontrivial)
            Instruction::JumpWithOffset(addr, _) => {
                log::debug!(
                    "Jumping from {:#05X} to {:#05X} + [0, 256)",
                    path.addr,
                    addr
                );

                let mut valid_jump_exists = false;

                for i in 0..=255 {
                    path.enter_trace(Some(instruction), Some(i));
                    let is_valid_jump = self.eval(path.fork(addr, i as u16).tag(InstructionTag::Valid));
                    valid_jump_exists = valid_jump_exists || is_valid_jump;
                    path.leave_trace();
                }

                if !valid_jump_exists {
                    path.enter_trace(Some(instruction), None);
                    path.save_trace(self, "Unable to evaluate at least one jump path as valid");
                    path.leave_trace();

                    if path.tag == InstructionTag::Proven {
                        log::error!(
                            "Unable to evaluate at least one jump path as valid from {:#05X}",
                            path.addr
                        );
                    }
                }

                valid_jump_exists
            }

            Instruction::CallSubroutine(addr) => {
                // i would check that path.addr + 2 is a valid address but technically the subroutine could never return
                // so it isn't certain that we traverse the next instruction, valid or not
                path.enter_trace(Some(instruction), None);
                let is_call_path_valid = self.eval(path.fork(addr, 0).subroutine());
                path.leave_trace();

                if is_call_path_valid {
                    // if the call path is valid, then the next instruction can be evaluated
                    self.eval(path.fork_offset(2));
                }

                is_call_path_valid
            }

            // TODO: return needs to do an explicit jump
            Instruction::SubroutineReturn => {
                if path.depth > 0 {
                    true
                } else {
                    path.enter_trace(Some(instruction), None);
                    path.save_trace(self, "Cannot return with empty call stack");
                    path.leave_trace();
                    if path.tag == InstructionTag::Proven {
                        log::error!(
                            "Attempted return at {:#05X} when stack is empty",
                            path.addr
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
                //path.enter_trace(Some(instruction), Some(0));
                let false_path_is_valid = self.eval(path.fork_offset(2));
                //path.leave_trace();

                path.enter_trace(Some(instruction), None);
                let true_path_is_valid = self.eval(path.fork_offset(4));
                path.leave_trace();

                false_path_is_valid || true_path_is_valid
            }

            // if we made it this far then the instruction doesn't introduce a discontinuity so evaluate the next
            _ => self.eval(path.fork_offset(2)),
        };

        log::trace!(
            "path {:#05X?} is being marked as {}, backtracking!",
            path.addr,
            if path_is_valid { "good" } else { "bad" }
        );

        if !path_is_valid {
            if path.tag < InstructionTag::Proven {
                self.tags[path.addr as usize] = InstructionTag::Reachable;
            }
            false
        } else {
            true
        }
    }

    pub fn write_addr_disasm(
        &self,
        addr: u16,
        addr_header: &mut impl std::fmt::Write,
        addr_bin: &mut impl std::fmt::Write,
        addr_asm: &mut impl std::fmt::Write,
        addr_asm_desc: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        let index = addr as usize;
        let params = self.instruction_params[index];
        let tag = self.tags[index];

        // address
        write!(addr_header, "{:#05X}", addr)?;

        // instruction tag symbol
        write!(addr_bin, " {}", tag.to_symbol())?;

        // instruction if parsable
        if tag >= InstructionTag::Parsable {
            write!(addr_bin, " {:04X}", params.bits)?;
        } else {
            write!(addr_bin, " {:02X}", self.memory[index])?;
        }

        if let Some(instruction) = self.instructions[index].as_ref() {
            write_inst_asm(instruction, self.rom.config.kind, addr_asm, addr_asm_desc)?;
        }

        Ok(())
    }

    pub fn write_issue_traces(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        writeln!(
            f,
            "FOUND {} ISSUE{}\n",
            self.traces.len(),
            if self.traces.len() == 1 { "" } else { "S" }
        )?;
        for trace in self.traces.iter() {
            let is_error = trace.tag == InstructionTag::Proven;

            let mut asm = String::new();
            let mut asm_desc = String::new();
            // TODO: fix this
            writeln!(
                f,
                "{} {}",
                if is_error { "ERROR:" } else { "WARNING:" },
                &trace.message
            )?;
            for entry in trace.entries.iter().rev() {
                write!(f, "  at {:#05X}", entry.addr)?;
                if let Some(inst) = entry.instruction.as_ref() {
                    match inst {
                        Instruction::Jump(addr) => {
                            write!(f, " JUMP {:#05X}", addr)?;
                        }
                        Instruction::JumpWithOffset(addr, _) => {
                            write!(f, " JUMP {:#05X}", addr)?;
                            if let Some(offset) = entry.payload {
                                write!(f, " (offset = {})", offset)?;
                            } else {
                                write!(f, " (offset = all)")?;
                            }
                        }

                        Instruction::CallSubroutine(addr) => {
                            write!(f, " CALL {:#05X}", addr)?;
                        }

                        Instruction::SkipIfEqualsConstant(_, _)
                        | Instruction::SkipIfNotEqualsConstant(_, _)
                        | Instruction::SkipIfEquals(_, _)
                        | Instruction::SkipIfNotEquals(_, _)
                        | Instruction::SkipIfKeyDown(_)
                        | Instruction::SkipIfKeyNotDown(_) => {
                            write!(f, " SKIP {:#05X}", entry.addr + 2)?
                        }
                        _ => {
                            asm.clear();
                            asm_desc.clear();
                            write_inst_asm(inst, self.rom.config.kind, &mut asm, &mut asm_desc)
                                .expect("Writing instruction to string failed");
                            write!(f, " {}", &asm)?;
                            if asm_desc.len() > 0 {
                                write!(f, " {} {}", ADDRESS_COMMENT_TOKEN, &asm_desc)?;
                            } else {
                            }
                        }
                    }
                }

                writeln!(f, "")?;
            }
        }

        Ok(())
    }
}

impl Display for Disassembler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut bin_header = String::new();
        let mut bin_opcode = String::new();
        let mut asm_content = String::new();
        let mut asm_comment = String::new();

        for (addr, byte, tag) in self.tags[PROGRAM_STARTING_ADDRESS as usize..]
            .iter()
            .take(self.rom.data.len())
            .enumerate()
            .map(|(i, tag)| {
                (
                    PROGRAM_STARTING_ADDRESS + i as u16,
                    self.memory[PROGRAM_STARTING_ADDRESS as usize + i],
                    *tag,
                )
            })
            .filter(|(addr, _, tag)| {
                *tag >= InstructionTag::Proven
                    || !self
                        .tags
                        .get(*addr as usize - 1)
                        .map_or(false, |&tag| tag >= InstructionTag::Proven)
            })
        {
            bin_header.clear();
            bin_opcode.clear();
            asm_content.clear();
            asm_comment.clear();

            self.write_addr_disasm(
                addr,
                &mut bin_header,
                &mut bin_opcode,
                &mut asm_content,
                &mut asm_comment,
            )?;

            let show_bin_comment = tag <= InstructionTag::Valid;
            let show_asm_content = tag >= InstructionTag::Valid;
            let show_asm_comment = show_asm_content && asm_comment.len() > 0;

            let mut content_length = 0;

            f.write_str(&bin_header)?;
            f.write_str(&bin_opcode)?;
            content_length += bin_header.len() + bin_opcode.len();

            if show_asm_content {
                f.write_char(' ')?;
                f.write_str(&asm_content)?;
                content_length += asm_content.len() + 1;
            }

            if show_bin_comment || show_asm_comment {
                write!(
                    f,
                    "{}{} ",
                    " ".repeat(INSTRUCTION_COLUMNS.saturating_sub(content_length)),
                    ADDRESS_COMMENT_TOKEN
                )?;
            }

            if show_bin_comment {
                write!(f, "2X GRAPHIC ")?;
                write_byte_str(f, byte, 2)?;
            } else if show_asm_comment {
                f.write_str(&asm_comment)?;
            }

            f.write_char('\n')?;
        }

        Ok(())
    }
}

pub fn write_byte_str(
    f: &mut impl std::fmt::Write,
    byte: u8,
    bit_width: usize,
) -> std::fmt::Result {
    for filled in (0..8).rev().map(|i| byte >> i & 1 == 1) {
        if filled {
            for _ in 0..bit_width {
                f.write_char('@')?;
            }
        } else {
            for _ in 0..bit_width {
                f.write_char('.')?;
            }
        }
    }

    Ok(())
}

pub fn write_inst_asm(
    inst: &Instruction,
    kind: RomKind,
    f: &mut impl std::fmt::Write,
    c: &mut impl std::fmt::Write,
) -> std::fmt::Result {
    match inst {
        // side effect of discontinuity instructions having no comments is it highlights a clear break in execution
        Instruction::Exit => write!(f, "exit"),
        Instruction::Jump(addr) => write!(f, "jp   {:#05X}", addr),
        Instruction::JumpWithOffset(addr, x) => write!(
            f, 
            "jp   v{:x} {:#05X}", 
            if kind == RomKind::SCHIP { 
                *x 
            } else { 
                0 
            }, 
            addr
        ),
        Instruction::CallSubroutine(addr) => write!(f, "call {:#05X}", addr),
        Instruction::SubroutineReturn => write!(f, "ret"),

        Instruction::SkipIfEqualsConstant(vx, value) => {
            write!(f, "se   v{:x} {}", vx, value)?;
            write!(c, "skip if v{:x} == {}", vx, value)
        }
        Instruction::SkipIfNotEqualsConstant(vx, value) => {
            write!(f, "sne  v{:x} {}", vx, value)?;
            write!(c, "skip if v{:x} != {}", vx, value)
        }
        Instruction::SkipIfEquals(vx, vy) => {
            write!(f, "se   v{:x} v{:x}", vx, vy)?;
            write!(c, "skip if v{:x} == v{:x}", vx, vy)
        }
        Instruction::SkipIfNotEquals(vx, vy) => {
            write!(f, "sne  v{:x} v{:x}", vx, vy)?;
            write!(c, "skip if v{:x} != v{:x}", vx, vy)
        }
        Instruction::SkipIfKeyDown(vx) => {
            write!(f, "skp  v{:x}", vx)?;
            write!(c, "skip if v{:x} keydown", vx)
        }
        Instruction::SkipIfKeyNotDown(vx) => {
            write!(f, "sknp v{:x}", vx)?;
            write!(c, "skip if v{:x} keyup", vx)
        }
        Instruction::GetKey(vx) => {
            write!(f, "ld   v{:x} k", vx)?;
            write!(c, "v{:x} = next keypress", vx)
        }
        Instruction::SetConstant(vx, value) => {
            write!(f, "ld   v{:x} {}", vx, value)?;
            write!(c, "v{:x} = {}", vx, value)
        }
        Instruction::AddConstant(vx, value) => {
            write!(f, "add  v{:x} {}", vx, value)?;
            write!(c, "v{:x} += {}", vx, value)
        }
        Instruction::Set(vx, vy) => {
            write!(f, "ld   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} = v{:x}", vx, vy)
        }
        Instruction::Or(vx, vy) => {
            write!(f, "or   v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} |= v{:x}", vx, vy)
        }
        Instruction::And(vx, vy) => {
            write!(f, "and  v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} &= v{:x}", vx, vy)
        }
        Instruction::Xor(vx, vy) => {
            write!(f, "xor  v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} ^= v{:x}", vx, vy)
        }
        Instruction::Add(vx, vy) => {
            write!(f, "add  v{:x} v{:x}", vx, vy)?;
            write!(c, "v{:x} += v{:x}", vx, vy)
        }
        Instruction::Sub(vx, vy, vx_minus_vy) => {
            if *vx_minus_vy {
                write!(f, "sub  v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} -= v{:x}", vx, vy)
            } else {
                write!(f, "subn v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} - v{:x}", vx, vy, vx)
            }
        }
        Instruction::Shift(vx, vy, right) => {
            if *right {
                write!(f, "shr  v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} >> 1", vx, vy)
            } else {
                write!(f, "shl  v{:x} v{:x}", vx, vy)?;
                write!(c, "v{:x} = v{:x} << 1", vx, vy)
            }
        }
        Instruction::GetDelayTimer(vx) => {
            write!(f, "ld   v{:x} dt", vx)?;
            write!(c, "v{:x} = delay timer", vx)
        }
        Instruction::SetDelayTimer(vx) => {
            write!(f, "ld   dt v{:x}", vx)?;
            write!(c, "delay timer = v{:x}", vx)
        }
        Instruction::SetSoundTimer(vx) => {
            write!(f, "ld   st v{:x}", vx)?;
            write!(c, "sound timer = v{:x}", vx)
        }
        Instruction::SetIndex(value) => {
            write!(f, "ld   i {:#05X}", value)?;
            write!(c, "i = {:#05X}", value)
        }
        Instruction::SetIndexToHexChar(vx) => {
            write!(f, "ld   f v{:x}", vx)?;
            write!(c, "i = hex v{:x}", vx)
        }
        Instruction::SetIndexToBigHexChar(vx) => {
            write!(f, "ld   hf v{:x}", vx)?;
            write!(c, "i = big hex v{:x}", vx)
        }
        Instruction::AddToIndex(value) => {
            write!(f, "add  i {}", value)?;
            write!(c, "i += {} ({:#05X})", value, value)
        }
        Instruction::Load(vx) => {
            write!(f, "ld   v{:x} i", vx)?;
            write!(c, "v0..=v{:x} = memory", vx)
        }
        Instruction::Store(vx) => {
            write!(f, "ld   i v{:x}", vx)?;
            write!(c, "memory = v0..=v{:x}", vx)
        }
        Instruction::LoadFlags(vx) => {
            write!(f, "ld   v{:x} r", vx)?;
            write!(c, "v0..=v{:x} = flags", vx)
        }
        Instruction::StoreFlags(vx) => {
            write!(f, "ld   r v{:x}", vx)?;
            write!(c, "flags = v0..=v{:x}", vx)
        }
        Instruction::StoreDecimal(vx) => {
            write!(f, "ld   b v{:x}", vx)?;
            write!(c, "save dec digits from v{:x}", vx)
        }
        Instruction::GenerateRandom(vx, bound) => {
            write!(f, "rnd  v{:x} {}", vx, bound)?;
            write!(c, "v{:x} = rand 0..={}", vx, bound)
        }
        Instruction::Display(vx, vy, height) => {
            write!(f, "drw  v{:x} v{:x} {}", vx, vy, height)?;
            if *height == 0 && kind == RomKind::SCHIP {
                write!(c, "draw 16x16 @ v{:x},v{:x}", vx, vy)
            } else {
                write!(c, "draw 8x{} @ v{:x},v{:x}", height, vx, vy)
            }
        }
        Instruction::ScrollDown(n) => {
            write!(f, "scd")?;
            write!(c, "scroll {} down", n)
        }
        Instruction::ScrollLeft => {
            write!(f, "scl")?;
            write!(c, "scroll left")
        }
        Instruction::ScrollRight => {
            write!(f, "scr")?;
            write!(c, "scroll right")
        }
        Instruction::LowResolution => {
            write!(f, "low")?;
            write!(c, "use lo-res")
        }
        Instruction::HighResolution => {
            write!(f, "high")?;
            write!(c, "use hi-res")
        }
        Instruction::ClearScreen => {
            write!(f, "cls")?;
            write!(c, "clear")
        }
    }
}
