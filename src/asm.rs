use crate::ch8::{
    instruct::{Instruction, InstructionParameters},
    interp::{Interpreter, PROGRAM_STARTING_ADDRESS},
    mem::{allocate_memory, MemoryRef},
    rom::{Rom, RomKind},
};

use std::{
    cell::Cell,
    fmt::{Display, Write},
    time::Instant,
};

pub const INSTRUCTION_COLUMNS: usize = 29;
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
    pub memory: Vec<u8>,
    pub tags: Vec<InstructionTag>,
    pub traces: Vec<Trace>,

    pub address_formatter: Cell<AddressFormatter>,
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
        let memory = allocate_memory(&rom);

        let instruction_params = memory.instruction_parameters().collect::<Vec<_>>();
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
            address_formatter: Default::default(),
        }
    }
}

impl Disassembler {
    pub fn rerun(&mut self) {
        self.tags
            .iter_mut()
            .for_each(|tag| *tag = (*tag).min(InstructionTag::Parsable));
        self.traces.clear();
        self.run();
    }

    // it is assumed that bytes > 0 and bytes <= 16
    pub fn needs_rerun(&mut self, interp: &Interpreter, mut index: u16, mut bytes: u16) -> bool {
        let memory = &interp.memory;
        let mut disasm_required = false;

        let mut instruction_bytes = [0; Instruction::MAX_INSTRUCTION_SIZE as usize];

        index = memory.address_sub(index, Instruction::MAX_INSTRUCTION_SIZE - 1);
        bytes += Instruction::MAX_INSTRUCTION_SIZE - 1;

        for i in 0..bytes {
            let addr = memory.address_add(index, i);
            memory.export(addr, &mut instruction_bytes);

            let instruction_param_bits = u32::from_be_bytes(instruction_bytes);
            if instruction_param_bits == self.instruction_params[addr as usize].bits {
                continue;
            }

            let instruction_params = InstructionParameters::new(instruction_param_bits);
            let instruction = instruction_params.try_decode(interp.rom.config.kind).ok();

            log::debug!(
                "Updating instruction at {:#05X} ({:04X})",
                addr,
                instruction_params
                    .significant_bytes(Instruction::size_or_default(&instruction))
            );

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

        disasm_required
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
            path.save_trace(self, format!("Unable to decode instruction \"{:04X}\"", self.instruction_params[path.addr as usize].default_significant_bytes()));
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
                    let is_valid_jump =
                        self.eval(path.fork(addr, i as u16).tag(InstructionTag::Valid));
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
                // i would check that the return address is a valid address but technically the subroutine could never return
                // so it isn't certain that we traverse the next instruction, valid or not
                path.enter_trace(Some(instruction), None);
                let is_call_path_valid = self.eval(path.fork(addr, 0).subroutine());
                path.leave_trace();

                if is_call_path_valid {
                    // if the call path is valid, then the next instruction can be evaluated
                    self.eval(path.fork_offset(instruction.size()));
                }

                is_call_path_valid
            }

            Instruction::SubroutineReturn => {
                if path.depth > 0 {
                    true
                } else {
                    path.enter_trace(Some(instruction), None);
                    path.save_trace(self, "Cannot return with empty call stack");
                    path.leave_trace();
                    if path.tag == InstructionTag::Proven {
                        log::error!("Attempted return at {:#05X} when stack is empty", path.addr);
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

                let false_path = path.fork_offset(instruction.size());
                let false_path_instruction_size =
                    Instruction::size_or_default(&self.instructions[false_path.addr as usize]);
                let false_path_is_valid = self.eval(false_path);

                path.enter_trace(Some(instruction), None);
                let true_path_is_valid = self.eval(
                    path.fork_offset(instruction.size() + false_path_instruction_size),
                );
                path.leave_trace();

                false_path_is_valid || true_path_is_valid
            }

            // if we made it this far then the instruction doesn't introduce a discontinuity so evaluate the next
            _ => self.eval(path.fork_offset(instruction.size())),
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

    pub fn write_addr_dasm(&self, addr: u16) -> std::fmt::Result {
        let kind = self.rom.config.kind;
        let index = addr as usize;
        let params = self.instruction_params[index];
        let tag = self.tags[index];
        let byte = self.memory[index];
        let instruction = self.instructions[index];

        let mut f = self.address_formatter.take();
        f.clear();

        // address
        if kind == RomKind::XOCHIP {
            write!(f.header, "{:#06X}", addr)?;
        } else {
            write!(f.header, "{:#05X}", addr)?;
        }

        // instruction tag symbol
        write!(f.tag, " {}", tag.to_symbol())?;

        // instruction if parsable
        if tag >= InstructionTag::Parsable {
            let bytes = Instruction::size_or_default(&instruction);
            write!(
                f.opcode,
                "{:0width$X}",
                params.significant_bytes(bytes),
                width = 2 * bytes as usize
            )?;
        } else {
            write!(f.opcode, "{:02X}  ", self.memory[index])?;
        }

        // Byte at this address
        write_byte_str(&mut f.bin, byte, 1).ok();

        if let Some(instruction) = instruction.as_ref() {
            write_inst_dasm(
                instruction,
                self.rom.config.kind,
                &mut f.asm,
                &mut f.asm_desc,
            )?;
        }

        self.address_formatter.set(f);

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
                        | Instruction::SkipIfKeyNotDown(_) => write!(
                            f,
                            " SKIP {:#05X}",
                            self.memory.address_add(entry.addr, inst.size())
                        )?,
                        _ => {
                            asm.clear();
                            asm_desc.clear();
                            write_inst_dasm(inst, self.rom.config.kind, &mut asm, &mut asm_desc)
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

    pub fn is_address_overlapping_instruction_tag(&self, address: u16, tag: InstructionTag) -> bool {
        for address_diff in 1..Instruction::MAX_INSTRUCTION_SIZE {
            let prior_address = self.memory.address_sub(address, address_diff) as usize;
            if self.tags[prior_address] >= tag 
                && Instruction::size_or_default(&self.instructions[prior_address]) > address_diff 
            {
                return true;
            }
        }

        false
    }
}

impl Display for Disassembler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                    || !self.is_address_overlapping_instruction_tag(*addr, InstructionTag::Proven)
            })
        {

            self.write_addr_dasm(addr)?;

            let address_formatter = self.address_formatter.take();

            let show_bin_comment = tag <= InstructionTag::Valid;
            let show_asm_content = tag >= InstructionTag::Valid;

            // TODO: remove show_asm_comment since it always runs
            //let show_asm_comment = show_asm_content; // && address_formatter.asm_desc.len() > 0;

            let mut content_length = 0;

            f.write_str(&address_formatter.header)?;
            // f.write_str(&address_formatter.opcode)?;
            content_length += address_formatter.header.len(); // + bin_opcode.len();

            if show_asm_content {
                f.write_char(' ')?;
                f.write_str(&address_formatter.asm)?;
                content_length += address_formatter.asm.len() + 1;
            }

            write!(
                f,
                "{}{}",
                " ".repeat(INSTRUCTION_COLUMNS.saturating_sub(content_length)),
                ADDRESS_COMMENT_TOKEN
            )?;

            f.write_str(&address_formatter.tag)?;
            f.write_char(' ')?;
            f.write_str(&address_formatter.opcode)?;
            write!(f, "{}", " ".repeat(2*Instruction::MAX_INSTRUCTION_SIZE as usize - address_formatter.opcode.len()))?;

            if show_bin_comment {
                write!(f, " 2X GRAPHIC ")?;
                write_byte_str(f, byte, 2)?;
            } else {
                f.write_char(' ')?;
                f.write_str(&address_formatter.asm_desc)?;
            }

            self.address_formatter.set(address_formatter);

            f.write_char('\n')?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct AddressFormatter {
    pub header: String,
    pub opcode: String,
    pub tag: String,
    pub bin: String,
    pub asm: String,
    pub asm_desc: String,
}

impl AddressFormatter {
    fn clear(&mut self) {
        self.header.clear();
        self.opcode.clear();
        self.tag.clear();
        self.bin.clear();
        self.asm.clear();
        self.asm_desc.clear();
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

// TODO change this to quirks instead of rom kind
pub fn write_inst_dasm(
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
            if kind == RomKind::SCHIP { *x } else { 0 },
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
        Instruction::WaitForKey(vx) => {
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
        Instruction::SetIndex(addr) => {
            write!(f, "ld   i {:#05X}", addr)?;
            write!(c, "i = {:#05X}", addr)
        }
        Instruction::SetIndexToLong(addr) => {
            write!(f, "lld  i {:#06X}", addr)?;
            write!(c, "i = {:#06X}", addr)
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
            write!(c, "v0..={:x} <- mem", vx)
        }
        Instruction::Store(vx) => {
            write!(f, "ld   i v{:x}", vx)?;
            write!(c, "mem <- v0..={:x}", vx)
        }
        Instruction::LoadRange(vx, vy) => {
            write!(f, "ld   v{:x} v{:x} i", vx, vy)?;
            write!(c, "v{:x}..={:x} <- mem", vx, vy)
        }
        Instruction::StoreRange(vx, vy) => {
            write!(f, "ld   i v{:x} v{:x}", vx, vy)?;
            write!(c, "mem <- v{:x}..={:x}", vx, vy)
        }
        Instruction::LoadFlags(vx) => {
            write!(f, "ld   v{:x} r", vx)?;
            write!(c, "v0..={:x} <- flags", vx)
        }
        Instruction::StoreFlags(vx) => {
            write!(f, "ld   r v{:x}", vx)?;
            write!(c, "flags <- v0..={:x}", vx)
        }
        Instruction::StoreBinaryCodedDecimal(vx) => {
            write!(f, "ld   b v{:x}", vx)?;
            write!(c, "mem <- bcd v{:x}", vx)
        }
        Instruction::GenerateRandom(vx, bound) => {
            write!(f, "rnd  v{:x} {}", vx, bound)?;
            write!(c, "v{:x} = rand 0..={}", vx, bound)
        }
        Instruction::SetPlane(n) => {
            write!(f, "pln  {}", n)?;
            write!(c, "select plane {}", n)
        }
        Instruction::Draw(vx, vy, height) => {
            write!(f, "drw  v{:x} v{:x} {}", vx, vy, height)?;
            if *height == 0 && kind == RomKind::SCHIP {
                write!(c, "draw 16x16 @ v{:x},v{:x}", vx, vy)
            } else {
                write!(c, "draw 8x{} @ v{:x},v{:x}", height, vx, vy)
            }
        }
        Instruction::ScrollUp(n) => {
            write!(f, "scu")?;
            write!(c, "scroll {} up", n)
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
            write!(c, "lo-res display")
        }
        Instruction::HighResolution => {
            write!(f, "high")?;
            write!(c, "hi-res display")
        }
        Instruction::ClearScreen => {
            write!(f, "cls")?;
            write!(c, "clear")
        }
        Instruction::LoadAudio => {
            write!(f, "ld   a i")?;
            write!(c, "audio <- mem")
        }
        Instruction::SetPitch(vx) => {
            write!(f, "ld   p v{:x}", vx)?;
            write!(c, "pitch = v{:x}", vx)
        }
    }
}
