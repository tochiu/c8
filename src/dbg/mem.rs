use super::core::Watchpoint;

use crate::{
    asm::{
        write_byte_str, Disassembler, InstructionTag, ADDRESS_COMMENT_TOKEN, INSTRUCTION_COLUMNS,
    },
    run::{
        interp::{Instruction, Interpreter},
        prog::PROGRAM_MEMORY_SIZE,
    },
};

use crossterm::event::{KeyCode, KeyEvent};
use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use std::{collections::HashSet, fs::File, io::Write, path::Path};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(super) enum MemoryPointer {
    ProgramCounter,
    Index,
}

impl MemoryPointer {
    pub(super) fn identifier(self) -> &'static str {
        match self {
            Self::Index => "i",
            Self::ProgramCounter => "pc",
        }
    }
}

struct AddressFlags;

impl AddressFlags {
    const DRAW: u8 = 0b1;
    const READ: u8 = 0b10;
    const WRITE: u8 = 0b100;
    const EXECUTE: u8 = 0b1000;

    fn extract(flags: u8) -> (bool, bool, bool, bool) {
        (
            flags & Self::DRAW == Self::DRAW,
            flags & Self::READ == Self::READ,
            flags & Self::WRITE == Self::WRITE,
            flags & Self::EXECUTE == Self::EXECUTE,
        )
    }
}

pub(super) struct Memory {
    pub verbose: bool,
    pub follow: Option<MemoryPointer>,
    flags: [u8; PROGRAM_MEMORY_SIZE as usize],
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
            verbose: false,
            follow: Some(MemoryPointer::ProgramCounter),
            flags: [0; PROGRAM_MEMORY_SIZE as usize],
        }
    }
}

impl Memory {
    pub(super) fn step(
        &mut self,
        widget_state: &mut MemoryWidgetState,
        interp: &Interpreter,
        pc: u16,
        index: u16,
        inst: Option<Instruction>,
    ) {
        widget_state.follow = true;
        if let Some(inst) = inst {
            if let Some(flags) = self.flags.get_mut(pc as usize) {
                *flags |= AddressFlags::EXECUTE;
            }

            match inst {
                Instruction::Display(_, _, height) => {
                    if let Some(addr) =
                        interp.checked_addr_add(index, height.saturating_sub(1) as u16)
                    {
                        for flags in self.flags[index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::DRAW;
                        }
                    }
                }
                Instruction::Load(vx) => {
                    if let Some(addr) = interp.checked_addr_add(index, vx as u16) {
                        for flags in self.flags[index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::READ;
                        }
                    }
                }
                Instruction::Store(vx) => {
                    if let Some(addr) = interp.checked_addr_add(index, vx as u16) {
                        for flags in self.flags[index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::WRITE;
                        }
                    }
                }
                Instruction::StoreDecimal(_) => {
                    if let Some(addr) = interp.checked_addr_add(index, 2) {
                        for flags in self.flags[index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::WRITE;
                        }
                    }
                }
                _ => (),
            }
        }
    }

    pub(super) fn handle_key_event(
        &mut self,
        event: KeyEvent,
        widget_state: &mut MemoryWidgetState,
        active: &mut bool,
    ) -> bool {
        match event.code {
            KeyCode::Esc => {
                *active = false;
            }
            KeyCode::Down | KeyCode::Char('s') | KeyCode::Char('S') => {
                widget_state.offset = widget_state.offset.saturating_add(1);
            }
            KeyCode::Up | KeyCode::Char('w') | KeyCode::Char('W') => {
                widget_state.offset = widget_state.offset.saturating_sub(1);
            }
            KeyCode::PageDown => {
                widget_state.offset_scale = widget_state.offset_scale.saturating_sub(1);
            }
            KeyCode::PageUp => {
                widget_state.offset_scale = widget_state.offset_scale.saturating_add(1);
            }
            KeyCode::Home => {
                widget_state.set_focus(0);
            }
            KeyCode::End => {
                widget_state.set_focus(u16::MAX);
            }
            _ => return false,
        }
        true
    }
}

pub(super) struct MemoryWidgetState {
    offset: i32,
    offset_scale: i32,
    focus: u16,
    follow: bool,
}

impl Default for MemoryWidgetState {
    fn default() -> Self {
        MemoryWidgetState {
            offset: 0,
            offset_scale: 0,
            focus: 0,
            follow: true,
        }
    }
}

impl MemoryWidgetState {
    pub(super) fn set_focus(&mut self, addr: u16) {
        self.offset = 0;
        self.offset_scale = 0;
        self.focus = addr;
    }

    pub(super) fn poke(&mut self) {
        self.follow = true;
    }
}

pub(super) struct MemoryWidget<'a> {
    pub active: bool,
    pub memory: &'a Memory,
    pub watchpoints: &'a HashSet<Watchpoint>,
    pub breakpoints: &'a HashSet<u16>,
    pub interpreter: &'a Interpreter,
    pub disassembler: &'a Disassembler,
}

impl<'a> MemoryWidget<'_> {
    const COMMENT_STYLE: Style = Style {
        fg: Some(Color::Yellow),
        bg: None,
        add_modifier: Modifier::empty(),
        sub_modifier: Modifier::empty(),
    };

    fn is_addr_drawable(&self, addr: u16) -> bool {
        let tag = self.disassembler.tags[addr as usize];
        self.memory.verbose
            || tag >= InstructionTag::Proven
            || addr == 0
            || addr == self.interpreter.pc
            || addr == self.interpreter.index
            || !self
                .disassembler
                .tags
                .get(addr as usize - 1)
                .map_or(false, |&tag| tag >= InstructionTag::Proven)
    }

    fn find_nearest_drawable_addr(&self, mut addr: u16, is_forwards: bool) -> Option<u16> {
        let addr_max = self.interpreter.memory.len() as u16 - 1;
        loop {
            if self.is_addr_drawable(addr) {
                return Some(addr);
            } else {
                if addr == if is_forwards { addr_max } else { 0 } {
                    return None;
                } else if is_forwards {
                    addr += 1;
                } else {
                    addr -= 1;
                }
            }
        }
    }

    fn highlight_line(line: &mut Vec<Span>, bg: Color, fg: Color, line_width: u16) {
        let content_len = line.iter().fold(0, |len, span| len + span.width());
        if content_len < line_width as usize {
            line.push(Span::raw(" ".repeat(line_width as usize - content_len)));
        }

        for span in line.iter_mut() {
            (*span).style = span.style.add_modifier(Modifier::BOLD).bg(bg).fg(fg);
        }
    }

    pub(super) fn write_to_file<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        let mut file = File::create(path)?;
        let mut addr_header = String::new();
        let mut addr_opcode = String::new();
        let mut addr_bin = String::new();
        let mut addr_asm = String::new();
        let mut addr_asm_desc = String::new();
        for addr in 0..self.interpreter.memory.len() as u16 {
            if !self.is_addr_drawable(addr) {
                continue;
            }

            let spans = self.addr_span(
                addr,
                0,
                false,
                &mut addr_header,
                &mut addr_opcode,
                &mut addr_bin,
                &mut addr_asm,
                &mut addr_asm_desc,
            );
            for span in spans.0 {
                write!(file, "{}", span.content)?;
            }
            writeln!(file, "")?;
        }
        Ok(())
    }

    fn addr_span(
        &self,
        addr: u16,
        addr_line_width: u16,
        addr_is_selected: bool,
        addr_header: &mut String,
        addr_opcode: &mut String,
        addr_bin: &mut String,
        addr_asm: &mut String,
        addr_asm_desc: &mut String,
    ) -> Spans {
        let byte = self.disassembler.memory[addr as usize];
        let tag = self.disassembler.tags[addr as usize];
        let flags = self.memory.flags[addr as usize];

        addr_header.clear();
        addr_opcode.clear();
        addr_bin.clear();
        addr_asm.clear();
        addr_asm_desc.clear();

        let is_breakpoint = self.breakpoints.contains(&addr);
        let is_watchpoint = self.watchpoints.contains(&Watchpoint::Address(addr));

        addr_header.push(if is_breakpoint { '@' } else { ' ' });

        addr_header.push(if is_watchpoint { '*' } else { ' ' });

        self.disassembler
            .write_addr_disasm(addr, addr_header, addr_opcode, addr_asm, addr_asm_desc)
            .ok();

        let (draw, read, write, exec) = AddressFlags::extract(flags);
        addr_header.push(' ');
        addr_header.push(if draw  { 'd' } else { '-' });
        addr_header.push(if read  { 'r' } else { '-' });
        addr_header.push(if write { 'w' } else { '-' });
        addr_header.push(if exec  { 'x' } else { '-' });

        let force_asm = addr == self.interpreter.pc;

        let show_addr_asm = addr_asm.len() > 0
            && (self.memory.verbose || tag >= InstructionTag::Valid || force_asm);
        let show_addr_bin = self.memory.verbose
            || tag < InstructionTag::Proven && (tag < InstructionTag::Parsable || !force_asm);
        let show_addr_asm_desc = addr_asm_desc.len() > 0 && (!show_addr_bin || self.memory.verbose);
        let show_comments = show_addr_bin || show_addr_asm_desc;

        if show_addr_bin {
            addr_bin.push(' ');
            write_byte_str(addr_bin, byte, 1).ok();
        }

        let content_len = addr_header.len()
            + addr_opcode.len()
            + if show_addr_asm { addr_asm.len() + 1 } else { 0 };
        let content_len_padded = if show_comments {
            content_len.max(INSTRUCTION_COLUMNS + 4)
        } else {
            content_len
        };
        let comment_len = if show_comments {
            2 + if show_addr_bin { addr_bin.len() } else { 0 }
                + if show_addr_asm_desc {
                    addr_asm_desc.len()
                } else {
                    0
                }
        } else {
            0
        };

        let highlight = if is_breakpoint {
            Color::Red
        } else if self.watchpoints.contains(&Watchpoint::Address(addr)) {
            Color::Blue
        } else if addr_is_selected || addr == self.interpreter.pc || addr == self.interpreter.index {
            Color::Black
        } else {
            Color::Reset
        };

        let mut spans: Vec<Span> = Vec::with_capacity(if show_comments { 2 } else { 1 });

        let mut content = String::with_capacity(content_len_padded);
        content.push_str(&addr_header);
        content.push_str(&addr_opcode);
        if show_addr_asm {
            content.push(' ');
            content.push_str(&addr_asm);
        }
        if show_comments {
            for _ in 0..content_len_padded.saturating_sub(content.len()) {
                content.push(' ');
            }
        }

        spans.push(Span::styled(content, Style::default().fg(highlight)));

        if show_comments {
            let mut comment = String::with_capacity(comment_len);
            comment.push_str(ADDRESS_COMMENT_TOKEN);
            if show_addr_bin {
                comment.push(' ');
                comment.push_str(&addr_bin);
            }
            if show_addr_asm_desc {
                comment.push(' ');
                comment.push_str(&addr_asm_desc);
            }

            spans.push(Span::styled(comment, MemoryWidget::COMMENT_STYLE));
        }

        if addr_is_selected && self.active {
            MemoryWidget::highlight_line(&mut spans, Color::White, highlight, addr_line_width);
        } else if addr == self.interpreter.pc {
            MemoryWidget::highlight_line(&mut spans, Color::LightGreen, highlight, addr_line_width);
        } else if addr == self.interpreter.index {
            MemoryWidget::highlight_line(
                &mut spans,
                Color::LightYellow,
                highlight,
                addr_line_width,
            );
        } else if addr_is_selected {
            MemoryWidget::highlight_line(&mut spans, Color::White, highlight, addr_line_width);
        }

        Spans::from(spans)
    }
}

impl<'a> StatefulWidget for MemoryWidget<'_> {
    type State = MemoryWidgetState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let addr_max = self.interpreter.memory.len() as u16 - 1;

        let (is_offset_pos, mut offset_abs) = {
            let offset = state
                .offset
                .saturating_sub(state.offset_scale * area.height as i32);
            (
                offset > 0,
                if self.active { offset.abs() as u32 } else { 0 },
            )
        };

        state.offset = 0;
        state.offset_scale = 0;

        if !self.active && state.follow {
            if let Some(pointer) = self.memory.follow {
                match pointer {
                    MemoryPointer::ProgramCounter => {
                        state.focus = self.interpreter.pc;
                    }
                    MemoryPointer::Index => {
                        state.focus = self.interpreter.index;
                    }
                }
            }
        }

        state.follow = false;
        state.focus = state.focus.min(addr_max);

        let Some(mut focus) = self.find_nearest_drawable_addr(state.focus, false).or(
            self.find_nearest_drawable_addr(state.focus, true)
        ) else {
            return
        };

        while offset_abs > 0 && focus.abs_diff(if is_offset_pos { addr_max } else { 0 }) > 0 {
            if let Some(nearest_addr) = self.find_nearest_drawable_addr(
                if is_offset_pos { focus + 1 } else { focus - 1 },
                is_offset_pos,
            ) {
                focus = nearest_addr;
                offset_abs -= 1;
            } else {
                break;
            }
        }

        state.focus = focus;

        if area.area() == 0 {
            return;
        }

        let mut addr_header = String::new();
        let mut addr_opcode = String::new();
        let mut addr_bin = String::new();
        let mut addr_asm = String::new();
        let mut addr_asm_desc = String::new();

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize);
        let lines_ahead = (area.height - area.height / 2) as usize;

        let addr_start = state.focus;
        let mut addr = addr_start;

        while lines.len() < lines_ahead && addr <= addr_max {
            if self.is_addr_drawable(addr) {
                lines.push(self.addr_span(
                    addr,
                    area.width,
                    lines.len() == 0,
                    &mut addr_header,
                    &mut addr_opcode,
                    &mut addr_bin,
                    &mut addr_asm,
                    &mut addr_asm_desc,
                ));
            }
            addr += 1;
        }

        let addr_ahead = addr.saturating_sub(1);
        addr = addr_start;

        while addr > 0 && lines.len() < area.height as usize {
            addr -= 1;
            if self.is_addr_drawable(addr) {
                lines.insert(
                    0,
                    self.addr_span(
                        addr,
                        area.width,
                        lines.len() == 0,
                        &mut addr_header,
                        &mut addr_opcode,
                        &mut addr_bin,
                        &mut addr_asm,
                        &mut addr_asm_desc,
                    ),
                );
            }
        }

        addr = addr_ahead;

        while addr < addr_max && lines.len() < area.height as usize {
            addr += 1;
            if self.is_addr_drawable(addr) {
                lines.push(self.addr_span(
                    addr,
                    area.width,
                    lines.len() == 0,
                    &mut addr_header,
                    &mut addr_opcode,
                    &mut addr_bin,
                    &mut addr_asm,
                    &mut addr_asm_desc,
                ));
            }
        }

        Paragraph::new(lines).render(area, buf);
    }
}
