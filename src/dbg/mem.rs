use crate::{
    disass::{write_byte_str, Disassembler, InstructionTag, INSTRUCTION_COLUMNS},
    vm::{
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

use std::fmt::Write;

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
    pub follow: Option<MemoryPointer>,
    flags: [u8; PROGRAM_MEMORY_SIZE as usize],
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
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
            _ => return false,
        }
        true
    }
}

pub(super) struct MemoryWidgetState {
    offset: i32,
    focus: u16,
    follow: bool,
}

impl Default for MemoryWidgetState {
    fn default() -> Self {
        MemoryWidgetState {
            offset: 0,
            focus: 0,
            follow: true,
        }
    }
}

impl MemoryWidgetState {
    pub(super) fn set_focus(&mut self, addr: u16) {
        self.offset = 0;
        self.focus = addr;
    }

    pub(super) fn poke(&mut self) {
        self.follow = true;
    }
}

pub(super) struct MemoryWidget<'a> {
    pub active: bool,
    pub memory: &'a Memory,
    pub interpreter: &'a Interpreter,
    pub disassembler: &'a Disassembler,
}

impl<'a> MemoryWidget<'_> {
    const COMMENT_DELIM: &'static str = "# ";

    fn is_addr_drawable(&self, addr: u16) -> bool {
        let tag = self.disassembler.tags[addr as usize];
        tag >= InstructionTag::Proven
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

    fn addr_span(
        &self,
        addr: u16,
        addr_line_width: u16,
        addr_header: &mut String,
        addr_opcode: &mut String,
        addr_content: &mut String,
        addr_comment: &mut String,
    ) -> Spans {
        let byte = self.disassembler.memory[addr as usize];
        let tag = self.disassembler.tags[addr as usize];
        let flags = self.memory.flags[addr as usize];

        addr_header.clear();
        addr_opcode.clear();
        addr_content.clear();
        addr_comment.clear();

        addr_content.push(' ');
        addr_comment.push_str(MemoryWidget::COMMENT_DELIM);

        self.disassembler
            .write_addr_disass(addr, addr_header, addr_opcode, addr_content, addr_comment)
            .ok();

        let (draw, read, write, exec) = AddressFlags::extract(flags);
        addr_header.push(' ');
        addr_header.push(if draw { 'd' } else { '-' });
        addr_header.push(if read { 'r' } else { '-' });
        addr_header.push(if write { 'w' } else { '-' });
        addr_header.push(if exec { 'x' } else { '-' });

        let force_asm = addr == self.interpreter.pc;

        let show_addr_content = tag >= InstructionTag::Valid || force_asm;
        let show_bin_comment =
            tag < InstructionTag::Proven && (tag < InstructionTag::Parsable || !force_asm);
        let show_addr_comment = show_bin_comment
            || show_addr_content && addr_comment.len() > MemoryWidget::COMMENT_DELIM.len();

        if show_addr_comment {
            if !show_addr_content {
                addr_content.clear();
            }

            let padding = (INSTRUCTION_COLUMNS + 4)
                .saturating_sub(addr_header.len() + addr_opcode.len() + addr_content.len());

            addr_content.reserve_exact(padding);
            for _ in 0..padding {
                addr_content.push(' ');
            }
        }

        if show_bin_comment {
            addr_comment.clear();
            write!(
                addr_comment,
                "{}{:#04X} ",
                MemoryWidget::COMMENT_DELIM,
                byte
            )
            .ok();
            write_byte_str(addr_comment, byte, 1).ok();
        }

        let mut spans: Vec<Span> = Vec::with_capacity(3);

        spans.push(Span::raw(addr_header.clone()));
        spans.push(Span::raw(addr_opcode.clone()));
        if show_addr_content || show_addr_comment {
            spans.push(Span::raw(addr_content.clone()));
        }
        if show_addr_comment {
            spans.push(Span::styled(
                addr_comment.clone(),
                Style::default().fg(Color::Yellow),
            ));
        }

        if addr == self.interpreter.pc {
            MemoryWidget::highlight_line(
                &mut spans,
                Color::LightMagenta,
                Color::Black,
                addr_line_width,
            );
        } else if addr == self.interpreter.index {
            MemoryWidget::highlight_line(
                &mut spans,
                Color::LightYellow,
                Color::Black,
                addr_line_width,
            );
        }

        Spans::from(spans)
    }
}

impl<'a> StatefulWidget for MemoryWidget<'_> {
    type State = MemoryWidgetState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let addr_max = self.interpreter.memory.len() as u16 - 1;

        let is_offset_pos = state.offset > 0;
        let mut offset_abs = if self.active {
            state.offset.abs() as u32
        } else {
            0
        };

        state.offset = 0;

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

        let Some(mut focus) = self.find_nearest_drawable_addr(state.focus, false).or(self.find_nearest_drawable_addr(state.focus, true)) else {
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
        let mut addr_content = String::new();
        let mut addr_comment = String::new();

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize);
        let lines_ahead = (area.height - area.height / 2) as usize;

        let addr_start = state.focus;
        let mut addr = addr_start;

        while lines.len() < lines_ahead && addr <= addr_max {
            if self.is_addr_drawable(addr) {
                let mut line = self.addr_span(
                    addr,
                    area.width,
                    &mut addr_header,
                    &mut addr_opcode,
                    &mut addr_content,
                    &mut addr_comment,
                );
                if lines.len() == 0 && self.active {
                    MemoryWidget::highlight_line(
                        &mut line.0,
                        Color::White,
                        Color::Black,
                        area.width,
                    );
                }
                lines.push(line);
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
                        &mut addr_header,
                        &mut addr_opcode,
                        &mut addr_content,
                        &mut addr_comment,
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
                    &mut addr_header,
                    &mut addr_opcode,
                    &mut addr_content,
                    &mut addr_comment,
                ));
            }
        }

        Paragraph::new(lines).render(area, buf);
    }
}
