use super::core::Watchpoint;

use crate::{
    asm::{
        Disassembler, InstructionTag, ADDRESS_COMMENT_TOKEN, INSTRUCTION_COLUMNS,
    },
    run::{interp::Interpreter, mem::extract_access_flags},
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

pub(super) struct Memory {
    pub verbose: bool,
    pub follow: Option<MemoryPointer>,
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
            verbose: false,
            follow: Some(MemoryPointer::ProgramCounter),
        }
    }
}

impl Memory {
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
        let addr_max = (self.interpreter.memory.len() - 1) as u16;
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
        for addr in 0..self.interpreter.memory.len() as u16 {
            if !self.is_addr_drawable(addr) {
                continue;
            }

            let spans = self.addr_span(addr, 0, false);
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
    ) -> Spans {
        let tag = self.disassembler.tags[addr as usize];
        let flags = self.interpreter.memory_access_flags[addr as usize];

        let is_breakpoint = self.breakpoints.contains(&addr);
        let is_watchpoint = self.watchpoints.contains(&Watchpoint::Address(addr));

        let breakpoint_char = if is_breakpoint { '@' } else { ' ' };
        let watchpoint_char = if is_watchpoint { '*' } else { ' ' };

        self.disassembler.write_addr_dasm(addr).ok();

        let (draw, read, write, exec) = extract_access_flags(flags);
        
        let force_asm = addr == self.interpreter.pc;

        let address_formatter = self.disassembler.address_formatter.take();

        let show_addr_asm = address_formatter.asm.len() > 0
            && (self.memory.verbose || tag >= InstructionTag::Valid || force_asm);
        let show_addr_bin = self.memory.verbose
            || tag < InstructionTag::Proven && (tag < InstructionTag::Parsable || !force_asm);
        let show_addr_asm_desc = address_formatter.asm_desc.len() > 0 && (!show_addr_bin || self.memory.verbose);
        let show_comments = show_addr_bin || show_addr_asm_desc;

        let content_len = 7 
            + address_formatter.header.len()
            //+ address_formatter.opcode.len()
            + if show_addr_asm { address_formatter.asm.len() + 1 } else { 0 };
        let content_len_padded = if show_comments {
            content_len.max(INSTRUCTION_COLUMNS + 1)
        } else {
            content_len
        };

        // TODO: factor in the opcode length
        let comment_len = if show_comments {
            2 + if show_addr_bin { address_formatter.bin.len() } else { 0 }
                + if show_addr_asm_desc {
                    address_formatter.asm_desc.len()
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
        } else if addr_is_selected || addr == self.interpreter.pc || addr == self.interpreter.index
        {
            Color::Black
        } else {
            Color::Reset
        };

        let mut spans: Vec<Span> = Vec::with_capacity(if show_comments { 2 } else { 1 });

        let mut content = String::with_capacity(content_len_padded);
        content.push(breakpoint_char);
        content.push(watchpoint_char);
        content.push_str(&address_formatter.header);
        content.push(' ');
        content.push(if draw { 'd' } else { '-' });
        content.push(if read { 'r' } else { '-' });
        content.push(if write { 'w' } else { '-' });
        content.push(if exec { 'x' } else { '-' });
        content.push_str(&address_formatter.tag);
        //content.push_str(&address_formatter.opcode);
        //content.push_str(&address_formatter.bin);
        if show_addr_asm {
            content.push(' ');
            content.push_str(&address_formatter.asm);
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
            comment.push(' ');
            comment.push_str(&address_formatter.opcode);

            if show_addr_bin {
                comment.push(' ');
                comment.push_str(&address_formatter.bin);
            }

            if show_addr_asm_desc {
                comment.push(' ');
                comment.push_str(&address_formatter.asm_desc);
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

        self.disassembler.address_formatter.set(address_formatter);

        Spans::from(spans)
    }
}

impl<'a> StatefulWidget for MemoryWidget<'_> {
    type State = MemoryWidgetState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let addr_max = (self.interpreter.memory.len() - 1) as u16;

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

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize);
        let lines_ahead = (area.height - area.height / 2) as usize;

        let addr_start = state.focus;
        let mut addr = addr_start;

        while lines.len() < lines_ahead && addr <= addr_max {
            if self.is_addr_drawable(addr) {
                lines.push(self.addr_span(addr, area.width, lines.len() == 0));
            }
            if addr == addr_max {
                break;
            }
            addr += 1;
        }

        let addr_ahead = addr.saturating_sub(1);
        addr = addr_start;

        while addr > 0 && lines.len() < area.height as usize {
            addr -= 1;
            if self.is_addr_drawable(addr) {
                lines.insert(0, self.addr_span(addr, area.width, lines.len() == 0));
            }
        }

        addr = addr_ahead;

        while addr < addr_max && lines.len() < area.height as usize {
            addr += 1;
            if self.is_addr_drawable(addr) {
                lines.push(self.addr_span(addr, area.width, lines.len() == 0));
            }
        }

        Paragraph::new(lines).render(area, buf);
    }
}
