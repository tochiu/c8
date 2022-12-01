use crate::{
    disass::write_inst_asm,
    vm::interp::{Instruction, Interpreter},
};

use crossterm::event::{KeyCode, KeyEvent};
use tui::{buffer::Buffer, layout::Rect, style::Style, widgets::{StatefulWidget, Widget, Paragraph}, text::Spans};

use std::{fmt::Write, cell::Cell};

#[derive(Default)]
pub(super) struct Shell {
    pub(super) input_enabled: bool,

    input: String,
    output: Vec<String>,
    output_line_buffer: Cell<String>,
    cursor_position: usize,
    cmd_queue: Vec<String>,
    history: Vec<String>,
    history_index: usize,
}

impl Shell {
    const PREFIX: &'static str = "(c8db) ";

    pub(super) fn handle_key_event(&mut self, event: KeyEvent) -> bool {
        if !self.input_enabled {
            return false;
        }

        let mut sink_input = true;

        match event.code {
            KeyCode::Backspace => {
                if self.cursor_position > 0 {
                    self.input.remove(self.cursor_position - 1);
                    self.cursor_position -= 1;
                }
            }
            KeyCode::PageDown | KeyCode::Down => {
                if self.history_index < self.history.len().saturating_sub(1) {
                    self.history_index += 1;
                    self.input.clear();
                    self.input.push_str(&self.history[self.history_index]);
                    self.cursor_position = self.input.len();
                }
            }
            KeyCode::PageUp | KeyCode::Up => {
                if self.history_index > 0 {
                    self.history_index -= 1;
                    self.input.clear();
                    self.input.push_str(&self.history[self.history_index]);
                    self.cursor_position = self.input.len();
                }
            }
            KeyCode::Enter => {
                let cmd = if self.input.is_empty() {
                    self.history.last().map(String::as_str).unwrap_or_default()
                } else {
                    self.input.trim()
                };

                if !cmd.is_empty() {
                    log::info!("issueing command: {}", cmd);
                    self.cmd_queue.push(cmd.into());
                    if self.history.last().map_or(true, |last_cmd| cmd != last_cmd) {
                        self.history.push(cmd.into());
                    }
                    self.history_index = self.history.len();
                    self.input.clear();
                    self.cursor_position = 0;
                }
            }
            KeyCode::Left => {
                self.cursor_position = self.cursor_position.saturating_sub(1);
            }
            KeyCode::Right => {
                self.cursor_position = self.cursor_position.saturating_add(1).min(self.input.len());
            }
            KeyCode::Home => {
                self.cursor_position = 0;
            }
            KeyCode::End => {
                self.cursor_position = self.input.len();
            }
            KeyCode::Char(char) => {
                if char.is_ascii() {
                    self.input.insert(self.cursor_position, char);
                    self.cursor_position += 1;
                }
            }
            _ => {
                sink_input = false;
            }
        }

        sink_input
    }

    pub(super) fn output_pc(&mut self, interp: &Interpreter) {
        let mut buf = format!("{:#05X?}: ", interp.pc);
        let mut inst_asm = String::new();
        let mut inst_comment = String::new();
        if let Ok(inst) = Instruction::try_from(interp.fetch()) {
            write_inst_asm(&inst, &mut inst_asm, &mut inst_comment).ok();
            write!(buf, "{}", inst_asm).ok();
            self.output.push(buf);
            if inst_comment.is_empty() {
                self.output.push(" ".into());
            } else {
                self.output
                    .push(format!("{}# {}", " ".repeat(11), inst_comment));
            }
        } else {
            buf.push_str(">> BAD INSTRUCTION <<");
            self.output.push(buf);
        }
    }

    pub(super) fn try_recv(&mut self) -> impl Iterator<Item = String> + '_ {
        self.cmd_queue.drain(..)
    }

    pub(super) fn echo(&mut self, content: &str) {
        let mut result = String::with_capacity(Shell::PREFIX.len() + content.len());
        result.push_str(Shell::PREFIX);
        result.push_str(content);
        self.output.push(result);
    }

    pub(super) fn print<T: Into<String>>(&mut self, content: T) {
        self.output.push(content.into());
    }

    pub(super) fn print_unrecognized_cmd(&mut self) {
        self.output
            .push("Command not recognized. Type \"h\" to get a list of commands.".into());
    }
}

pub(super) struct OutputWidget<'a> {
    output: &'a [String],
    output_draw_buffer: &'a Cell<String>,
}

impl<'a> From<&'a Shell> for OutputWidget<'a> {
    fn from(shell: &'a Shell) -> Self {
        OutputWidget { output: &shell.output, output_draw_buffer: &shell.output_line_buffer }
    }
}

impl<'a> OutputWidget<'_> {
    fn flush_line_buf(line_buf: &mut String, lines: &mut Vec<Spans>) {
        if !line_buf.is_empty() {
            lines.push(Spans::from(line_buf.clone()));
            line_buf.clear();
        }
    }
}

impl<'a> Widget for OutputWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.area() == 0 {
            return
        }

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize + 4);
        let mut line_buf = self.output_draw_buffer.take();
        let max_line_width = area.width as usize;
        
        for mut entry in self.output.iter().rev().map(String::as_str) {
            let start = lines.len();
            while let Some(whitespace_len) = entry.find(|c: char| !c.is_whitespace()) {
                let rest = &entry[whitespace_len..];

                let token_len = rest.find(char::is_whitespace).unwrap_or(entry.len() - whitespace_len);
                let token = &rest[..token_len];

                if line_buf.len() + whitespace_len + token_len > max_line_width {
                    if token_len > max_line_width {
                        for token_chunk in token.as_bytes().chunks(max_line_width) {
                            OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
                            line_buf.push_str(std::str::from_utf8(token_chunk).unwrap_or_default());
                        }
                    } else {
                        OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
                        line_buf.push_str(token);
                    }
                } else {
                    line_buf.push_str(&entry[..whitespace_len + token_len]);
                }

                if whitespace_len + token_len < entry.len() {
                    entry = &entry[whitespace_len + token_len..];
                } else {
                    break;
                }
            }

            OutputWidget::flush_line_buf(&mut line_buf, &mut lines);

            if lines.len() > start {
                lines[start..].reverse();
            }

            if lines.len() >= area.height as usize {
                if lines.len() > area.height as usize {
                    lines.truncate(area.height as usize);
                }
                break;
            }
        }

        lines.reverse();
        let line_count = lines.len();

        Paragraph::new(lines).render(
            Rect::new(
                area.x,
                area.bottom().saturating_sub(line_count as u16),
                area.width,
                line_count as u16
            ),
            buf,
        );
    }
}

#[derive(Default)]
pub(super) struct CommandLineWidgetState {
    input_offset: usize,
}
pub(super) struct CommandLineWidget<'a> {
    shell: &'a Shell,
}

impl<'a> CommandLineWidget<'_> {
    fn compute_draw_params(&self, area: Rect) -> (u16, u16, usize, usize, usize) {
        let cmd_x = area.left();
        let cmd_y = area.bottom().saturating_sub(1);
        let cmd_width = area.width as usize;
        let cmd_prefix_width = Shell::PREFIX.len();
        let input_area_width = cmd_width.saturating_sub(cmd_prefix_width);

        (cmd_x, cmd_y, cmd_width, cmd_prefix_width, input_area_width)
    }

    pub(super) fn cursor_position(
        &self,
        area: Rect,
        state: &mut CommandLineWidgetState,
    ) -> Option<(u16, u16)> {
        if area.area() == 0 || !self.shell.input_enabled {
            None
        } else {
            let (cmd_x, cmd_y, _, cmd_prefix_width, input_area_width) =
                self.compute_draw_params(area);

            if input_area_width > 0 {
                if self.shell.cursor_position < state.input_offset {
                    state.input_offset = self.shell.cursor_position
                } else if self.shell.cursor_position
                    >= state.input_offset + input_area_width as usize
                {
                    state.input_offset =
                        self.shell.cursor_position - (input_area_width as usize - 1)
                }

                if state.input_offset + (input_area_width - 1) as usize > self.shell.input.len() {
                    state.input_offset = self
                        .shell
                        .input
                        .len()
                        .saturating_sub(input_area_width as usize);
                }

                let cursor_x = cmd_x
                    + cmd_prefix_width as u16
                    + (self.shell.cursor_position - state.input_offset) as u16;
                let cursor_y = cmd_y;

                Some((cursor_x, cursor_y))
            } else {
                None
            }
        }
    }
}

impl<'a> From<&'a Shell> for CommandLineWidget<'a> {
    fn from(shell: &'a Shell) -> Self {
        CommandLineWidget { shell }
    }
}

impl<'a> StatefulWidget for CommandLineWidget<'a> {
    type State = CommandLineWidgetState;

    // NOTE: this function assumes that self.shell.cursor_position is within the bounds of 0 and the length of the shell input string inclusive
    //       it also assumes that self.cursor_position() has been called prior to this function call to update the input_offset
    //       if these assumptions hold true then we can take a slice of the input from input_offset onwards without panicking
    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if area.area() == 0 {
            return;
        }

        let shell = self.shell;

        if shell.input_enabled {
            let (cmd_x, cmd_y, cmd_width, cmd_prefix_width, input_area_width) =
                self.compute_draw_params(area);

            buf.set_stringn(
                cmd_x,
                cmd_y,
                Shell::PREFIX,
                cmd_width as usize,
                Style::default(),
            );
            buf.set_stringn(
                cmd_x.saturating_add(cmd_prefix_width as u16),
                cmd_y,
                &shell.input[state.input_offset..],
                input_area_width as usize,
                Style::default(),
            );
        }
    }
}
