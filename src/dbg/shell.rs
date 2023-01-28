use crate::{asm::write_inst_dasm, ch8::interp::Interpreter};

use crossterm::event::{KeyCode, KeyEvent};
use tui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Paragraph, StatefulWidget, Widget},
};

use std::{
    cell::Cell,
    collections::{vec_deque::Iter, VecDeque},
    fmt::Write,
};

const MAX_OUTPUT_MESSAGES: usize = 1000;

#[derive(Default)]
pub(super) struct Shell {
    pub(super) input_enabled: bool,

    input: String,
    output: VecDeque<Spans<'static>>,
    output_offset: usize,
    output_line_buffer: Cell<Vec<Span<'static>>>,
    cursor_position: usize,
    cmd_queue: Vec<String>,
    history: Vec<String>,
    history_index: usize,
}

impl Shell {
    const PREFIX_INPUT: &'static str = "(c8db) ";
    const PREFIX_ERROR: &'static str = "ERROR: ";

    pub(super) fn new() -> Self {
        Self {
            input_enabled: true,
            output: VecDeque::with_capacity(MAX_OUTPUT_MESSAGES),
            ..Default::default()
        }
    }

    pub(super) fn handle_input_key_event(&mut self, event: KeyEvent) -> bool {
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

    pub(super) fn handle_output_key_event(&mut self, event: KeyEvent, active: &mut bool) -> bool {
        match event.code {
            KeyCode::Esc => {
                *active = false;
                self.output_offset = 0;
            }
            KeyCode::Down | KeyCode::Char('s') | KeyCode::Char('S') => {
                self.output_offset = self.output_offset.saturating_sub(1);
            }
            KeyCode::Up | KeyCode::Char('w') | KeyCode::Char('W') => {
                self.output_offset = self.output_offset.saturating_add(1).min(self.output.len());
            }
            KeyCode::Home => {
                self.output_offset = self.output.len();
            }
            KeyCode::End => {
                self.output_offset = 0;
            }
            _ => return false,
        }
        true
    }

    pub(super) fn output_pc(&mut self, interp: &Interpreter) {
        let mut buf = format!("{:#05X?}: ", interp.pc);
        let mut inst_asm = String::new();
        let mut inst_comment = String::new();
        if let Some(inst) = interp.instruction {
            write_inst_dasm(
                &inst,
                interp.rom.config.kind,
                &mut inst_asm,
                &mut inst_comment,
            )
            .ok();
            write!(buf, "{}", inst_asm).ok();
            self.print(buf);
            if inst_comment.is_empty() {
                self.print("");
            } else {
                self.print(format!("{}# {}", " ".repeat(11), inst_comment));
            }
        } else {
            buf.push_str("BAD INSTRUCTION");
            self.print(buf);
        }
    }

    pub(super) fn try_recv(&mut self) -> impl Iterator<Item = String> + '_ {
        self.cmd_queue.drain(..)
    }

    pub(super) fn echo(&mut self, content: &str) {
        self.print(Spans::from(vec![
            Span::styled(
                Shell::PREFIX_INPUT,
                Style::default().add_modifier(Modifier::BOLD),
            ),
            Span::raw(content.to_string()),
        ]));
    }

    pub(super) fn print<T: Into<Spans<'static>>>(&mut self, content: T) {
        if self.output.len() >= MAX_OUTPUT_MESSAGES {
            self.output.pop_front();
        }

        self.output.push_back(content.into());
    }

    pub(super) fn error<T: Into<String>>(&mut self, content: T) {
        self.print(Spans::from(vec![
            Span::styled(
                Shell::PREFIX_ERROR,
                Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
            ),
            Span::styled(content.into(), Style::default().fg(Color::Red)),
        ]));
    }

    pub(super) fn as_output_widget(&self) -> OutputWidget {
        OutputWidget {
            output: self
                .output
                .range(..self.output.len().saturating_sub(self.output_offset)),
            output_draw_buffer: &self.output_line_buffer,
        }
    }
}

pub(super) struct OutputWidget<'a> {
    output: Iter<'a, Spans<'a>>,
    output_draw_buffer: &'a Cell<Vec<Span<'static>>>,
}

impl<'a> OutputWidget<'_> {
    fn flush_line_buf<'b>(line_buf: &mut Vec<Span<'b>>, lines: &mut Vec<Spans<'b>>) {
        if !line_buf.is_empty() {
            lines.push(Spans::from(line_buf.clone()));
            line_buf.clear();
        }
    }
}

impl<'a> Widget for OutputWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.area() == 0 {
            return;
        }

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize + 4);
        let mut line_buf = self.output_draw_buffer.take();
        let mut line_buf_content_len = 0;

        let max_line_width = area.width as usize;

        for line in self.output.rev() {
            let start = lines.len();

            for span in line.0.iter() {
                let mut entry = span.content.as_ref();
                let original_entry_len = entry.len();
                let style = span.style;

                while let Some(whitespace_len) = entry.find(|c: char| !c.is_whitespace()) {
                    let rest = &entry[whitespace_len..];

                    let token_len = rest
                        .find(char::is_whitespace)
                        .unwrap_or(entry.len() - whitespace_len);
                    let token = &rest[..token_len];

                    if line_buf_content_len + whitespace_len + token_len > max_line_width {
                        if token_len > max_line_width {
                            for token_chunk in token.as_bytes().chunks(max_line_width) {
                                OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
                                let chunk = std::str::from_utf8(token_chunk).unwrap_or_default();
                                line_buf.push(Span::styled(chunk, style));
                                line_buf_content_len = chunk.len();
                            }
                        } else {
                            OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
                            line_buf.push(Span::styled(token, style));
                            line_buf_content_len = token.len();
                        }
                    } else {
                        line_buf.push(Span::styled(&entry[..whitespace_len + token_len], style));
                        line_buf_content_len += whitespace_len + token_len;
                    }

                    entry = &entry[whitespace_len + token_len..];
                }

                // Handle trailing whitespace before next span
                if !entry.is_empty() || original_entry_len == 0 {
                    if line_buf_content_len + entry.len() > max_line_width {
                        OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
                        line_buf_content_len = 0;
                    } else {
                        line_buf.push(Span::styled(entry, style));
                        line_buf_content_len += entry.len();
                    }
                }
            }

            OutputWidget::flush_line_buf(&mut line_buf, &mut lines);
            line_buf_content_len = 0;

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
                line_count as u16,
            ),
            buf,
        );
    }
}

#[derive(Default)]
pub(super) struct InputWidgetState {
    input_offset: usize,
}
pub(super) struct InputWidget<'a> {
    shell: &'a Shell,
}

impl<'a> InputWidget<'_> {
    fn compute_draw_params(&self, area: Rect) -> (u16, u16, usize, usize, usize) {
        let cmd_x = area.left();
        let cmd_y = area.bottom().saturating_sub(1);
        let cmd_width = area.width as usize;
        let cmd_prefix_width = Shell::PREFIX_INPUT.len();
        let input_area_width = cmd_width.saturating_sub(cmd_prefix_width);

        (cmd_x, cmd_y, cmd_width, cmd_prefix_width, input_area_width)
    }

    pub(super) fn cursor_position(
        &self,
        area: Rect,
        state: &mut InputWidgetState,
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

impl<'a> From<&'a Shell> for InputWidget<'a> {
    fn from(shell: &'a Shell) -> Self {
        InputWidget { shell }
    }
}

impl<'a> StatefulWidget for InputWidget<'a> {
    type State = InputWidgetState;

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
                Shell::PREFIX_INPUT,
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
