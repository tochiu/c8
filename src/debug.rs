use crate::{
    disass::write_inst_asm,
    vm::{
        disp::{Display, DisplayWidget, DISPLAY_WINDOW_HEIGHT, DISPLAY_WINDOW_WIDTH},
        interp::{Instruction, Interpreter},
        run::{VMRunner, VM},
    },
};

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind};
use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::Style,
    text::Spans,
    widgets::{Block, Borders, Paragraph, StatefulWidget, Widget},
};

use std::{collections::HashSet, fmt::Write};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Watchpoint {
    Index,
    Register(u8),
}

#[derive(Default)]
pub struct DebugWatchState {
    registers: [u8; 16],
    index: u16,
}

pub enum DebugEvent {
    WatchpointChange(Watchpoint, u16, u16),
    BreakpointReached(u16),
}

#[derive(Default)]
pub struct Debugger {
    active: bool,

    breakpoints: HashSet<u16>,
    watchpoints: HashSet<Watchpoint>,
    watch_state: DebugWatchState,
    event_queue: Vec<DebugEvent>,

    shell: Shell,
    shell_active: bool,

    vm_visible: bool,
}

impl Debugger {
    pub fn init(vm: &VM) -> Self {
        let mut dbg = Self::default();
        dbg.active = false;
        dbg.vm_visible = true;
        dbg.shell_active = true;
        dbg.shell.input_enabled = true;
        dbg.shell.prefix.push_str("(c8db) ");
        dbg.activate(vm);
        dbg
    }

    pub fn is_active(&self) -> bool {
        self.active
    }

    pub fn handle_input_event(
        &mut self,
        event: Event,
        vm_runner: &mut VMRunner,
        vm: &mut VM,
    ) -> bool {
        let mut render = false;

        let history_index = self.shell.history_index;
        let input_len = self.shell.input.len();
        let cursor_position = self.shell.cursor_position;

        match event {
            Event::Key(key_event) => {
                if let KeyEventKind::Press | KeyEventKind::Repeat = key_event.kind {
                    if self.active {
                        if self.shell_active {
                            self.shell.handle_key_event(key_event);
                        }
                    } else if key_event.code == KeyCode::Esc {
                        self.pause(vm_runner, vm);
                        render = true;
                    }
                }
            }
            _ => (),
        }

        let cmds = self.shell.cmd_queue.drain(..);

        if self.active {
            for cmd_str in cmds.collect::<Vec<_>>() {
                self.shell.output.push(self.shell.prefix.clone() + &cmd_str);
                // TODO: add more commands here

                let mut cmd_args = cmd_str.split_ascii_whitespace();

                let Some(cmd) = cmd_args.next() else {
                    self.shell.output_unrecognized_cmd();
                    continue;
                };

                match cmd {
                    "r" | "c" | "run" | "cont" | "continue" => {
                        log::info!("c8vm resume!");
                        vm_runner.resume().unwrap();
                        self.deactivate();
                        render = true;
                        break;
                    }
                    "s" | "step" => {
                        let amt = cmd_args
                            .next()
                            .and_then(|arg| arg.parse::<u128>().ok())
                            .unwrap_or(1)
                            .min(u16::MAX as u128) as usize;
                        log::info!("stepping {}!", amt);

                        for _ in 0..amt {
                            vm.step(1.0 / vm_runner.config().instruction_frequency as f64)
                                .unwrap();
                            self.shell.output_pc(vm.interpreter());
                        }

                        if amt > 1 {
                            self.shell.output.push(format!("Stepped {} times", amt));
                        }

                        render = true;
                    }
                    "show" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.output_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = true;
                        render = true;
                    }
                    "hide" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.output_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = false;
                        render = false;
                    }
                    _ => {
                        self.shell.output_unrecognized_cmd();
                        render = true;
                    }
                }
            }
        }

        if !(self.shell.input.len() == input_len
            && self.shell.cursor_position == cursor_position
            && self.shell.history_index == history_index)
        {
            render = true;
        }

        render
    }

    pub fn pause(&mut self, vm_runner: &mut VMRunner, vm: &VM) {
        log::info!("c8vm interrupt!");
        vm_runner.pause().unwrap();
        self.activate(vm);
    }

    pub fn activate(&mut self, vm: &VM) {
        self.shell.output.push("Paused.".into());
        self.shell.output_pc(vm.interpreter());
        self.active = true;
    }

    pub fn deactivate(&mut self) {
        self.shell.output.push("Continuing.".into());
        self.active = false;
    }

    pub fn step(&mut self, vm: &VM) -> bool {
        let interp = vm.interpreter();
        for &watchpoint in self.watchpoints.iter() {
            let (old_val, new_val) = match watchpoint {
                Watchpoint::Index => (self.watch_state.index, interp.index),
                Watchpoint::Register(vx) => (
                    self.watch_state.registers[vx as usize] as u16,
                    interp.registers[vx as usize] as u16,
                ),
            };

            if old_val != new_val {
                self.event_queue
                    .push(DebugEvent::WatchpointChange(watchpoint, old_val, new_val));
            }
        }

        if self.breakpoints.contains(&interp.pc) {
            self.event_queue
                .push(DebugEvent::BreakpointReached(interp.pc));
        }

        if !self.active && !self.event_queue.is_empty() {
            self.activate(vm);
        }

        return self.event_queue.is_empty();
    }
}

#[derive(Default)]
pub struct Shell {
    prefix: String,
    cursor_position: usize,
    cmd_queue: Vec<String>,
    history: Vec<String>,
    history_index: usize,
    input: String,
    input_enabled: bool,
    output: Vec<String>,
}

impl Shell {
    fn handle_key_event(&mut self, event: KeyEvent) {
        if !self.input_enabled {
            return;
        }

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
                self.input.insert(self.cursor_position, char);
                self.cursor_position += 1;
            }
            _ => (),
        }
    }

    pub fn output_pc(&mut self, interp: &Interpreter) {
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

    pub fn output_unrecognized_cmd(&mut self) {
        self.output
            .push("Command not recognized. Type \"h\" to get a list of commands.".into());
    }
}

// render

#[derive(Default)]
pub struct DebuggerWidgetState {
    shell: ShellWidgetState,
}

pub struct DebuggerWidget<'a> {
    pub logging: bool,
    pub dbg: &'a Debugger,
    pub vm: &'a VM,
    pub vm_disp: &'a Display,
}

impl<'a> DebuggerWidget<'a> {
    pub fn cursor_position(
        &self,
        area: Rect,
        state: &mut DebuggerWidgetState,
    ) -> Option<(u16, u16)> {
        if self.can_draw_shell(area) {
            ShellWidget::from(&self.dbg.shell).cursor_position(area, &mut state.shell)
        } else {
            None
        }
    }

    pub fn logger_area(&self, area: Rect) -> Rect {
        self.areas(area).3
    }

    fn areas(&self, area: Rect) -> (Rect, Rect, Rect, Rect) {
        let (region, shell) = if self.can_draw_shell(area) {
            let rects = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(area.height.saturating_sub(1)),
                    Constraint::Length(1),
                ])
                .split(area);
            (rects[0], rects[1])
        } else {
            (area, Rect::default())
        };

        let (main, region) = {
            let rects = Layout::default()
                .direction(Direction::Horizontal)
                .constraints(if self.dbg.vm_visible {
                    [
                        Constraint::Length(region.width.saturating_sub(DISPLAY_WINDOW_WIDTH)),
                        Constraint::Length(DISPLAY_WINDOW_WIDTH),
                    ]
                } else if self.logging {
                    [Constraint::Percentage(80), Constraint::Percentage(20)]
                } else {
                    [Constraint::Percentage(100), Constraint::Percentage(0)]
                })
                .split(region);
            (rects[0], rects[1])
        };

        let (vm, logger) = {
            let rects = Layout::default()
                .direction(Direction::Vertical)
                .constraints(if self.dbg.vm_visible && self.logging {
                    [
                        Constraint::Length(DISPLAY_WINDOW_HEIGHT),
                        Constraint::Length(region.height.saturating_sub(DISPLAY_WINDOW_HEIGHT)),
                    ]
                } else if self.logging {
                    [Constraint::Percentage(0), Constraint::Percentage(100)]
                } else {
                    [Constraint::Percentage(100), Constraint::Percentage(0)]
                })
                .split(region);
            (rects[0], rects[1])
        };

        (main, shell, vm, logger)
    }

    fn main_areas(&self, mut area: Rect) -> (Rect, Rect, Rect, Rect, Rect, Rect) {
        let mut rects = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(area.width.saturating_sub(14) / 4),
                Constraint::Length(14),
                Constraint::Length(area.width.saturating_sub(area.width.saturating_sub(14) / 4)),
            ])
            .split(area);

        let (output_area, memory_area) = (rects[0], rects[2]);

        area = rects[1];
        rects = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),
                Constraint::Length(17),
                Constraint::Length(4),
                Constraint::Length(1 + self.vm.interpreter().stack.len().max(1) as u16),
            ])
            .split(area);

        let (pointer_area, register_area, timer_area, stack_area) =
            (rects[0], rects[1], rects[2], rects[3]);

        (
            memory_area,
            pointer_area,
            register_area,
            timer_area,
            stack_area,
            output_area,
        )
    }

    fn can_draw_shell(&self, area: Rect) -> bool {
        self.dbg.shell_active && area.area() > 0
    }
}

impl<'a> StatefulWidget for DebuggerWidget<'_> {
    type State = DebuggerWidgetState;
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer, state: &mut Self::State) {
        let (main_area, shell_area, vm_area, _) = self.areas(area);
        let (memory_area, pointer_area, register_area, timer_area, stack_area, output_area) =
            self.main_areas(main_area);

        if self.dbg.vm_visible {
            DisplayWidget {
                display: self.vm_disp,
                logging: false,
            }
            .render(vm_area, buf);
        }

        let base_border = Borders::TOP.union(Borders::LEFT);

        let pointer_block = Block::default().title(" Pointers ").borders(base_border);
        let timer_block = Block::default().title(" Timers ").borders(base_border);
        let stack_block = Block::default()
            .title(" Stack ")
            .borders(base_border.union(Borders::BOTTOM));
        let output_block = Block::default().title(" Output ").borders(Borders::TOP);
        let output_text_area = output_block.inner(output_area);

        let mut lines = self
            .dbg
            .shell
            .output
            .iter()
            .map(|out| out.as_bytes().chunks(output_text_area.width as usize))
            .flatten()
            .rev()
            .take(output_text_area.height as usize)
            .map(|bytes| Spans::from(std::str::from_utf8(bytes).unwrap_or("**unparsable**")))
            .collect::<Vec<_>>();
        let line_count = lines.len() as u16;

        lines.reverse();

        Paragraph::new(lines).render(
            Rect::new(
                output_text_area.x,
                output_text_area.bottom().saturating_sub(line_count),
                output_text_area.width,
                line_count,
            ),
            buf,
        );

        output_block.render(output_area, buf);

        Block::default()
            .title(" Memory ")
            .borders(base_border.union(Borders::BOTTOM).union(
                if self.logging || self.dbg.vm_visible {
                    Borders::NONE
                } else {
                    Borders::RIGHT
                },
            ))
            .render(memory_area, buf);

        let interp = self.vm.interpreter();

        Paragraph::new(vec![
            Spans::from(format!("pc {:#05X}", interp.pc)),
            Spans::from(format!("i  {:#05X}", interp.index)),
        ])
        .block(pointer_block)
        .render(pointer_area, buf);

        Paragraph::new(
            interp
                .registers
                .iter()
                .enumerate()
                .map(|(i, val)| Spans::from(format!("v{:x} {:0>3} ({:#04X})", i, val, val)))
                .collect::<Vec<_>>(),
        )
        .block(Block::default().title(" Registers ").borders(base_border))
        .render(register_area, buf);

        Paragraph::new(vec![
            Spans::from(format!("sound {:0>7.3}", self.vm.sound_timer())),
            Spans::from(format!("delay {:0>7.3}", self.vm.delay_timer())),
            Spans::from(format!("  |-> {:0>3}", interp.input.delay_timer)),
        ])
        .block(timer_block)
        .render(timer_area, buf);

        Paragraph::new(
            interp
                .stack
                .iter()
                .enumerate()
                .map(|(i, addr)| Spans::from(format!("#{:0>2} {:#05X}", i, addr)))
                .collect::<Vec<_>>(),
        )
        .block(stack_block)
        .render(stack_area, buf);

        if self.can_draw_shell(area) {
            ShellWidget::from(&self.dbg.shell).render(shell_area, buf, &mut state.shell)
        }
    }
}

#[derive(Default)]
pub struct ShellWidgetState {
    input_offset: usize,
}
pub struct ShellWidget<'a> {
    shell: &'a Shell,
}

impl<'a> ShellWidget<'_> {
    fn compute_draw_params(&self, area: Rect) -> (u16, u16, usize, usize, usize) {
        let cmd_x = area.left();
        let cmd_y = area.bottom().saturating_sub(1);
        let cmd_width = area.width as usize;
        let cmd_prefix_width = self.shell.prefix.len();
        let input_area_width = cmd_width.saturating_sub(cmd_prefix_width);

        (cmd_x, cmd_y, cmd_width, cmd_prefix_width, input_area_width)
    }

    fn cursor_position(&self, area: Rect, state: &mut ShellWidgetState) -> Option<(u16, u16)> {
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

impl<'a> From<&'a Shell> for ShellWidget<'a> {
    fn from(shell: &'a Shell) -> Self {
        ShellWidget { shell }
    }
}

impl<'a> StatefulWidget for ShellWidget<'a> {
    type State = ShellWidgetState;

    // NOTE: this function assumes that self.shell.cursor_position is within the bounds of 0 and the length of the shell input string inclusive
    //       it also assumes that self.cursor_position() has been called prior to this function call to update the input_offset
    //       if these assumptions hold true then we can take a slice of the input from input_offset onwards without panicking
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer, state: &mut Self::State) {
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
                &shell.prefix,
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
