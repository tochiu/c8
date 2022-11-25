use crate::{
    disass::{write_byte_str, write_inst_asm, Disassembler, InstructionTag, INSTRUCTION_COLUMNS},
    vm::{
        disp::{Display, DisplayWidget, DISPLAY_WINDOW_HEIGHT, DISPLAY_WINDOW_WIDTH},
        interp::{Instruction, Interpreter},
        prog::PROGRAM_MEMORY_SIZE,
        run::{VMRunner, VM},
    },
};

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind};
use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Paragraph, StatefulWidget, Widget},
};

use std::{collections::{HashSet, HashMap}, fmt::Write, num::IntErrorKind};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Watchpoint {
    Pointer(MemoryPointer),
    Register(u8),
    Address(u16)
}

#[derive(Default)]
pub struct WatchState {
    registers: [u8; 16],
    index: u16,
    pc: u16,
    addresses: HashMap<u16, u8>,
    instruction: Option<Instruction>,
}

impl From<&Interpreter> for WatchState {
    fn from(interp: &Interpreter) -> Self {
        WatchState {
            registers: interp.registers,
            index: interp.index,
            pc: interp.pc,
            addresses: Default::default(),
            instruction: Instruction::try_from(interp.fetch()).ok(),
        }
    }
}

impl WatchState {
    fn update(&mut self, event_queue: &mut Vec<DebugEvent>, interp: &Interpreter, watchpoints: &HashSet<Watchpoint>) {
        for &watchpoint in watchpoints.iter() {
            match watchpoint {
                Watchpoint::Pointer(pointer) => match pointer {
                    MemoryPointer::Index => {
                        let old_val = self.index;
                        let new_val = interp.index;

                        if old_val != new_val {
                            event_queue.push(DebugEvent::WatchpointTrigger(watchpoint, old_val, new_val));
                        }
                    },
                    MemoryPointer::ProgramCounter => {
                        let old_val = self.pc;
                        let new_val = interp.pc;

                        if old_val != new_val {
                            event_queue.push(DebugEvent::WatchpointTrigger(watchpoint, old_val, new_val));
                        }
                    },
                },
                Watchpoint::Register(vx) => {
                    let old_val = self.registers[vx as usize];
                    let new_val = interp.registers[vx as usize];

                    if old_val != new_val {
                        event_queue.push(DebugEvent::WatchpointTrigger(watchpoint, old_val as u16, new_val as u16));
                    }
                }
                Watchpoint::Address(addr) => {
                    let Some(&old_val) = self.addresses.get(&addr) else {
                        unreachable!("address watchpoint {:#05X} must store state in hashtable", addr)
                    };
                    let new_val = interp.memory[addr as usize];

                    if old_val != new_val {
                        self.addresses.insert(addr, new_val);
                        event_queue.push(DebugEvent::WatchpointTrigger(watchpoint, old_val as u16, new_val as u16));
                    }
                }
            }
        }

        self.pc = interp.pc;
        self.index = interp.index;
        self.registers.copy_from_slice(&interp.registers);
        self.instruction = Instruction::try_from(interp.fetch()).ok();
    }
}

pub enum DebugEvent {
    WatchpointTrigger(Watchpoint, u16, u16),
    BreakpointReached(u16),
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum MemoryPointer {
    ProgramCounter,
    Index,
}

pub struct AddressFlags;

impl AddressFlags {
    const DRAW: u8 = 0b1;
    const READ: u8 = 0b10;
    const WRITE: u8 = 0b100;
    const EXECUTE: u8 = 0b1000;

    pub fn extract(flags: u8) -> (bool, bool, bool, bool) {
        (flags & Self::DRAW == Self::DRAW, flags & Self::READ == Self::READ, flags & Self::WRITE == Self::WRITE, flags & Self::EXECUTE == Self::EXECUTE)
    }
}

fn parse_addr(arg: &str) -> Option<u16> {
    let (arg, radix) = if arg.starts_with("0x") {
        (arg.trim_start_matches("0x"), 16)
    } else {
        (arg, 10)
    };

    u16::from_str_radix(arg, radix).or_else(|err| {
        match err.kind() {
            IntErrorKind::PosOverflow => Ok(u16::MAX),
            IntErrorKind::NegOverflow => Ok(0),
            _ => Err(err),
        }
    }).ok().map(|addr| addr.min(PROGRAM_MEMORY_SIZE - 1))
}

pub struct Debugger {
    active: bool,

    breakpoints: HashSet<u16>,
    watchpoints: HashSet<Watchpoint>,
    watch_state: WatchState,
    event_queue: Vec<DebugEvent>,

    disassembler: Disassembler,

    memory_addr: u16,
    memory_addr_flags: [u8; PROGRAM_MEMORY_SIZE as usize],
    memory_follow: Option<MemoryPointer>,
    memory_active: bool,

    shell: Shell,
    shell_active: bool,

    vm_visible: bool,
}

impl Debugger {
    pub fn init(vm: &VM) -> Self {
        let mut dbg = Debugger {
            active: false,
            breakpoints: Default::default(),
            watchpoints: Default::default(),
            watch_state: WatchState::from(vm.interpreter()),
            event_queue: Default::default(),

            disassembler: Disassembler::from(vm.interpreter().program.clone()),

            memory_addr: 0,
            memory_addr_flags: [0; PROGRAM_MEMORY_SIZE as usize],
            memory_follow: Some(MemoryPointer::ProgramCounter),
            memory_active: false,

            shell: Default::default(),
            shell_active: false,

            vm_visible: false,
        };

        dbg.set_memory_addr(vm.interpreter().pc);

        dbg.vm_visible = true;
        dbg.shell_active = true;
        dbg.shell.input_enabled = true;
        dbg.shell.prefix.push_str("(c8db) ");
        dbg.disassembler.run();
        dbg.activate(vm);
        dbg
    }

    pub fn set_memory_addr(&mut self, addr: u16) {
        self.memory_addr = addr.clamp(0, PROGRAM_MEMORY_SIZE - 1);
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
                        } else if self.memory_active {
                            let should_render = self.handle_memory_key_event(key_event, &vm);
                            render = render || should_render;
                        }
                    } else if key_event.code == KeyCode::Esc {
                        self.pause(vm_runner, vm);
                        render = true;
                    }
                }
            }
            _ => (),
        }

        render = render || self.shell.cmd_queue.len() > 0;
        let cmds = self.shell.cmd_queue.drain(..);

        if self.active {
            for cmd_str in cmds.collect::<Vec<_>>() {
                self.shell.output.push(self.shell.prefix.clone() + &cmd_str);

                let lowercase_cmd_str = cmd_str.to_ascii_lowercase();
                let mut cmd_args = lowercase_cmd_str.split_ascii_whitespace();

                let Some(cmd) = cmd_args.next() else {
                    self.shell.output_unrecognized_cmd();
                    continue;
                };

                // TODO: add more commands here
                match cmd {
                    "r" | "c" | "run" | "cont" | "continue" => {
                        log::info!("c8vm resume!");
                        self.deactivate();
                        vm_runner.resume().unwrap();
                        break;
                    }
                    "s" | "step" => {
                        let amt = cmd_args
                            .next()
                            .and_then(|arg| arg.parse::<u128>().ok())
                            .unwrap_or(1)
                            .min(u16::MAX as u128) as usize;
                        log::info!("stepping {}!", amt);

                        let mut amt_stepped = 0;

                        for step in 0..amt {
                            amt_stepped = step + 1;
                            vm.step(1.0 / vm_runner.config().instruction_frequency as f64)
                                .unwrap();
                            self.shell.output_pc(vm.interpreter());
                            if !self.step(vm) {
                                break;
                            }
                        }

                        if amt_stepped > 1 {
                            self.shell
                                .output
                                .push(format!("Stepped {} times", amt_stepped));
                        }
                    }
                    "w" | "watch" => {
                        let Some(arg) = cmd_args.next() else {
                            self.shell.output.push("Please specify a register or pointer to watch".into());
                            continue;
                        };

                        let watchpoint_already_exists = !match arg {
                            "pc" => self.watchpoints.insert(Watchpoint::Pointer(MemoryPointer::ProgramCounter)),
                            "i" | "index" => self.watchpoints.insert(Watchpoint::Pointer(MemoryPointer::Index)),
                            _ => {
                                if arg.starts_with('v') {
                                    if let Some(register) = u8::from_str_radix(&arg[1..], 16).ok().and_then(|x| if x < 16 { Some(x) } else { None }) {
                                        self.watchpoints.insert(Watchpoint::Register(register))
                                    } else {
                                        self.shell.output.push("Please specify a valid register (v0..vf) to watch".into());
                                        continue;
                                    }
                                } else if let Some(addr) = parse_addr(arg) {
                                    self.watch_state.addresses.insert(addr, vm.interpreter().memory[addr as usize]);
                                    self.watchpoints.insert(Watchpoint::Address(addr))
                                } else {
                                    self.shell.output.push("Please specify a register or pointer to watch".into());
                                    continue;
                                }
                            }
                        };

                        self.shell.output.push(
                            if watchpoint_already_exists {
                                format!("Watchpoint {} already exists", arg)
                            } else {
                                format!("Watching {}", arg)
                            }
                        );
                    }
                    "b" | "break" | "breakpoint" => {
                        let Some(addr) = cmd_args.next().and_then(parse_addr) else {
                            self.shell.output.push(format!("Please specify a valid address in the range [{:#05X}, {:#05X}]", 0, PROGRAM_MEMORY_SIZE - 1));
                            continue;
                        };

                        if self.breakpoints.insert(addr) {
                            self.shell.output.push(format!("Breakpoint at {:#05X}", addr));
                        } else {
                            self.shell.output.push(format!("Breakpoint at {:#05X} already exists", addr));
                        }
                    }
                    "show" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.output_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = true;
                    }
                    "hide" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.output_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = false;
                    }
                    "m" | "mem" => {
                        self.memory_active = true;
                        self.shell_active = false;
                    }
                    "g" | "goto" => {
                        let Some(arg) = cmd_args.next() else {
                            self.shell.output_unrecognized_cmd();
                            continue;
                        };

                        match arg {
                            "start" => {
                                self.set_memory_addr(0);
                            }
                            "end" => {
                                self.set_memory_addr(PROGRAM_MEMORY_SIZE - 1);
                            }
                            "pc" => {
                                self.set_memory_addr(vm.interpreter().pc);
                            }
                            "i" | "index" => {
                                self.set_memory_addr(vm.interpreter().index);
                            }
                            _ => if let Some(addr) = parse_addr(arg) {
                                self.set_memory_addr(addr);
                            } else {
                                self.shell.output_unrecognized_cmd();
                            }
                        }
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

    pub fn handle_memory_key_event(&mut self, event: KeyEvent, vm: &VM) -> bool {
        match event.code {
            KeyCode::Esc => {
                self.memory_active = false;
                self.shell_active = true;
                true
            }
            KeyCode::Down | KeyCode::Char('s') | KeyCode::Char('S') => {
                let mut new_addr = self
                    .memory_addr
                    .saturating_add(1)
                    .clamp(0, PROGRAM_MEMORY_SIZE - 1);
                let memory_widget = MemoryWidget {
                    disassembler: &self.disassembler,
                    vm,
                    addr: self.memory_addr,
                    active: self.memory_active,
                    memory_addr_flags: &self.memory_addr_flags
                };

                loop {
                    if memory_widget.is_addr_rendered(new_addr) {
                        self.set_memory_addr(new_addr);
                        return true;
                    } else if new_addr < PROGRAM_MEMORY_SIZE - 1 {
                        new_addr += 1;
                    } else {
                        return false;
                    }
                }
            }
            KeyCode::Up | KeyCode::Char('w') | KeyCode::Char('W') => {
                let mut new_addr = self.memory_addr.saturating_sub(1);
                let memory_widget = MemoryWidget {
                    disassembler: &self.disassembler,
                    vm,
                    addr: self.memory_addr,
                    active: self.memory_active,
                    memory_addr_flags: &self.memory_addr_flags
                };

                loop {
                    if memory_widget.is_addr_rendered(new_addr) {
                        self.set_memory_addr(new_addr);
                        return true;
                    } else if new_addr > 0 {
                        new_addr -= 1;
                    } else {
                        return false;
                    }
                }
            }
            _ => false,
        }
    }

    pub fn pause(&mut self, vm_runner: &mut VMRunner, vm: &VM) {
        log::info!("c8vm interrupt!");
        vm_runner.pause().unwrap();
        self.activate(vm);
    }

    pub fn activate(&mut self, vm: &VM) {
        if self.active {
            return
        }

        self.shell.output.push("Paused.".into());
        self.shell.output_pc(vm.interpreter());
        self.active = true;
    }

    pub fn deactivate(&mut self) {
        if !self.active {
            return
        }

        self.shell.output.push("Continuing.".into());
        self.active = false;
    }

    pub fn step(&mut self, vm: &VM) -> bool {
        let interp = vm.interpreter();

        let exec_pc = self.watch_state.pc;
        let exec_inst = self.watch_state.instruction;
        let exec_index = self.watch_state.index;
        
        self.watch_state.update(&mut self.event_queue, interp, &self.watchpoints);

        if self.breakpoints.contains(&interp.pc) {
            self.event_queue
                .push(DebugEvent::BreakpointReached(interp.pc));
        }

        // update read write execute flags

        if let Some(inst) = exec_inst {
            if let Some(flags) = self.memory_addr_flags.get_mut(exec_pc as usize) {
                *flags |= AddressFlags::EXECUTE;
            }

            match inst {
                Instruction::Display(_, _, height) => {
                    if let Some(addr) = interp.checked_addr_add(exec_index, height.saturating_sub(1) as u16) {
                        for flags in self.memory_addr_flags[exec_index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::DRAW;
                        }
                    }
                }
                Instruction::Load(vx) => {
                    if let Some(addr) = interp.checked_addr_add(exec_index, vx as u16) {
                        for flags in self.memory_addr_flags[exec_index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::READ;
                        }
                    }
                }
                Instruction::Store(vx) => {
                    if let Some(addr) = interp.checked_addr_add(exec_index, vx as u16) {
                        for flags in self.memory_addr_flags[exec_index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::WRITE;
                        }
                    }
                }
                Instruction::StoreDecimal(_) => {
                    if let Some(addr) = interp.checked_addr_add(exec_index, 2) {
                        for flags in self.memory_addr_flags[exec_index as usize..=addr as usize].iter_mut() {
                            *flags |= AddressFlags::WRITE;
                        }
                    }
                }
                _ => ()
            }
        }

        // update disassembler
        self.disassembler.update(vm.interpreter());

        // update memory addr
        if let Some(ptr) = self.memory_follow {
            self.set_memory_addr(match ptr {
                MemoryPointer::Index => interp.index,
                MemoryPointer::ProgramCounter => interp.pc,
            });
        }

        if self.event_queue.is_empty() {
            return true;
        } else {
            self.activate(vm);
            for debug_event in self.event_queue.drain(..) {
                match debug_event {
                    DebugEvent::BreakpointReached(addr) => {
                        self.shell.output.push(format!("Breakpoint {:#05X} reached", addr));
                    }
                    DebugEvent::WatchpointTrigger(watchpoint, old, new) => {
                        match watchpoint {
                            Watchpoint::Pointer(pointer) => {
                                let identifier = match pointer {
                                    MemoryPointer::Index => "i",
                                    MemoryPointer::ProgramCounter => "pc"
                                };

                                self.shell.output.push(format!("Pointer {} changed", identifier));
                                self.shell.output.push(format!("Old value = {:#05X}", old));
                                self.shell.output.push(format!("New value = {:#05X}", new));
                            }
                            Watchpoint::Register(register) => {
                                self.shell.output.push(format!("Register v{:x} changed", register));
                                self.shell.output.push(format!("Old value = {:0>3} ({:#05X})", old, old));
                                self.shell.output.push(format!("New value = {:0>3} ({:#05X})", new, new));
                            }
                            Watchpoint::Address(addr) => {
                                self.shell.output.push(format!("Address {:#05X} changed", addr));
                                self.shell.output.push(format!("Old value = {:0>3} ({:#04X})", old, old));
                                self.shell.output.push(format!("New value = {:0>3} ({:#04X})", new, new));
                            }
                        }
                    }
                }
            }
            return false;
        }
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
        let (region, bottom) = {
            let rects = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(area.height.saturating_sub(1)),
                    Constraint::Length(1),
                ])
                .split(area);
            (rects[0], rects[1])
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

        (main, bottom, vm, logger)
    }

    fn main_areas(&self, mut area: Rect) -> (Rect, Rect, Rect, Rect, Rect, Rect) {
        let mut rects = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(area.width.saturating_sub(14) / 3),
                Constraint::Length(14),
                Constraint::Length(area.width.saturating_sub(area.width.saturating_sub(14) / 3)),
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
        if area.area() == 0 {
            return;
        }

        let (main_area, bottom_area, vm_area, _) = self.areas(area);
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

        let memory_block =
            Block::default()
                .title(" Memory ")
                .borders(base_border.union(Borders::BOTTOM).union(
                    if self.logging || self.dbg.vm_visible {
                        Borders::NONE
                    } else {
                        Borders::RIGHT
                    },
                ));
        let memory_lines_area = memory_block.inner(memory_area);
        
        if output_text_area.area() > 0 {
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
        }

        memory_block.render(memory_area, buf);
        MemoryWidget {
            vm: self.vm,
            disassembler: &self.dbg.disassembler,
            addr: self.dbg.memory_addr,
            active: self.dbg.memory_active,
            memory_addr_flags: &self.dbg.memory_addr_flags
        }
        .render(memory_lines_area, buf);

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
            ShellWidget::from(&self.dbg.shell).render(bottom_area, buf, &mut state.shell)
        } else if self.dbg.memory_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(bottom_area, bottom_area_style);
            Paragraph::new("Esc to exit memory navigation")
                .style(bottom_area_style)
                .render(bottom_area, buf);
        }
    }
}

pub struct MemoryWidget<'a> {
    disassembler: &'a Disassembler,
    vm: &'a VM,
    addr: u16,
    active: bool,
    memory_addr_flags: &'a [u8; PROGRAM_MEMORY_SIZE as usize],
}

impl<'a> MemoryWidget<'_> {
    const COMMENT_DELIM: &'static str = "# ";

    fn is_addr_rendered(&self, addr: u16) -> bool {
        let tag = self.disassembler.tags[addr as usize];
        tag >= InstructionTag::Proven
            || addr == 0
            || addr == self.addr
            || self.vm.interpreter().pc == addr
            || self.vm.interpreter().index == addr
            || !self
                .disassembler
                .tags
                .get(addr as usize - 1)
                .map_or(false, |&tag| tag >= InstructionTag::Proven)
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
        let flags = self.memory_addr_flags[addr as usize];

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

        let force_asm = addr == self.vm.interpreter().pc;

        let show_addr_content = tag >= InstructionTag::Valid || force_asm;
        let show_bin_comment =
            tag < InstructionTag::Proven && (tag < InstructionTag::Parsable || !force_asm);
        let show_addr_comment = show_bin_comment
            || show_addr_content && addr_comment.len() > MemoryWidget::COMMENT_DELIM.len();

        if show_addr_comment {
            if !show_addr_content {
                addr_content.clear();
            }

            let padding =
                (INSTRUCTION_COLUMNS + 4).saturating_sub(addr_header.len() + addr_opcode.len() + addr_content.len());

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

        let is_addr_pc = addr == self.vm.interpreter().pc;
        let is_addr_index = addr == self.vm.interpreter().index;
        let is_addr_selected = addr == self.addr && self.active;

        if is_addr_pc || is_addr_index || is_addr_selected {
            let line_len = spans.iter().fold(0, |len, span| len + span.width());
            if line_len < addr_line_width as usize {
                spans.push(Span::raw(" ".repeat(addr_line_width as usize - line_len)));
            }

            let highlight_color = if is_addr_selected {
                Color::White
            } else if is_addr_pc {
                Color::LightMagenta
            } else {
                Color::LightYellow
            };

            for span in spans.iter_mut() {
                (*span).style = span
                    .style
                    .add_modifier(Modifier::BOLD)
                    .bg(highlight_color)
                    .fg(Color::Black);
            }
        }

        Spans::from(spans)
    }
}

impl<'a> Widget for MemoryWidget<'_> {
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer) {
        if area.area() == 0 {
            return;
        }

        let mut addr_header = String::new();
        let mut addr_opcode = String::new();
        let mut addr_content = String::new();
        let mut addr_comment = String::new();

        let mut lines: Vec<Spans> = Vec::with_capacity(area.height as usize);
        let mut addr = self.addr;
        let mut addr_ahead = addr;

        let lines_ahead = (area.height - area.height / 2) as usize;

        while lines.len() < lines_ahead && addr < PROGRAM_MEMORY_SIZE {
            if self.is_addr_rendered(addr) {
                lines.push(self.addr_span(
                    addr,
                    area.width,
                    &mut addr_header,
                    &mut addr_opcode,
                    &mut addr_content,
                    &mut addr_comment,
                ));
                addr_ahead = addr;
            }
            addr += 1;
        }

        addr = self.addr.min(PROGRAM_MEMORY_SIZE);
        while addr > 0 && lines.len() < area.height as usize {
            addr -= 1;
            if self.is_addr_rendered(addr) {
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
        while addr < PROGRAM_MEMORY_SIZE - 1 && lines.len() < area.height as usize {
            addr += 1;
            if self.is_addr_rendered(addr) {
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
