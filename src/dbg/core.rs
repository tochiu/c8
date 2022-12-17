use super::{
    cli::*,
    hist::{History, HistoryWidget},
    mem::*,
    shell::*,
};

use crate::{
    asm::Disassembler,
    config::C8Config,
    run::{
        core::Runner,
        disp::{Display, DisplayWidget, DISPLAY_WINDOW_HEIGHT, DISPLAY_WINDOW_WIDTH},
        input::KEY_ORDERING,
        interp::{Instruction, Interpreter},
        vm::VM,
    },
};

use ansi_to_tui::IntoText;
use clap::Parser;
use crossterm::event::{Event, KeyCode, KeyEventKind};
use tui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Paragraph, StatefulWidget, Widget},
};

use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(super) enum Watchpoint {
    Pointer(MemoryPointer),
    Register(u8),
    Address(u16),
}

impl std::fmt::Display for Watchpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Watchpoint::Pointer(pointer) => write!(f, "{}", pointer.identifier()),
            Watchpoint::Register(vx) => write!(f, "v{:x}", vx),
            Watchpoint::Address(addr) => write!(f, "{:#05X}", addr),
        }
    }
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
            instruction: interp.fetch().and_then(Instruction::try_from).ok(),
        }
    }
}

impl WatchState {
    fn update(
        &mut self,
        interp: &Interpreter,
        watchpoints: &HashSet<Watchpoint>,
        event_queue: &mut Vec<DebugEvent>,
    ) {
        for &watchpoint in watchpoints.iter() {
            match watchpoint {
                Watchpoint::Pointer(pointer) => match pointer {
                    MemoryPointer::Index => {
                        let old_val = self.index;
                        let new_val = interp.index;

                        if old_val != new_val {
                            event_queue
                                .push(DebugEvent::WatchpointTrigger(watchpoint, old_val, new_val));
                        }
                    }
                    MemoryPointer::ProgramCounter => {
                        let old_val = self.pc;
                        let new_val = interp.pc;

                        if old_val != new_val {
                            event_queue
                                .push(DebugEvent::WatchpointTrigger(watchpoint, old_val, new_val));
                        }
                    }
                },
                Watchpoint::Register(vx) => {
                    let old_val = self.registers[vx as usize];
                    let new_val = interp.registers[vx as usize];

                    if old_val != new_val {
                        event_queue.push(DebugEvent::WatchpointTrigger(
                            watchpoint,
                            old_val as u16,
                            new_val as u16,
                        ));
                    }
                }
                Watchpoint::Address(addr) => {
                    let Some(&old_val) = self.addresses.get(&addr) else {
                        unreachable!("address watchpoint {:#05X} must store state in hashtable", addr)
                    };
                    let new_val = interp.memory[addr as usize];

                    if old_val != new_val {
                        self.addresses.insert(addr, new_val);
                        event_queue.push(DebugEvent::WatchpointTrigger(
                            watchpoint,
                            old_val as u16,
                            new_val as u16,
                        ));
                    }
                }
            }
        }

        self.pc = interp.pc;
        self.index = interp.index;
        self.registers.copy_from_slice(&interp.registers);
        self.instruction = interp.fetch().and_then(Instruction::try_from).ok();
    }
}

pub(super) enum DebugEvent {
    WatchpointTrigger(Watchpoint, u16, u16),
    BreakpointReached(u16),
}

pub struct Debugger {
    active: bool,

    history: History,
    history_active: bool,

    breakpoints: HashSet<u16>,
    watchpoints: HashSet<Watchpoint>,
    watch_state: WatchState,
    event_queue: Vec<DebugEvent>,

    disassembler: Disassembler,

    memory: Memory,
    memory_active: bool,
    memory_visible: bool,
    memory_widget_state: Cell<MemoryWidgetState>,

    keyboard_shows_qwerty: bool,

    runner_frequency: u16,

    shell: Shell,
    shell_input_active: bool,
    shell_output_active: bool,

    vm_visible: bool,
    vm_exception: Option<String>,
}

impl Debugger {
    pub fn new(vm: &VM, config: &C8Config) -> Self {
        let mut dbg = Debugger {
            active: false,

            history: Default::default(),
            history_active: false,

            breakpoints: Default::default(),
            watchpoints: Default::default(),
            watch_state: WatchState::from(vm.interpreter()),
            event_queue: Default::default(),

            disassembler: Disassembler::from(vm.interpreter().program.clone()),

            memory: Default::default(),
            memory_active: false,
            memory_visible: true,
            memory_widget_state: Default::default(),

            keyboard_shows_qwerty: true,

            runner_frequency: config.instruction_frequency,

            shell: Shell::new(),
            shell_input_active: true,
            shell_output_active: false,

            vm_visible: true,
            vm_exception: None,
        };

        dbg.disassembler.run();
        dbg.history.start_recording();
        dbg.activate(vm);
        dbg
    }

    pub fn is_active(&self) -> bool {
        self.active
    }

    pub fn frequency(&self) -> u16 {
        self.runner_frequency
    }

    fn activate(&mut self, vm: &VM) {
        if self.active {
            return;
        }

        self.shell.print("Paused.");
        self.shell.output_pc(vm.interpreter());
        self.active = true;
    }

    fn deactivate(&mut self) {
        if !self.active {
            return;
        }

        self.shell.print("Continuing.");
        self.active = false;
    }

    fn stepn(&mut self, vm: &mut VM, amt: usize, time_step: f32) -> usize {
        let mut amt_stepped = 0;

        vm.time_step = time_step;
        vm.clear_event_queue();
        self.history.clear_redo_history();
        for step in 0..amt {
            if !self.step(vm) {
                break;
            }
            amt_stepped = step + 1;
        }

        amt_stepped
    }

    fn redon(&mut self, vm: &mut VM, mut amt: usize) -> usize {
        amt = amt.min(self.history.redo_amount());
        let mut amt_stepped = 0;
        vm.clear_event_queue();
        for step in 0..amt {
            if !self.step(vm) {
                break;
            }
            amt_stepped = step + 1;
        }

        amt_stepped
    }

    pub fn step(&mut self, vm: &mut VM) -> bool {
        if let Some(e) = self.vm_exception.as_ref() {
            self.shell.error(e);
            self.activate(vm);
            return false;
        }

        let step_result = if self.history.is_recording() {
            self.history.step(vm)
        } else {
            vm.handle_inputs();
            vm.step()
        };

        let mut should_continue = step_result.is_ok();
        if let Err(e) = step_result {
            self.shell.error(&e);
            self.vm_exception = Some(e);
            self.activate(vm);
        }

        let interp = vm.interpreter();

        // update memory draw read write execute (drwx) flags
        self.memory.step(
            self.memory_widget_state.get_mut(),
            interp,
            self.watch_state.pc,
            self.watch_state.index,
            self.watch_state.instruction,
        );

        // update disassembler
        match self.watch_state.instruction {
            Some(Instruction::Store(bytes)) => {
                self.disassembler
                    .update(vm.interpreter(), self.watch_state.index, bytes as u16);
            }
            Some(Instruction::StoreDecimal(_)) => {
                self.disassembler
                    .update(vm.interpreter(), self.watch_state.index, 3);
            }
            _ => (),
        }

        // update watch state
        self.watch_state
            .update(interp, &self.watchpoints, &mut self.event_queue);

        // update breakpoints
        if self.breakpoints.contains(&interp.pc) {
            self.event_queue
                .push(DebugEvent::BreakpointReached(interp.pc));
        }

        // handle debug events emitted
        if !self.event_queue.is_empty() {
            should_continue = false;
            self.activate(vm);
            for debug_event in self.event_queue.drain(..) {
                match debug_event {
                    DebugEvent::BreakpointReached(addr) => {
                        self.shell
                            .print(format!("Breakpoint {:#05X} reached", addr));
                    }
                    DebugEvent::WatchpointTrigger(watchpoint, old, new) => match watchpoint {
                        Watchpoint::Pointer(pointer) => {
                            let identifier = match pointer {
                                MemoryPointer::Index => "i",
                                MemoryPointer::ProgramCounter => "pc",
                            };

                            self.shell.print(format!("Pointer {} changed", identifier));
                            self.shell.print(format!("Old value = {:#05X}", old));
                            self.shell.print(format!("New value = {:#05X}", new));
                        }
                        Watchpoint::Register(register) => {
                            self.shell
                                .print(format!("Register v{:x} changed", register));
                            self.shell
                                .print(format!("Old value = {:0>3} ({:#05X})", old, old));
                            self.shell
                                .print(format!("New value = {:0>3} ({:#05X})", new, new));
                        }
                        Watchpoint::Address(addr) => {
                            self.shell.print(format!("Address {:#05X} changed", addr));
                            self.shell
                                .print(format!("Old value = {:0>3} ({:#04X})", old, old));
                            self.shell
                                .print(format!("New value = {:0>3} ({:#04X})", new, new));
                        }
                    },
                }
            }
        }

        should_continue
    }

    pub fn handle_input_event(&mut self, event: Event, runner: &mut Runner, vm: &mut VM) -> bool {
        let mut sink_event = false;

        'handler: {
            let Event::Key(key_event) = event else {
                break 'handler;
            };

            let (KeyEventKind::Press | KeyEventKind::Repeat) = key_event.kind else {
                break 'handler;
            };

            if self.active {
                if self.shell_input_active {
                    sink_event = self.shell.handle_input_key_event(key_event);
                } else if self.shell_output_active {
                    sink_event = self
                        .shell
                        .handle_output_key_event(key_event, &mut self.shell_output_active);
                    if !self.shell_output_active {
                        self.shell_input_active = true;
                    }
                } else if self.memory_active {
                    sink_event = self.memory.handle_key_event(
                        key_event,
                        self.memory_widget_state.get_mut(),
                        &mut self.memory_active,
                    );
                    if !self.memory_active {
                        self.shell_input_active = true;
                    }
                } else if self.history_active {
                    let mut payload = (0, false);
                    sink_event = self.history.handle_key_event(
                        key_event,
                        &mut self.history_active,
                        &mut payload,
                    );
                    if !self.history_active {
                        self.shell_input_active = true;
                    }
                    let (seek_amt, seek_forwards) = payload;
                    if seek_amt > 0 {
                        if seek_forwards {
                            self.redon(vm, seek_amt);
                        } else {
                            self.history.undo(vm, seek_amt);
                            self.memory_widget_state.get_mut().poke();
                        }
                    }
                }
            } else if key_event.code == KeyCode::Esc {
                log::info!("c8vm interrupt!");
                sink_event = true;
                if let Err(e) = runner.pause() {
                    log::warn!("Failed to pause runner: {}", e);
                    break 'handler;
                }
                self.activate(vm);
            }
        }

        for input in self.shell.try_recv().collect::<Vec<_>>() {
            if !self.active {
                break;
            }

            self.shell.echo(&input);

            let Ok(mut args) = shell_words::split(&input) else {
                self.shell.print("Failed to parse command: mismatched quotes");
                continue
            };

            // Aliasing that I was too lazy to implement idiomtically in clap
            if args.first().map_or(false, |cmd| cmd == "h") {
                args[0] = "help".into();
            } else if args
                .first()
                .map_or(false, |cmd| cmd == "version" || cmd == "v")
            {
                args[0] = "--version".into();
            }

            match DebugCli::try_parse_from(args) {
                Ok(DebugCli { command }) => {
                    self.handle_command(command, runner, vm);
                }
                Err(cli_err) => {
                    self.shell.print("");
                    match cli_err.render().ansi().to_string().into_text() {
                        Ok(text) => {
                            for line in text {
                                self.shell.print(line);
                            }
                            self.shell.print(vec![
                                Span::raw("Execute "),
                                Span::styled(
                                    "output",
                                    Style::default()
                                        .fg(Color::Yellow)
                                        .add_modifier(Modifier::BOLD),
                                ),
                                Span::styled(
                                    " [aliases: o, out]",
                                    Style::default().fg(Color::Yellow),
                                ),
                                Span::raw(" to view the full output"),
                            ]);
                            self.shell.print("");
                        }
                        Err(text_err) => {
                            log::error!(
                                "Failed to parse ANSI text to tui::text::Text: {}",
                                text_err
                            );
                            for line in cli_err.to_string().lines() {
                                self.shell.print(line.to_string());
                            }
                        }
                    }
                }
            }
        }

        sink_event
    }

    fn handle_command(&mut self, command: DebugCliCommand, runner: &mut Runner, vm: &mut VM) {
        match command {
            DebugCliCommand::Continue => {
                if let Some(e) = self.vm_exception.as_ref() {
                    self.shell.error(e);
                    return;
                }

                if let Err(e) = runner.resume() {
                    log::warn!("Failed to resume runner: {}", e);
                    return;
                }

                self.deactivate();
                self.history.clear_redo_history();
                vm.clear_event_queue();
                vm.keyboard_mut().clear();
            }
            DebugCliCommand::Step { amount } => {
                let amt_stepped = self.stepn(
                    vm,
                    amount,
                    1.0 / runner.config().instruction_frequency as f32,
                );

                if amt_stepped > 1 {
                    self.shell.print(format!("Stepped {} times", amt_stepped));
                } else if amt_stepped == 1 {
                    self.shell.output_pc(vm.interpreter());
                }
            }
            DebugCliCommand::Hertz { hertz } => {
                if let Err(e) = runner.set_instruction_frequency(hertz) {
                    self.shell.error(e);
                    return;
                }

                self.runner_frequency = hertz;
                self.shell.print(format!("Set frequency to {}Hz", hertz));
            }
            DebugCliCommand::Record => {
                if self.history.is_recording() {
                    self.shell.print("Already recording VM state");
                    return;
                }
                self.history.start_recording();
                self.shell.print("Recording VM state");
            }
            DebugCliCommand::Stop => {
                if self.history.is_recording() {
                    self.history.stop_recording();
                    self.shell.print("Stopped recording VM state");
                } else {
                    self.shell.print("Already not recording VM state");
                }
            }
            DebugCliCommand::Redo { amount } => {
                let amt_stepped = self.redon(vm, amount);
                if amt_stepped > 1 {
                    self.shell
                        .print(format!("Redid {} instructions", amt_stepped));
                } else if amt_stepped == 1 {
                    self.shell.output_pc(vm.interpreter());
                }
            }
            DebugCliCommand::Undo { amount } => {
                let amt_rewinded = self.history.undo(vm, amount);
                if amt_rewinded > 0 {
                    self.vm_exception = None;
                    self.memory_widget_state.get_mut().poke();
                    if amt_rewinded > 1 {
                        self.shell.print(format!("Rewound {} times", amt_rewinded));
                    } else {
                        self.shell.output_pc(vm.interpreter());
                    }
                } else {
                    self.shell.print("Nothing to rewind");
                }
            }
            DebugCliCommand::History => {
                if !self.history.is_recording() {
                    self.shell.print("Not recording VM state");
                    return;
                }
                self.history_active = true;
                self.shell_input_active = false;
            }
            DebugCliCommand::Output => {
                self.shell_output_active = true;
                self.shell_input_active = false;
            }
            DebugCliCommand::Memory => {
                self.memory_active = true;
                self.shell_input_active = false;
            }
            DebugCliCommand::Goto { location } => {
                let address = match location {
                    GotoOption::SemanticLocation(SemanticLocation::Start) => 0,
                    GotoOption::SemanticLocation(SemanticLocation::End) => {
                        vm.interpreter().memory.len() as u16 - 1
                    }
                    GotoOption::Pointer(Pointer::Pc) => vm.interpreter().pc,
                    GotoOption::Pointer(Pointer::I) => vm.interpreter().index,
                    GotoOption::Address(address) => address,
                };

                if (address as usize) >= vm.interpreter().memory.len() {
                    self.shell.print("Address is out of bounds");
                    return;
                }

                self.memory_widget_state.get_mut().set_focus(address);
            }
            DebugCliCommand::Follow { pointer } => {
                self.memory.follow = Some(match pointer {
                    Pointer::Pc => MemoryPointer::ProgramCounter,
                    Pointer::I => MemoryPointer::Index,
                });
                self.memory_widget_state.get_mut().poke();
            }
            DebugCliCommand::Unfollow => {
                if let Some(pointer) = self.memory.follow {
                    self.memory.follow = None;
                    self.shell
                        .print(format!("Unfollowing {}", pointer.identifier()));
                } else {
                    self.shell.print("Already unfollowed");
                }
            }
            DebugCliCommand::Break { address } => {
                if (address as usize) >= vm.interpreter().memory.len() {
                    self.shell.print("Address is out of bounds");
                    return;
                }

                if self.breakpoints.insert(address) {
                    self.shell
                        .print(format!("Breakpoint set at {:#05X}", address));
                } else {
                    self.shell
                        .print(format!("Breakpoint set at {:#05X} already exists", address));
                }
            }
            DebugCliCommand::Watch { watchpoint } => {
                let watchpoint = match watchpoint {
                    WatchOption::Pointer(Pointer::Pc) => {
                        Watchpoint::Pointer(MemoryPointer::ProgramCounter)
                    }
                    WatchOption::Pointer(Pointer::I) => Watchpoint::Pointer(MemoryPointer::Index),
                    WatchOption::Register(register) => Watchpoint::Register(register.to_index()),
                    WatchOption::Address(address) => {
                        if (address as usize) >= vm.interpreter().memory.len() {
                            self.shell.print("Address is out of bounds");
                            return;
                        }

                        Watchpoint::Address(address)
                    }
                };

                if self.watchpoints.insert(watchpoint) {
                    if let Watchpoint::Address(address) = watchpoint {
                        self.watch_state
                            .addresses
                            .insert(address, vm.interpreter().memory[address as usize]);
                    }
                    self.shell
                        .print(format!("Watchpoint {} is set", watchpoint));
                } else {
                    self.shell
                        .print(format!("Watchpoint {} already exists", watchpoint));
                }
            }
            DebugCliCommand::Show { view } => match view {
                ShowHideOption::Display => {
                    self.vm_visible = true;
                }
                ShowHideOption::Memory { verbose } => {
                    self.memory_visible = true;
                    self.memory.verbose = verbose;
                }
            },
            DebugCliCommand::Hide { view } => match view {
                ShowHideOption::Display => {
                    self.vm_visible = false;
                }
                ShowHideOption::Memory { verbose } => {
                    if verbose {
                        self.memory.verbose = false;
                    } else {
                        self.memory_visible = false;
                    }
                }
            },
            DebugCliCommand::Info { what } => match what {
                WatchBreakOption::Break => {
                    if self.breakpoints.is_empty() {
                        self.shell.print("No breakpoints set");
                    } else {
                        self.shell.print("Breakpoints:");
                        for breakpoint in self.breakpoints.iter() {
                            self.shell.print(format!("    - {:#05X}", breakpoint));
                        }
                    }
                }
                WatchBreakOption::Watch => {
                    if self.watchpoints.is_empty() {
                        self.shell.print("No watchpoints set");
                    } else {
                        self.shell.print("Watchpoints:");
                        for watchpoint in self.watchpoints.iter() {
                            self.shell.print(format!(
                                "    - {}",
                                match watchpoint {
                                    Watchpoint::Register(register) => format!("v{:x}", register),
                                    Watchpoint::Pointer(pointer) =>
                                        pointer.identifier().to_string(),
                                    Watchpoint::Address(addr) => format!("{:#05X}", addr),
                                }
                            ));
                        }
                    }
                }
            },
            DebugCliCommand::Key { command } => match command {
                KeyCommand::Down { key } => {
                    vm.keyboard_mut().handle_focus();
                    vm.keyboard_mut().handle_key_down(key);
                    self.shell.print(format!(
                        "Key \"{:X}\" ({}) down",
                        key.to_code(),
                        key.to_str()
                    ));
                }
                KeyCommand::Up { key } => {
                    vm.keyboard_mut().handle_key_up(key);
                    self.shell
                        .print(format!("Key \"{:X}\" ({}) up", key.to_code(), key.to_str()));
                }
                KeyCommand::Press { key } => {
                    vm.keyboard_mut().handle_key_up(key);
                    vm.keyboard_mut().handle_focus();
                    vm.keyboard_mut().handle_key_down(key);
                    vm.keyboard_mut().handle_key_up(key);
                    self.shell.print(format!(
                        "Key \"{:X}\" ({}) pressed",
                        key.to_code(),
                        key.to_str()
                    ));
                }
                KeyCommand::Switch => {
                    self.keyboard_shows_qwerty = !self.keyboard_shows_qwerty;
                    self.shell.print(format!(
                        "Keyboard layout switched to {}",
                        if self.keyboard_shows_qwerty {
                            "QWERTY"
                        } else {
                            "CHIP-8"
                        }
                    ));
                }
            },
            DebugCliCommand::Clear { command } => match command {
                ClearCommand::Keyboard => {
                    vm.keyboard_mut().clear();
                    self.shell.print("Cleared keyboard");
                }
                ClearCommand::Watch { watchpoint } => {
                    let watchpoint = match watchpoint {
                        WatchOption::Pointer(Pointer::Pc) => {
                            Watchpoint::Pointer(MemoryPointer::ProgramCounter)
                        }
                        WatchOption::Pointer(Pointer::I) => {
                            Watchpoint::Pointer(MemoryPointer::Index)
                        }
                        WatchOption::Register(register) => {
                            Watchpoint::Register(register.to_index())
                        }
                        WatchOption::Address(address) => {
                            if (address as usize) >= vm.interpreter().memory.len() {
                                self.shell.print("Address is out of bounds");
                                return;
                            }

                            Watchpoint::Address(address)
                        }
                    };

                    if self.watchpoints.remove(&watchpoint) {
                        if let Watchpoint::Address(addr) = watchpoint {
                            self.watch_state.addresses.remove(&addr);
                        }
                        self.shell
                            .print(format!("Cleared watchpoint {}", watchpoint));
                    } else {
                        self.shell.print(format!("No watchpoint {}", watchpoint));
                    }
                }
                ClearCommand::Break {
                    breakpoint: address,
                } => {
                    if self.breakpoints.remove(&address) {
                        self.shell
                            .print(format!("Cleared breakpoint at {:#05X}", address));
                    } else {
                        self.shell
                            .print(format!("No breakpoint at {:#05X}", address));
                    }
                }
                ClearCommand::All { what } => match what {
                    WatchBreakOption::Break => {
                        self.breakpoints.clear();
                        self.shell.print("Cleared all breakpoints");
                    }
                    WatchBreakOption::Watch => {
                        self.watchpoints.clear();
                        self.watch_state.addresses.clear();
                        self.shell.print("Cleared all watchpoints");
                    }
                },
            },
            DebugCliCommand::Dump { what } => match what {
                DumpOption::Memory { path } => {
                    let path_string = path.as_path().display().to_string();
                    match (MemoryWidget {
                        active: self.memory_active,
                        memory: &self.memory,
                        watchpoints: &self.watchpoints,
                        breakpoints: &self.breakpoints,
                        interpreter: vm.interpreter(),
                        disassembler: &self.disassembler,
                    }
                    .write_to_file(path))
                    {
                        Ok(()) => self
                            .shell
                            .print(format!("Dumped memory to \"{}\"", path_string)),
                        Err(e) => self.shell.print(format!(
                            "Failed to dump memory to \"{}\": {}",
                            path_string, e
                        )),
                    };
                }
            },
        }
    }
}

#[derive(Default)]
pub struct DebuggerWidgetState {
    input: InputWidgetState,
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
            InputWidget::from(&self.dbg.shell).cursor_position(area, &mut state.input)
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
                .constraints(if self.dbg.vm_visible && !self.dbg.shell_output_active {
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
                .constraints(
                    if self.dbg.vm_visible && !self.dbg.shell_output_active && self.logging {
                        [
                            Constraint::Length(DISPLAY_WINDOW_HEIGHT),
                            Constraint::Length(region.height.saturating_sub(DISPLAY_WINDOW_HEIGHT)),
                        ]
                    } else if self.logging {
                        [Constraint::Percentage(0), Constraint::Percentage(100)]
                    } else {
                        [Constraint::Percentage(100), Constraint::Percentage(0)]
                    },
                )
                .split(region);
            (rects[0], rects[1])
        };

        (main, bottom, vm, logger)
    }

    fn dbg_areas(&self, mut area: Rect) -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) {
        if self.dbg.shell_output_active {
            return (
                Rect::default(),
                Rect::default(),
                Rect::default(),
                Rect::default(),
                Rect::default(),
                Rect::default(),
                area,
                Rect::default(),
            );
        }

        let mut rects = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(if self.dbg.memory_active {
                [
                    Constraint::Length(0),
                    Constraint::Length(15),
                    Constraint::Length(area.width.saturating_sub(area.width.saturating_sub(15))),
                ]
            } else if self.dbg.memory_visible {
                [
                    Constraint::Length(area.width.saturating_sub(15) / 3),
                    Constraint::Length(15),
                    Constraint::Length(
                        area.width.saturating_sub(area.width.saturating_sub(15) / 3),
                    ),
                ]
            } else {
                [
                    Constraint::Length(area.width.saturating_sub(15)),
                    Constraint::Length(15),
                    Constraint::Length(0),
                ]
            })
            .split(area);

        let (left_area, memory_area) = (rects[0], rects[2]);

        let (history_area, output_area) = {
            if self.dbg.history_active {
                (left_area, Rect::default())
            } else if self.dbg.history.is_recording() && self.dbg.shell_input_active {
                let rects = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
                    .split(left_area);
                (rects[0], rects[1])
            } else if self.dbg.shell_input_active {
                (Rect::default(), left_area)
            } else {
                (Rect::default(), Rect::default())
            }
        };

        area = rects[1];
        rects = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(10),
                Constraint::Length(3),
                Constraint::Length(17),
                Constraint::Length(4),
                Constraint::Length(1 + self.vm.interpreter().stack.len().max(1) as u16),
            ])
            .split(area);

        let (keyboard_area, pointer_area, register_area, timer_area, stack_area) =
            (rects[0], rects[1], rects[2], rects[3], rects[4]);

        (
            memory_area,
            keyboard_area,
            pointer_area,
            register_area,
            timer_area,
            stack_area,
            output_area,
            history_area,
        )
    }

    fn can_draw_shell(&self, area: Rect) -> bool {
        self.dbg.shell_input_active && area.area() > 0
    }
}

impl<'a> StatefulWidget for DebuggerWidget<'_> {
    type State = DebuggerWidgetState;
    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if area.area() == 0 {
            return;
        }

        let (main_area, bottom_area, vm_area, _) = self.areas(area);
        let (
            memory_area,
            keyboard_area,
            pointer_area,
            register_area,
            timer_area,
            stack_area,
            output_area,
            history_area,
        ) = self.dbg_areas(main_area);

        let base_border = Borders::TOP.union(Borders::LEFT);

        // VM
        if self.dbg.vm_visible {
            DisplayWidget {
                display: self.vm_disp,
                logging: false,
            }
            .render(vm_area, buf);
        }

        // Output
        let output_block = Block::default().title(" Output ").borders(Borders::TOP);
        self.dbg
            .shell
            .as_output_widget()
            .render(output_block.inner(output_area), buf);
        output_block.render(output_area, buf);

        // History
        HistoryWidget {
            history: &self.dbg.history,
            active: self.dbg.history_active,
            border: if self.dbg.shell_input_active {
                Borders::TOP
            } else {
                Borders::TOP.union(Borders::BOTTOM)
            },
        }
        .render(history_area, buf);

        // Memory
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
        let mut memory_state = self.dbg.memory_widget_state.take();
        MemoryWidget {
            active: self.dbg.memory_active,
            memory: &self.dbg.memory,
            watchpoints: &self.dbg.watchpoints,
            breakpoints: &self.dbg.breakpoints,
            interpreter: self.vm.interpreter(),
            disassembler: &self.dbg.disassembler,
        }
        .render(memory_block.inner(memory_area), buf, &mut memory_state);
        memory_block.render(memory_area, buf);
        self.dbg.memory_widget_state.set(memory_state);

        let interp = self.vm.interpreter();

        //Keyboard
        let (key_down_state, key_just_down, key_just_up) = self.vm.keyboard().state();
        let just_key = self.vm.interpreter().pick_key(key_just_down, key_just_up);

        let mut keyboard_span_iter = KEY_ORDERING.iter().map(|key| {
            Span::styled(
                if self.dbg.keyboard_shows_qwerty {
                    format!(" {} ", key.to_str())
                } else {
                    format!(" {:X} ", key.to_code())
                },
                if key_down_state >> key.to_code() as u16 & 1 == 1 {
                    Style::default()
                        .fg(Color::Black)
                        .bg(if just_key == &Some(key.to_code()) {
                            Color::Yellow
                        } else {
                            Color::White
                        })
                } else {
                    Style::default().fg(if just_key == &Some(key.to_code()) {
                        Color::Yellow
                    } else {
                        Color::Reset
                    })
                },
            )
        });

        let mut keyboard_row_spans: Vec<Spans> = Vec::with_capacity(4);
        let mut keyboard_row: Vec<Span> = Vec::with_capacity(5);
        keyboard_row.push(Span::raw(" "));
        while let Some(span) = keyboard_span_iter.next() {
            keyboard_row.push(span);
            if keyboard_row.len() == 5 {
                keyboard_row_spans.push(Spans::from(""));
                keyboard_row_spans.push(Spans::from(keyboard_row.clone()));
                keyboard_row.clear();
                keyboard_row.push(Span::raw(" "));
            }
        }

        Paragraph::new(keyboard_row_spans)
            .block(Block::default().title(" Keyboard ").borders(base_border))
            .render(keyboard_area, buf);

        // Pointers
        Paragraph::new(vec![
            Spans::from(format!(
                "{}pc {:#05X}",
                if self
                    .dbg
                    .watchpoints
                    .contains(&Watchpoint::Pointer(MemoryPointer::ProgramCounter))
                {
                    "*"
                } else {
                    "-"
                },
                interp.pc
            )),
            Spans::from(format!(
                "{}i  {:#05X}",
                if self
                    .dbg
                    .watchpoints
                    .contains(&Watchpoint::Pointer(MemoryPointer::Index))
                {
                    "*"
                } else {
                    "-"
                },
                interp.index
            )),
        ])
        .block(Block::default().title(" Pointers ").borders(base_border))
        .render(pointer_area, buf);

        // Registers
        Paragraph::new(
            interp
                .registers
                .iter()
                .enumerate()
                .map(|(i, val)| {
                    Spans::from(format!(
                        "{}v{:x} {:0>3} ({:#04X})",
                        if self
                            .dbg
                            .watchpoints
                            .contains(&Watchpoint::Register(i as u8))
                        {
                            "*"
                        } else {
                            "-"
                        },
                        i,
                        val,
                        val
                    ))
                })
                .collect::<Vec<_>>(),
        )
        .block(Block::default().title(" Registers ").borders(base_border))
        .render(register_area, buf);

        // Timers
        Paragraph::new(vec![
            Spans::from(format!("-sound {:0>7.3}", self.vm.sound_timer())),
            Spans::from(format!("-delay {:0>7.3}", self.vm.delay_timer())),
            Spans::from(format!("   |-> {:0>3}", interp.input.delay_timer)),
        ])
        .block(Block::default().title(" Timers ").borders(base_border))
        .render(timer_area, buf);

        // Stack
        Paragraph::new(
            interp
                .stack
                .iter()
                .enumerate()
                .map(|(i, addr)| Spans::from(format!("#{:0>2} {:#05X}", i, addr)))
                .collect::<Vec<_>>(),
        )
        .block(
            Block::default()
                .title(" Stack ")
                .borders(base_border.union(Borders::BOTTOM)),
        )
        .render(stack_area, buf);

        // Bottom (Command line or messages)
        if self.can_draw_shell(area) {
            InputWidget::from(&self.dbg.shell).render(bottom_area, buf, &mut state.input)
        } else if self.dbg.shell_output_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(bottom_area, bottom_area_style);
            Paragraph::new("Esc to exit output navigation")
                .style(bottom_area_style)
                .render(bottom_area, buf);
        } else if self.dbg.memory_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(bottom_area, bottom_area_style);
            Paragraph::new("Esc to exit memory navigation")
                .style(bottom_area_style)
                .render(bottom_area, buf);
        } else if self.dbg.history_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(bottom_area, bottom_area_style);
            Paragraph::new("Esc to exit history navigation")
                .style(bottom_area_style)
                .render(bottom_area, buf);
        }
    }
}
