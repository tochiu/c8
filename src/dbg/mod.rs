pub mod cli;
pub mod hist;
pub mod mem;
pub mod shell;

use {
    cli::*,
    hist::{History, HistoryWidget},
    mem::*,
    shell::*,
};

use crate::{
    asm::Disassembler,
    ch8::{
        disp::DisplayMode,
        input::KEY_ORDERING,
        instruct::Instruction,
        interp::Interpreter,
        rom::RomKind,
        run::Runner,
        vm::{VM, VM_FRAME_RATE},
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
enum Watchpoint {
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
            instruction: interp.instruction(),
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
    }
}

enum DebugEvent {
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
    disassembler_needs_update: bool,

    memory: Memory,
    memory_active: bool,
    memory_visible: bool,
    memory_widget_state: Cell<MemoryWidgetState>,

    keyboard_shows_qwerty: bool,

    runner_target_execution_frequency: u32,

    shell: Shell,
    shell_input_active: bool,
    shell_output_active: bool,

    vm_visible: bool,
    vm_exception: Option<String>,
    vm_executing: bool,
}

impl Debugger {
    pub fn new(vm: &VM, initial_target_execution_frequency: u32) -> Self {
        let mut dbg = Debugger {
            active: false,

            history: History::new(vm.interpreter().rom.config),
            history_active: false,

            breakpoints: Default::default(),
            watchpoints: Default::default(),
            watch_state: WatchState::from(vm.interpreter()),
            event_queue: Default::default(),

            disassembler: Disassembler::from(vm.interpreter().rom.clone()),
            disassembler_needs_update: false,

            memory: Memory::from(vm.interpreter().memory.as_slice()), //Default::default(),
            memory_active: false,
            memory_visible: true,
            memory_widget_state: Default::default(),

            keyboard_shows_qwerty: true,

            runner_target_execution_frequency: initial_target_execution_frequency,

            shell: Shell::new(),
            shell_input_active: true,
            shell_output_active: false,

            vm_visible: true,
            vm_exception: None,
            vm_executing: true,
        };

        dbg.disassembler.run();
        dbg.activate(vm);
        dbg
    }

    pub fn is_active(&self) -> bool {
        self.active
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

    fn stepn(&mut self, vm: &mut VM, amt: usize, cycles_per_frame: u32) -> usize {
        let mut amt_stepped = 0;

        vm.set_cycles_per_frame(cycles_per_frame);
        vm.clear_event_queue();
        self.history.clear_redo_history();
        for step in 0..amt {
            if !self.step(vm, 1) {
                break;
            }
            amt_stepped = step + 1;
        }

        amt_stepped
    }

    fn redon(&mut self, vm: &mut VM, mut amt: usize) -> usize {
        amt = amt.min(self.history.redo_amount());
        vm.clear_event_queue();
        for step in 0..amt {
            if !self.step(vm, 1) {
                return step // TODO: if redo has issues it still returns 1 more than actually redone because it still did a step
            }
            self.history.restore_external_state(vm);
        }

        amt
    }

    fn step_once(&mut self, vm: &mut VM) -> bool {
        let mut should_continue = match self.history.step(vm, &mut self.memory.access_flags) {
            Ok(cont) => {
                if !cont {
                    self.shell.print("Program has finished executing.");
                    self.vm_executing = false;
                    self.activate(vm);
                }
                cont
            }
            Err(e) => {
                self.shell.error(&e);
                self.vm_executing = false;
                self.vm_exception = Some(e);
                self.activate(vm);
                false
            }
        };

        // update disassembler
        match self.watch_state.instruction {
            Some(Instruction::Store(vx)) => {
                self.disassembler_needs_update |= self.disassembler.needs_rerun(
                    vm.interpreter(),
                    self.watch_state.index,
                    vx as u16 + 1,
                );
            }
            Some(Instruction::StoreRange(vstart, vend)) => {
                self.disassembler_needs_update |= self.disassembler.needs_rerun(
                    vm.interpreter(),
                    self.watch_state.index,
                    vstart.abs_diff(vend) as u16 + 1,
                );
            }
            Some(Instruction::StoreBinaryCodedDecimal(_)) => {
                self.disassembler_needs_update |=
                    self.disassembler
                        .needs_rerun(vm.interpreter(), self.watch_state.index, 3);
            }
            _ => (),
        }

        // update watch state
        self.watch_state
            .update(vm.interpreter(), &self.watchpoints, &mut self.event_queue);

        // update breakpoints
        if self.breakpoints.contains(&vm.interpreter().pc) {
            self.event_queue
                .push(DebugEvent::BreakpointReached(vm.interpreter().pc));
        }

        if !self.event_queue.is_empty() {
            should_continue = false;
            self.activate(vm);
        }

        should_continue
    }

    pub fn step(&mut self, vm: &mut VM, amt: usize) -> bool {
        if let Some(e) = self.vm_exception.as_ref() {
            self.shell.error(e);
            self.activate(vm);
            return false;
        }

        if !self.vm_executing {
            self.shell.print("Program has finished executing.");
            self.activate(vm);
            return false;
        }

        vm.flush_external_input();

        let mut should_continue = self.step_once(vm);

        vm.clear_ephemeral_state();
        vm.flush_external_input();

        if should_continue {
            for _ in 0..amt - 1 {
                if !self.step_once(vm) {
                    should_continue = false;
                    break;
                }
            }
        }

        self.memory_widget_state.get_mut().poke();

        // handle debug events emitted
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
                            self.history.undo(vm, seek_amt, &mut self.memory.access_flags);
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
                    self.runner_target_execution_frequency / VM_FRAME_RATE,
                );

                if amt_stepped > 1 {
                    self.shell.print(format!("Stepped {} times", amt_stepped));
                } else if amt_stepped == 1 {
                    self.shell.output_pc(vm.interpreter());
                }
            }

            DebugCliCommand::Hertz { mut hertz } => {
                if let Err(e) = runner.set_execution_frequency(hertz) {
                    self.shell.error(e);
                    return;
                }

                hertz -= hertz % VM_FRAME_RATE;
                hertz = hertz.max(VM_FRAME_RATE);

                self.runner_target_execution_frequency = hertz;
                self.shell
                    .print(format!("Set execution frequency to {}Hz", hertz));
            }

            DebugCliCommand::Redo { amount } => {
                if self.history.redo_amount() == 0 {
                    self.shell.print("Nothing to redo");
                    return;
                }

                let amt_stepped = self.redon(vm, amount);
                if amt_stepped > 1 {
                    self.shell
                        .print(format!("Redid {} instructions", amt_stepped));
                } else if amt_stepped == 1 {
                    self.shell.output_pc(vm.interpreter());
                }
            }

            DebugCliCommand::Undo { amount } => {
                let amt_rewinded = self.history.undo(vm, amount, &mut self.memory.access_flags);
                if amt_rewinded > 0 {
                    self.vm_exception = None;
                    self.vm_executing = true;
                    self.memory_widget_state.get_mut().poke();
                    if amt_rewinded > 1 {
                        self.shell
                            .print(format!("Undid {} instructions", amt_rewinded));
                    } else {
                        self.shell.output_pc(vm.interpreter());
                    }
                } else {
                    self.shell.print("Nothing to undo");
                }
            }

            DebugCliCommand::History => {
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
                        (vm.interpreter().memory.len() - 1) as u16
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

    pub fn prepare_render(&mut self) {
        if self.active && self.disassembler_needs_update {
            self.disassembler_needs_update = false;
            self.disassembler.rerun();
        }
    }
}

pub struct DebuggerWidgetState {
    input: InputWidgetState,
    pub logger_area: Rect,
    pub logger_border: Borders,
}

impl Default for DebuggerWidgetState {
    fn default() -> Self {
        Self {
            input: InputWidgetState::default(),
            logger_area: Rect::default(),
            logger_border: Borders::ALL,
        }
    }
}

pub struct DebuggerWidget<'a> {
    pub logging: bool,
    pub dbg: &'a Debugger,
    pub vm: &'a VM,
}

#[derive(Default)]
pub struct DebuggerWidgetAreas {
    pub history: Rect,
    pub output: Rect,
    pub keyboard: Rect,
    pub pointers: Rect,
    pub registers: Rect,
    pub timers: Rect,
    pub stack: Rect,
    pub memory: Rect,
    pub audio: Rect,
    pub flags: Rect,
    pub planes: Rect,
    pub display: Rect,
    pub logger: Rect,
    pub command_line: Rect,
}

pub struct DebuggerWidgetBorders {
    pub history: Borders,
    pub output: Borders,
    pub keyboard: Borders,
    pub pointers: Borders,
    pub registers: Borders,
    pub timers: Borders,
    pub stack: Borders,
    pub memory: Borders,
    pub audio: Borders,
    pub flags: Borders,
    pub planes: Borders,
    pub display: Borders,
    pub logger: Borders,
    pub command_line: Borders,
}

impl Default for DebuggerWidgetBorders {
    fn default() -> Self {
        Self {
            history: Borders::NONE,
            output: Borders::NONE,
            keyboard: Borders::NONE,
            pointers: Borders::NONE,
            registers: Borders::NONE,
            timers: Borders::NONE,
            stack: Borders::NONE,
            memory: Borders::NONE,
            audio: Borders::NONE,
            flags: Borders::NONE,
            planes: Borders::NONE,
            display: Borders::NONE,
            logger: Borders::NONE,
            command_line: Borders::NONE,
        }
    }
}

impl<'a> DebuggerWidget<'a> {
    const GENERAL_STATE_COLUMN_WIDTH: u16 = 15;
    const KEYBOARD_STATE_HEIGHT: u16 = 10;
    const POINTERS_STATE_HEIGHT: u16 = 3;
    const REGISTERS_STATE_HEIGHT: u16 = 17;
    const TIMERS_STATE_HEIGHT: u16 = 5;
    const AUDIO_STATE_HEIGHT: u16 = 8;
    const SCHIP_FLAG_STATE_HEIGHT: u16 = 9;
    const XOCHIP_FLAG_STATE_HEIGHT: u16 = 17;
    const PLANES_STATE_HEIGHT: u16 = 4;

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

    fn build_layout(&self, terminal_area: Rect) -> (DebuggerWidgetAreas, DebuggerWidgetBorders) {
        let [above_command_line_area, command_line_area] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(terminal_area.height.saturating_sub(1)),
                Constraint::Length(1),
            ])
            .split(terminal_area)[..] else { unreachable!() };
        let command_line_borders = Borders::NONE;

        if self.dbg.shell_output_active {
            return (
                DebuggerWidgetAreas {
                    output: above_command_line_area,
                    command_line: command_line_area,
                    ..Default::default()
                },
                DebuggerWidgetBorders {
                    output: Borders::TOP,
                    command_line: command_line_borders,
                    ..Default::default()
                },
            );
        }

        if self.dbg.memory_active {
            return (
                DebuggerWidgetAreas {
                    memory: above_command_line_area,
                    command_line: command_line_area,
                    ..Default::default()
                },
                DebuggerWidgetBorders {
                    memory: Borders::TOP,
                    command_line: command_line_borders,
                    ..Default::default()
                },
            );
        }

        let display_mode = self.vm.interpreter().display.mode;
        let (mut display_window_width, mut display_window_height) =
            display_mode.window_dimensions();
        display_window_height = if self.dbg.vm_visible {
            display_window_height.saturating_sub(1)
        } else {
            0
        };
        display_window_width = if display_mode == DisplayMode::HighResolution && !self.logging {
            display_window_width
        } else {
            display_window_width.saturating_sub(1)
        };

        let non_column_with_display_width =
            terminal_area.width.saturating_sub(display_window_width);
        let left_of_column_with_display_width =
            if display_mode == DisplayMode::LowResolution {
                (non_column_with_display_width / 2)
                    .max(Self::GENERAL_STATE_COLUMN_WIDTH)
                    .min(non_column_with_display_width)
            } else {
                non_column_with_display_width
            };

        let [left_of_column_with_display, column_with_display, right_of_column_with_display] =
            Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Length(left_of_column_with_display_width),
                    Constraint::Length(display_window_width),
                    Constraint::Length(
                        non_column_with_display_width - left_of_column_with_display_width,
                    ),
                ])
                .split(above_command_line_area)[..] else { unreachable!() };

        let [display_area, below_display_area] =
            Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(display_window_height),
                    Constraint::Length(
                        column_with_display
                            .height
                            .saturating_sub(display_window_height),
                    ),
                ])
                .split(column_with_display)[..] else { unreachable!() };
        let display_area_borders = if self.dbg.vm_visible {
            if display_mode == DisplayMode::HighResolution {
                Borders::ALL.difference(Borders::BOTTOM)
            } else {
                Borders::ALL
                    .difference(Borders::BOTTOM)
                    .difference(Borders::RIGHT)
            }
        } else {
            Borders::NONE
        };

        let rom_kind = self.vm.interpreter().rom.config.kind;
        let is_rom_at_least_schip = rom_kind >= RomKind::SCHIP;
        let is_rom_at_least_xochip = rom_kind >= RomKind::XOCHIP;

        let second_general_area_width = if is_rom_at_least_schip {
            Self::GENERAL_STATE_COLUMN_WIDTH
        } else {
            0
        };

        let (second_general_left_area_width, second_general_right_area_width) = if display_mode == DisplayMode::HighResolution {
            (second_general_area_width, 0)
        } else {
            (0, second_general_area_width)
        };

        let [second_general_right_area, right_most_column] =
            Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Length(second_general_right_area_width),
                    Constraint::Length(
                        right_of_column_with_display
                            .width
                            .saturating_sub(second_general_right_area_width),
                    ),
                ])
                .split(right_of_column_with_display)[..] else { unreachable!() };

        let [top_right_most_column, bottom_right_most_column] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(display_window_height),
                Constraint::Length(
                    right_of_column_with_display
                        .height
                        .saturating_sub(display_window_height),
                ),
            ])
            .split(right_most_column)[..] else { unreachable!() };

        let memory_window_width = DisplayMode::LowResolution.window_dimensions().0;
        let [memory_area, right_of_memory_area_in_display_column] =
            Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Length(memory_window_width),
                    Constraint::Length(
                        below_display_area.width.saturating_sub(memory_window_width),
                    ),
                ])
                .split(below_display_area)[..] else { unreachable!() };
        let memory_area_borders = Borders::ALL.difference(Borders::RIGHT);

        let [left_most_area, second_general_left_area, chip8_general_area] = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(
                    left_of_column_with_display
                        .width
                        .saturating_sub(Self::GENERAL_STATE_COLUMN_WIDTH + second_general_left_area_width),
                ),
                Constraint::Length(second_general_left_area_width),
                Constraint::Length(Self::GENERAL_STATE_COLUMN_WIDTH),
            ])
            .split(left_of_column_with_display)[..] else { unreachable!() };
        
        let [top_left_most_area, bottom_left_most_area] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage(50),
                Constraint::Percentage(50)
            ])
            .split(left_most_area)[..] else { unreachable!() };
        
        let output_area = if self.logging && display_mode == DisplayMode::HighResolution { bottom_left_most_area } else { left_most_area };
        let output_area_borders = Borders::TOP;

        let [keyboard_area, pointers_area, registers_area, timers_area, stack_area] =
            Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(Self::KEYBOARD_STATE_HEIGHT),
                    Constraint::Length(Self::POINTERS_STATE_HEIGHT),
                    Constraint::Length(Self::REGISTERS_STATE_HEIGHT),
                    Constraint::Length(Self::TIMERS_STATE_HEIGHT),
                    Constraint::Length(1 + self.vm.interpreter().stack.len().max(1) as u16),
                ])
                .split(chip8_general_area)[..] else { unreachable!() };
        let keyboard_area_borders = Borders::TOP.union(Borders::LEFT);
        let pointers_area_borders = Borders::TOP.union(Borders::LEFT);
        let registers_area_borders = Borders::TOP.union(Borders::LEFT);
        let timers_area_borders = Borders::TOP.union(Borders::LEFT);
        let stack_area_borders = Borders::ALL.difference(Borders::RIGHT);

        let second_general_area = if second_general_left_area_width > 0 {
            second_general_left_area
        } else {
            second_general_right_area
        };

        let [planes_area, audio_area, flags_area] = 
            Layout::default()
                .direction(Direction::Vertical)
                .constraints(
                    if is_rom_at_least_xochip {
                        [
                            Constraint::Length(Self::PLANES_STATE_HEIGHT),
                            Constraint::Length(Self::AUDIO_STATE_HEIGHT),
                            Constraint::Length(Self::XOCHIP_FLAG_STATE_HEIGHT)
                        ]
                    } else {
                        [
                            Constraint::Length(0),
                            Constraint::Length(0),
                            Constraint::Length(Self::SCHIP_FLAG_STATE_HEIGHT)
                        ]
                    }
                )
                .split(second_general_area)[..] else { unreachable!() };
        let planes_area_borders = Borders::TOP.union(Borders::LEFT);
        let audio_area_borders = Borders::TOP.union(Borders::LEFT);
        let flags_area_borders = Borders::ALL.difference(Borders::RIGHT);

        let history_area = match display_mode {
            DisplayMode::LowResolution => {
                if self.logging {
                    bottom_right_most_column
                } else {
                    right_most_column
                }
            }
            DisplayMode::HighResolution => right_of_memory_area_in_display_column,
        };
        let history_area_borders = Borders::ALL;

        let (logger_area, logger_area_borders) = if self.logging {
            match display_mode {
                DisplayMode::LowResolution => {
                    (top_right_most_column, Borders::ALL.difference(Borders::BOTTOM))
                }
                DisplayMode::HighResolution => (top_left_most_area, Borders::TOP),
            }
        } else {
            (Rect::default(), Borders::NONE)
        };

        (
            DebuggerWidgetAreas {
                history: history_area,
                output: output_area,
                keyboard: keyboard_area,
                pointers: pointers_area,
                registers: registers_area,
                timers: timers_area,
                stack: stack_area,
                memory: memory_area,
                planes: planes_area,
                audio: audio_area,
                flags: flags_area,
                display: display_area,
                logger: logger_area,
                command_line: command_line_area,
            },
            DebuggerWidgetBorders {
                history: history_area_borders,
                output: output_area_borders,
                keyboard: keyboard_area_borders,
                pointers: pointers_area_borders,
                registers: registers_area_borders,
                timers: timers_area_borders,
                stack: stack_area_borders,
                memory: memory_area_borders,
                planes: planes_area_borders,
                audio: audio_area_borders,
                flags: flags_area_borders,
                display: display_area_borders,
                logger: logger_area_borders,
                command_line: command_line_borders,
            },
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

        let (layout_areas, layout_borders) = self.build_layout(area);

        state.logger_area = layout_areas.logger;
        state.logger_border = layout_borders.logger;

        let display_widget = self.vm.to_display_widget();

        // Display
        let display_block = Block::default()
            .title(display_widget.build_title())
            .borders(layout_borders.display);
        display_widget.render(display_block.inner(layout_areas.display), buf);
        display_block.render(layout_areas.display, buf);

        // Output
        let output_block = Block::default()
            .title(" Output ")
            .borders(layout_borders.output);
        self.dbg
            .shell
            .as_output_widget()
            .render(output_block.inner(layout_areas.output), buf);
        output_block.render(layout_areas.output, buf);

        // History
        HistoryWidget {
            history: &self.dbg.history,
            active: self.dbg.history_active,
            border: layout_borders.history,
        }
        .render(layout_areas.history, buf);

        // Memory
        let memory_block = Block::default()
            .title(" Memory ")
            .borders(layout_borders.memory);
        let mut memory_state = self.dbg.memory_widget_state.take();
        MemoryWidget {
            active: self.dbg.memory_active,
            memory: &self.dbg.memory,
            watchpoints: &self.dbg.watchpoints,
            breakpoints: &self.dbg.breakpoints,
            interpreter: self.vm.interpreter(),
            disassembler: &self.dbg.disassembler,
        }
        .render(
            memory_block.inner(layout_areas.memory),
            buf,
            &mut memory_state,
        );
        memory_block.render(layout_areas.memory, buf);
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
            .block(
                Block::default()
                    .title(" Keyboard ")
                    .borders(layout_borders.keyboard),
            )
            .render(layout_areas.keyboard, buf);

        // Pointers
        let pc_is_watchpoint = self
            .dbg
            .watchpoints
            .contains(&Watchpoint::Pointer(MemoryPointer::ProgramCounter));
        let index_is_watchpoint = self
            .dbg
            .watchpoints
            .contains(&Watchpoint::Pointer(MemoryPointer::Index));
        Paragraph::new(vec![
            Spans::from(Span::styled(
                format!(
                    "{}pc {:#05X}",
                    if pc_is_watchpoint { "*" } else { "-" },
                    interp.pc
                ),
                if pc_is_watchpoint {
                    Style::default().fg(Color::Blue)
                } else {
                    Style::default()
                },
            )),
            Spans::from(Span::styled(
                format!(
                    "{}i  {:#05X}",
                    if index_is_watchpoint { "*" } else { "-" },
                    interp.index
                ),
                if index_is_watchpoint {
                    Style::default().fg(Color::Blue)
                } else {
                    Style::default()
                },
            )),
        ])
        .block(
            Block::default()
                .title(" Pointers ")
                .borders(layout_borders.pointers),
        )
        .render(layout_areas.pointers, buf);

        // Registers
        Paragraph::new(
            interp
                .registers
                .iter()
                .enumerate()
                .map(|(i, val)| {
                    let is_watched = self
                        .dbg
                        .watchpoints
                        .contains(&Watchpoint::Register(i as u8));
                    Spans::from(Span::styled(
                        format!(
                            "{}v{:x} {:0>3} ({:#04X})",
                            if is_watched { "*" } else { "-" },
                            i,
                            val,
                            val
                        ),
                        if is_watched {
                            Style::default().fg(Color::Blue)
                        } else {
                            Style::default()
                        },
                    ))
                })
                .collect::<Vec<_>>(),
        )
        .block(
            Block::default()
                .title(" Registers ")
                .borders(layout_borders.registers),
        )
        .render(layout_areas.registers, buf);

        // Timers
        Paragraph::new(vec![
            Spans::from(format!(
                " vsync {:0>3.0}%",
                self.vm.precise_vsync_progress() * 100.0
            )),
            Spans::from(format!(" sound {:0>6.2}", self.vm.precise_sound_timer())),
            Spans::from(format!(" delay {:0>6.2}", self.vm.precise_delay_timer())),
            Spans::from(format!("   |-> {:0>3}", self.vm.delay_timer())),
        ])
        .block(
            Block::default()
                .title(" Timers ")
                .borders(layout_borders.timers),
        )
        .render(layout_areas.timers, buf);

        // Stack
        Paragraph::new(
            interp
                .stack
                .iter()
                .enumerate()
                .map(|(i, addr)| Spans::from(format!(" #{:0>2} {:#05X}", i, addr)))
                .collect::<Vec<_>>(),
        )
        .block(
            Block::default()
                .title(" Stack ")
                .borders(layout_borders.stack),
        )
        .render(layout_areas.stack, buf);

        // Planes
        Paragraph::new(vec![
            Spans::from(""),
            Spans::from((0..4).fold(
                vec![Span::raw(" ")],
                |mut vec, plane| {
                    vec.push(Span::styled(
                        format!(" {} ", plane + 1),
                        if self.vm.interpreter().display.selected_plane_bitflags >> plane & 1 == 1 {
                            Style::default().fg(Color::Black).bg(Color::White)
                        } else {
                            Style::default()
                        },
                    ));
                    if plane == 3 {
                        vec.push(Span::raw(" "))
                    }

                    vec
                },
            ))
        ])
        .block(
            Block::default()
                .title(" Planes ")
                .borders(layout_borders.planes),
        )
        .render(layout_areas.planes, buf);

        // Audio
        Paragraph::new(
            [
                Spans::from(format!(" pitch {:0>3}", self.vm.interpreter().audio.pitch)),
                Spans::from(format!("   |-> {}Hz", self.vm.interpreter().audio.sample_rate().round() as u32)),
                Spans::from(format!(" audio")),
            ]
            .into_iter()
            .chain(
                self.vm
                    .interpreter()
                    .audio
                    .buffer
                    .chunks_exact(4)
                    .map(|bytes| {
                        Spans::from(format!(
                            " {:02x} {:02x}  {:02x} {:02x} ",
                            bytes[0], bytes[1], bytes[2], bytes[3]
                        ))
                    }),
            )
            .collect::<Vec<_>>(),
        )
        .block(
            Block::default()
                .title(" Audio ")
                .borders(layout_borders.audio),
        )
        .render(layout_areas.audio, buf);

        // RPL Flags
        Paragraph::new(
            interp
                .flags[..if self.vm.interpreter().rom.config.kind == RomKind::XOCHIP { 16 } else { 8 }]
                .iter()
                .enumerate()
                .map(|(i, val)| Spans::from(Span::raw(format!("-s{:x} {:0>3} ({:#04X})", i, val, val))))
                .collect::<Vec<_>>(),
        )
        .block(
            Block::default()
                .title(" Save Flags ")
                .borders(layout_borders.flags),
        )
        .render(layout_areas.flags, buf);

        // Bottom (Command line or messages)
        if self.can_draw_shell(area) {
            InputWidget::from(&self.dbg.shell).render(
                layout_areas.command_line,
                buf,
                &mut state.input,
            )
        } else if self.dbg.shell_output_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(layout_areas.command_line, bottom_area_style);
            Paragraph::new(" Esc to exit output navigation")
                .style(bottom_area_style)
                .render(layout_areas.command_line, buf);
        } else if self.dbg.memory_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(layout_areas.command_line, bottom_area_style);
            Paragraph::new(" Esc to exit memory navigation")
                .style(bottom_area_style)
                .render(layout_areas.command_line, buf);
        } else if self.dbg.history_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(layout_areas.command_line, bottom_area_style);
            Paragraph::new(" Esc to exit history navigation")
                .style(bottom_area_style)
                .render(layout_areas.command_line, buf);
        }
    }
}
