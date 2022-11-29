use super::{mem::*, shell::*};

use crate::{
    disass::Disassembler,
    vm::{
        disp::{Display, DisplayWidget, DISPLAY_WINDOW_HEIGHT, DISPLAY_WINDOW_WIDTH},
        interp::{Instruction, Interpreter},
        prog::PROGRAM_MEMORY_SIZE,
        run::{VMRunner, VM},
    },
};

use crossterm::event::{Event, KeyCode, KeyEventKind};
use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::Spans,
    widgets::{Block, Borders, Paragraph, StatefulWidget, Widget}, buffer::Buffer,
};

use std::{collections::{HashSet, HashMap}, cell::Cell};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(super) enum Watchpoint {
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
    fn update(&mut self, interp: &Interpreter, watchpoints: &HashSet<Watchpoint>, event_queue: &mut Vec<DebugEvent>) {
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

pub(super) enum DebugEvent {
    WatchpointTrigger(Watchpoint, u16, u16),
    BreakpointReached(u16),
}

pub struct Debugger {
    active: bool,

    breakpoints: HashSet<u16>,
    watchpoints: HashSet<Watchpoint>,
    watch_state: WatchState,
    event_queue: Vec<DebugEvent>,

    disassembler: Disassembler,

    memory: Memory,
    memory_active: bool,
    memory_widget_state: Cell<MemoryWidgetState>,

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

            memory: Default::default(),
            memory_active: false,
            memory_widget_state: Default::default(),

            shell: Default::default(),
            shell_active: false,

            vm_visible: false,
        };

        dbg.vm_visible = true;
        dbg.shell_active = true;
        dbg.shell.input_enabled = true;
        dbg.disassembler.run();
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
        let mut sink_event = false;

        match event {
            Event::Key(key_event) => {
                if let KeyEventKind::Press | KeyEventKind::Repeat = key_event.kind {
                    if self.active {
                        if self.shell_active {
                            sink_event = self.shell.handle_key_event(key_event);
                        } else if self.memory_active {
                            sink_event = self.memory.handle_key_event(key_event, self.memory_widget_state.get_mut(), &mut self.memory_active);
                            if !self.memory_active {
                                self.shell_active = true;
                            }
                        }
                    } else if key_event.code == KeyCode::Esc {
                        self.pause(vm_runner, vm);
                        sink_event = true;
                    }
                }
            }
            _ => (),
        }

        let cmds = self.shell.try_recv();

        if self.active {
            for cmd_str in cmds.collect::<Vec<_>>() {
                self.shell.echo(&cmd_str);

                let lowercase_cmd_str = cmd_str.to_ascii_lowercase();
                let mut cmd_args = lowercase_cmd_str.split_ascii_whitespace();

                let Some(cmd) = cmd_args.next() else {
                    self.shell.print_unrecognized_cmd();
                    continue;
                };

                // TODO: add more commands here
                match cmd {
                    "r" | "c" | "run" | "cont" | "continue" => {
                        log::info!("c8vm resume!");
                        self.deactivate();
                        vm.clear_event_queue();
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
                            vm.clear_event_queue();
                            vm.step(1.0 / vm_runner.config().instruction_frequency as f64)
                                .unwrap();
                            self.shell.output_pc(vm.interpreter());
                            if !self.step(vm) {
                                break;
                            }
                        }

                        if amt_stepped > 1 {
                            self.shell.print(format!("Stepped {} times", amt_stepped));
                        }
                    }
                    "w" | "watch" => {
                        let Some(arg) = cmd_args.next() else {
                            self.shell.print("Please specify a register or pointer to watch");
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
                                        self.shell.print("Please specify a valid register (v0..vf) to watch");
                                        continue;
                                    }
                                } else if let Some(addr) = vm.interpreter().parse_addr(arg) {
                                    self.watch_state.addresses.insert(addr, vm.interpreter().memory[addr as usize]);
                                    self.watchpoints.insert(Watchpoint::Address(addr))
                                } else {
                                    self.shell.print("Please specify a register or pointer to watch");
                                    continue;
                                }
                            }
                        };

                        self.shell.print(
                            if watchpoint_already_exists {
                                format!("Watchpoint {} already exists", arg)
                            } else {
                                format!("Watching {}", arg)
                            }
                        );
                    }
                    "b" | "break" | "breakpoint" => {
                        let Some(addr) = cmd_args.next().and_then(|arg| vm.interpreter().parse_addr(arg)) else {
                            self.shell.print(format!("Please specify a valid address in the range [{:#05X}, {:#05X}]", 0, PROGRAM_MEMORY_SIZE - 1).as_str());
                            continue;
                        };

                        if self.breakpoints.insert(addr) {
                            self.shell.print(format!("Breakpoint at {:#05X}", addr));
                        } else {
                            self.shell.print(format!("Breakpoint at {:#05X} already exists", addr));
                        }
                    }
                    "show" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.print_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = true;
                    }
                    "hide" => {
                        let Some("vm") = cmd_args.next() else {
                            self.shell.print_unrecognized_cmd();
                            continue;
                        };

                        self.vm_visible = false;
                    }
                    "m" | "mem" | "memory" => {
                        self.memory_active = true;
                        self.shell_active = false;
                    }
                    "g" | "goto" => {
                        let Some(arg) = cmd_args.next() else {
                            self.shell.print_unrecognized_cmd();
                            continue;
                        };
                        
                        match arg {
                            "start" => {
                                self.memory_widget_state.get_mut().set_focus(0)
                            },
                            "end" => {
                                self.memory_widget_state.get_mut().set_focus(vm.interpreter().memory.len() as u16 - 1)
                            },
                            "pc" => {
                                self.memory_widget_state.get_mut().set_focus(vm.interpreter().pc)
                            },
                            "i" | "index" => {
                                self.memory_widget_state.get_mut().set_focus(vm.interpreter().index)
                            },
                            _ => if let Some(addr) = vm.interpreter().parse_addr(arg) {
                                self.memory_widget_state.get_mut().set_focus(addr)
                            } else {
                                self.shell.print_unrecognized_cmd()
                            }
                        };
                    }
                    "f" | "follow" => {
                        let Some(arg) = cmd_args.next() else {
                            self.shell.print("Follow command syntax:");
                            self.shell.print("    - follow (pc/i)");
                            continue;
                        };

                        match arg {
                            "pc" => {
                                self.memory.follow = Some(MemoryPointer::ProgramCounter);
                                self.memory_widget_state.get_mut().poke();
                            }
                            "i" | "index" => {
                                self.memory.follow = Some(MemoryPointer::Index);
                                self.memory_widget_state.get_mut().poke();
                            }
                            _ => {
                                self.shell.print("Please specify a pointer (pc/i) to follow");
                                continue;
                            }
                        }

                        self.shell.print(format!("Following {}", arg));
                    }
                    "uf" | "unfollow" => {
                        if let Some(pointer) = self.memory.follow {
                            self.memory.follow = None;
                            self.shell.print(format!("Unfollowing {}", pointer.identifier()));
                        } else {
                            self.shell.print("Already unfollowed");
                        }
                    }
                    _ => {
                        self.shell.print_unrecognized_cmd();
                    }
                }
            }
        }

        sink_event
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

        self.shell.print("Paused.");
        self.shell.output_pc(vm.interpreter());
        self.active = true;
    }

    pub fn deactivate(&mut self) {
        if !self.active {
            return
        }

        self.shell.print("Continuing.");
        self.active = false;
    }

    pub fn step(&mut self, vm: &VM) -> bool {
        let interp = vm.interpreter();

        // update memory draw read write execute (drwx) flags
        self.memory.step(
            self.memory_widget_state.get_mut(), 
            interp, 
            self.watch_state.pc, 
            self.watch_state.index, 
            self.watch_state.instruction
        );

        // update disassembler
        match self.watch_state.instruction {
            Some(Instruction::Store(bytes)) => {
                self.disassembler.update(vm.interpreter(), self.watch_state.index, bytes as u16);
            }
            Some(Instruction::StoreDecimal(_)) => {
                self.disassembler.update(vm.interpreter(), self.watch_state.index, 3);
            }
            _ => ()
        }
        
        // update watch state
        self.watch_state.update(interp, &self.watchpoints, &mut self.event_queue);

        // update breakpoints
        if self.breakpoints.contains(&interp.pc) {
            self.event_queue
                .push(DebugEvent::BreakpointReached(interp.pc));
        }

        // handle debug events emitted
        if self.event_queue.is_empty() {
            return true;
        } else {
            self.activate(vm);
            for debug_event in self.event_queue.drain(..) {
                match debug_event {
                    DebugEvent::BreakpointReached(addr) => {
                        self.shell.print(format!("Breakpoint {:#05X} reached", addr));
                    }
                    DebugEvent::WatchpointTrigger(watchpoint, old, new) => {
                        match watchpoint {
                            Watchpoint::Pointer(pointer) => {
                                let identifier = match pointer {
                                    MemoryPointer::Index => "i",
                                    MemoryPointer::ProgramCounter => "pc"
                                };

                                self.shell.print(format!("Pointer {} changed", identifier));
                                self.shell.print(format!("Old value = {:#05X}", old));
                                self.shell.print(format!("New value = {:#05X}", new));
                            }
                            Watchpoint::Register(register) => {
                                self.shell.print(format!("Register v{:x} changed", register));
                                self.shell.print(format!("Old value = {:0>3} ({:#05X})", old, old));
                                self.shell.print(format!("New value = {:0>3} ({:#05X})", new, new));
                            }
                            Watchpoint::Address(addr) => {
                                self.shell.print(format!("Address {:#05X} changed", addr));
                                self.shell.print(format!("Old value = {:0>3} ({:#04X})", old, old));
                                self.shell.print(format!("New value = {:0>3} ({:#04X})", new, new));
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
pub struct DebuggerWidgetState {
    cmd_line: CommandLineWidgetState,
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
            CommandLineWidget::from(&self.dbg.shell).cursor_position(area, &mut state.cmd_line)
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
            .constraints(
                if self.dbg.shell_active {
                    [
                        Constraint::Length(area.width.saturating_sub(14) / 3),
                        Constraint::Length(14),
                        Constraint::Length(area.width.saturating_sub(area.width.saturating_sub(14) / 3)),
                    ]
                } else {
                    [
                        Constraint::Length(0),
                        Constraint::Length(14),
                        Constraint::Length(area.width.saturating_sub(area.width.saturating_sub(14))),
                    ]
                }
            )
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
    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        if area.area() == 0 {
            return;
        }

        let (main_area, bottom_area, vm_area, _) = self.areas(area);
        let (memory_area, pointer_area, register_area, timer_area, stack_area, output_area) =
            self.main_areas(main_area);
        
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
        OutputWidget::from(&self.dbg.shell).render(output_block.inner(output_area), buf);
        output_block.render(output_area, buf);

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
            interpreter: self.vm.interpreter(),
            disassembler: &self.dbg.disassembler
        }
        .render(memory_block.inner(memory_area), buf, &mut memory_state);
        memory_block.render(memory_area, buf);
        self.dbg.memory_widget_state.set(memory_state);

        let interp = self.vm.interpreter();

        // Pointers
        Paragraph::new(vec![
            Spans::from(format!("pc {:#05X}", interp.pc)),
            Spans::from(format!("i  {:#05X}", interp.index)),
        ])
        .block(Block::default().title(" Pointers ").borders(base_border))
        .render(pointer_area, buf);

        // Registers
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

        // Timers
        Paragraph::new(vec![
            Spans::from(format!("sound {:0>7.3}", self.vm.sound_timer())),
            Spans::from(format!("delay {:0>7.3}", self.vm.delay_timer())),
            Spans::from(format!("  |-> {:0>3}", interp.input.delay_timer)),
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
        .block(Block::default()
        .title(" Stack ")
        .borders(base_border.union(Borders::BOTTOM)))
        .render(stack_area, buf);

        // Bottom (Command line or messages)
        if self.can_draw_shell(area) {
            CommandLineWidget::from(&self.dbg.shell).render(bottom_area, buf, &mut state.cmd_line)
        } else if self.dbg.memory_active {
            let bottom_area_style = Style::default().bg(Color::White).fg(Color::Black);
            buf.set_style(bottom_area, bottom_area_style);
            Paragraph::new("Esc to exit memory navigation")
                .style(bottom_area_style)
                .render(bottom_area, buf);
        }
    }
}