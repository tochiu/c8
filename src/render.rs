use crate::{
    dbg::core::{DebuggerWidget, DebuggerWidgetState, Debugger},
    run::{
        core::{C8Lock, Interval, IntervalAccuracy},
        disp::{Display, DisplayWidget},
        vm::VM, rom::RomConfig,
    },
};

use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::{CrosstermBackend, Backend},
    style::{Color, Style},
    widgets::{Block, Borders}, Frame,
};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};
use anyhow::{Result, anyhow, Context};

use std::{
    io::{self, stdout},
    ops::DerefMut, sync::mpsc::{Sender, channel, TryRecvError}, thread::{JoinHandle, self}, time::Duration,
};

type Terminal = tui::Terminal<CrosstermBackend<io::Stdout>>;

fn cleanup_terminal(terminal: &mut Terminal) -> Result<()> {
    // clean up the terminal so its usable after program exit
    disable_raw_mode()
        .context("Failed to disable terminal raw mode")?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)
        .context("Failed to leave alternate terminal screen")?;
    terminal.show_cursor()
        .context("Failed to show terminal cursor")?;
    Ok(())
}

pub fn panic_cleanup_terminal() -> Result<()> {
    cleanup_terminal(
        &mut tui::Terminal::new(CrosstermBackend::new(stdout()))
            .context("Failed to create interface to terminal backend")?
    )
}

pub fn spawn_render_thread(c8: C8Lock, config: RomConfig) -> (Sender<()>, JoinHandle<()>) {
    let (render_sender, render_receiver) = channel::<()>();
    let render_thread_handle = thread::spawn(move || {
        // change terminal to an alternate screen so user doesnt lose terminal history on exit
        // and enable raw mode so we have full authority over event handling and output
        enable_raw_mode()
            .expect("Failed to enable terminal raw mode");

        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen)
            .expect("Failed to enter alternate terminal screen");

        let mut terminal = tui::Terminal::new(CrosstermBackend::new(stdout))
            .expect("Failed to create interface to terminal backend");

        let mut renderer = Renderer {
            dbg_widget_state: Default::default(),
            dbg_visible: false,
            config,
        };

        let mut interval = Interval::new(
            "render",
            Duration::from_millis(16),
            Duration::from_millis(16),
            IntervalAccuracy::Default
        );

        let mut should_redraw = false;

        loop {
            if render_receiver.try_iter().last().is_some() {
                should_redraw = true;
            }

            if let Err(TryRecvError::Disconnected) = render_receiver.try_recv() {
                if let Err(e) = cleanup_terminal(&mut terminal) {
                    eprintln!("Failed to cleanup terminal: {}", e);
                }
                return
            }

            renderer.step(&mut terminal, should_redraw, &c8)
                .expect("Failed render step");
            should_redraw = false;

            interval.sleep();
        }
    });

    (render_sender, render_thread_handle)
}

struct Renderer {
    config: RomConfig,
    dbg_visible: bool,
    dbg_widget_state: DebuggerWidgetState
}

impl Renderer {
    fn step(&mut self, terminal: &mut Terminal, should_redraw: bool, c8: &C8Lock) -> Result<()> {
        let mut _guard = c8.lock()
            .map_err(|_| anyhow!("Failed to lock C8 for render step"))?;

        let (vm, maybe_dbg) = _guard.deref_mut();

        let maybe_display = vm.extract_new_display();

        let is_dbg_visible = maybe_dbg.as_ref().map_or(false, Debugger::is_active);
        let should_draw = should_redraw 
            || maybe_display.is_some() 
            || is_dbg_visible != self.dbg_visible;

        if should_draw {
            self.dbg_visible = is_dbg_visible;
            if is_dbg_visible {
                let Some(dbg) = maybe_dbg else {
                    unreachable!("debugger must exist for debugger draw call to be made")
                };

                terminal.draw(|f| {
                    self.render_debugger(f, dbg, vm);
                })?;
            } else {
                let display = maybe_display.unwrap_or_else(|| vm.interpreter().output.display.clone());
                let time_step = vm.time_step;
                drop(_guard);
                
                terminal.draw(|f| {
                    self.render_virtual_machine(f, &display, (1.0/time_step).round().clamp(0.0, u16::MAX as f32) as u16);
                })?;
            }
        }

        Ok(())
    }

    fn render_debugger<B: Backend>(&mut self, f: &mut Frame<B>, dbg: &Debugger, vm: &VM) {
        let dbg_area = f.size();
        let dbg_widget = DebuggerWidget {
            dbg,
            vm,
            logging: self.config.logging,
        };

        if self.config.logging {
            f.render_widget(logger_widget(), dbg_widget.logger_area(dbg_area));
        }

        if let Some((x, y)) = dbg_widget.cursor_position(dbg_area, &mut self.dbg_widget_state) {
            f.set_cursor(x, y);
        }

        f.render_stateful_widget(dbg_widget, dbg_area, &mut self.dbg_widget_state);
    }

    fn render_virtual_machine<B: Backend>(&self, f: &mut Frame<B>, display: &Display, instruction_frequency: u16) {
        let display_area = f.size();
        let display_widget = DisplayWidget {
            title: &self.config.name,
            rom_kind: self.config.kind,
            logging: self.config.logging,
            instruction_frequency: instruction_frequency, 
            display,
        };

        if self.config.logging {
            f.render_widget(
                logger_widget(),
                display_widget.logger_area(display_area),
            );
        }

        f.render_widget(display_widget, display_area);
    }
}

fn logger_widget() -> TuiLoggerWidget<'static> {
    TuiLoggerWidget::default()
        .block(
            Block::default()
                .title(" Log ")
                .border_style(Style::default().fg(Color::White))
                .borders(Borders::ALL),
        )
        .output_separator('|')
        .output_timestamp(Some("%H:%M:%S%.3f".to_string()))
        .output_level(Some(TuiLoggerLevelOutput::Abbreviated))
        .output_target(false)
        .output_file(false)
        .output_line(false)
        .style_error(Style::default().fg(Color::Red))
        .style_debug(Style::default().fg(Color::Cyan))
        .style_warn(Style::default().fg(Color::Yellow))
        .style_trace(Style::default().fg(Color::White))
        .style_info(Style::default().fg(Color::Green))
}