use crate::{
    dbg::{Debugger, DebuggerWidget, DebuggerWidgetState},
    run::{
        disp::{Display, DisplayWidget},
        rom::RomConfig,
        vm::VM,
        C8Lock
    },
};

use anyhow::{anyhow, Context, Result};
use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Style, Modifier},
    widgets::{Block, Borders, Gauge},
    Frame, text::Span,
};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};

use std::{
    io::{self, stdout},
    ops::DerefMut,
    sync::mpsc::{channel, Sender, TryRecvError},
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

type Terminal = tui::Terminal<CrosstermBackend<io::Stdout>>;

pub const TARGET_FRAME_DURATION: Duration = Duration::from_nanos(16_666_667); // 60 FPS

fn cleanup_terminal(terminal: &mut Terminal) -> Result<()> {
    // clean up the terminal so its usable after program exit
    disable_raw_mode().context("Failed to disable terminal raw mode")?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)
        .context("Failed to leave alternate terminal screen")?;
    terminal
        .show_cursor()
        .context("Failed to show terminal cursor")?;
    Ok(())
}

pub fn panic_cleanup_terminal() -> Result<()> {
    cleanup_terminal(
        &mut tui::Terminal::new(CrosstermBackend::new(stdout()))
            .context("Failed to create interface to terminal backend")?,
    )
}

pub fn spawn_render_thread(c8: C8Lock, config: RomConfig) -> (Sender<()>, JoinHandle<()>) {
    let (render_sender, render_receiver) = channel::<()>();
    let render_thread_handle = thread::spawn(move || {
        // change terminal to an alternate screen so user doesnt lose terminal history on exit
        // and enable raw mode so we have full authority over event handling and output
        enable_raw_mode().expect("Failed to enable terminal raw mode");

        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen).expect("Failed to enter alternate terminal screen");

        let mut terminal = tui::Terminal::new(CrosstermBackend::new(stdout))
            .expect("Failed to create interface to terminal backend");

        let mut renderer = Renderer {
            dbg_widget_state: Default::default(),
            dbg_visible: false,
            config,
        };

        let mut should_redraw = false;

        let mut frame_start = Instant::now();

        loop {
            if render_receiver.try_iter().last().is_some() {
                should_redraw = true;
            }

            if let Err(TryRecvError::Disconnected) = render_receiver.try_recv() {
                if let Err(e) = cleanup_terminal(&mut terminal) {
                    eprintln!("Failed to cleanup terminal: {}", e);
                }
                return;
            }

            renderer
                .step(&mut terminal, should_redraw, &c8)
                .expect("Failed render step");
            should_redraw = false;

            frame_start = frame_start.checked_add(TARGET_FRAME_DURATION).expect("Could not calculate next frame start");
            thread::sleep(frame_start.saturating_duration_since(Instant::now()));
        }
    });

    (render_sender, render_thread_handle)
}

struct Renderer {
    config: RomConfig,
    dbg_visible: bool,
    dbg_widget_state: DebuggerWidgetState,
}

impl Renderer {
    fn step(&mut self, terminal: &mut Terminal, should_redraw: bool, c8: &C8Lock) -> Result<()> {
        let mut _guard = c8
            .lock()
            .map_err(|_| anyhow!("Failed to lock C8 for render step"))?;

        let (vm, maybe_dbg) = _guard.deref_mut();

        let maybe_display = vm.extract_new_display();

        let is_dbg_visible = maybe_dbg.as_ref().map_or(false, Debugger::is_active);
        let should_draw =
            should_redraw || maybe_display.is_some() || is_dbg_visible != self.dbg_visible;

        if should_draw {
            self.dbg_visible = is_dbg_visible;
            if is_dbg_visible {
                let Some(dbg) = maybe_dbg else {
                    unreachable!("debugger must exist for debugger draw call to be made")
                };

                terminal.draw(|f| {
                    dbg.prepare_render();
                    self.render_debugger(f, dbg, vm);
                })?;
            } else {
                let display =
                    maybe_display.unwrap_or_else(|| vm.interpreter().display.clone());
                let hz = (1.0/vm.time_step).clamp(0.0, u32::MAX as f64).round() as u32;
                let volume = vm.audio().volume();
                drop(_guard);

                terminal.draw(|f| {
                    self.render_virtual_machine(f, &display, hz, volume);
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

        if let Some((x, y)) = dbg_widget.cursor_position(dbg_area, &mut self.dbg_widget_state) {
            f.set_cursor(x, y);
        }

        f.render_stateful_widget(dbg_widget, dbg_area, &mut self.dbg_widget_state);
        f.render_widget(
            logger_widget(self.dbg_widget_state.logger_border),
            self.dbg_widget_state.logger_area,
        );
    }

    fn render_virtual_machine<B: Backend>(
        &self,
        f: &mut Frame<B>,
        display: &Display,
        execution_frequency: u32,
        volume: f32
    ) {
        let area = f.size();
        let display_widget = DisplayWidget {
            rom_name: &self.config.name,
            rom_kind: self.config.kind,
            logging: self.config.logging,
            execution_frequency,
            display,
        };

        let (display_width, display_height) = display.mode.window_dimensions();
        let [display_column, logger_column, ..] = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(display_width),
                Constraint::Length(area.width.saturating_sub(display_width)),
            ])
            .split(area)[..] else { unreachable!() };

        let [display_row, volume_row, logger_row] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(display_height),
                Constraint::Length(1),
                Constraint::Length(area.height.saturating_sub(display_height + 1)),
            ])
            .split(area)[..] else { unreachable!() };

        if self.config.logging {
            f.render_widget(
                logger_widget(Borders::ALL),
                if logger_column.area() >= logger_row.area() {
                    logger_column
                } else {
                    logger_row
                },
            );
        }

        let display_block = Block::default()
            .title(display_widget.title())
            .borders(Borders::ALL);
        let display_area = display_row.intersection(display_column);
        f.render_widget(display_widget, display_block.inner(display_area));
        f.render_widget(display_block, display_area);

        let volume_area = volume_row.intersection(display_column);
        f.render_widget(
            Gauge::default()
                .block(Block::default().borders(Borders::LEFT.union(Borders::RIGHT)))
                .gauge_style(Style::default().fg(Color::White).bg(Color::Gray).add_modifier(Modifier::BOLD))
                .label(Span::styled("(DOWN - ) Volume (UP   = )", Style::default().fg(Color::Black)))
                .percent((volume * 100.0).round().clamp(0.0, 100.0) as u16), 
            volume_area
        );
    }
}

pub fn logger_widget(borders: Borders) -> TuiLoggerWidget<'static> {
    TuiLoggerWidget::default()
        .block(
            Block::default()
                .title(" Log ")
                .border_style(Style::default().fg(Color::White))
                .borders(borders),
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
