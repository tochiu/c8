use crate::{
    config::C8VMConfig,
    dbg::debug::{DebuggerWidget, DebuggerWidgetState, Debugger},
    vm::{
        disp::{Display, DisplayWidget},
        run::VMWare,
    },
};

use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

use tui::{
    backend::CrosstermBackend,
    style::{Color, Style},
    widgets::{Block, Borders},
    Terminal,
};

use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};

use std::{
    io::{self, stdout},
    ops::DerefMut,
};

pub struct Renderer {
    config: C8VMConfig,
    terminal: Terminal<CrosstermBackend<io::Stdout>>,
    vm_disp: Display,
    vm_ware: VMWare,
    drawing_dbg: bool,
    widget_state: ((), Option<DebuggerWidgetState>),
    //             ^^ first field left blank because VM currently doesn't require a screen state struct but might in the future
}

impl Renderer {
    // change terminal to an alternate screen so user doesnt lose terminal history on exit
    // and enable raw mode so we have full authority over event handling and output
    pub fn setup(vm_ware: VMWare, config: C8VMConfig) -> Result<Renderer, io::Error> {
        enable_raw_mode()?;
        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen)?;
        Ok(Renderer {
            terminal: Terminal::new(CrosstermBackend::new(stdout))?,
            widget_state: (
                (),
                if config.debugging {
                    Some(DebuggerWidgetState::default())
                } else {
                    None
                },
            ),
            drawing_dbg: false,
            vm_disp: Display::from(config.title.clone()),
            vm_ware,
            config,
        })
    }

    // clean up the terminal so its usable after program exit
    pub fn exit(&mut self) -> Result<(), io::Error> {
        disable_raw_mode()?;
        execute!(self.terminal.backend_mut(), LeaveAlternateScreen)?;
        self.terminal.show_cursor()?;
        Ok(())
    }

    pub fn step(&mut self, redraw: bool) -> Result<(), io::Error> {
        let mut _guard = self.vm_ware.lock().unwrap();
        let (vm, maybe_dbg) = _guard.deref_mut();

        let vm_disp_updated = if let Some(buf) = vm.extract_new_frame() {
            self.vm_disp.buffer = buf;
            true
        } else {
            false
        };

        let drawing_dbg = maybe_dbg.as_ref().map_or(false, Debugger::is_active);
        let should_draw = redraw || vm_disp_updated || drawing_dbg != self.drawing_dbg;

        if should_draw {
            self.drawing_dbg = drawing_dbg;
            if drawing_dbg {
                let Some(dbg) = maybe_dbg else {
                    unreachable!("debugger must exist for debugger draw call to be made")
                };
    
                self.terminal.draw(|f| {
                    let Some(dbg_widget_state) = self.widget_state.1.as_mut() else {
                        unreachable!("debugger screen state must exist for debugger draw call to be made")
                    };
        
                    let dbg_area = f.size();
                    let dbg_widget = DebuggerWidget {
                        dbg,
                        vm,
                        vm_disp: &self.vm_disp,
                        logging: self.config.logging,
                    };
        
                    if self.config.logging {
                        f.render_widget(Renderer::logger_widget(), dbg_widget.logger_area(dbg_area));
                    }
        
                    if let Some((x, y)) = dbg_widget.cursor_position(dbg_area, dbg_widget_state) {
                        f.set_cursor(x, y);
                    }
        
                    f.render_stateful_widget(dbg_widget, dbg_area, dbg_widget_state);
                })?;
            } else {
                drop(_guard);
                
                self.terminal.draw(|f| {
                    let display_area = f.size();
                    let display_widget = DisplayWidget {
                        display: &self.vm_disp,
                        logging: self.config.logging,
                    };
        
                    if self.config.logging {
                        f.render_widget(
                            Renderer::logger_widget(),
                            display_widget.logger_area(display_area),
                        );
                    }
        
                    f.render_widget(display_widget, display_area);
                })?;
            }
        }

        Ok(())
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
}
