use crate::{
    config::C8VMConfig,
    debug::{DebuggerWidget, DebuggerWidgetState},
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

#[derive(Clone, Copy)]
pub enum RenderRequest {
    RedrawScreen,
    Draw(Screen),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Screen {
    VM,
    Debugger,
}

pub struct Renderer {
    config: C8VMConfig,
    terminal: Terminal<CrosstermBackend<io::Stdout>>,
    vm_disp: Display,
    vm_ware: VMWare,
    screen: Screen,
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
            screen: if config.debugging {
                Screen::Debugger
            } else {
                Screen::VM
            },
            widget_state: (
                (),
                if config.debugging {
                    Some(DebuggerWidgetState::default())
                } else {
                    None
                },
            ),
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

    pub fn step(&mut self, requests: &[RenderRequest]) -> Result<(), io::Error> {
        let mut render_screen: Option<Screen> = None;

        for &request in requests {
            render_screen = match request {
                RenderRequest::RedrawScreen => render_screen.or(Some(self.screen)),
                RenderRequest::Draw(screen) => Some(screen),
            }
        }

        // update buffer in display struct if we may potentially render vm
        if render_screen == Some(Screen::VM) || render_screen == None && self.screen == Screen::VM {
            if let Some(buf) = self.vm_ware.lock().unwrap().0.extract_new_frame() {
                self.vm_disp.buffer = buf;
                render_screen = Some(Screen::VM);
            }
        }

        let Some(screen) = render_screen else {
            return Ok(())
        };

        self.screen = screen;

        match screen {
            Screen::Debugger => self.draw_dbg()?,
            Screen::VM => self.draw_vm()?,
        }

        Ok(())
    }

    fn draw_vm(&mut self) -> Result<(), io::Error> {
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

        Ok(())
    }

    fn draw_dbg(&mut self) -> Result<(), io::Error> {
        self.terminal.draw(|f| {
            let mut _guard = self.vm_ware.lock().unwrap();
            let (vm, Some(dbg)) = _guard.deref_mut() else {
                unreachable!("debugger must exist for debugger draw call to be made")
            };
            let Some(dbg_widget_state) = self.widget_state.1.as_mut() else {
                unreachable!("debugger screen state must exist for debugger draw call to be made")
            };

            let dbg_area = f.size();
            let dbg_widget = DebuggerWidget {
                dbg,
                vm,
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
