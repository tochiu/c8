use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen}
};
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
    Terminal as TUITerminal,
};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};

use std::{io::{self, stdout}, sync::{Arc, Mutex}, ops::Deref};

use crate::debug::{Debugger, DebuggerWidget, DebuggerWidgetState};

pub const DISPLAY_WIDTH: u8 = 64;
pub const DISPLAY_HEIGHT: u8 = 32;

pub const EMPTY_DISPLAY: DisplayBuffer = [0; DISPLAY_HEIGHT as usize];

// Each u64 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u64s represents the display height
// NOTE: The most signficant bit corresponds to the left-most pixel on the row
pub type DisplayBuffer = [u64; DISPLAY_HEIGHT as usize];

pub enum RenderEvent {
    Refresh,
}

struct DisplayWidget<'a> {
    buf: &'a DisplayBuffer,
}

impl<'a> Widget for DisplayWidget<'_> {
    fn render(self, render_area: Rect, term_buf: &mut tui::buffer::Buffer) {
        // we position the rectangle at (1, 1) because of the 1-pixel wide window border around the display
        let display_area = Rect::new(1, 1, DISPLAY_WIDTH as u16, DISPLAY_HEIGHT as u16);

        // if there is no common area between where the display must be drawn (display_area) and our allowed render area then we do nothing
        if !render_area.intersects(display_area) {
            return;
        }

        // otherwise we draw the part of the display buffer that intersects the allowed render area

        let intersect_area = render_area.intersection(display_area);

        // now get the bit for each position (x, y) in the intersection area
        // and change the color of the terminal background at that position accordingly

        for y in intersect_area.top()..intersect_area.bottom() {
            // reverse the bits of the row so now the least significant bit is our left most pixel (this just makes writing the bitwise ops simpler)
            let bits = self.buf[y as usize - display_area.top() as usize].reverse_bits();
            for x in intersect_area.left()..intersect_area.right() {
                // x - left side of the display area gives us our right shift offset so now just do an AND mask with 1 to get bit at that position
                // set color according to value
                term_buf
                    .get_mut(x, y)
                    .set_bg(if bits >> (x - display_area.left()) & 1 == 1 {
                        Color::Green
                    } else {
                        Color::Gray
                    });
            }
        }
    }
}

pub struct Terminal {
    title: String,
    inner: TUITerminal<CrosstermBackend<io::Stdout>>,
    logger_enabled: bool,
    dbg: Option<(DebuggerWidgetState, Arc<Mutex<Debugger>>)>
}

impl Terminal {
    // change terminal to an alternate screen so user doesnt lose terminal history on exit
    // and enable raw mode so we have full authority over event handling and output
    pub fn setup(title: String, logger_enabled: bool, dbg: Option<Arc<Mutex<Debugger>>>) -> Result<Terminal, io::Error> {
        enable_raw_mode()?;
        let mut stdout = stdout();
        execute!(stdout, EnterAlternateScreen)?;
        Ok(Terminal {
            title,
            inner: TUITerminal::new(CrosstermBackend::new(stdout))?,
            logger_enabled,
            dbg: dbg.map(|dbg| (Default::default(), dbg))
        })
    }

    // clean up the terminal so its usable after program exit
    pub fn exit(&mut self) -> Result<(), io::Error> {
        disable_raw_mode()?;
        execute!(self.inner.backend_mut(), LeaveAlternateScreen)?;
        self.inner.show_cursor()?;
        Ok(())
    }

    // output the given vm display buffer to the terminal
    pub fn draw(&mut self, buf: &DisplayBuffer) -> Result<(), io::Error> {
        self.inner.draw(|f| {
            if let Some((dbg_widget_state, dbg)) = self.dbg.as_mut() {
                let _guard = dbg.lock().unwrap();
                let dbg = _guard.deref();

                if dbg.active {
                    let dbg_area = f.size();
                    let dbg_widget = DebuggerWidget {
                        logger_enabled: self.logger_enabled,
                        dbg
                    };
                    let logger_area = dbg_widget.logger_area(dbg_area);

                    if let Some((x, y)) = dbg_widget.cursor_position(dbg_area, dbg_widget_state) {
                        f.set_cursor(x, y);
                    }

                    f.render_stateful_widget(dbg_widget, dbg_area, dbg_widget_state);
                    
                    drop(_guard);

                    if self.logger_enabled {
                        f.render_widget(Terminal::logger_widget(), logger_area);
                    }

                    return // we dont want to exec code below
                }
            }

            let screen_area = f.size();

            /* prepare vm display window with the program title at the top */
            let vm_window = Block::default()
                .title(self.title.as_str())
                .border_style(Style::default().fg(Color::White))
                .borders(Borders::ALL);

            if self.logger_enabled {
                /* divide screen into two regions flowing horizontally */
                let rects = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints(
                        if screen_area.width > (64 + 2) * 2 {
                            [
                                Constraint::Length(64 + 2),
                                Constraint::Length(screen_area.width - (64 + 2)),
                            ]
                        } else {
                            [Constraint::Percentage(50), Constraint::Percentage(50)]
                        }
                        .as_ref(),
                    )
                    .split(screen_area);

                /* draw the vm window with display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(rects[0]));
                f.render_widget(vm_window, rects[0]);

                /* draw the vm logger */
                f.render_widget(Terminal::logger_widget(), rects[1]);
            } else {
                /* draw the vm window with display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(screen_area));
                f.render_widget(vm_window, screen_area);
            }
        })?;

        Ok(())
    }

    pub fn logger_widget() -> TuiLoggerWidget<'static> {
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