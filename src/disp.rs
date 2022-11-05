use std::io;

use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
    Terminal as TUITerminal,
};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};

pub const DISPLAY_WIDTH: u8 = 64;
pub const DISPLAY_HEIGHT: u8 = 32;

pub type DisplayBuffer = [u64; DISPLAY_HEIGHT as usize];

pub struct Display {
    buf: DisplayBuffer,
    rerender: bool,
}

impl Default for Display {
    fn default() -> Self {
        Display {
            buf: [0; DISPLAY_HEIGHT as usize],
            rerender: false,
        }
    }
}

impl Display {
    pub fn update(&mut self, buf: &DisplayBuffer) {
        self.buf = *buf;
        self.refresh();
    }

    pub fn refresh(&mut self) {
        self.rerender = true;
    }

    pub fn extract_new_frame(&mut self) -> Option<DisplayBuffer> {
        if self.rerender {
            self.rerender = false;
            Some(self.buf)
        } else {
            None
        }
    }
}

struct DisplayWidget<'a> {
    buf: &'a DisplayBuffer,
}

impl<'a> Widget for DisplayWidget<'_> {
    fn render(self, term_draw_area: Rect, term_buf: &mut tui::buffer::Buffer) {
        let display_area = Rect::new(1, 1, DISPLAY_WIDTH as u16, DISPLAY_HEIGHT as u16);
        if !term_draw_area.intersects(display_area) {
            return;
        }

        let intersection = term_draw_area.intersection(display_area);

        for y in intersection.top()..intersection.bottom() {
            let bits = self.buf[y as usize - display_area.top() as usize].reverse_bits();
            for x in intersection.left()..intersection.right() {
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
}

impl Terminal {
    pub fn setup(title: String, logger_enabled: bool) -> Result<Terminal, io::Error> {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        Ok(Terminal {
            title,
            inner: TUITerminal::new(CrosstermBackend::new(stdout))?,
            logger_enabled,
        })
    }

    pub fn exit(&mut self) -> Result<(), io::Error> {
        disable_raw_mode()?;
        execute!(self.inner.backend_mut(), LeaveAlternateScreen)?;
        self.inner.show_cursor()?;
        Ok(())
    }

    pub fn draw(&mut self, buf: &DisplayBuffer) -> Result<(), io::Error> {
        self.inner.draw(|f| {
            let vm_window = Block::default()
                .title(self.title.as_str())
                .border_style(Style::default().fg(Color::White))
                .borders(Borders::ALL);

            if self.logger_enabled {
                /* divide screen for the logger and display */
                let rects = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
                    .split(f.size());

                /* draw the display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(rects[0]));
                f.render_widget(vm_window, rects[0]);

                let tui_w = TuiLoggerWidget::default()
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
                    .style_info(Style::default().fg(Color::Green));

                /* draw the logger */
                f.render_widget(tui_w, rects[1]);
            } else {
                /* draw the display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(f.size()));
                f.render_widget(vm_window, f.size());
            }
        })?;

        Ok(())
    }
}
