use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
    Terminal as TUITerminal,
};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget};

use crossterm::{
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

use std::io;

pub const DISPLAY_WIDTH: u8 = 64;
pub const DISPLAY_HEIGHT: u8 = 32;

// Each u64 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u64s represents the display height
// NOTE: The most signficant bit corresponds to the left-most pixel on the row 
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

    // if rerender is true, set it to false and return the display buffer, otherwise do nothing
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
}

impl Terminal {

    // change terminal to an alternate screen so user doesnt lose terminal history on exit 
    // and enable raw mode so we have full authority over event handling and output
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
            /* prepare vm display window with the program title at the top */
            let vm_window = Block::default()
                .title(self.title.as_str())
                .border_style(Style::default().fg(Color::White))
                .borders(Borders::ALL);

            if self.logger_enabled {
                /* divide screen into two equally sized regions flowing horizontally */
                let rects = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
                    .split(f.size());

                /* draw the vm window with display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(rects[0]));
                f.render_widget(vm_window, rects[0]);

                /* prepare to draw the vm logger */
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

                /* draw the vm logger */
                f.render_widget(tui_w, rects[1]);
            } else {
                /* draw the vm window with display */
                f.render_widget(DisplayWidget { buf }, vm_window.inner(f.size()));
                f.render_widget(vm_window, f.size());
            }
        })?;

        Ok(())
    }
}
