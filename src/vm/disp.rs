use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
};

pub const DISPLAY_WIDTH: u8 = 64;
pub const DISPLAY_HEIGHT: u8 = 32;

const DISPLAY_WINDOW_WIDTH: u16 = DISPLAY_WIDTH as u16 + 2;

const CLEAR_DISPLAY: DisplayBuffer = [0; DISPLAY_HEIGHT as usize];

// Each u64 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u64s represents the display height
// NOTE: The most signficant bit corresponds to the left-most pixel on the row
pub type DisplayBuffer = [u64; DISPLAY_HEIGHT as usize];

pub struct Display {
    pub title: String,
    pub buffer: DisplayBuffer,
}

impl From<String> for Display {
    fn from(title: String) -> Self {
        Display { title, buffer: CLEAR_DISPLAY }
    }
}

pub struct DisplayWidget<'a> {
    pub display: &'a Display,
    pub logging: bool,
}

impl<'a> DisplayWidget<'_> {
    pub fn logger_area(&self, area: Rect) -> Rect {
        self.areas(area).1
    }

    fn areas(&self, area: Rect) -> (Rect, Rect) {
        if self.logging {
            let rects = Layout::default()
                .direction(Direction::Horizontal)
                .constraints(
                    if area.width > 2 * DISPLAY_WINDOW_WIDTH {
                        [
                            Constraint::Length(DISPLAY_WINDOW_WIDTH),
                            Constraint::Length(area.width - DISPLAY_WINDOW_WIDTH),
                        ]
                    } else {
                        [Constraint::Percentage(50), Constraint::Percentage(50)]
                    }
                    .as_ref(),
                )
                .split(area);
            (rects[0], rects[1])
        } else {
            (area, Rect::default())
        }
    }
}

// TODO: when wake up figure out blank screen bug

impl<'a> Widget for DisplayWidget<'_> {
    fn render(self, area: Rect, buf: &mut tui::buffer::Buffer) {
        // prepare display window
        let window = Block::default()
            .title(self.display.title.as_str())
            .border_style(Style::default().fg(Color::White))
            .borders(Borders::ALL);
        let window_area = self.areas(area).0;
        let window_inner_area = window.inner(window_area);

        window.render(window_area, buf);

        let display_area = Rect::new(
            window_inner_area.x,
            window_inner_area.y,
            DISPLAY_WIDTH as u16,
            DISPLAY_HEIGHT as u16,
        );

        // if there is no common area between where the display must be drawn (display_area) and our allowed render area then we return
        if !window_inner_area.intersects(display_area) {
            return;
        }

        // otherwise we draw the part of the display buffer that intersects the allowed render area
        let intersect_area = window_inner_area.intersection(display_area);

        // now get the bit for each position (x, y) in the intersection area
        // and change the color of the terminal background at that position accordingly

        for y in intersect_area.top()..intersect_area.bottom() {
            // reverse the bits of the row so now the least significant bit is our left most pixel (this just makes writing the bitwise ops simpler)
            let bits = self.display.buffer[y as usize - display_area.top() as usize].reverse_bits();
            for x in intersect_area.left()..intersect_area.right() {
                // x - left side of the display area gives us our right shift offset so now just do an AND mask with 1 to get bit at that position
                // set color according to value
                buf.get_mut(x, y)
                    .set_bg(if bits >> (x - display_area.left()) & 1 == 1 {
                        Color::Green
                    } else {
                        Color::Gray
                    });
            }
        }
    }
}
