use tui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
};

pub const DISPLAY_WINDOW_WIDTH: u16 = DISPLAY_WIDTH as u16 + 2;
pub const DISPLAY_WINDOW_HEIGHT: u16 = DISPLAY_HEIGHT as u16 + 2;

const DISPLAY_WIDTH: u8 = 64;
const DISPLAY_HEIGHT: u8 = 32;

const CLEAR_DISPLAY: DisplayBuffer = [0; DISPLAY_HEIGHT as usize];

// Each u64 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u64s represents the display height
// NOTE: The most signficant bit corresponds to the left-most pixel on the row
pub type DisplayBuffer = [u64; DISPLAY_HEIGHT as usize];

pub fn write_to_display(
    buf: &mut DisplayBuffer,
    sprite: &[u8],
    mut pos_x: u8,
    mut pos_y: u8,
    height: u8,
) -> bool {
    pos_x %= DISPLAY_WIDTH;
    pos_y %= DISPLAY_HEIGHT;

    let mut flag = false;

    // iterate over rows that aren't clipped by the display height
    //     and map row index to mutable ref of row in display buffer
    //     and expanding 8-bit row data to 64 bits by padding (64 - 8) zeros to the right and then shifting everything to the right by pos_x amount
    //     now we have built a dst &mut u64 (display_row) and src u64 (sprite_row) that we can xor
    for (display_row, sprite_row) in buf[pos_y as usize..]
        .iter_mut()
        .zip(sprite[..(height.min(DISPLAY_HEIGHT - pos_y) as usize)].iter())
        .map(|(display_row, sprite_row)| {
            (
                display_row,
                (*sprite_row as u64) << (u64::BITS - u8::BITS) >> pos_x,
            )
        })
    {
        // if any 2 bits are both 1 then we need to set register VF (VFLAG) to 1
        flag = flag || *display_row & sprite_row != 0;
        *display_row ^= sprite_row;
        //log::trace!("Display Row: {:064b}", display_row);
    }

    flag
}

pub struct Display {
    pub title: String,
    pub buffer: DisplayBuffer,
    pub instruction_frequency: u16
}

impl Display {
    pub fn new(title: String, instruction_frequency: u16) -> Self {
        Self {
            title,
            buffer: CLEAR_DISPLAY,
            instruction_frequency
        }
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
            .title(format!("{}{}Hz ", self.display.title.as_str(), self.display.instruction_frequency))
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
