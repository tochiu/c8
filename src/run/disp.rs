use super::rom::RomKind;

use tui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
};

#[derive(Clone, PartialEq, Eq)]
pub enum DisplayMode {
    LowResolution,
    HighResolution,
}

impl DisplayMode {
    pub fn dimensions(&self) -> (u8, u8) {
        match self {
            Self::LowResolution => (LORES_DISPLAY_WIDTH, LORES_DISPLAY_HEIGHT),
            Self::HighResolution => (HIRES_DISPLAY_WIDTH, HIRES_DISPLAY_HEIGHT),
        }
    }
}

const LORES_DISPLAY_WIDTH: u8 = 64;
const LORES_DISPLAY_HEIGHT: u8 = 32;

const HIRES_DISPLAY_WIDTH: u8 = 128;
const HIRES_DISPLAY_HEIGHT: u8 = 64;

const CLEAR_DISPLAY: DisplayBuffer = [0; HIRES_DISPLAY_HEIGHT as usize];

// Each u128 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u128 represents the display height
// NOTE: The most signficant bit corresponds to the left-most pixel on the row
pub type DisplayBuffer = [u128; HIRES_DISPLAY_HEIGHT as usize];

#[derive(Clone, PartialEq, Eq)]
pub struct Display {
    pub mode: DisplayMode,
    pub buffer: DisplayBuffer,
}

impl Default for Display {
    fn default() -> Self {
        Self {
            mode: DisplayMode::LowResolution,
            buffer: CLEAR_DISPLAY,
        }
    }
}

impl Display {
    pub fn set_mode(&mut self, mode: DisplayMode) {
        self.mode = mode;
        self.clear();
    }

    pub fn clear(&mut self) {
        self.buffer.fill(0);
    }
    
    pub fn scroll_down(&mut self, amt: usize) {
        let range_end = self.buffer.len() - amt;
        self.buffer.copy_within(..range_end, amt);
        self.buffer[..amt].fill(0);
    }

    pub fn scroll_left(&mut self) {
        self.buffer.iter_mut().for_each(|row| *row <<= 4);
    }

    pub fn scroll_right(&mut self) {
        self.buffer.iter_mut().for_each(|row| *row >>= 4);
    }

    pub fn draw(
        &mut self,
        sprite: &[u8],
        mut pos_x: u8,
        mut pos_y: u8,
        height: u8,
        is_16bit_sprite: bool,
    ) -> bool {
        let (display_width, display_height) = self.mode.dimensions();
        let buf = &mut self.buffer;
        let bytes_per_chunk = if is_16bit_sprite { 2 } else { 1 };
        let mask = if self.mode == DisplayMode::HighResolution {
            u128::MAX
        } else {
            !(u64::MAX as u128)
        };

        pos_x %= display_width;
        pos_y %= display_height;

        let mut flag = false;

        // iterate over rows that aren't clipped by the display height
        //     and map row index to mutable ref of row in display buffer
        //     and expanding 8-bit row data to 64 bits by padding (64 - 8) zeros to the right and then shifting everything to the right by pos_x amount
        //     now we have built a dst &mut u128 (display_row) and src u128 (sprite_row) that we can xor
        for (display_row, sprite_row) in buf[pos_y as usize..].iter_mut().zip(
            sprite[..(height.min(display_height - pos_y) as usize) * bytes_per_chunk]
                .chunks_exact(bytes_per_chunk)
                .map(|chunk| {
                    chunk
                        .iter()
                        .fold(0u128, |row, byte| (row << 8) | *byte as u128)
                        << (128 - 8 * bytes_per_chunk)
                        >> pos_x
                        & mask
                }),
        ) {
            // if any 2 bits are both 1 then we need to set register VF (VFLAG) to 1
            flag = flag || *display_row & sprite_row != 0;
            *display_row ^= sprite_row;
        }

        flag
    }
}

pub struct DisplayWidget<'a, 'b> {
    pub display: &'a Display,
    pub logging: bool,
    pub title: &'b str,
    pub rom_kind: RomKind,
    pub instruction_frequency: u16,
}

impl<'a, 'b> DisplayWidget<'_, '_> {
    pub fn logger_area(&self, area: Rect) -> Rect {
        self.areas(area).1
    }

    fn areas(&self, area: Rect) -> (Rect, Rect) {
        let (display_width, _) = self.display.mode.dimensions();
        if self.logging {
            let rects = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Length(display_width as u16 + 2),
                    Constraint::Length(area.width.saturating_sub(display_width as u16 + 2)),
                ])
                .split(area);
            (rects[0], rects[1])
        } else {
            (area, Rect::default())
        }
    }
}

// TODO: when wake up figure out blank screen bug

impl<'a, 'b> Widget for DisplayWidget<'_, '_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // prepare display window
        let window = Block::default()
            .title(format!(" {} Virtual Machine ({}) {}Hz ", self.rom_kind, self.title, self.instruction_frequency))
            .border_style(Style::default().fg(Color::White))
            .borders(Borders::ALL);
        let window_area = self.areas(area).0;
        let window_inner_area = window.inner(window_area);

        window.render(window_area, buf);

        let (display_width, display_height) = self.display.mode.dimensions();

        let display_area = Rect::new(
            window_inner_area.x,
            window_inner_area.y,
            display_width as u16,
            display_height as u16/2,
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
            let up_bits = self.display.buffer[2*(y as usize - display_area.top() as usize)].reverse_bits();
            let down_bits = self.display.buffer[2*(y as usize - display_area.top() as usize) + 1].reverse_bits();

            for x in intersect_area.left()..intersect_area.right() {
                // x - left side of the display area gives us our right shift offset so now just do an AND mask with 1 to get bit at that position
                // set color according to value
                let color_top = up_bits >> (x - display_area.left()) & 1 == 1;
                let color_bottom = down_bits >> (x - display_area.left()) & 1 == 1;

                let cell = buf.get_mut(x, y);

                match (color_top, color_bottom) {
                    (true, true) => cell.set_bg(Color::Green),
                    (true, false) => cell.set_bg(Color::Gray).set_fg(Color::Green).set_symbol("▀"),
                    (false, true) => cell.set_bg(Color::Gray).set_fg(Color::Green).set_symbol("▄"),
                    (false, false) => cell.set_bg(Color::Gray),
                };
            }
        }
    }
}
