use super::rom::RomKind;

use tui::{buffer::Buffer, layout::Rect, style::Color, widgets::Widget};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DisplayMode {
    LowResolution,
    HighResolution,
}

impl DisplayMode {
    pub fn dimensions(&self) -> (u16, u16) {
        match self {
            Self::LowResolution => (LORES_DISPLAY_WIDTH, LORES_DISPLAY_HEIGHT),
            Self::HighResolution => (HIRES_DISPLAY_WIDTH, HIRES_DISPLAY_HEIGHT),
        }
    }

    pub fn window_dimensions(&self) -> (u16, u16) {
        let (width, height) = self.dimensions();
        (width as u16 + 2, height as u16 / 2 + 2)
    }
}

const LORES_DISPLAY_WIDTH: u16 = 64;
const LORES_DISPLAY_HEIGHT: u16 = 32;

const HIRES_DISPLAY_WIDTH: u16 = 128;
const HIRES_DISPLAY_HEIGHT: u16 = 64;

const CLEAR_DISPLAY: DisplayBuffer = [0; HIRES_DISPLAY_HEIGHT as usize];

// Each u128 represents a row of the display with each bit representing whether that pixel should be on or not
// The number of u128 represents the display height
// NOTE: The left-most pixel on the row corresponds to the most significant bit
pub type DisplayBuffer = [u128; HIRES_DISPLAY_HEIGHT as usize];

#[derive(Clone, PartialEq, Eq, Debug)]
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
        mut pos_x: u16,
        mut pos_y: u16,
        height: u16,
        is_16bit_sprite: bool,
    ) -> bool {
        let (display_width, display_height) = self.mode.dimensions();
        let buf = &mut self.buffer;
        let bytes_per_row = if is_16bit_sprite { 2 } else { 1 };
        let mask = if self.mode == DisplayMode::HighResolution {
            u128::MAX
        } else {
            !(u64::MAX as u128)
        };

        pos_x %= display_width;
        pos_y %= display_height;

        let mut flag = false;

        // iterate over bytes_per_row chunks and combine them into a u128
        // then shift the row all the way to the left and shift it back to the right by pos_x
        // then AND it with the mask to make sure we don't draw outside the display
        for (display_row, sprite_row) in buf[pos_y as usize..].iter_mut().zip(
            sprite[..(height.min(display_height - pos_y) as usize) * bytes_per_row]
                .chunks_exact(bytes_per_row)
                .map(|chunk| {
                    chunk
                        .iter()
                        .fold(0u128, |row, byte| (row << 8) | *byte as u128)
                        << (128 - 8 * bytes_per_row)
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
    pub rom_name: &'b str,
    pub rom_kind: RomKind,
    pub execution_frequency: u16,
}

impl<'a, 'b> DisplayWidget<'_, '_> {
    pub fn title(&self) -> String {
        format!(
            " {} Virtual Machine ({}) {}Hz ",
            self.rom_kind, self.rom_name, self.execution_frequency
        )
    }
}

impl<'a, 'b> Widget for DisplayWidget<'_, '_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let (display_width, display_height) = self.display.mode.dimensions();

        // terminal pixel height is twice the width but there is a unicode top-half block (▀) and bottom-half block (▄)
        // so for each pixel in the row of the terminal we can use half-block color and the background color to represent 2 pixels in the display
        // so for each row of the terminal we can fit 2 rows of the display
        for area_row_index in 0..area.height.min(display_height as u16 / 2) {
            // NOTE: we reverse the bits so the left most pixel is the least significant bit (this makes writing the bitwise ops simpler)
            let row_top_bits = self.display.buffer[2 * area_row_index as usize + 0].reverse_bits();
            let row_bot_bits = self.display.buffer[2 * area_row_index as usize + 1].reverse_bits();

            for area_column_index in 0..area.width.min(display_width as u16) {
                let top_is_on = row_top_bits >> area_column_index & 1 == 1;
                let bot_is_on = row_bot_bits >> area_column_index & 1 == 1;

                let cell =
                    buf.get_mut(area.left() + area_column_index, area.top() + area_row_index);

                if top_is_on && bot_is_on {
                    cell.set_bg(Color::Green);
                } else {
                    cell.set_bg(Color::Gray);
                    cell.set_fg(Color::Green);
                    if top_is_on {
                        cell.set_symbol("▀");
                    } else if bot_is_on {
                        cell.set_symbol("▄");
                    }
                }
            }
        }
    }
}
