use super::rom::RomKind;

use crate::set::preset::COLOR_PRESETS;

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
    pub selected_plane_bitflags: u8,
    pub mode: DisplayMode,
    pub planes: [DisplayBuffer; 4],
    pub colors: [Color; 16],
}

impl Default for Display {
    fn default() -> Self {
        Self {
            selected_plane_bitflags: 0b0001,
            mode: DisplayMode::LowResolution,
            planes: [CLEAR_DISPLAY; 4],
            colors: COLOR_PRESETS[0].1,
        }
    }
}

impl Display {
    pub fn set_mode(&mut self, mode: DisplayMode) {
        self.mode = mode;
        self.clear();
    }

    pub fn selected_planes_mut(&mut self) -> impl Iterator<Item = &mut DisplayBuffer> {
        self.planes.iter_mut().enumerate().filter_map(|(i, plane)| {
            if self.selected_plane_bitflags >> i & 1 == 1 {
                Some(plane)
            } else {
                None
            }
        })
    }

    pub fn clear(&mut self) {
        self.selected_planes_mut()
            .for_each(|plane| *plane = CLEAR_DISPLAY);
    }

    pub fn scroll_up(&mut self, amt: usize) {
        let (_, height) = self.mode.dimensions();
        for buffer in self.selected_planes_mut() {
            let fill_start = (height as usize).saturating_sub(amt);
            buffer.copy_within(amt.., 0);
            buffer[fill_start..height as usize].fill(0);
        }
    }

    pub fn scroll_down(&mut self, amt: usize) {
        let (_, height) = self.mode.dimensions();
        for buffer in self.selected_planes_mut() {
            let range_end = height as usize - amt;
            buffer.copy_within(..range_end, amt);
            buffer[..amt].fill(0);
        }
    }

    pub fn scroll_left(&mut self) {
        let amount = if self.mode == DisplayMode::HighResolution {
            4
        } else {
            4 // Octo does 4 no matter what
        };
        for buffer in self.selected_planes_mut() {
            buffer.iter_mut().for_each(|row| *row <<= amount);
        }
    }

    pub fn scroll_right(&mut self) {
        let amount = if self.mode == DisplayMode::HighResolution {
            4
        } else {
            4 // Octo does 4 no matter what
        };
        for buffer in self.selected_planes_mut() {
            buffer.iter_mut().for_each(|row| *row >>= amount);
        }
    }

    pub fn draw(
        &mut self,
        memory: &[u8],
        mut pos_x: u16,
        mut pos_y: u16,
        height: usize,
        bytes_per_row: usize,
        wrap: bool,
    ) -> bool {
        let (display_width, display_height) = self.mode.dimensions();
        let mask = if self.mode == DisplayMode::HighResolution {
            u128::MAX
        } else {
            !(u64::MAX as u128)
        };

        pos_x %= display_width;
        pos_y %= display_height;

        let sprite_bytes = height * bytes_per_row;
        let clipped_sprite_height = height.min((display_height - pos_y) as usize);
        let rendered_sprite_bytes = clipped_sprite_height * bytes_per_row;

        let mut flag = false;
        for (i, plane) in self.selected_planes_mut().enumerate() {
            let sprite_start = sprite_bytes * i;
            let sprite = &memory[sprite_start..sprite_start + rendered_sprite_bytes];
            flag |= draw_plane(plane, sprite, pos_x, pos_y, bytes_per_row, mask);

            if wrap {
                let mut workspace = [0; 128];

                let width = 8 * (bytes_per_row as usize);
                let clipped_sprite_width = width.min((display_width - pos_x) as usize);
                if clipped_sprite_width < width {
                    slice_sprite(
                        &mut workspace,
                        memory,
                        clipped_sprite_width,
                        0,
                        clipped_sprite_height,
                        bytes_per_row,
                    );
                    flag |= draw_plane(plane, &workspace, 0, pos_y, bytes_per_row, mask);
                }

                if clipped_sprite_height < height {
                    slice_sprite(
                        &mut workspace,
                        memory,
                        0,
                        clipped_sprite_height,
                        height - clipped_sprite_height,
                        bytes_per_row,
                    );
                    flag |= draw_plane(plane, &workspace, pos_x, 0, bytes_per_row, mask);
                }

                if clipped_sprite_width < width && clipped_sprite_height < height {
                    slice_sprite(
                        &mut workspace,
                        memory,
                        clipped_sprite_width,
                        clipped_sprite_height,
                        height - clipped_sprite_height,
                        bytes_per_row,
                    );
                    flag |= draw_plane(plane, &workspace, 0, 0, bytes_per_row, mask);
                }
            }
        }
        flag
    }
}

fn slice_sprite(
    dst: &mut [u8],
    sprite: &[u8],
    x: usize,
    y: usize,
    height: usize,
    bytes_per_row: usize,
) {
    dst.fill(0);
    let mut offset = 0;
    for row_slice_bytes in sprite[bytes_per_row * y..bytes_per_row * (y + height)]
        .chunks_exact(bytes_per_row)
        .map(|row| &row[x / 8..])
    {
        let byte_shift = x % 8;
        if byte_shift == 0 {
            for byte in row_slice_bytes.iter() {
                dst[offset] = *byte;
                offset += 1;
            }
        } else {
            dst[offset] = row_slice_bytes[0] << byte_shift;
            for byte in row_slice_bytes[1..].iter() {
                dst[offset] |= *byte >> (8 - byte_shift);
                offset += 1;
                dst[offset] = *byte << byte_shift;
            }
            offset += 1;
        }
    }
}

fn draw_plane(
    plane: &mut DisplayBuffer,
    sprite: &[u8],
    pos_x: u16,
    pos_y: u16,
    bytes_per_row: usize,
    mask: u128,
) -> bool {
    let mut flag = false;

    // iterate over bytes_per_row chunks and combine them into a u128
    // then shift the row all the way to the left and shift it back to the right by pos_x
    // then AND it with the mask to make sure we don't draw outside the display
    for (display_row, sprite_row) in
        plane[pos_y as usize..]
            .iter_mut()
            .zip(sprite.chunks_exact(bytes_per_row).map(|chunk| {
                chunk
                    .iter()
                    .fold(0u128, |row, byte| (row << 8) | *byte as u128)
                    << (128 - 8 * bytes_per_row)
                    >> pos_x
                    & mask
            }))
    {
        // if any 2 bits are both 1 then we need to set register VF (VFLAG) to 1
        flag = flag || *display_row & sprite_row != 0;
        *display_row ^= sprite_row;
    }

    flag
}

pub struct DisplayWidget<'a, 'b> {
    pub display: &'a Display,
    pub logging: bool,
    pub rom_name: &'b str,
    pub rom_kind: RomKind,
    pub execution_frequency: u32,
}

impl<'a, 'b> DisplayWidget<'_, '_> {
    pub fn title(&self) -> String {
        format!(
            " {} Virtual Machine ({}) {}Hz ",
            self.rom_kind, self.rom_name, self.execution_frequency
        )
    }

    fn pixel_stream(
        plane: &DisplayBuffer,
        width: usize,
        height: usize,
    ) -> impl Iterator<Item = bool> + '_ {
        plane
            .iter()
            .take(height)
            .map(move |plane_row| (0..width).map(|shift| (*plane_row >> (127 - shift) & 1 == 1)))
            .flatten()
    }
}

impl<'a, 'b> Widget for DisplayWidget<'_, '_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let (display_width, display_height) = self.display.mode.dimensions();

        // terminal pixel height is twice the width but there is a unicode top-half block (▀) and bottom-half block (▄)
        // so for each pixel in the row of the terminal we can use half-block color and the background color to represent 2 pixels in the display
        // so for each row of the terminal we can fit 2 rows of the display

        let rendered_display_width = area.width.min(display_width) as usize;
        let rendered_display_height = 2 * area.height.min(display_height) as usize;

        let mut pixel_streams = [0, 1, 2, 3].map(|i| {
            (i, DisplayWidget::pixel_stream(
                &self.display.planes[i],
                rendered_display_width,
                rendered_display_height,
            ))
        });

        for i in 0..rendered_display_width * rendered_display_height {
            let color =
                self.display.colors[pixel_streams.iter_mut().fold(0, |color_index, (plane_index, stream)| {
                    color_index
                        | (stream
                            .next()
                            .expect("Stream must be the size of the rendered area")
                            as usize)
                            << *plane_index
                })];

            let x = i % rendered_display_width;
            let y = i / rendered_display_width;

            let cell = buf.get_mut(area.left() + x as u16, area.top() + y as u16 / 2);

            if y % 2 == 0 {
                cell.set_bg(color);
            } else {
                cell.set_fg(color).set_symbol("▄");
            }
        }
    }
}
