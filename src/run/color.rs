use tui::{
    layout::{Alignment, Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, StatefulWidget, Widget},
};

use super::preset::COLOR_PRESETS;

struct HSV {
    h: f32,
    s: f32,
    v: f32,
}

impl HSV {
    fn rgb(self) -> Color {
        let c = self.v * self.s;
        let x = c * (1.0 - ((self.h / 60.0) % 2.0 - 1.0).abs());
        let m = self.v - c;

        let (r0, g0, b0) = match (self.h / 60.0).round() as u8 {
            0 => (c, x, 0.0),
            1 => (x, c, 0.0),
            2 => (0.0, c, x),
            3 => (0.0, x, c),
            4 => (x, 0.0, c),
            5 => (c, 0.0, x),
            _ => (c, x, 0.0),
        };

        Color::Rgb(
            ((r0 + m) * 255.0).round() as u8,
            ((g0 + m) * 255.0).round() as u8,
            ((b0 + m) * 255.0).round() as u8,
        )
    }
}

pub struct ColorViewWidget;

impl Widget for ColorViewWidget {
    fn render(self, area: tui::layout::Rect, buf: &mut tui::buffer::Buffer) {
        let h = 24.0;

        let sat_len = 32.min(area.width);

        for x in 0..sat_len {
            for y in 0..16.min(area.height) {
                let s = x as f32 / 31.0 + (1.0 - sat_len.saturating_sub(1) as f32 / 31.0);
                let v0 = 1.0 - (2 * y) as f32 / 31.0;
                let v1 = 1.0 - (2 * y + 1) as f32 / 31.0;

                buf.get_mut(area.left() + x, area.top() + y)
                    .set_symbol("▄")
                    .set_bg(HSV { h, s, v: v0 }.rgb())
                    .set_fg(HSV { h, s, v: v1 }.rgb());
            }
        }
    }
}

const COLOR_PRESET_CHOICE: usize = 1;

pub struct ColorPickerWidget;

impl Widget for ColorPickerWidget {
    fn render(self, area: tui::layout::Rect, buf: &mut tui::buffer::Buffer) {
        if area.area() == 0 {
            return;
        }

        let [top_half, _] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(16),
                Constraint::Length(area.height.saturating_sub(16))
            ])
            .split(area)[..] else { unreachable!() };

        let [hsv_area, top_right_area] = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(top_half.width.saturating_sub(16 + 2).min(30)),
                Constraint::Length(16 + 2 + top_half.width.saturating_sub(16 + 2 + 30)),
            ])
            .split(top_half)[..] else { unreachable!() };

        let [sv_area, hue_area] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(hsv_area.height.saturating_sub(1)),
                Constraint::Length(1)
            ])
            .split(hsv_area)[..] else { unreachable!() };

        let [preset_list_area, selected_preset_colors] = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Length(16.min(top_right_area.width.saturating_sub(2))),
                Constraint::Length(top_right_area.width.saturating_sub(16.min(top_right_area.width.saturating_sub(2))))
            ])
            .split(top_right_area)[..] else { unreachable!() };

        let [_, custom_hex_color_area, custom_rgb_color_area, custom_hsv_color_area, ..] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(COLOR_PRESETS.len() as u16 + 1),
                Constraint::Length(3),
                Constraint::Length(3),
                Constraint::Length(3),
                Constraint::Length(preset_list_area.height.saturating_sub(COLOR_PRESETS.len() as u16 + 1 + 3 * 3))
            ])
            .split(preset_list_area)[..] else { unreachable!() };

        Paragraph::new("#FF6600")
            .alignment(Alignment::Center)
            .block(
                Block::default()
                    .title(" HEX ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Black)),
            )
            .style(Style::default().fg(Color::Black))
            .render(custom_hex_color_area, buf);

        Paragraph::new("255 102 000")
            .alignment(Alignment::Center)
            .block(
                Block::default()
                    .title(" RGB ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Black)),
            )
            .style(Style::default().fg(Color::Black))
            .render(custom_rgb_color_area, buf);

        Paragraph::new("024° 100% 100%")
            .alignment(Alignment::Center)
            .block(
                Block::default()
                    .title(" HSV ")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Black)),
            )
            .style(Style::default().fg(Color::Black))
            .render(custom_hsv_color_area, buf);

        let mut preset_list_state = ListState::default();
        preset_list_state.select(Some(COLOR_PRESET_CHOICE));

        StatefulWidget::render(
            List::new(
                COLOR_PRESETS
                    .iter()
                    .map(|x| ListItem::new(x.0))
                    .chain(std::iter::once(ListItem::new("Custom")))
                    .collect::<Vec<_>>(),
            )
            .block(Block::default().style(Style::default().bg(Color::White).fg(Color::Black)))
            .highlight_style(
                Style::default()
                    .add_modifier(Modifier::BOLD)
                    .bg(Color::Black)
                    .fg(Color::White),
            )
            .highlight_symbol(" >> "),
            preset_list_area,
            buf,
            &mut preset_list_state,
        );

        for y in 0..16.min(selected_preset_colors.height) {
            let color = COLOR_PRESETS[COLOR_PRESET_CHOICE].1[y as usize];
            for x in 0..selected_preset_colors.width {
                buf.get_mut(selected_preset_colors.x + x, selected_preset_colors.y + y)
                    .set_bg(color);
            }
        }

        ColorViewWidget::render(ColorViewWidget, sv_area, buf);

        if hue_area.area() > 0 {
            for i in 0..hue_area.width {
                buf.get_mut(hue_area.x + i, hue_area.y).set_bg(
                    HSV {
                        h: i as f32 / hue_area.width as f32 * 360.0,
                        s: 1.0,
                        v: 1.0,
                    }
                    .rgb(),
                );
            }
        }
    }
}
