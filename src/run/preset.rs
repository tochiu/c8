use tui::style::Color;

pub const COLOR_PRESETS: [(&'static str, [Color; 16]); 6] = [
    GRAYSCALE_COLOR_PRESET,
    OCTO_COLOR_PRESET,
    LCD_COLOR_PRESET,
    HOTDOG_COLOR_PRESET,
    CGA0_COLOR_PRESET,
    CGA1_COLOR_PRESET,
];

const GRAYSCALE_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "Grayscale",
    [
        Color::Rgb(000, 000, 000),
        Color::Rgb(255, 255, 255),
        Color::Rgb(085, 085, 085),
        Color::Rgb(170, 170, 170),
        Color::Rgb(017, 017, 017),
        Color::Rgb(238, 238, 238),
        Color::Rgb(092, 092, 092),
        Color::Rgb(187, 187, 187),
        Color::Rgb(034, 034, 034),
        Color::Rgb(221, 221, 221),
        Color::Rgb(119, 119, 119),
        Color::Rgb(204, 204, 204),
        Color::Rgb(051, 051, 051),
        Color::Rgb(153, 153, 153),
        Color::Rgb(068, 068, 068),
        Color::Rgb(102, 102, 102),
    ],
);

const OCTO_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "Octo",
    [
        Color::Rgb(153, 102, 000),
        Color::Rgb(255, 204, 000),
        Color::Rgb(255, 102, 000),
        Color::Rgb(102, 034, 000),
        Color::Rgb(153, 102, 000),
        Color::Rgb(255, 204, 000),
        Color::Rgb(255, 102, 000),
        Color::Rgb(102, 034, 000),
        Color::Rgb(153, 102, 000),
        Color::Rgb(255, 204, 000),
        Color::Rgb(255, 102, 000),
        Color::Rgb(102, 034, 000),
        Color::Rgb(153, 102, 000),
        Color::Rgb(255, 204, 000),
        Color::Rgb(255, 102, 000),
        Color::Rgb(102, 034, 000),
    ],
);

const LCD_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "LCD",
    [
        Color::Rgb(0xF9, 0xFF, 0xB3),
        Color::Rgb(0x3D, 0x80, 0x26),
        Color::Rgb(0xAB, 0xCC, 0x47),
        Color::Rgb(0x00, 0x13, 0x1A),
        Color::Rgb(0xF9, 0xFF, 0xB3),
        Color::Rgb(0x3D, 0x80, 0x26),
        Color::Rgb(0xAB, 0xCC, 0x47),
        Color::Rgb(0x00, 0x13, 0x1A),
        Color::Rgb(0xF9, 0xFF, 0xB3),
        Color::Rgb(0x3D, 0x80, 0x26),
        Color::Rgb(0xAB, 0xCC, 0x47),
        Color::Rgb(0x00, 0x13, 0x1A),
        Color::Rgb(0xF9, 0xFF, 0xB3),
        Color::Rgb(0x3D, 0x80, 0x26),
        Color::Rgb(0xAB, 0xCC, 0x47),
        Color::Rgb(0x00, 0x13, 0x1A),
    ],
);

const HOTDOG_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "Hot Dog",
    [
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0xFF, 0xFF, 0xFF),
    ],
);

const CGA0_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "CGA 0",
    [
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0x00, 0xFF, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0x00, 0xFF, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0x00, 0xFF, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0x00, 0xFF, 0x00),
        Color::Rgb(0xFF, 0x00, 0x00),
        Color::Rgb(0xFF, 0xFF, 0x00),
    ],
);

const CGA1_COLOR_PRESET: (&'static str, [Color; 16]) = (
    "CGA 1",
    [
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0xFF),
        Color::Rgb(0x00, 0xFF, 0xFF),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0xFF),
        Color::Rgb(0x00, 0xFF, 0xFF),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0xFF),
        Color::Rgb(0x00, 0xFF, 0xFF),
        Color::Rgb(0xFF, 0xFF, 0xFF),
        Color::Rgb(0x00, 0x00, 0x00),
        Color::Rgb(0xFF, 0x00, 0xFF),
        Color::Rgb(0x00, 0xFF, 0xFF),
        Color::Rgb(0xFF, 0xFF, 0xFF),
    ],
);
