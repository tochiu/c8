use super::{
    interp::PROGRAM_STARTING_ADDRESS,
    mem::{DEFAULT_PROGRAM_MEMORY_SIZE, XOCHIP_PROGRAM_MEMORY_SIZE},
};

use crate::asm::Disassembler;

use std::{ffi::OsStr, fmt::Display, fs::read, io, path::Path};

#[derive(Copy, Clone)]
pub struct RomConfig {
    pub kind: RomKind, 
    pub quirks: RomQuirks,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RomKind {
    CLASSIC,
    CHIP8,
    SCHIP,
    XOCHIP,
}

#[derive(Clone, Copy)]
pub struct RomQuirks {
    pub bit_shift_modifies_vx_in_place: bool,
    pub load_store_leaves_index_unchanged: bool,
    pub jump_with_offset_uses_vx: bool,
    pub and_or_xor_clears_flag_register: bool,
    pub sprites_clip_at_screen_edges: bool,
    pub wait_for_vertical_sync: bool,
}

impl RomKind {
    pub fn max_size(self) -> usize {
        if self == RomKind::XOCHIP {
            XOCHIP_PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS as usize
        } else {
            DEFAULT_PROGRAM_MEMORY_SIZE - PROGRAM_STARTING_ADDRESS as usize
        }
    }

    pub fn default_cycles_per_frame(self) -> u32 {
        match self {
            Self::CLASSIC => 10,
            Self::CHIP8 => 10,
            Self::SCHIP => 30,
            Self::XOCHIP => 1000,
        }
    }

    pub fn default_rom_quirks(self) -> RomQuirks {
        match self {
            Self::CLASSIC => RomQuirks {
                bit_shift_modifies_vx_in_place: false,
                load_store_leaves_index_unchanged: false,
                jump_with_offset_uses_vx: false,
                and_or_xor_clears_flag_register: true,
                sprites_clip_at_screen_edges: true,
                wait_for_vertical_sync: true,
            },
            Self::CHIP8 => RomQuirks {
                bit_shift_modifies_vx_in_place: true,
                load_store_leaves_index_unchanged: true,
                jump_with_offset_uses_vx: false,
                and_or_xor_clears_flag_register: false,
                sprites_clip_at_screen_edges: true,
                wait_for_vertical_sync: false,
            },
            Self::SCHIP => RomQuirks {
                bit_shift_modifies_vx_in_place: true,
                load_store_leaves_index_unchanged: true,
                jump_with_offset_uses_vx: true,
                and_or_xor_clears_flag_register: false,
                sprites_clip_at_screen_edges: true,
                wait_for_vertical_sync: false,
            },
            Self::XOCHIP => RomQuirks {
                bit_shift_modifies_vx_in_place: false,
                load_store_leaves_index_unchanged: false,
                jump_with_offset_uses_vx: false,
                and_or_xor_clears_flag_register: false,
                sprites_clip_at_screen_edges: false,
                wait_for_vertical_sync: false,
            },
        }
    }
}

impl Display for RomKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CHIP8 => write!(f, "CHIP8"),
            Self::CLASSIC => write!(f, "CHIP8 (CLASSIC)"),
            Self::SCHIP => write!(f, "SCHIP"),
            Self::XOCHIP => write!(f, "X0-CHIP"),
        }
    }
}

#[derive(Clone)]
pub struct Rom {
    pub config: RomConfig,
    pub data: Vec<u8>,
    pub name: String,
}

impl Rom {
    pub fn read<P: AsRef<Path>>(path: P, kind: Option<RomKind>, quirks: Option<RomQuirks>) -> io::Result<Rom> {
        let data = read(path.as_ref())?;
        let kind =
            kind.unwrap_or_else(|| match path.as_ref().extension().and_then(OsStr::to_str) {
                Some("sc8") => RomKind::SCHIP,
                Some("xo8") => RomKind::XOCHIP,
                _ => {
                    if data.len() > DEFAULT_PROGRAM_MEMORY_SIZE {
                        RomKind::XOCHIP
                    } else {
                        let mut dasm = Disassembler::from(Rom {
                            config: RomConfig {
                                kind: RomKind::CHIP8,
                                quirks: RomKind::CHIP8.default_rom_quirks()
                            },
                            data: data.clone(),
                            name: String::new(),
                        });

                        dasm.run();

                        let suggested_rom_kind = dasm.suggested_rom_kind();
                        while suggested_rom_kind != dasm.rom.config.kind {
                            dasm.rom.config.kind = suggested_rom_kind;
                            dasm.rom.config.quirks = quirks.unwrap_or(suggested_rom_kind.default_rom_quirks());
                            dasm.reset();
                            dasm.run();
                        }

                        suggested_rom_kind
                    }
                }
            });

        let rom = Rom {
            name: path
                .as_ref()
                .file_stem()
                .and_then(OsStr::to_str)
                .unwrap_or("Untitled")
                .into(),
            config: RomConfig {
                kind,
                quirks: quirks.unwrap_or(kind.default_rom_quirks())
            },
            data,
        };

        let max_rom_size = rom.config.kind.max_size();

        if rom.data.len() < 2 {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("ROM size ({}B) is below minimum size (2B)", rom.data.len()),
            ))
        } else if rom.data.len() > max_rom_size as usize {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "ROM size ({}B) exceeds maximum size ({}B)",
                    rom.data.len(),
                    max_rom_size
                ),
            ))
        } else {
            Ok(rom)
        }
    }
}
