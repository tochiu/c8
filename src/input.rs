pub use crossterm::event::{KeyEventKind as CrosstermKeyEventKind, KeyEvent as CrosstermKeyEvent};
pub use device_query::DeviceState as KeyboardBackend;

use device_query::{Keycode as DeviceKey};//, DeviceQuery};
use crossterm::event::KeyCode as CrosstermKey;

use crate::interp::InterpreterInput;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum Key {
    One,
    Two,
    Three,
    Four,
    Q,
    W,
    E,
    R,
    A,
    S,
    D,
    F,
    Z,
    X,
    C,
    V,
}

impl Key {
    fn to_code(self) -> u8 {
        match self {
            Key::One => 0x1,
            Key::Two => 0x2,
            Key::Three => 0x3,
            Key::Four => 0xC,
            Key::Q => 0x4,
            Key::W => 0x5,
            Key::E => 0x6,
            Key::R => 0xD,
            Key::A => 0x7,
            Key::S => 0x8,
            Key::D => 0x9,
            Key::F => 0xE,
            Key::Z => 0xA,
            Key::X => 0x0,
            Key::C => 0xB,
            Key::V => 0xF,
        }
    }
}

impl TryFrom<DeviceKey> for Key {
    type Error = &'static str;
    fn try_from(key: DeviceKey) -> Result<Self, Self::Error> {
        match key {
            DeviceKey::Key1 => Ok(Key::One),
            DeviceKey::Key2 => Ok(Key::Two),
            DeviceKey::Key3 => Ok(Key::Three),
            DeviceKey::Key4 => Ok(Key::Four),
            DeviceKey::Q => Ok(Key::Q),
            DeviceKey::W => Ok(Key::W),
            DeviceKey::E => Ok(Key::E),
            DeviceKey::R => Ok(Key::R),
            DeviceKey::A => Ok(Key::A),
            DeviceKey::S => Ok(Key::S),
            DeviceKey::D => Ok(Key::D),
            DeviceKey::F => Ok(Key::F),
            DeviceKey::Z => Ok(Key::Z),
            DeviceKey::X => Ok(Key::X),
            DeviceKey::C => Ok(Key::C),
            DeviceKey::V => Ok(Key::V),
            _ => Err("not a valid key"),
        }
    }
}

impl TryFrom<CrosstermKey> for Key {
    type Error = &'static str;
    fn try_from(key: CrosstermKey) -> Result<Self, Self::Error> {
        match key {
            CrosstermKey::Char(c) => {
                match c {
                    '1' => Ok(Key::One),
                    '2' => Ok(Key::Two),
                    '3' => Ok(Key::Three),
                    '4' => Ok(Key::Four),
                    'Q' | 'q' => Ok(Key::Q),
                    'W' | 'w' => Ok(Key::W),
                    'E' | 'e' => Ok(Key::E),
                    'R' | 'r' => Ok(Key::R),
                    'A' | 'a' => Ok(Key::A),
                    'S' | 's' => Ok(Key::S),
                    'D' | 'd' => Ok(Key::D),
                    'F' | 'f' => Ok(Key::F),
                    'Z' | 'z' => Ok(Key::Z),
                    'X' | 'x' => Ok(Key::X),
                    'C' | 'c' => Ok(Key::C),
                    'V' | 'v' => Ok(Key::V),
                    _ => Err("not a valid char")
                }
            }
            _ => Err("not a valid key")
        }
    }
}

#[derive(Debug, Default)]
pub struct Keyboard {
    focused: bool,
    focused_pressed_keys: u16,
    //focus_persisting: bool,
    key_down_change: Option<u8>,
    key_up_change: Option<u8>
}

impl Keyboard {
    pub fn handle_focus(&mut self) {
        self.focused = true;
        log::debug!("focus gained");
    }

    pub fn handle_unfocus(&mut self) {
        self.focused = false;
        self.focused_pressed_keys = 0;
        //self.focus_persisting = false;
        self.key_down_change = None;
        self.key_up_change = None;

        log::debug!("clearing pressed keys because of focus lost");
    }

    pub fn handle_crossterm_poke(&mut self, event: CrosstermKeyEvent) {
        if !self.focused {
            self.focused = true;
            log::debug!("focus gained from terminal key event");

            if let Ok(key) = Key::try_from(event.code) {
                // release event only works on kitty terminals (which we dont setup for) but lets just be sure
                if event.kind == CrosstermKeyEventKind::Release {
                    self.handle_key_up(key);
                } else {
                    self.handle_key_down(key);
                }
            }
        }
    }

    pub fn handle_key_down(&mut self, key: Key) {
        if !self.focused {
            return;
        }

        if self.focused_pressed_keys >> key.to_code() & 1 == 0 {
            self.key_down_change = Some(key.to_code());
            self.focused_pressed_keys |= 1 << key.to_code();

            log::debug!("focused key change detected: bitmap {:#018b}", self.focused_pressed_keys);
            log::debug!("changed key -> pressed key {:?} code {:X?}", key, key.to_code());
        }
    }

    pub fn handle_key_up(&mut self, key: Key) {
        if !self.focused {
            return;
        }

        if self.focused_pressed_keys >> key.to_code() & 1 == 1 {
            self.key_up_change = Some(key.to_code());
            self.focused_pressed_keys &= !(1 << key.to_code());

            log::debug!("focused key change detected: bitmap {:#018b}", self.focused_pressed_keys);
            log::debug!("changed key -> released key {:?} code {:X?}", key, key.to_code());
        }
    }

    pub fn flush(&mut self, input: &mut InterpreterInput) {
        input.pressed_keys = self.focused_pressed_keys;
        input.just_pressed_key = self.key_down_change;
        input.just_released_key = self.key_up_change;

        self.key_down_change = None;
        self.key_up_change = None;
    }

    // pub fn update(&mut self, backend: &KeyboardBackend) {
    //     if self.focused {
    //         let pressed_keys = backend
    //             .get_keys()
    //             .into_iter()
    //             .filter_map(|device_key| <DeviceKey as TryInto<Key>>::try_into(device_key).ok())
    //             .fold(0, |pressed_keys, key| pressed_keys | 1 << key.to_code());

    //         let pressed_keys_diff = pressed_keys ^ self.focused_pressed_keys;
    //         self.focused_pressed_keys = pressed_keys;

    //         // if pressed_keys_diff != 0 && self.focus_persisting {
                

    //         //     //return (pressed_keys, Some((key_code, is_pressed)));
    //         // }

    //         if self.focus_persisting {
    //             if pressed_keys_diff != 0 {
    //                 let key_code = pressed_keys_diff.trailing_zeros() as u8;
    //                 let is_pressed = pressed_keys >> key_code & 1 == 1;
                    
    //                 if is_pressed {
    //                     self.key_down_change = Some(key_code);
    //                 } else {
    //                     self.key_up_change = Some(key_code);
    //                 }

    //                 log::debug!("focused key change detected: bitmap {:#018b}", pressed_keys);
    //                 log::debug!(
    //                     "updating {} key code to {:X?}",
    //                     if is_pressed { "pressed" } else { "released" },
    //                     key_code
    //                 );
    //             }
    //         } else {
    //             self.focus_persisting = true;
    //         }

    //         self.focus_persisting = true;
    //     }
    // }
}
