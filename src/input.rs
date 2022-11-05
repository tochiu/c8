pub use crossterm::event::KeyEventKind;

use device_query::{DeviceQuery, DeviceState, Keycode as DeviceKey};

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
    fn to_code(&self) -> u8 {
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

#[derive(Debug, Default)]
pub struct Keyboard {
    state: DeviceState,
    focused: bool,
    focused_pressed_keys: u16,
    focus_persisting: bool,
}

impl Keyboard {
    pub fn handle_focus(&mut self) {
        self.focused = true;
        log::debug!("focus gained");
    }

    pub fn handle_unfocus(&mut self) {
        self.focused = false;
        self.focused_pressed_keys = 0;
        self.focus_persisting = false;
        log::debug!("clearing pressed keys because of focus lost");
    }

    pub fn handle_poke(&mut self) {
        // called when we want to force a focus and allow input changes to count as persisting
        if !self.focused {
            self.focused = true;
            self.focus_persisting = true;
            log::debug!("focus gained from poke");
        }
    }

    pub fn update(&mut self) -> (u16, Option<(u8, bool)>) {
        if self.focused {
            let pressed_keys = self
                .state
                .get_keys()
                .into_iter()
                .filter_map(|device_key| <DeviceKey as TryInto<Key>>::try_into(device_key).ok())
                .fold(0, |pressed_keys, key| pressed_keys | 1 << key.to_code());

            let pressed_keys_diff = pressed_keys ^ self.focused_pressed_keys;
            self.focused_pressed_keys = pressed_keys;

            if pressed_keys_diff != 0 && self.focus_persisting {
                let key_code = pressed_keys_diff.trailing_zeros() as u8;
                let is_pressed = pressed_keys >> key_code & 1 == 1;
                log::debug!("focused key change detected: bitmap {:#018b}", pressed_keys);
                log::debug!(
                    "sending {} key code {:X?}",
                    if is_pressed { "pressed" } else { "released" },
                    key_code
                );

                return (pressed_keys, Some((key_code, is_pressed)));
            }

            self.focus_persisting = true;
            (pressed_keys, None)
        } else {
            (0, None)
        }
    }
}
