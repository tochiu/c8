use super::interp::InterpreterInput;

use crossterm::event::KeyCode as CrosstermKey;
use device_query::Keycode as DeviceKey;

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

pub const KEY_ORDERING: [Key; 16] = [
    Key::One,
    Key::Two,
    Key::Three,
    Key::Four,
    Key::Q,
    Key::W,
    Key::E,
    Key::R,
    Key::A,
    Key::S,
    Key::D,
    Key::F,
    Key::Z,
    Key::X,
    Key::C,
    Key::V,
];

impl Key {
    // Key enum to byte key code specified by CHIP-8 interpreters
    pub fn to_code(self) -> u8 {
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

    pub fn to_str(self) -> &'static str {
        match self {
            Key::One => "1",
            Key::Two => "2",
            Key::Three => "3",
            Key::Four => "4",
            Key::Q => "Q",
            Key::W => "W",
            Key::E => "E",
            Key::R => "R",
            Key::A => "A",
            Key::S => "S",
            Key::D => "D",
            Key::F => "F",
            Key::Z => "Z",
            Key::X => "X",
            Key::C => "C",
            Key::V => "V",
        }
    }
}

impl From<Key> for &'static str {
    fn from(key: Key) -> Self {
        key.to_str()
    }
}

impl TryFrom<&str> for Key {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "1" => Ok(Key::One),
            "2" => Ok(Key::Two),
            "3" => Ok(Key::Three),
            "4" => Ok(Key::Four),
            "Q" | "q" => Ok(Key::Q),
            "W" | "w" => Ok(Key::W),
            "E" | "e" => Ok(Key::E),
            "R" | "r" => Ok(Key::R),
            "A" | "a" => Ok(Key::A),
            "S" | "s" => Ok(Key::S),
            "D" | "d" => Ok(Key::D),
            "F" | "f" => Ok(Key::F),
            "Z" | "z" => Ok(Key::Z),
            "X" | "x" => Ok(Key::X),
            "C" | "c" => Ok(Key::C),
            "V" | "v" => Ok(Key::V),
            _ => Err("not a valid key"),
        }
    }
}

impl TryFrom<u8> for Key {
    type Error = &'static str;
    fn try_from(code: u8) -> Result<Self, Self::Error> {
        match code {
            0x1 => Ok(Key::One),
            0x2 => Ok(Key::Two),
            0x3 => Ok(Key::Three),
            0xC => Ok(Key::Four),
            0x4 => Ok(Key::Q),
            0x5 => Ok(Key::W),
            0x6 => Ok(Key::E),
            0xD => Ok(Key::R),
            0x7 => Ok(Key::A),
            0x8 => Ok(Key::S),
            0x9 => Ok(Key::D),
            0xE => Ok(Key::F),
            0xA => Ok(Key::Z),
            0x0 => Ok(Key::X),
            0xB => Ok(Key::C),
            0xF => Ok(Key::V),
            _ => Err("not a valid key code"),
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
            CrosstermKey::Char(c) => match c {
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
                _ => Err("not a valid char"),
            },
            _ => Err("not a valid key"),
        }
    }
}
// Keyboard holds state necessary for providing keyboard state to CHIP-8 interpeters
#[derive(Debug, Default, Clone, Copy)]
pub struct Keyboard {
    focused: bool,

    // This field is a bitmap of the keyboard state because each key has
    // a hexadecimal value and there are exactly 16 of them so we can do this
    // This field is needed for the SkipIfKeyDown and SkipIfKeyUp instructions
    // A 1 is key down and a 0 is key up
    focused_down_keys: u16,

    // These fields are used in the GetKey instruction since it must wait for a change
    // These fields are ephemeral and are therefore supposed to be cleared on flush (which should be called each interpreter step)
    key_down_change: Option<u8>,
    key_up_change: Option<u8>,
}

impl PartialEq for Keyboard {
    fn eq(&self, other: &Self) -> bool {
        self.focused_down_keys == other.focused_down_keys
            && self.key_down_change == other.key_down_change
            && self.key_up_change == other.key_up_change
    }
}

impl Keyboard {
    pub fn clear(&mut self) {
        *self = Keyboard::default();
    }

    pub fn state(&self) -> (&u16, &Option<u8>, &Option<u8>) {
        (
            &self.focused_down_keys,
            &self.key_down_change,
            &self.key_up_change,
        )
    }

    // on terminal focus
    pub fn handle_focus(&mut self) {
        if !self.focused {
            self.focused = true;
            log::info!("focus gained");
        }
    }

    // on terminal unfocus
    pub fn handle_unfocus(&mut self) {
        self.focused = false;
        self.focused_down_keys = 0;
        self.key_down_change = None;
        self.key_up_change = None;

        log::info!("clearing pressed keys because of focus lost");
    }

    // we default set focused to false but the terminal could just not fire a terminal focus event at the start or not support such events in the first place
    // therefore if we receive an event from crossterm but our state is unfocued we know this must be incorrect and update it accordingly
    // afterwards we must handle the trigger key if possible because we cannot rely on device_query being able to capture events after crossterm (a data race)
    pub fn handle_focusing_key_down(&mut self, key: Key) {
        if !self.focused {
            self.handle_focus();
            self.handle_key_down(key);
        }
    }

    pub fn handle_key_down(&mut self, key: Key) {
        if !self.focused {
            return;
        }

        if self.focused_down_keys >> key.to_code() & 1 == 0 {
            // make change if the bit corresponding to the key is 0 (released)
            self.key_down_change = Some(key.to_code());
            self.focused_down_keys |= 1 << key.to_code();

            log::debug!(
                "focused key change detected: bitmap {:#018b}",
                self.focused_down_keys
            );
            log::info!(
                "changed key -> pressed key {:?} code {:X?}",
                key,
                key.to_code()
            );
        }
    }

    pub fn handle_key_up(&mut self, key: Key) {
        if !self.focused {
            return;
        }

        if self.focused_down_keys >> key.to_code() & 1 == 1 {
            // make change if the bit corresponding to the key if 1 (pressed)
            self.key_up_change = Some(key.to_code());
            self.focused_down_keys &= !(1 << key.to_code());

            log::debug!(
                "focused key change detected: bitmap {:#018b}",
                self.focused_down_keys
            );
            log::info!(
                "changed key -> released key {:?} code {:X?}",
                key,
                key.to_code()
            );
        }
    }

    // Update interpreter input with relevant keyboard state and clear ephemeral state
    pub fn flush(&mut self, input: &mut InterpreterInput) {
        input.down_keys = self.focused_down_keys;
        input.just_pressed_key = self.key_down_change;
        input.just_released_key = self.key_up_change;
    }

    pub fn clear_ephemeral_state(&mut self) {
        self.key_down_change = None;
        self.key_up_change = None;
    }
}
