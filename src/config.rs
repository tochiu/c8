const DEFAULT_INSTRUCTION_FREQUENCY: u32 = 2000;

pub const GOOD_INSTRUCTION_FREQUENCY_DIFF: f64 = 1.0;
pub const OKAY_INSTRUCTION_FREQUENCY_DIFF: f64 = 10.0;

#[derive(Clone, Debug)]
pub struct C8Config {
    pub title: String,
    pub instruction_frequency: u32,
    pub debugging: bool,
    pub logging: bool
}

impl Default for C8Config {
    fn default() -> Self {
        C8Config {
            title: String::new(),
            instruction_frequency: DEFAULT_INSTRUCTION_FREQUENCY,
            debugging: false, 
            logging: false 
        }
    }
}