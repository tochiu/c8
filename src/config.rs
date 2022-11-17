const INSTRUCTION_FREQUENCY: u32 = 2000;
const TIMER_FREQUENCY: u32 = 60;

pub const GOOD_IPS_DIFF: f64 = 1.0;
pub const OKAY_IPS_DIFF: f64 = 10.0;

#[derive(Clone, Debug)]
pub struct C8VMConfig {
    pub title: String,
    pub instruction_frequency: u32,
    pub timer_frequency: u32,
    pub debugging: bool,
    pub logging: bool
}

impl Default for C8VMConfig {
    fn default() -> Self {
        C8VMConfig {
            title: String::new(),
            instruction_frequency: INSTRUCTION_FREQUENCY, 
            timer_frequency: TIMER_FREQUENCY, 
            debugging: false, 
            logging: false 
        }
    }
}