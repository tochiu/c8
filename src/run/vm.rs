use super::{
    disp::Display,
    input::{Key, Keyboard},
    interp::{Instruction, Interpreter, InterpreterHistoryFragment, InterpreterRequest},
    rom::Rom,
};

use std::sync::mpsc::Receiver;

// TODO: maybe support sound (right now the sound timer does nothing external)

const TIMER_TICKS_PER_SECOND: f32 = 60.0;
const VERTICAL_BLANKS_PER_SECOND: f32 = 60.0;

#[derive(Debug)]
pub enum VMEvent {
    KeyUp(Key),
    KeyDown(Key),
    Focus,
    Unfocus,
    FocusingKeyDown(Key),
}

pub struct VM {
    // Time elapsed since last time step was called
    pub time_step: f32,

    // Vertical blank is time elapsed since draw instruction was last allowed to execute (COSMAC VIP only)
    vertical_blank_time: f32,

    // Total time step and total executions since last execution frequency measurement
    stopwatch_time: f32,
    stopwatch_executions: u16,

    interpreter: Interpreter,

    // Event receiver and queue
    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO
    display: bool,
    keyboard: Keyboard,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f32,
    delay_timer: f32,
}

impl VM {
    pub fn new(rom: Rom, recv: Receiver<VMEvent>) -> Self {
        VM {
            time_step: 0.0,

            vertical_blank_time: 0.0,

            stopwatch_time: 0.0,
            stopwatch_executions: 0,

            interpreter: Interpreter::from(rom),

            event: recv,
            event_queue: Vec::new(),

            display: false,
            keyboard: Keyboard::default(),

            // these precise external timers will be mapped
            // to a byte range when executing interpreter instructions
            sound_timer: 0.0,
            delay_timer: 0.0,
        }
    }

    pub fn undo(&mut self, state: &VMHistoryFragment) {
        self.time_step = state.time_step;
        self.vertical_blank_time = state.vertical_blank_time;
        self.keyboard = state.keyboard;
        self.sound_timer = state.sound_timer;
        self.delay_timer = state.delay_timer;

        match &state.interpreter.instruction {
            &Some(
                Instruction::ClearScreen
                | Instruction::ScrollDown(_)
                | Instruction::ScrollLeft
                | Instruction::ScrollRight
                | Instruction::LowResolution
                | Instruction::HighResolution
                | Instruction::Display(_, _, _),
            ) => {
                self.display = true;
            }
            _ => (),
        }

        self.interpreter.undo(&state.interpreter);
    }

    pub fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }

    pub fn keyboard(&self) -> &Keyboard {
        &self.keyboard
    }

    pub fn keyboard_mut(&mut self) -> &mut Keyboard {
        &mut self.keyboard
    }

    pub fn sound_timer(&self) -> f32 {
        self.sound_timer
    }

    pub fn delay_timer(&self) -> f32 {
        self.delay_timer
    }

    pub fn truncated_delay_timer(&self) -> u8 {
        self.delay_timer.ceil() as u8
    }

    pub fn queue_events(&mut self) {
        self.event_queue.extend(self.event.try_iter());
    }

    pub fn clear_events(&mut self) {
        for _ in self.event.try_iter() {
            continue; // no-op
        }
    }

    pub fn vsync(&self) -> f32 {
        self.vertical_blank_time * VERTICAL_BLANKS_PER_SECOND
    }

    pub fn clear_event_queue(&mut self) {
        self.queue_events();
        self.event_queue.clear();
    }

    pub fn drain_event_queue(&mut self) {
        self.queue_events();
        for event in self.event_queue.drain(..) {
            log::debug!("Processing Event {:?}", event);
            match event {
                VMEvent::KeyUp(key) => self.keyboard.handle_key_up(key),
                VMEvent::KeyDown(key) => self.keyboard.handle_key_down(key),
                VMEvent::Focus => self.keyboard.handle_focus(),
                VMEvent::Unfocus => self.keyboard.handle_unfocus(),
                VMEvent::FocusingKeyDown(key) => self.keyboard.handle_focusing_key_down(key),
            }
        }
    }

    pub fn extract_new_display(&mut self) -> Option<Display> {
        if self.display {
            self.display = false;
            Some(self.interpreter.output.display.clone())
        } else {
            None
        }
    }

    pub fn extract_execution_frequency(&mut self) -> f32 {
        let frequency = if self.stopwatch_time > 0.0 {
            self.stopwatch_executions as f32 / self.stopwatch_time
        } else {
            0.0
        };
        self.stopwatch_time = 0.0;
        self.stopwatch_executions = 0;
        frequency
    }

    pub fn step(&mut self) -> Result<bool, String> {
        self.drain_event_queue();

        self.stopwatch_time += self.time_step;
        self.stopwatch_executions += 1;

        // update timers and clamp between the bounds of a byte because that is the data type
        self.sound_timer = (self.sound_timer - self.time_step * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f32, u8::MAX as f32);
        self.delay_timer = (self.delay_timer - self.time_step * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f32, u8::MAX as f32);

        self.vertical_blank_time += self.time_step;
        let vertical_blank = if self.vertical_blank_time >= (1.0 / VERTICAL_BLANKS_PER_SECOND) {
            self.vertical_blank_time = self
                .vertical_blank_time
                .rem_euclid(1.0 / VERTICAL_BLANKS_PER_SECOND);
            true
        } else {
            false
        };

        // update interpreter input
        let delay_timer = self.truncated_delay_timer();
        let mut input = &mut self.interpreter.input;

        self.keyboard.flush(input);
        input.delay_timer = delay_timer;
        input.vertical_blank = vertical_blank;

        // interpret next instruction
        let should_continue = self.interpreter.step()?;

        self.keyboard.clear_ephemeral_state();

        // handle any external request by the executed instruction
        if let Some(request) = self.interpreter.output.request {
            match request {
                InterpreterRequest::Display => self.display = true,
                InterpreterRequest::SetDelayTimer(time) => self.delay_timer = time as f32,
                InterpreterRequest::SetSoundTimer(time) => self.sound_timer = time as f32,
            }
        }

        Ok(should_continue)
    }

    pub fn to_history_fragment(&self) -> VMHistoryFragment {
        VMHistoryFragment {
            time_step: self.time_step,
            vertical_blank_time: self.vertical_blank_time,
            keyboard: self.keyboard,
            sound_timer: self.sound_timer,
            delay_timer: self.delay_timer,
            interpreter: self.interpreter.to_history_fragment()
        }
    }

    pub fn update_memory_access_flags(&mut self, executed_fragment: &InterpreterHistoryFragment) {
        self.interpreter.update_memory_access_flags(executed_fragment);
    }
}

#[derive(PartialEq)]
pub struct VMHistoryFragment {
    pub time_step: f32,
    pub vertical_blank_time: f32,
    pub keyboard: Keyboard,
    pub sound_timer: f32,
    pub delay_timer: f32,
    pub interpreter: InterpreterHistoryFragment,
}

impl VMHistoryFragment {
    pub fn restore(&self, vm: &mut VM) {
        vm.time_step = self.time_step;
        vm.vertical_blank_time = self.vertical_blank_time;
        vm.keyboard = self.keyboard;
        vm.sound_timer = self.sound_timer;
        vm.delay_timer = self.delay_timer;
    }

    pub fn log_diff(&self, other: &Self) {
        if self.time_step != other.time_step {
            log::debug!("Time step difference {:?} -> {:?}", self.time_step, other.time_step);
        }
        if self.vertical_blank_time != other.vertical_blank_time {
            log::debug!(
                "Vertical blank time difference {:?} -> {:?}",
                self.vertical_blank_time,
                other.vertical_blank_time
            );
        }
        if self.keyboard != other.keyboard {
            log::debug!("Keyboard difference {:?} -> {:?}", self.keyboard, other.keyboard);
        }
        if self.sound_timer != other.sound_timer {
            log::debug!(
                "Sound timer difference {:?} -> {:?}",
                self.sound_timer,
                other.sound_timer
            );
        }
        if self.delay_timer != other.delay_timer {
            log::debug!(
                "Delay timer difference {:?} -> {:?}",
                self.delay_timer,
                other.delay_timer
            );
        }
        self.interpreter.log_diff(&other.interpreter);
    }
}
