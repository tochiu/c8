use super::{
    audio::AudioEvent,
    disp::Display,
    input::{Key, Keyboard},
    instruct::Instruction,
    interp::*,
    rom::Rom,
};

use std::{
    sync::mpsc::{Receiver, Sender},
    time::Duration,
};

const DELAY_TIMER_TICKS_PER_SECOND: f32 = 60.0;
const SOUND_TIMER_TICKS_PER_SECOND: f32 = 60.0;

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
    stopwatch_executions: usize,

    interpreter: Interpreter,

    // Event receiver and queue
    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO
    display: bool, // TODO handle new frame indication outside like sound
    keyboard: Keyboard,
    audio: Sender<AudioEvent>,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f32,
    delay_timer: f32,
}

impl VM {
    pub fn new(rom: Rom, recv: Receiver<VMEvent>, sound: Sender<AudioEvent>) -> Self {
        let vm = VM {
            time_step: 0.0,

            vertical_blank_time: 0.0,

            stopwatch_time: 0.0,
            stopwatch_executions: 0,

            interpreter: Interpreter::from(rom),

            event: recv,
            event_queue: Vec::new(),

            display: false,
            keyboard: Keyboard::default(),
            audio: sound,

            // these precise external timers will be mapped
            // to a byte range when executing interpreter instructions
            sound_timer: 0.0,
            delay_timer: 0.0,
        };

        vm.audio
            .send(AudioEvent::SetBuffer(vm.interpreter.audio.buffer))
            .expect("Failed to initialize audio buffer");
        vm.audio
            .send(AudioEvent::SetPitch(vm.interpreter.audio.pitch))
            .expect("Failed to initialize audio pitch");

        vm
    }

    pub fn undo(&mut self, state: &VMHistoryFragment) {
        self.time_step = state.time_step;
        self.vertical_blank_time = state.vertical_blank_time;
        self.keyboard = state.keyboard;
        self.delay_timer = state.delay_timer;
        self.sound_timer = state.sound_timer;

        self.audio
            .send(AudioEvent::SetTimer(Duration::from_secs_f32(
                self.sound_timer / SOUND_TIMER_TICKS_PER_SECOND,
            )))
            .expect("Failed to restore sound timer");

        if let Some(Instruction::Draw(_, _, _)) = state.interpreter.instruction {
            self.display = true;
        }

        match state.interpreter.extra.as_deref() {
            Some(&InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { .. }) => {
                self.display = true;
            }

            Some(&InterpreterHistoryFragmentExtra::WillLoadAudio { prior_buffer, .. }) => {
                self.audio
                    .send(AudioEvent::SetBuffer(prior_buffer))
                    .expect("Failed to restore audio pattern buffer");
            }

            Some(&InterpreterHistoryFragmentExtra::WillSetPitch { prior_pitch }) => {
                self.audio
                    .send(AudioEvent::SetPitch(prior_pitch))
                    .expect("Failed to restore audio pitch");
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
        self.event.try_iter().last();
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
            Some(self.interpreter.display.clone())
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
        self.sound_timer = (self.sound_timer - self.time_step * SOUND_TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f32, u8::MAX as f32);
        self.delay_timer = (self.delay_timer - self.time_step * DELAY_TIMER_TICKS_PER_SECOND)
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

        // handle any output by the executed instruction
        if let Some(output) = self.interpreter.output {
            match output {
                InterpreterOutput::Display => self.display = true,
                InterpreterOutput::SetDelayTimer(ticks) => self.delay_timer = ticks as f32,
                InterpreterOutput::SetSoundTimer(ticks) => {
                    self.sound_timer = ticks as f32;
                    self.audio
                        .send(AudioEvent::SetTimer(Duration::from_secs_f32(
                            ticks as f32 / SOUND_TIMER_TICKS_PER_SECOND,
                        )))
                        .ok();
                }
                InterpreterOutput::UpdateAudioBuffer => {
                    self.audio
                        .send(AudioEvent::SetBuffer(self.interpreter.audio.buffer))
                        .ok();
                }
                InterpreterOutput::UpdateAudioPitch => {
                    self.audio
                        .send(AudioEvent::SetPitch(self.interpreter.audio.pitch))
                        .ok();
                }
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
            interpreter: self.interpreter.to_history_fragment(),
        }
    }

    pub fn update_memory_access_flags(&mut self, executed_fragment: &InterpreterHistoryFragment) {
        self.interpreter
            .update_memory_access_flags(executed_fragment);
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
            log::debug!(
                "Time step difference {:?} -> {:?}",
                self.time_step,
                other.time_step
            );
        }
        if self.vertical_blank_time != other.vertical_blank_time {
            log::debug!(
                "Vertical blank time difference {:?} -> {:?}",
                self.vertical_blank_time,
                other.vertical_blank_time
            );
        }
        if self.keyboard != other.keyboard {
            log::debug!(
                "Keyboard difference {:?} -> {:?}",
                self.keyboard,
                other.keyboard
            );
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
