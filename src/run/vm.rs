use super::{
    audio::{AudioEvent, AudioController},
    disp::Display,
    input::{Key, Keyboard},
    instruct::Instruction,
    interp::*,
    rom::{Rom, RomKind},
};

use std::{
    sync::mpsc::Receiver,
    time::Duration,
};

const DELAY_TIMER_TICKS_PER_SECOND: f64 = 60.0;
const SOUND_TIMER_TICKS_PER_SECOND: f64 = 60.0;

const VERTICAL_BLANKS_PER_SECOND: f64 = 60.0;

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
    pub time_step: f64,

    // Vertical blank is time elapsed since draw instruction was last allowed to execute (COSMAC VIP only)
    vertical_blank_time: f64,
    vertical_sync_enabled: bool,

    interpreter: Interpreter,

    // Event receiver and queue
    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO
    display: bool, // TODO handle new frame indication outside like sound
    keyboard: Keyboard,
    audio: AudioController,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f64,
    delay_timer: f64,
}

impl VM {
    pub fn new(rom: Rom, recv: Receiver<VMEvent>, mut audio: AudioController) -> Self {
        let interpreter = Interpreter::from(rom);

        audio.apply_event(AudioEvent::SetBuffer(interpreter.audio.buffer));
        audio.apply_event(AudioEvent::SetPitch(interpreter.audio.pitch));

        VM {
            time_step: 0.0,

            vertical_blank_time: 0.0,
            vertical_sync_enabled: interpreter.rom.config.kind == RomKind::COSMACVIP,

            event: recv,
            event_queue: Vec::new(),

            display: false,
            keyboard: Keyboard::default(),
            audio,

            interpreter,

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
        self.delay_timer = state.delay_timer;
        self.sound_timer = state.sound_timer;

        self.audio.apply_event(AudioEvent::SetTimer(Duration::from_secs_f64(self.sound_timer / SOUND_TIMER_TICKS_PER_SECOND)));

        if let Some(Instruction::Draw(_, _, _)) = state.interpreter.instruction {
            self.display = true;
        }

        match state.interpreter.extra.as_deref() {
            Some(&InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { .. }) => {
                self.display = true;
            }

            Some(&InterpreterHistoryFragmentExtra::WillLoadAudio { prior_buffer, .. }) => {
                self.audio
                    .apply_event(AudioEvent::SetBuffer(prior_buffer));
            }

            Some(&InterpreterHistoryFragmentExtra::WillSetPitch { prior_pitch }) => {
                self.audio
                    .apply_event(AudioEvent::SetPitch(prior_pitch));
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

    pub fn update_audio(&mut self) {
        self.audio.update()
    }

    pub fn pause_audio(&mut self) {
        self.audio.apply_event(AudioEvent::Pause)
    }

    pub fn resume_audio(&mut self) {
        self.audio.apply_event(AudioEvent::Resume)
    }

    pub fn sound_timer(&self) -> f64 {
        self.sound_timer
    }

    pub fn delay_timer(&self) -> f64 {
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

    pub fn vsync(&self) -> f64 {
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

    pub fn stepn(&mut self, amt: u32) -> Result<bool, String> {
        self.drain_event_queue();

        self.keyboard.flush(&mut self.interpreter.input);

        let should_continue = self.step_interpreter()?;

        self.keyboard.clear_ephemeral_state();
        self.keyboard.flush(&mut self.interpreter.input);

        if !should_continue {
            return Ok(false);
        }

        for _ in 0..amt - 1 {
            if !self.step_interpreter()? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    fn step_interpreter(&mut self) -> Result<bool, String> {
        // update timers and clamp between the bounds of a byte because that is the data type
        self.sound_timer = (self.sound_timer - self.time_step * SOUND_TIMER_TICKS_PER_SECOND).max(0.0);
        self.delay_timer = (self.delay_timer - self.time_step * DELAY_TIMER_TICKS_PER_SECOND).max(0.0);

        if self.vertical_sync_enabled {
            self.vertical_blank_time += self.time_step;
            self.interpreter.input.vertical_blank = if self.vertical_blank_time >= (1.0 / VERTICAL_BLANKS_PER_SECOND) {
                self.vertical_blank_time = self
                    .vertical_blank_time
                    .rem_euclid(1.0 / VERTICAL_BLANKS_PER_SECOND);
                true
            } else {
                false
            };
        }

        // update interpreter input
        self.interpreter.input.delay_timer = self.truncated_delay_timer();

        // interpret next instruction
        let result = self.interpreter.step();

        if let Some(output) = self.interpreter.output {
            match output {
                InterpreterOutput::Display => self.display = true,
                InterpreterOutput::SetDelayTimer(ticks) => self.delay_timer = ticks as f64,
                InterpreterOutput::SetSoundTimer(ticks) => {
                    self.sound_timer = ticks as f64;
                    self.audio
                        .apply_event(AudioEvent::SetTimer(Duration::from_secs_f64(
                            ticks as f64 / SOUND_TIMER_TICKS_PER_SECOND,
                        )));
                }
                InterpreterOutput::UpdateAudioBuffer => {
                    self.audio
                        .apply_event(AudioEvent::SetBuffer(self.interpreter.audio.buffer));
                }
                InterpreterOutput::UpdateAudioPitch => {
                    self.audio
                        .apply_event(AudioEvent::SetPitch(self.interpreter.audio.pitch));
                }
            }
        }

        result
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
    pub time_step: f64,
    pub vertical_blank_time: f64,
    pub keyboard: Keyboard,
    pub sound_timer: f64,
    pub delay_timer: f64,
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
