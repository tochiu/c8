use super::{
    audio::{AudioController, AudioEvent},
    disp::Display,
    input::{Key, Keyboard},
    instruct::Instruction,
    interp::*,
    rom::{Rom, RomKind},
};

use std::{sync::mpsc::Receiver, time::Duration};

pub const VM_FRAME_RATE: u32 = 60;
pub const VM_FRAME_DURATION: Duration = Duration::from_nanos(1_000_000_000 / VM_FRAME_RATE as u64); // 60 FPS

#[derive(Debug)]
pub enum VMEvent {
    KeyUp(Key),
    KeyDown(Key),
    Focus,
    Unfocus,
    FocusingKeyDown(Key),
    VolumeChange(bool),
}

#[derive(Default)]
struct VMSprint {
    cycles: u32,
    set_sound_timer_cycle: u32,
    set_delay_timer_cycle: u32,
}

pub struct VM {
    // Time elapsed since last time step was called
    cycles_per_frame: u32,

    interpreter: Interpreter,

    // Event receiver and queue
    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO
    display: bool, // TODO handle new frame indication outside like sound
    keyboard: Keyboard,
    audio: AudioController,

    vsync_timer: u8,
    vsync_timer_cycle_offset: u32,
    vsync_enabled: bool,

    sound_timer: u8,
    sound_timer_cycle_offset: u32,

    delay_timer: u8,
    delay_timer_cycle_offset: u32,
}

impl VM {
    pub fn new(
        rom: Rom,
        recv: Receiver<VMEvent>,
        mut audio: AudioController,
        cycles_per_frame: u32,
    ) -> Self {
        let vsync_enabled = rom.config.kind == RomKind::COSMACVIP;
        let interpreter = Interpreter::from(rom);

        audio.apply_event(AudioEvent::SetBuffer(interpreter.audio.buffer));
        audio.apply_event(AudioEvent::SetPitch(interpreter.audio.pitch));

        VM {
            cycles_per_frame,

            interpreter,

            event: recv,
            event_queue: Vec::new(),

            display: false,
            keyboard: Keyboard::default(),
            audio,

            vsync_timer: 0,
            vsync_timer_cycle_offset: 0,
            vsync_enabled,

            sound_timer: 0,
            sound_timer_cycle_offset: 0,

            delay_timer: 0,
            delay_timer_cycle_offset: 0,
        }
    }

    pub fn set_cycles_per_frame(&mut self, cycles_per_frame: u32) {
        self.sound_timer_cycle_offset = (self.sound_timer_cycle_offset as f64
            / self.cycles_per_frame as f64
            * cycles_per_frame as f64)
            .round() as u32;
        self.delay_timer_cycle_offset = (self.delay_timer_cycle_offset as f64
            / self.cycles_per_frame as f64
            * cycles_per_frame as f64)
            .round() as u32;
        self.vsync_timer_cycle_offset = (self.vsync_timer_cycle_offset as f64
            / self.cycles_per_frame as f64
            * cycles_per_frame as f64)
            .round() as u32;
        self.cycles_per_frame = cycles_per_frame;
    }

    pub fn cycles_per_frame(&self) -> u32 {
        self.cycles_per_frame
    }

    pub fn undo(&mut self, state: &VMHistoryFragment) {
        self.cycles_per_frame = state.cycles_per_frame;
        self.keyboard = state.keyboard;
        self.vsync_timer = state.vsync_timer;
        self.vsync_timer_cycle_offset = state.vsync_timer_cycle_offset;
        self.sound_timer = state.sound_timer;
        self.sound_timer_cycle_offset = state.sound_timer_cycle_offset;
        self.delay_timer = state.delay_timer;
        self.delay_timer_cycle_offset = state.delay_timer_cycle_offset;

        self.audio
            .apply_event(AudioEvent::SetTimer(Duration::from_secs_f32(
                self.precise_sound_timer() / VM_FRAME_RATE as f32,
            )));

        if let Some(Instruction::Draw(_, _, _)) = state.interpreter.instruction {
            self.display = true;
        }

        match state.interpreter.extra.as_deref() {
            Some(&InterpreterHistoryFragmentExtra::WillDrawEntireDisplay { .. }) => {
                self.display = true;
            }

            Some(&InterpreterHistoryFragmentExtra::WillLoadAudio { prior_buffer, .. }) => {
                self.audio.apply_event(AudioEvent::SetBuffer(prior_buffer));
            }

            Some(&InterpreterHistoryFragmentExtra::WillSetPitch { prior_pitch }) => {
                self.audio.apply_event(AudioEvent::SetPitch(prior_pitch));
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

    pub fn audio(&self) -> &AudioController {
        &self.audio
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

    pub fn delay_timer(&self) -> u8 {
        self.delay_timer
    }

    pub fn precise_sound_timer(&self) -> f32 {
        (self.sound_timer as f32
            - self.sound_timer_cycle_offset as f32 / self.cycles_per_frame as f32)
            .max(0.0)
    }

    pub fn precise_delay_timer(&self) -> f32 {
        (self.delay_timer as f32
            - self.delay_timer_cycle_offset as f32 / self.cycles_per_frame as f32)
            .max(0.0)
    }

    pub fn precise_vsync_progress(&self) -> f32 {
        if self.vsync_timer == 0 {
            1.0
        } else {
            self.vsync_timer_cycle_offset as f32 / self.cycles_per_frame as f32
        }
    }

    pub fn queue_events(&mut self) {
        self.event_queue.extend(self.event.try_iter());
    }

    pub fn clear_events(&mut self) {
        self.event.try_iter().last();
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
                VMEvent::VolumeChange(increasing) => {
                    if increasing {
                        self.audio
                            .set_volume((self.audio.volume() + 0.05).clamp(0.0, 1.0))
                    } else {
                        self.audio
                            .set_volume((self.audio.volume() - 0.05).clamp(0.0, 1.0))
                    }
                }
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

    pub fn clear_ephemeral_state(&mut self) {
        self.keyboard.clear_ephemeral_state();
    }

    pub fn flush_external_input(&mut self) {
        self.drain_event_queue();
        self.keyboard.flush(&mut self.interpreter.input);
    }

    pub fn flush_external_input_and_stepn(&mut self, amt: u32) -> Result<bool, String> {
        self.flush_external_input();

        let should_continue = self.stepn(1)?;

        self.clear_ephemeral_state();

        if !should_continue {
            return Ok(false);
        }

        self.stepn(amt - 1)
    }

    pub fn stepn(&mut self, mut amt: u32) -> Result<bool, String> {
        self.flush_timers(VMSprint::default());
        while amt > 0 {
            let sprint_amt = amt.min(self.min_cycles_before_timer_tick());
            let mut sprint = VMSprint {
                cycles: sprint_amt,
                ..Default::default()
            };

            for cycle in 1..=sprint_amt {
                if !self.interpreter.step() {
                    return self.interpreter.stop_result();
                }

                if let Some(output) = self.interpreter.output.take() {
                    match output {
                        InterpreterOutput::Display => self.display = true,
                        InterpreterOutput::SetDelayTimer(ticks) => {
                            sprint.set_delay_timer_cycle = cycle;
                            self.interpreter.input.delay_timer = ticks;
                            self.delay_timer = ticks;
                            self.delay_timer_cycle_offset = 0;
                        }
                        InterpreterOutput::SetSoundTimer(ticks) => {
                            sprint.set_sound_timer_cycle = cycle;
                            self.sound_timer = ticks;
                            self.sound_timer_cycle_offset = 0;
                            self.audio
                                .apply_event(AudioEvent::SetTimer(Duration::from_secs_f32(
                                    ticks as f32 / VM_FRAME_RATE as f32,
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
            }

            // we can pull this outside interpreter step loop because
            // we never step the interpreter past a point where the timers are due to be ticked
            if self.vsync_enabled && self.vsync_timer == 0 {
                self.vsync_timer = 1;
            }

            amt -= sprint_amt;
            self.flush_timers(sprint);
        }

        Ok(true)
    }

    pub fn to_history_fragment(&self) -> VMHistoryFragment {
        VMHistoryFragment {
            cycles_per_frame: self.cycles_per_frame,
            keyboard: self.keyboard,
            interpreter: self.interpreter.to_history_fragment(),
            vsync_timer: self.vsync_timer,
            vsync_timer_cycle_offset: self.vsync_timer_cycle_offset,
            sound_timer: self.sound_timer,
            sound_timer_cycle_offset: self.sound_timer_cycle_offset,
            delay_timer: self.delay_timer,
            delay_timer_cycle_offset: self.delay_timer_cycle_offset,
        }
    }

    pub fn update_memory_access_flags(&mut self, executed_fragment: &InterpreterHistoryFragment) {
        self.interpreter
            .update_memory_access_flags(executed_fragment);
    }

    fn flush_timers(&mut self, sprint: VMSprint) {
        update_timer(
            sprint.cycles - sprint.set_sound_timer_cycle,
            self.cycles_per_frame,
            &mut self.sound_timer,
            &mut self.sound_timer_cycle_offset,
        );
        update_timer(
            sprint.cycles - sprint.set_delay_timer_cycle,
            self.cycles_per_frame,
            &mut self.delay_timer,
            &mut self.delay_timer_cycle_offset,
        );
        update_timer(
            sprint.cycles,
            self.cycles_per_frame,
            &mut self.vsync_timer,
            &mut self.vsync_timer_cycle_offset,
        );

        self.interpreter.input.delay_timer = self.delay_timer;

        if self.vsync_enabled {
            self.interpreter.input.vertical_blank = self.vsync_timer == 0 && sprint.cycles == 0;
        }
    }

    fn min_cycles_before_timer_tick(&self) -> u32 {
        if self.vsync_enabled && self.vsync_timer == 0 {
            return 1;
        }

        [
            (self.sound_timer, self.sound_timer_cycle_offset),
            (self.delay_timer, self.delay_timer_cycle_offset),
            (self.vsync_timer, self.vsync_timer_cycle_offset),
        ]
        .iter()
        .map(|(timer, offset)| {
            if *timer > 0 {
                self.cycles_per_frame - offset
            } else {
                u32::MAX
            }
        })
        .min()
        .expect("There should be at least one timer")
    }
}

fn update_timer(cycles: u32, cycles_per_frame: u32, timer: &mut u8, timer_cycle_offset: &mut u32) {
    if *timer == 0 {
        return;
    }

    *timer_cycle_offset += cycles;

    if *timer_cycle_offset < cycles_per_frame {
        return;
    }

    let timer_ticks = *timer_cycle_offset / cycles_per_frame;
    *timer = timer.saturating_sub(timer_ticks.min(u8::MAX as u32) as u8);

    if *timer == 0 {
        *timer_cycle_offset = 0;
    } else {
        *timer_cycle_offset %= cycles_per_frame;
    }
}

#[derive(PartialEq)]
pub struct VMHistoryFragment {
    pub cycles_per_frame: u32,
    pub keyboard: Keyboard,
    pub interpreter: InterpreterHistoryFragment,
    pub vsync_timer: u8,
    pub vsync_timer_cycle_offset: u32,
    pub sound_timer: u8,
    pub sound_timer_cycle_offset: u32,
    pub delay_timer: u8,
    pub delay_timer_cycle_offset: u32,
}

impl VMHistoryFragment {
    pub fn restore(&self, vm: &mut VM) {
        vm.cycles_per_frame = self.cycles_per_frame;
        vm.keyboard = self.keyboard;
        vm.vsync_timer = self.vsync_timer;
        vm.vsync_timer_cycle_offset = self.vsync_timer_cycle_offset;
        vm.sound_timer = self.sound_timer;
        vm.sound_timer_cycle_offset = self.sound_timer_cycle_offset;
        vm.delay_timer = self.delay_timer;
        vm.delay_timer_cycle_offset = self.delay_timer_cycle_offset;
    }

    pub fn log_diff(&self, other: &Self) {
        if self.cycles_per_frame != other.cycles_per_frame {
            log::debug!(
                "Cycles per frame difference {:?} -> {:?}",
                self.cycles_per_frame,
                other.cycles_per_frame
            );
        }
        if self.vsync_timer != other.vsync_timer {
            log::debug!(
                "Vsync time difference {:?} -> {:?}",
                self.vsync_timer,
                other.vsync_timer
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
