use super::{
    disp::DisplayBuffer,
    input::{Key, Keyboard},
    interp::{Interpreter, InterpreterHistoryFragment, InterpreterRequest},
    prog::Program,
};

use std::sync::mpsc::Receiver;

// TODO: maybe support sound (right now the sound timer does nothing external)

const TIMER_TICKS_PER_SECOND: f32 = 60.0;

#[derive(Debug)]
pub enum VMEvent {
    KeyUp(Key),
    KeyDown(Key),
    Focus,
    Unfocus,
    FocusingKeyDown(Key),
}

#[derive(Debug)]
pub struct VMHistoryFragment {
    pub time_step: f32,
    pub keyboard: Keyboard,
    pub sound_timer: f32,
    pub delay_timer: f32,
    pub interp_state: InterpreterHistoryFragment,
}

impl PartialEq for VMHistoryFragment {
    fn eq(&self, other: &Self) -> bool {
        self.keyboard == other.keyboard
            && self.sound_timer == other.sound_timer
            && self.delay_timer == other.delay_timer
            && self.interp_state == other.interp_state
    }
}

impl From<&VM> for VMHistoryFragment {
    fn from(vm: &VM) -> Self {
        VMHistoryFragment {
            time_step: vm.time_step,
            keyboard: vm.keyboard,
            sound_timer: vm.sound_timer,
            delay_timer: vm.delay_timer,
            interp_state: (&vm.interp).into(),
        }
    }
}

impl VMHistoryFragment {
    pub fn can_replace(&self, other: &Self, interp: &Interpreter) -> bool {
        if self.interp_state.are_get_key_forms(&other.interp_state) {
            let (_, kd2, ku2) = other.keyboard.state();
            interp.pick_key(kd2, ku2).is_none()
        } else {
            false
        }
    }

    pub fn restore_keyboard(&self, vm: &mut VM) {
        *vm.keyboard_mut() = self.keyboard;
    }
}

pub struct VM {
    pub time_step: f32,

    interp: Interpreter,

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
    pub fn new(program: Program, recv: Receiver<VMEvent>) -> Self {
        VM {
            time_step: 0.0,

            interp: Interpreter::from(program),

            event: recv,
            event_queue: Vec::new(),

            // Virtualized IO
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
        self.keyboard = state.keyboard;
        self.sound_timer = state.sound_timer;
        self.delay_timer = state.delay_timer;
        if state.interp_state.does_modify_display() {
            self.display = true;
        }
        self.interp.undo(&state.interp_state);
    }

    pub fn interpreter(&self) -> &Interpreter {
        &self.interp
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

    pub fn extract_new_frame(&mut self) -> Option<DisplayBuffer> {
        if self.display {
            self.display = false;
            Some(self.interp.output.display)
        } else {
            None
        }
    }

    pub fn handle_inputs(&mut self) {
        self.drain_event_queue();

        // update timers and clamp between the bounds of a byte because that is the data type
        self.sound_timer = (self.sound_timer - self.time_step * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f32, u8::MAX as f32);
        self.delay_timer = (self.delay_timer - self.time_step * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f32, u8::MAX as f32);

        // update interpreter input
        let delay_timer = self.truncated_delay_timer();
        let mut input = self.interp.input_mut();

        self.keyboard.flush(input); // the keyboard will write its state to the input
        input.delay_timer = delay_timer; // delay timer is ceiled to the nearest 8-bit number
    }

    pub fn step(&mut self) -> Result<(), String> {
        // interpret next instruction
        let output = self.interp.step()?;

        self.keyboard.clear_ephemeral_state();

        // handle any external request by the executed instruction
        if let Some(request) = output.request {
            match request {
                InterpreterRequest::Display => self.display = true,
                InterpreterRequest::SetDelayTimer(time) => self.delay_timer = time as f32,
                InterpreterRequest::SetSoundTimer(time) => self.sound_timer = time as f32,
            }
        }

        Ok(())
    }
}
