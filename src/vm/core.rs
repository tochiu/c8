use super::{
    input::{Key, Keyboard},
    interp::{Interpreter, InterpreterError, InterpreterRequest},
    prog::Program,
    disp::DisplayBuffer
};

use std::sync::mpsc::Receiver;

// TODO: maybe support sound (right now the sound timer does nothing external)

const TIMER_TICKS_PER_SECOND: f64 = 60.0;

#[derive(Debug)]
pub enum VMEvent {
    KeyUp(Key),
    KeyDown(Key),
    Focus,
    Unfocus,
    FocusingKeyDown(Key),
}

pub struct VM {
    interp: Interpreter,

    event: Receiver<VMEvent>,
    event_queue: Vec<VMEvent>,

    // Virtualized IO

    display: bool,
    keyboard: Keyboard,

    // these precise external timers will be mapped
    // to a byte range when executing interpreter instructions
    sound_timer: f64,
    delay_timer: f64,
}

impl VM {
    pub fn new(program: Program, recv: Receiver<VMEvent>) -> Self {
        VM {
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

    pub fn interpreter(&self) -> &Interpreter {
        &self.interp
    }

    pub fn keyboard(&self) -> &Keyboard {
        &self.keyboard
    }

    pub fn keyboard_mut(&mut self) -> &mut Keyboard {
        &mut self.keyboard
    }

    pub fn sound_timer(&self) -> f64 {
        self.sound_timer
    }

    pub fn delay_timer(&self) -> f64 {
        self.delay_timer
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

    pub fn step(&mut self, time_elapsed: f64) -> Result<(), InterpreterError> {
        self.drain_event_queue();

        // update timers and clamp between the bounds of a byte because that is the data type
        self.sound_timer = (self.sound_timer - time_elapsed * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f64, u8::MAX as f64);
        self.delay_timer = (self.delay_timer - time_elapsed * TIMER_TICKS_PER_SECOND)
            .clamp(u8::MIN as f64, u8::MAX as f64);

        // update interpreter input
        let mut input = self.interp.input_mut();

        self.keyboard.flush(input); // the keyboard will write its state to the input
        input.delay_timer = self.delay_timer.ceil() as u8; // delay timer is ceiled to the nearest 8-bit number

        // interpret next instruction
        let output = self.interp.step()?;

        // handle any external request by the executed instruction
        if let Some(request) = output.request {
            match request {
                InterpreterRequest::Display => self.display = true,
                InterpreterRequest::SetDelayTimer(time) => self.delay_timer = time as f64,
                InterpreterRequest::SetSoundTimer(time) => self.sound_timer = time as f64,
            }
        }

        Ok(())
    }
}