pub mod color;
pub mod preset;

use crate::{ch8::{
    input::Key,
    run::{RunResult, Runner},
    vm::VMEvent,
}, render::RenderController};

use crossterm::event::{
    poll, read, Event, KeyCode as CrosstermKey, KeyEventKind, KeyModifiers as CrosstermKeyModifiers,
};
use device_query::DeviceQuery;

use std::{
    collections::HashSet,
    ops::DerefMut,
    thread::{self, JoinHandle},
    time::Duration
};

pub fn spawn_run_thread(mut runner: Runner, render: RenderController, debugging: bool, logging: bool) -> JoinHandle<RunResult> {

    // main thread
    let c8 = runner.c8();
    let vm_event_sender = runner.vm_event_sender();

    let main_thread = thread::spawn(move || -> RunResult {
        let device_state = device_query::DeviceState::new();
        let mut last_keys = HashSet::new();

        // start runner
        if !debugging {
            runner.resume().expect("Unable to resume runner");
        }

        loop {
            // event loop
            let terminal_event_received =
                poll(Duration::from_millis(15)).expect("Unable to poll for terminal events");

            if runner.is_finished() {
                return runner.exit();
            }

            if terminal_event_received {
                let event = read().expect("Unable to read terminal event");
                let mut sink_vm_events = false;

                if debugging {
                    let mut _guard = c8.lock().expect("Unable to lock c8");
                    let (vm, Some(dbg)) = _guard.deref_mut() else {
                        unreachable!("Debug runs should contain a debugger");
                    };

                    sink_vm_events = sink_vm_events || dbg.is_active();

                    if dbg.handle_input_event(event.clone(), &mut runner, vm) {
                        render.trigger();
                    }

                    sink_vm_events = sink_vm_events || dbg.is_active();
                }

                match event {
                    Event::Resize(_, _) => {
                        render.trigger();
                    }
                    Event::FocusGained => {
                        if !sink_vm_events {
                            vm_event_sender
                                .send(VMEvent::Focus)
                                .expect("Unable to send VM focus event");
                        }
                    }
                    Event::FocusLost => {
                        if !sink_vm_events {
                            vm_event_sender
                                .send(VMEvent::Unfocus)
                                .expect("Unable to send VM unfocus event");
                        }
                    }
                    Event::Key(key_event) => {
                        // Esc or Crtl+C interrupt handler
                        if (key_event.code == CrosstermKey::Esc && !sink_vm_events) // Esc is an exit if debugger isnt sinking keys
                            || key_event.modifiers.contains(CrosstermKeyModifiers::CONTROL) // Ctrl+C is a hard exit
                                && (key_event.code == CrosstermKey::Char('c')
                                    || key_event.code == CrosstermKey::Char('C'))
                        {
                            // exit virtual machine
                            return runner.exit();
                        } else if !sink_vm_events {
                            match key_event.code {
                                CrosstermKey::Char('-') => {
                                    vm_event_sender.send(VMEvent::VolumeChange(false)).ok();
                                }
                                CrosstermKey::Char('=') => {
                                    vm_event_sender.send(VMEvent::VolumeChange(true)).ok();
                                }
                                _ => {
                                    // kinda expecting a crossterm key event to mean renderer is in focus
                                    if let KeyEventKind::Repeat | KeyEventKind::Press =
                                        key_event.kind
                                    {
                                        if let Ok(key) = Key::try_from(key_event.code) {
                                            vm_event_sender
                                                .send(VMEvent::FocusingKeyDown(key))
                                                .expect(
                                                    "Unable to send VM focusing key down event",
                                                );
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => (),
                };
            }

            // execute device query step
            let keys = HashSet::from_iter(
                device_state
                    .get_keys()
                    .into_iter()
                    .filter_map(|keycode| Key::try_from(keycode).ok()),
            );

            for &key in keys.difference(&last_keys) {
                vm_event_sender
                    .send(VMEvent::KeyDown(key))
                    .expect("Unable to send VM key down event");
            }

            for &key in last_keys.difference(&keys) {
                vm_event_sender
                    .send(VMEvent::KeyUp(key))
                    .expect("Unable to send VM key up event");
            }

            last_keys = keys;

            // TODO attach event listener to logger instead of polling to update
            if logging {
                render.trigger();
            }
        }
    });

    main_thread
}
