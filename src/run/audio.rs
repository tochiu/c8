use super::rom::RomKind;

use rodio::{
    buffer::SamplesBuffer,
    source::{Repeat, Source},
    OutputStream, Sink,
};

use std::{
    sync::mpsc::{channel, Sender, RecvTimeoutError},
    thread::JoinHandle,
    time::{Duration, Instant},
};


pub const AUDIO_BUFFER_SIZE_BYTES: usize = 16;

const AUDIO_BUFFER_SIZE_BITS: usize = AUDIO_BUFFER_SIZE_BYTES * u8::BITS as usize;

const BASE_SAMPLE_RATE: u32 = 4000;
const DEFAULT_RECV_TIMEOUT: Duration = Duration::from_millis(1000);

pub struct Audio {
    pub buffer: [u8; AUDIO_BUFFER_SIZE_BYTES],
    pub pitch: u8,
}

impl From<RomKind> for Audio {
    fn from(kind: RomKind) -> Self {
        if kind == RomKind::XOCHIP {
            Audio {
                buffer: [0; AUDIO_BUFFER_SIZE_BYTES],
                pitch: 64,
            }
        } else {
            Audio {
                buffer: [
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
                    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                ],
                pitch: 247,
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AudioEvent {
    SetTimer(Duration),
    SetBuffer([u8; AUDIO_BUFFER_SIZE_BYTES]),
    SetPitch(u8),
    Pause,
    Resume,
}

pub fn spawn_audio_thread() -> (Sender<AudioEvent>, JoinHandle<()>) {
    let (event_sender, event_receiver) = channel();
    let handle = std::thread::spawn(move || {
        // Get a output stream handle to the default physical sound device
        let (_stream, stream_handle) = OutputStream::try_default()
            .expect("Failed to get default audio output stream");
        let sink = Sink::try_new(&stream_handle)
            .expect("Failed to create audio sink");

        let mut paused = false;
        let mut playback_offset = 0.0;
        let mut current_buffer = [0; AUDIO_BUFFER_SIZE_BYTES];
        let mut remaining_duration = Duration::ZERO;

        let mut recv_last_instant = Instant::now();
        let mut recv_timeout = DEFAULT_RECV_TIMEOUT;

        loop {
            let recv_result = event_receiver.recv_timeout(recv_timeout);

            let now = Instant::now();

            if !paused {
                let elapsed_duration = now.duration_since(recv_last_instant).min(remaining_duration);
                remaining_duration -= elapsed_duration;

                // NOTE: Maintaining the playback offset is not an exact process.
                // Because the playback happens in a separate thread thats more or less divorced from this thread,
                // we cannot actually be sure of the true playback offset. Of course we can build a custom source
                // that keeps track of the playback offset and updates an atomic varaible but that seems like a 
                // possible performance concern and a lot of work for something that's not really important. It is
                // accurate enough for now to just approximate the playback offset based on playback speed and
                // playback time elapsed.
                playback_offset = sink.speed() * BASE_SAMPLE_RATE as f32 * elapsed_duration.as_secs_f32() % AUDIO_BUFFER_SIZE_BITS as f32;
            }

            

            match recv_result.as_ref().cloned() {
                Ok(event) => match event {
                    AudioEvent::SetTimer(duration) => {
                        remaining_duration = duration;
                        if !remaining_duration.is_zero() {
                            if sink.empty() {
                                sink.append(buffer_to_source(&current_buffer, playback_offset));
                            }
                            if !paused {
                                sink.play();
                            }
                        }
                    }
                    AudioEvent::SetBuffer(buffer) => 'guard: {
                        log::info!("Audio thread received event: {:?}", recv_result);
                        if buffer == current_buffer {
                            break 'guard;
                        }

                        current_buffer = buffer;
                        if !sink.empty() {
                            sink.clear();
                        }

                        if !remaining_duration.is_zero() {
                            sink.append(buffer_to_source(&current_buffer, playback_offset));
                            if !paused {
                                sink.play();
                            }
                        }
                    }
                    AudioEvent::SetPitch(pitch) => {
                        sink.set_speed(2.0_f32.powf((pitch as f32 - 64.0) / 48.0));
                    }
                    AudioEvent::Pause => 'guard: {
                        if paused {
                            break 'guard;
                        }
                        paused = true;
                        sink.pause();
                    }
                    AudioEvent::Resume => 'guard: {
                        if !paused {
                            break 'guard;
                        }
                        paused = false;
                        sink.play();
                    }
                }
                Err(RecvTimeoutError::Disconnected) => {
                    sink.stop();
                    return;
                }
                Err(RecvTimeoutError::Timeout) => (),
            }                

            recv_last_instant = now;
            recv_timeout = {
                if paused || sink.empty() {
                    DEFAULT_RECV_TIMEOUT
                } else if remaining_duration.is_zero() {
                    if !sink.empty() {
                        sink.clear();
                    }
                    playback_offset = 0.0;
                    DEFAULT_RECV_TIMEOUT
                } else {
                    remaining_duration
                }
            };
        }
    });

    (event_sender, handle)
}

fn buffer_to_source(buffer: &[u8; AUDIO_BUFFER_SIZE_BYTES], playback_offset: f32) -> Repeat<SamplesBuffer<f32>> {
    let mut data = Vec::with_capacity(AUDIO_BUFFER_SIZE_BITS);
    data.extend(
        buffer
            .iter()
            .map(|byte| {
                (0..8)
                    .rev()
                    .into_iter()
                    .map(|shift| if *byte >> shift & 1 == 1 { 1.0 } else { 0.0 })
            })
            .flatten(),
    );
    data.rotate_left(playback_offset.round() as usize % AUDIO_BUFFER_SIZE_BITS);

    SamplesBuffer::new(1, BASE_SAMPLE_RATE, data).repeat_infinite()
}