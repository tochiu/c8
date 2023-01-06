use super::rom::RomKind;

use rodio::{source::Source, OutputStream, Sink};

use std::{
    sync::{
        mpsc::{channel, RecvTimeoutError, Sender},
        Arc, Mutex,
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};

pub const AUDIO_BUFFER_SIZE_BYTES: usize = 16;

const AUDIO_BUFFER_SIZE_BITS: usize = AUDIO_BUFFER_SIZE_BYTES * u8::BITS as usize;

const BASE_SAMPLE_RATE: u32 = 4000;
const DEFAULT_RECV_TIMEOUT: Duration = Duration::from_millis(1000);
const DEFAULT_VOLUME: f32 = 0.25;

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

pub struct AudioSourceData {
    buffer: [f32; AUDIO_BUFFER_SIZE_BITS],
    sample_rate: u32,
    playback_offset: usize,
    volume: f32,
}

impl AudioSourceData {
    fn set_buffer(&mut self, buffer: &[u8; AUDIO_BUFFER_SIZE_BYTES]) {
        self.buffer.iter_mut().enumerate().for_each(|(i, sample)| {
            *sample = if buffer[i / 8] >> (7 - i % 8) & 1 == 1 {
                1.0
            } else {
                0.0
            }
        });
    }
}

pub struct AudioSource {
    data: Arc<Mutex<AudioSourceData>>,
}

impl AudioSource {
    pub fn new(data: Arc<Mutex<AudioSourceData>>) -> Self {
        data.lock()
            .expect("Unable to lock audio source data")
            .playback_offset = 0;
        AudioSource { data }
    }
}

impl Source for AudioSource {
    fn current_frame_len(&self) -> Option<usize> {
        Some(4)
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        self.data
            .lock()
            .expect("Unable to lock audio source data")
            .sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
}

impl Iterator for AudioSource {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let mut data = self.data.lock().expect("Unable to lock audio source data");
        let sample = data.volume*data.buffer[data.playback_offset];
        data.playback_offset = (data.playback_offset + 1) % data.buffer.len();
        Some(sample)
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
        let (_stream, stream_handle) =
            OutputStream::try_default().expect("Failed to get default audio output stream");
        let sink = Sink::try_new(&stream_handle).expect("Failed to create audio sink");

        sink.set_volume(DEFAULT_VOLUME);

        let data = Arc::new(Mutex::new(AudioSourceData {
            buffer: [0.0; AUDIO_BUFFER_SIZE_BITS],
            sample_rate: BASE_SAMPLE_RATE,
            playback_offset: 0,
            volume: 0.0,
        }));

        let mut paused = false;
        let mut is_silent = true;
        let mut current_buffer = [0; AUDIO_BUFFER_SIZE_BYTES];
        let mut remaining_duration = Duration::ZERO;

        let mut recv_last_instant = Instant::now();
        let mut recv_timeout = DEFAULT_RECV_TIMEOUT;

        sink.append(AudioSource::new(Arc::clone(&data)));
        sink.play();

        loop {
            let recv_result = event_receiver.recv_timeout(recv_timeout);

            let now = Instant::now();

            if !paused {
                let elapsed_duration = now
                    .duration_since(recv_last_instant)
                    .min(remaining_duration);
                remaining_duration -= elapsed_duration;
            }

            if let Ok(event) = recv_result.as_ref() {
                log::info!("Audio thread received event: {:?}", event);
            }

            match recv_result.as_ref().cloned() {
                Ok(event) => match event {
                    AudioEvent::SetTimer(duration) => {
                        remaining_duration = duration;
                        if !remaining_duration.is_zero() {
                            if is_silent {
                                data.lock()
                                    .expect("Unable to lock audio source data")
                                    .volume = 1.0;
                                is_silent = false;
                            }
                        }
                    }
                    AudioEvent::SetBuffer(buffer) => 'guard: {
                        if buffer == current_buffer {
                            break 'guard;
                        }

                        current_buffer = buffer;
                        data.lock()
                            .expect("Unable to lock audio source data")
                            .set_buffer(&buffer);
                    }
                    AudioEvent::SetPitch(pitch) => {
                        data.lock().unwrap().sample_rate = (BASE_SAMPLE_RATE as f32
                            * 2.0_f32.powf((pitch as f32 - 64.0) / 48.0))
                        .round() as u32;
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
                },
                Err(RecvTimeoutError::Disconnected) => {
                    sink.stop();
                    return;
                }
                Err(RecvTimeoutError::Timeout) => (),
            }

            recv_last_instant = now;
            recv_timeout = {
                if paused || is_silent {
                    DEFAULT_RECV_TIMEOUT
                } else if remaining_duration.is_zero() {
                    data.lock()
                        .expect("Unable to lock audio source data")
                        .volume = 0.0;
                    is_silent = true;
                    DEFAULT_RECV_TIMEOUT
                } else {
                    remaining_duration
                }
            };
        }
    });

    (event_sender, handle)
}