use super::rom::RomKind;

use rodio::{source::Source, OutputStream, Sink};

use std::{
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicU64, AtomicU8, Ordering},
        mpsc::{channel, RecvTimeoutError, Sender},
        Arc
    },
    thread::JoinHandle,
    time::{Duration, Instant},
};

pub const AUDIO_BUFFER_SIZE_BYTES: usize = 16;

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

#[derive(Clone)]
pub struct AudioSource {
    buffer: [Arc<AtomicU64>; 2],
    playback_offset: Arc<AtomicU8>,
    is_audible: Arc<AtomicBool>,
    sample_rate: Arc<AtomicU32>,
}

impl AudioSource {
    pub fn new() -> Self {
        AudioSource {
            buffer: [Arc::new(AtomicU64::new(0)), Arc::new(AtomicU64::new(0))],
            playback_offset: Arc::new(AtomicU8::new(0)),
            is_audible: Arc::new(AtomicBool::new(false)),
            sample_rate: Arc::new(AtomicU32::new(BASE_SAMPLE_RATE)),
        }
    }
}

impl AudioSource {
    fn set_buffer(&self, buffer: &[u8; AUDIO_BUFFER_SIZE_BYTES]) {
        buffer
            .chunks_exact(8)
            .map(|bytes| {
                bytes
                    .iter()
                    .fold(0, |bit64, byte| (bit64 << 8) | *byte as u64)
            })
            .zip(self.buffer.iter())
            .for_each(|(bit64, atomic64)| {
                atomic64.store(bit64, Ordering::Release);
            });
    }
    fn set_audible(&self, is_audible: bool) {
        if is_audible {
            self.playback_offset.store(0, Ordering::Release);
        }
        self.is_audible.store(is_audible, Ordering::Release);
    }
    fn set_sample_rate(&self, sample_rate: u32) {
        self.sample_rate.store(sample_rate, Ordering::Release);
    }
}

impl Source for AudioSource {
    fn current_frame_len(&self) -> Option<usize> {
        Some(8)
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate.load(Ordering::Acquire)
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
}

impl Iterator for AudioSource {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let playback_offset = self.playback_offset.fetch_add(1, Ordering::SeqCst) % 128;
        if self.is_audible.load(Ordering::Acquire) {
            let is_waveform_peak = self.buffer[playback_offset as usize / 64]
                .load(Ordering::Acquire)
                >> (63 - (playback_offset % 64))
                & 1 
                == 1;
            Some(if is_waveform_peak { 1.0 } else { 0.0 })
        } else {
            Some(0.0)
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
        let (_stream, stream_handle) =
            OutputStream::try_default().expect("Failed to get default audio output stream");
        let sink = Sink::try_new(&stream_handle).expect("Failed to create audio sink");

        sink.set_volume(DEFAULT_VOLUME);

        let source = AudioSource::new();

        let mut paused = false;
        let mut is_silent = true;
        let mut current_buffer = [0; AUDIO_BUFFER_SIZE_BYTES];
        let mut remaining_duration = Duration::ZERO;

        let mut recv_last_instant = Instant::now();
        let mut recv_timeout = DEFAULT_RECV_TIMEOUT;

        sink.append(source.clone());
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
                log::trace!("Audio thread received event: {:?}", event);
            }

            match recv_result.as_ref().cloned() {
                Ok(event) => match event {
                    AudioEvent::SetTimer(duration) => {
                        remaining_duration = duration;
                        if !remaining_duration.is_zero() {
                            if is_silent {
                                source.set_audible(true);
                                is_silent = false;
                            }
                        }
                    }
                    AudioEvent::SetBuffer(buffer) => 'guard: {
                        if buffer == current_buffer {
                            break 'guard;
                        }

                        current_buffer = buffer;
                        source.set_buffer(&buffer);
                    }
                    AudioEvent::SetPitch(pitch) => {
                        source.set_sample_rate(
                            (BASE_SAMPLE_RATE as f32 * 2.0_f32.powf((pitch as f32 - 64.0) / 48.0))
                                .round() as u32,
                        );
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
                    source.set_audible(false);
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
