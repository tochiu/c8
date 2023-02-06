use super::rom::RomKind;

use rodio::{source::Source, OutputStream, Sink};

use std::{
    sync::{
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

pub const RODIO_SAMPLING_RATE: u32 = 96000;

pub const AUDIO_BUFFER_SIZE_BYTES: usize = 16;

const BASE_SAMPLE_RATE: f32 = 4000.0;
const DEFAULT_VOLUME: f32 = 0.5;

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

impl Audio {
    pub fn sample_rate(&self) -> f32 {
        chip8_pitch_to_sample_rate(self.pitch)
    }
}

#[derive(Clone)]
pub struct AudioSource {
    buffer: [Arc<AtomicU64>; 2],
    playback_offset_bits: Arc<AtomicU32>,
    is_audible: Arc<AtomicBool>,
    sample_rate_bits: Arc<AtomicU32>,
}

impl AudioSource {
    pub fn new() -> Self {
        AudioSource {
            buffer: [Arc::new(AtomicU64::new(0)), Arc::new(AtomicU64::new(0))],
            playback_offset_bits: Arc::new(AtomicU32::new(0)),
            is_audible: Arc::new(AtomicBool::new(false)),
            sample_rate_bits: Arc::new(AtomicU32::new(BASE_SAMPLE_RATE.to_bits())),
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
            self.playback_offset_bits.store(0, Ordering::Release);
        }
        self.is_audible.store(is_audible, Ordering::Release);
    }

    fn set_sample_rate(&self, sample_rate: f32) {
        self.sample_rate_bits
            .store(sample_rate.to_bits(), Ordering::Release);
    }
}

impl Source for AudioSource {
    fn current_frame_len(&self) -> Option<usize> {
        None
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        RODIO_SAMPLING_RATE
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
}

impl Iterator for AudioSource {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        let sampling_rate = f32::from_bits(self.sample_rate_bits.load(Ordering::Acquire));

        let Ok(playback_offset_bits) = self.playback_offset_bits.fetch_update(
            Ordering::SeqCst,
            Ordering::SeqCst,
            |playback_offset_bits| {
                let next_offset = (f32::from_bits(playback_offset_bits) + sampling_rate / RODIO_SAMPLING_RATE as f32) % 128.0;
                Some(next_offset.to_bits())
            }
        ) else {
            unreachable!("fetch_update should never return None")
        };

        let playback_offset = (f32::from_bits(playback_offset_bits).round() % 128.0) as usize;

        if self.is_audible.load(Ordering::Acquire) {
            let is_waveform_peak = self.buffer[playback_offset / 64].load(Ordering::Acquire)
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

pub struct AudioController {
    sink: Sink,
    source: AudioSource,
    paused: bool,
    silent: bool,
    volume: f32,
    buffer: [u8; AUDIO_BUFFER_SIZE_BYTES],
    remaining_duration: Duration,
    remaining_duration_instant: Instant,
}

impl AudioController {
    fn new(sink: Sink) -> Self {
        let source = AudioSource::new();

        sink.set_volume(DEFAULT_VOLUME);
        sink.append(source.clone());
        sink.play();

        let controller = AudioController {
            sink,
            source,
            paused: false,
            silent: true,
            volume: DEFAULT_VOLUME,
            buffer: [0; AUDIO_BUFFER_SIZE_BYTES],
            remaining_duration: Duration::ZERO,
            remaining_duration_instant: Instant::now(),
        };

        controller
    }

    pub fn update(&mut self) {
        let now = Instant::now();
        if !self.paused {
            self.remaining_duration = self
                .remaining_duration
                .saturating_sub(now.duration_since(self.remaining_duration_instant));
        }
        self.remaining_duration_instant = now;
        if self.remaining_duration.is_zero() {
            self.source.set_audible(false);
            self.silent = true;
        }
    }

    pub fn volume(&self) -> f32 {
        self.volume
    }

    pub fn set_volume(&mut self, volume: f32) {
        self.volume = volume;
        self.sink.set_volume(volume);
    }

    pub fn apply_event(&mut self, event: AudioEvent) {
        match event {
            AudioEvent::SetTimer(duration) => {
                self.remaining_duration = duration;
                if !self.remaining_duration.is_zero() {
                    if self.silent {
                        self.source.set_audible(true);
                        self.silent = false;
                    }

                    self.remaining_duration_instant = Instant::now();
                }
            }
            AudioEvent::SetBuffer(buffer) => 'guard: {
                if buffer == self.buffer {
                    break 'guard;
                }

                self.buffer = buffer;
                self.source.set_buffer(&buffer);
            }
            AudioEvent::SetPitch(pitch) => {
                self.source
                    .set_sample_rate(chip8_pitch_to_sample_rate(pitch));
            }
            AudioEvent::Pause => 'guard: {
                if self.paused {
                    break 'guard;
                }
                self.paused = true;
                self.sink.pause();
            }
            AudioEvent::Resume => 'guard: {
                if !self.paused {
                    break 'guard;
                }
                self.paused = false;
                self.sink.play();
            }
        }
    }
}

fn chip8_pitch_to_sample_rate(pitch: u8) -> f32 {
    BASE_SAMPLE_RATE as f32 * 2.0_f32.powf((pitch as f32 - 64.0) / 48.0)
}

pub fn spawn_audio_stream() -> (OutputStream, AudioController) {
    // Get a output stream handle to the default physical sound device
    let (stream, stream_handle) =
        OutputStream::try_default().expect("Failed to get default audio output stream");
    let controller =
        AudioController::new(Sink::try_new(&stream_handle).expect("Failed to create audio sink"));

    (stream, controller)
}
