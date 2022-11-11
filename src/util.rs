use std::time::{Duration, Instant};

/*
 * Intervals keep state about the time elapsed since the previous sleep call
 * and compensate for it in the next sleep call so the starting time of logic 
 * running between each sleep will be spaced apart by the desired interval
 * 
 * Because sleep fundamentally cannot execute for exactly the amount requested and logic between each sleep will elapse for a nonzero duration
 * we cut the duration overslept and the execution time of the logic from the original sleep duration to try and resynchronize
 * 
 * Deviations from the target interval that require more than one interval to compensate for will not be compensated for but instead forgotten
 * For example if we execute for 0.8 seconds but are supposed to execute within 0.2 second intervals at a time then 
 * we will not try to compensate for the extra 0.6 seconds across multiple intervals and instead refuse to sleep in the next interval (0.2 - 0.6 <= 0)
 * and forget the remaining time to cut out
 * 
 * This behavior exists because recovering from time deviations bigger than the target interval will lead to subsequent intervals being much 
 * smaller than our target interval when what we are optimizing for is minimal absolute deviation
 * 
 * I don't really think this construct is necessary but I thought I might need it 
 * Anyway having this gives me peace of mind and also I already wrote it and could probably use it somewhere else later
 */
pub struct Interval {
    // interval name
    #[allow(dead_code)]
    name: &'static str,

    // desired duration of interval
    interval: Duration, 

    // max_quantum is the maximum duration we can go without sleeping before being forced to sleep
    // compensation can lead to no sleeping if oversleeping and/or executing logic between sleeps is longer than the desired interval
    // allowing this to occur unchecked can lead to deadlock since other threads could potentially be starved of execution time
    max_quantum: Duration,

    // instant last sleep was complete
    task_start: Instant,

    // how much we overslept by
    oversleep_duration: Duration,

    // how long weve gone since an actual sleep
    quantum_duration: Duration,
}

impl Interval {
    pub fn new(name: &'static str, interval: Duration, max_quantum: Duration) -> Self {
        Interval {
            name,
            interval,
            max_quantum,
            task_start: Instant::now(),
            oversleep_duration: Duration::ZERO,
            quantum_duration: Duration::ZERO,
        }
    }

    pub fn reset(&mut self) {
        self.task_start = Instant::now();
        self.oversleep_duration = Duration::ZERO;
        self.quantum_duration = Duration::ZERO;
    }

    pub fn sleep(&mut self) {
        let task_duration = self.task_start.elapsed(); // time since the end of our last sleep

        // update our quantum duration
        self.quantum_duration += task_duration;

        // we kept track of how much we overslept last time so this and our task duration combined is our overshoot
        // we must subtract from the original to stay on schedule
        let mut sleep_duration = self
            .interval
            .saturating_sub(task_duration)
            .saturating_sub(self.oversleep_duration);
        
        // if our compensation leads to no sleeping and we havent exceeded our max quantum then we dont sleep
        if sleep_duration.is_zero() && self.quantum_duration < self.max_quantum {
            self.oversleep_duration = Duration::ZERO;
        } else {
            // otherwise if were not sleeping but we are past our max quantum we must sleep (right now its hardcoded to a constant 1ms)
            if sleep_duration.is_zero() && self.quantum_duration >= self.max_quantum {
                sleep_duration = Duration::from_millis(1);
            }

            let now = Instant::now();

            spin_sleep::sleep(sleep_duration); // sleep

            // store how much we overslept by and reset our quantum duration since we slept for a nonzero duration
            self.oversleep_duration = now.elapsed().saturating_sub(sleep_duration);
            self.quantum_duration = Duration::ZERO;
        }

        // log::trace!(
        //     "name: {}, task: {} us, sleep: {} us, oversleep: {} us",
        //     self.name,
        //     task_duration.as_micros(),
        //     sleep_duration.as_micros(),
        //     self.oversleep_duration.as_micros()
        // );
        
        // update task start to now since sleep is done
        self.task_start = Instant::now();
    }
}