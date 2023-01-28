use std::{collections::BTreeMap, time::Duration, fmt::Display};

use crossterm::style::Stylize;

pub const GOOD_EXECUTION_FREQUENCY_PERCENT_DIFF: f64 = 1.0;
pub const OKAY_EXECUTION_FREQUENCY_PERCENT_DIFF: f64 = 10.0;

pub struct C8Stats {
    pub frequency_stats: BTreeMap<u32, (Duration, u64)>,
    pub up_time: Duration,
    pub simulated_time: f64,
    pub rom_name: String,
}

impl C8Stats {
    pub fn new(rom_name: String) -> Self {
        Self {
            frequency_stats: BTreeMap::new(),
            up_time: Duration::ZERO,
            simulated_time: 0.0,
            rom_name
        }
    }

    pub fn update_frequency_stats(
        &mut self,
        freq: u32,
        duration: Duration,
        instructions: u64,
    ) {
        if instructions == 0 {
            return;
        }
    
        let (total_duration, count) = self.frequency_stats.entry(freq).or_insert((Duration::ZERO, 0));
        *total_duration = total_duration.saturating_add(duration);
        *count += instructions;
    } 
}

impl Display for C8Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} \"{}\" runtime",
            format!("Analyzing").green().bold(),
            self.rom_name
        )?;

        for (&target_ips, &(runtime_duration, instructions_executed)) in self.frequency_stats.iter()
        {
            let ips = instructions_executed as f64 / runtime_duration.as_secs_f64();
            let ips_diff = (ips - target_ips as f64) / target_ips as f64 * 100.0;
            let color_ips_diff = if ips_diff.abs() > OKAY_EXECUTION_FREQUENCY_PERCENT_DIFF {
                Stylize::red
            } else if ips_diff.abs() > GOOD_EXECUTION_FREQUENCY_PERCENT_DIFF {
                Stylize::yellow
            } else {
                Stylize::green
            };

            write!(f, "\n    {}", format!("|").blue().bold())?;

            writeln!(
                f,
                "\n    {} Runner ({:#04}Hz): {:.3}s",
                format!("|").blue().bold(),
                target_ips,
                runtime_duration.as_secs_f64()
            )?;
            write!(
                f,
                "    {} Runner continuously executed at {:#07.2}Hz",
                format!("=").blue().bold(),
                if ips.is_finite() { ips } else { 0.0 }
            )?;

            if ips.is_finite() {
                write!(
                    f,
                    " ( {} from {:#04}Hz target )",
                    color_ips_diff(format!(
                        "{}{:.2}%",
                        if ips_diff >= 0.0 { "+" } else { "" },
                        ips_diff
                    ))
                    .bold(),
                    target_ips
                )?;
            }

            write!(f, "\n    {}", format!("|").blue().bold())?;

            write!(
                f,
                "\n    {}  Simulated Time: {:.3}s",
                format!("=").blue().bold(),
                //target_ips,
                self.simulated_time
            )?;

            write!(
                f,
                "\n    {} C8 Program Time: {:.3}s",
                format!("=").blue().bold(),
                //target_ips,
                self.up_time.as_secs_f64()
            )?;
        }

        Ok(())
    }
}