use embassy_time::{Duration, Instant};

pub struct ClockTime {
    duration: Duration,
}

impl ClockTime {
    pub fn new() -> Self {
        Self {
            duration: Duration::from_secs(0),
        }
    }

    /// Returns timestamp in seconds since UNIX EPOCH for the initialized generator
    fn timestamp_sec(&self) -> u64 {
        self.get_time().as_secs()
    }

    pub fn get_time(&self) -> Instant {
        Instant::now() + self.duration
    }

    pub fn get_date_time(&self) -> DateTime {
        let now = self.get_time();

        DateTime {
            year: 0,
            month: 0,
            day: 0,
            hour: ((now.as_secs() / (60 * 60)) % 24) as u8,
            minute: ((now.as_secs() / (60)) % 60) as u8,
            second: (now.as_secs()  % 60) as u8,
            millis: (now.as_millis()%1_000)as u16,
            micros: (now.as_micros()%1_000)as u16,
        }
    }

    pub fn update(&mut self, now: Instant) {
        let ntp_now = now.as_micros();
        let new_epoch = ntp_now - Instant::now().as_micros();
        self.duration = Duration::from_micros(new_epoch);
    }
    /// Returns the fractional part of the timestamp in whole micro seconds.
    /// That method **should not** return microseconds since UNIX EPOCH
    fn timestamp_subsec_micros(&self) -> u32 {
        self.get_time().as_micros() as u32 % 1_000_000
    }

    pub fn get_unix_time(&self) -> UnixTimeStamp {
        UnixTimeStamp {
            secs: self.timestamp_sec(),
            micro_secs: self.timestamp_subsec_micros(),
        }
    }
}

pub struct UnixTimeStamp {
    pub secs: u64,
    pub micro_secs: u32,
}

pub struct DateTime {
    pub year: u16,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub millis: u16,
    pub micros: u16,
}
