#![no_std]
use clock_time::{DateTime, UnixTimeStamp};
use core::cell::RefCell;
use core::ops::DerefMut;
use cortex_m::interrupt::{self, Mutex};
use embassy_time::Instant;
#[cfg(feature = "sntp")]
pub mod sntpc;

mod clock_time;

static WALL_CLOCK: Mutex<RefCell<Option<clock_time::ClockTime>>> = Mutex::new(RefCell::new(None));

pub fn init_clock_time() {
    interrupt::free(|cs| {
        WALL_CLOCK
            .borrow(cs)
            .replace(Some(clock_time::ClockTime::new()));
    });
}

pub fn update_clock_time(now: Instant) {
    interrupt::free(|cs| {
        if let Some(ref mut clock) = WALL_CLOCK.borrow(cs).borrow_mut().deref_mut() {
            clock.update(now);
        }
    });
}

pub fn get_time() -> Instant {
    let mut result = Instant::from_ticks(0);
    interrupt::free(|cs| {
        if let Some(clock) = WALL_CLOCK.borrow(cs).borrow_mut().deref_mut() {
            result = clock.get_time();
        }
    });
    result
}

pub fn get_date_time() -> DateTime {
    let mut result = DateTime {
        year: 0,
        month: 0,
        day: 0,
        hour: 0,
        minute: 0,
        second: 0,
        millis: 0,
        micros: 0,
    };
    interrupt::free(|cs| {
        if let Some(clock) = WALL_CLOCK.borrow(cs).borrow_mut().deref_mut() {
            result = clock.get_date_time();
        }
    });
    result
}

pub fn get_unix_time() -> UnixTimeStamp {
    let mut result =  UnixTimeStamp{
        secs: 0,
        micro_secs: 0,
    };
    interrupt::free(|cs| {
        if let Some(clock) = WALL_CLOCK.borrow(cs).borrow_mut().deref_mut() {
            result = clock.get_unix_time();
        }
    });
    result
}
