use embassy_rp::gpio::{self};
use embassy_rp::pio::{
    FifoJoin, PioInstance, PioInstanceBase, PioStateMachine, PioStateMachineInstance,
    ShiftDirection, Sm0, SmInstance,
};
use embassy_rp::pio_instr_util;
use embassy_rp::relocate::RelocatedProgram;
use embassy_time::{Duration, Timer};
use smart_leds::RGB8;
pub struct Ws2812<P: PioInstance, S: SmInstance> {
    sm: PioStateMachineInstance<P, S>,
}

impl<P: PioInstance, S: SmInstance> Ws2812<P, S> {
    pub fn new(mut sm: PioStateMachineInstance<P, S>, pin: gpio::AnyPin) -> Self {
        // Setup sm0

        // prepare the PIO program
        let side_set = pio::SideSet::new(false, 1, false);
        let mut a: pio::Assembler<32> = pio::Assembler::new_with_side_set(side_set);

        const T1: u8 = 2; // start bit
        const T2: u8 = 5; // data bit
        const T3: u8 = 3; // stop bit
        const CYCLES_PER_BIT: u32 = (T1 + T2 + T3) as u32;

        let mut wrap_target = a.label();
        let mut wrap_source = a.label();
        let mut do_zero = a.label();
        a.set_with_side_set(pio::SetDestination::PINDIRS, 1, 0);
        a.bind(&mut wrap_target);
        // Do stop bit
        a.out_with_delay_and_side_set(pio::OutDestination::X, 1, T3 - 1, 0);
        // Do start bit
        a.jmp_with_delay_and_side_set(pio::JmpCondition::XIsZero, &mut do_zero, T1 - 1, 1);
        // Do data bit = 1
        a.jmp_with_delay_and_side_set(pio::JmpCondition::Always, &mut wrap_target, T2 - 1, 1);
        a.bind(&mut do_zero);
        // Do data bit = 0
        a.nop_with_delay_and_side_set(T2 - 1, 0);
        a.bind(&mut wrap_source);

        let prg = a.assemble_with_wrap(wrap_source, wrap_target);

        let relocated = RelocatedProgram::new(&prg);
        sm.write_instr(relocated.origin() as usize, relocated.code());
        pio_instr_util::exec_jmp(&mut sm, relocated.origin());

        // Pin config
        let out_pin = sm.make_pio_pin(pin);
        sm.set_set_pins(&[&out_pin]);
        sm.set_sideset_base_pin(&out_pin);
        sm.set_sideset_count(1);

        // Clock config
        // TODO CLOCK_FREQ should come from embassy_rp
        const CLOCK_FREQ: u32 = 125_000_000;
        const WS2812_FREQ: u32 = 800_000;

        let bit_freq = WS2812_FREQ * CYCLES_PER_BIT;
        let mut int = CLOCK_FREQ / bit_freq;
        let rem = CLOCK_FREQ - (int * bit_freq);
        let frac = (rem * 256) / bit_freq;
        // 65536.0 is represented as 0 in the pio's clock divider
        if int == 65536 {
            int = 0;
        }

        sm.set_clkdiv((int << 8) | frac);
        let pio::Wrap { source, target } = relocated.wrap();
        sm.set_wrap(source, target);

        // FIFO config
        sm.set_autopull(true);
        sm.set_fifo_join(FifoJoin::TxOnly);
        sm.set_pull_threshold(24);
        sm.set_out_shift_dir(ShiftDirection::Left);

        sm.set_enable(true);

        Self { sm }
    }

    pub async fn write(&mut self, colors: &[RGB8]) {
        for color in colors {
            let word =
                (u32::from(color.g) << 24) | (u32::from(color.r) << 16) | (u32::from(color.b) << 8);
            self.sm.wait_push(word).await;
        }
    }
}

/// Input a value 0 to 255 to get a color value
/// The colours are a transition r - g - b - back to r.
pub fn status_light_val(mut wheel_pos: u8, dimm: u8) -> RGB8 {
    if wheel_pos <= 127 {
        wheel_pos = wheel_pos * 2;
        return ((255) / dimm, (wheel_pos) / dimm, 0).into();
    } else {
        wheel_pos = (wheel_pos - 128) * 2;
        return ((255 - wheel_pos) / dimm, 255 / dimm, 0).into();
    }
}

#[embassy_executor::task]
pub async fn led_task(mut ws2812: Ws2812<PioInstanceBase<1>, Sm0>) -> ! {
    // This is the number of leds in the strin>. Helpfully, the sparkfun thing plus and adafruit
    // feather boards for the 2040 both have one built in.
    const NUM_LEDS: usize = 1;
    let mut data = [RGB8::default(); NUM_LEDS];

    // Loop forever making RGB values and pushing them out to the WS2812.
    loop {
        for j in 0..(255) {
            for i in 0..NUM_LEDS {
                data[i] = status_light_val(j, 10);
            }
            ws2812.write(&data).await;

            Timer::after(Duration::from_millis(500)).await;
        }
    }
}
