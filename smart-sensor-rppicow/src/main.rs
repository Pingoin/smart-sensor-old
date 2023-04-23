#![no_std]
#![no_main]
#![feature(type_alias_impl_trait)]
#![feature(async_fn_in_trait)]
#![allow(incomplete_features)]

use cyw43_pio::PioSpi;
use defmt::*;
use embassy_executor::Spawner;
use embassy_net::tcp::TcpSocket;
use embassy_net::{Config, Ipv4Address, Ipv4Cidr, Stack, StackResources};
use embassy_rp::adc::{self, Adc};
use embassy_rp::gpio::{Level, Output, Pin};
use embassy_rp::interrupt;
use embassy_rp::peripherals::{DMA_CH0, PIN_23, PIN_25};
use embassy_rp::pio::{Pio0, PioPeripheral, PioStateMachineInstance, Sm0};
use embassy_time::{Duration, Ticker, Timer};
use heapless::Vec;

mod converts;
mod neo_pixel;

use smart_sensor::{mutex_box::MutexBox, round_to_n_places, SensorData};
use static_cell::StaticCell;
use {defmt_rtt as _, panic_probe as _};

use rust_mqtt::{
    client::{client::MqttClient, client_config::ClientConfig},
    utils::rng_generator::CountingRng,
};

use crate::converts::convert_to_celsius;
use crate::neo_pixel::{led_task, Ws2812};

macro_rules! singleton {
    ($val:expr) => {{
        type T = impl Sized;
        static STATIC_CELL: StaticCell<T> = StaticCell::new();
        STATIC_CELL.init_with(move || $val)
    }};
}

static SENSOR_DATA: MutexBox<SensorData> = MutexBox::new("sensor_data");
const MAX_MQTT_PACKET: usize = 1024;

#[embassy_executor::task]
async fn wifi_task(
    runner: cyw43::Runner<
        'static,
        Output<'static, PIN_23>,
        PioSpi<PIN_25, PioStateMachineInstance<Pio0, Sm0>, DMA_CH0>,
    >,
) -> ! {
    runner.run().await
}

#[embassy_executor::task]
async fn net_task(stack: &'static Stack<cyw43::NetDriver<'static>>) -> ! {
    stack.run().await
}

#[embassy_executor::task]
async fn mqtt_task(stack: &'static Stack<cyw43::NetDriver<'static>>, server: Ipv4Address) {
    let remote_endpoint = (server, 1883);
    let mut ticker = Ticker::every(Duration::from_secs(10));
    loop {
        info!("opening");

        // Then we can use it!
        let mut rx_buffer = [0; MAX_MQTT_PACKET];
        let mut tx_buffer = [0; MAX_MQTT_PACKET];

        let mut recv_buffer = [0; MAX_MQTT_PACKET];
        let mut write_buffer = [0; MAX_MQTT_PACKET];

        let mut sock = TcpSocket::new(stack, &mut rx_buffer, &mut tx_buffer);
        sock.set_timeout(Some(embassy_net::SmolDuration::from_secs(5)));
        sock.set_keep_alive(Some(embassy_net::SmolDuration::from_secs(2)));

        let mut config = ClientConfig::new(
            rust_mqtt::client::client_config::MqttVersion::MQTTv5,
            CountingRng(20000),
        );
        config.add_max_subscribe_qos(rust_mqtt::packet::v5::publish_packet::QualityOfService::QoS1);
        config.add_client_id("client");
        // config.add_username(USERNAME);
        // config.add_password(PASSWORD);
        config.max_packet_size = MAX_MQTT_PACKET as u32;

        info!("connecting to {:?}...", remote_endpoint);

        loop {
            match sock.connect(remote_endpoint).await {
                Ok(_) => {
                    info!("connected TCP");
                    break;
                }
                Err(err) => {
                    warn!("connect error: {:?}", err);
                    info!("retrying...");
                    Timer::after(Duration::from_millis(500)).await;
                }
            }
        }
        let mut client =
            MqttClient::<_, 5, _>::new(sock, &mut write_buffer, 100, &mut recv_buffer, 100, config);
        loop {
            match client.connect_to_broker().await {
                Ok(_) => {
                    info!("connected MQTT");
                    break;
                }
                Err(err) => {
                    warn!("error connecting to mqtt: {:?}", err);
                    info!("retrying...");
                    Timer::after(Duration::from_millis(500)).await;
                }
            };
        }

        let mut sensordata = SensorData::default();

        SENSOR_DATA.open_locked(|data| {
            sensordata = data.clone();
            if let Some(voltage) = data.system_voltage {
                data.system_voltage = Some(voltage + 1.0);
            }
        });

        let mut buff = [0; MAX_MQTT_PACKET];
        match serde_json_core::ser::to_slice(&sensordata, &mut buff) {
            Ok(size) => loop {
                match client
                    .send_message(
                        "hello/gurken",
                        &buff[..size],
                        rust_mqtt::packet::v5::publish_packet::QualityOfService::QoS0,
                        true,
                    )
                    .await
                {
                    Ok(_) => {
                        info!("pong: {}/{}", &sensordata.system_voltage, size);
                        Timer::after(Duration::from_millis(5)).await;
                        break;
                    }
                    Err(err) => {
                        warn!("error publish to mqtt: {:?}", err);
                        info!("retrying...");
                        Timer::after(Duration::from_millis(500)).await;
                    }
                }
            },
            Err(_err) => warn!("error serilizing"),
        }
        client.disconnect().await.unwrap();
        info!("closed");
        ticker.next().await;
    }
}

#[embassy_executor::main]
async fn main(spawner: Spawner) {
    {
        let mut sens = SensorData::default();
        sens.system_voltage = Some(0.0);
        sens.version = (0, 1, 0);
        SENSOR_DATA.init(sens);
    }

    // The constant `CONFIG` is auto-generated by `toml_config`.
    let app_config = smart_sensor::CONFIG;
    info!("Hello World");
    let p = embassy_rp::init(Default::default());
    // Include the WiFi firmware and Country Locale Matrix (CLM) blobs.
    let fw = include_bytes!("../firmware/43439A0.bin");
    let clm = include_bytes!("../firmware/43439A0_clm.bin");

    // To make flashing faster for development, you may want to flash the firmwares independently
    // at hardcoded addresses, instead of baking them into the program with `include_bytes!`:
    //     probe-rs-cli download 43439A0.bin --format bin --chip RP2040 --base-address 0x10100000
    //     probe-rs-cli download 43439A0_clm.bin --format bin --chip RP2040 --base-address 0x10140000
    //let fw = unsafe { core::slice::from_raw_parts(0x10100000 as *const u8, 224190) };
    //let clm = unsafe { core::slice::from_raw_parts(0x10140000 as *const u8, 4752) };

    let pwr = Output::new(p.PIN_23, Level::Low);
    let cs = Output::new(p.PIN_25, Level::High);

    let (_, sm, _, _, _) = p.PIO0.split();
    let dma = p.DMA_CH0;
    let spi = PioSpi::new(sm, cs, p.PIN_24, p.PIN_29, dma);

    let state = singleton!(cyw43::State::new());
    let (net_device, mut control, runner) = cyw43::new(state, pwr, spi, fw).await;
    unwrap!(spawner.spawn(wifi_task(runner)));

    control.init(clm).await;
    control
        .set_power_management(cyw43::PowerManagementMode::PowerSave)
        .await;

    //let config = Config::Dhcp(Default::default());
    let config = Config::Static(embassy_net::StaticConfig {
        address: Ipv4Cidr::new(Ipv4Address::from_bytes(&app_config.ip), 24),
        dns_servers: Vec::new(),
        gateway: Some(Ipv4Address::from_bytes(&app_config.gateway)),
    });

    // Generate random seed
    let seed = 0x0123_4567_89ab_cdef; // chosen by fair dice roll. guarenteed to be random.

    // Init network stack
    let stack = &*singleton!(Stack::new(
        net_device,
        config,
        singleton!(StackResources::<2>::new()),
        seed
    ));

    unwrap!(spawner.spawn(net_task(stack)));
    //control.join_open(env!("WIFI_NETWORK")).await;
    control
        .join_wpa2(app_config.wifi_ssid, app_config.wifi_psk)
        .await;
    let mut ticker = Ticker::every(Duration::from_secs(10));
    unwrap!(spawner.spawn(mqtt_task(
        stack,
        Ipv4Address::from_bytes(&app_config.mqtt_server)
    )));

    let irq = interrupt::take!(ADC_IRQ_FIFO);
    let mut adc = Adc::new(p.ADC, irq, adc::Config::default());
    let (_pio0, sm0, _sm1, _sm2, _sm3) = p.PIO1.split();
    let ws2812 = Ws2812::new(sm0, p.PIN_19.degrade());

    unwrap!(spawner.spawn(led_task(ws2812)));

    let mut led = true;
    loop {
        control.gpio_set(0, led).await;
        led = !led;
        let temp = adc.read_temperature().await;
        let temp = round_to_n_places(convert_to_celsius(temp), 1);

        SENSOR_DATA.open_locked(|data| {
            data.core_temperature = Some(temp);
        });

        info!("Temp: {} degrees", temp);
        ticker.next().await;
    }
}
