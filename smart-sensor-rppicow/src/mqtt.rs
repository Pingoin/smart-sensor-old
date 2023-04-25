use crate::SENSOR_DATA;
use defmt::*;
use embassy_net::{tcp::TcpSocket, Ipv4Address, Stack};
use embassy_time::{Duration, Ticker, Timer};
use rust_mqtt::{
    client::{client::MqttClient, client_config::ClientConfig},
    utils::rng_generator::CountingRng,
};
use smart_sensor::SensorData;

const MAX_MQTT_PACKET: usize = 1024;

#[embassy_executor::task]
pub async fn mqtt_task(stack: &'static Stack<cyw43::NetDriver<'static>>, server: Ipv4Address) {
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
        let mut client = MqttClient::<_, 5, _>::new(
            sock,
            &mut write_buffer,
            MAX_MQTT_PACKET,
            &mut recv_buffer,
            MAX_MQTT_PACKET,
            config,
        );
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
