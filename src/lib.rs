#![no_std]


/// This configuration is picked up at compile time by `build.rs` from the
/// file `cfg.toml`.
#[toml_cfg::toml_config]
pub struct Config {
    #[default("")]
    wifi_ssid: &'static str,
    #[default("")]
    wifi_psk: &'static str,
    #[default([0;4])]
    ip:[u8;4],
    #[default([0,0,0,0])]
    gateway:[u8;4],
    #[default([0,0,0,0])]
    mqtt_server:[u8;4],
}

pub fn get_config()->Config{
    return CONFIG;
}