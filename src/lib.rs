#![no_std]

use serde::{Deserialize, Serialize};
pub mod mutex_box;

/// This configuration is picked up at compile time by `build.rs` from the
/// file `cfg.toml`.
#[toml_cfg::toml_config]
pub struct Config {
    #[default("")]
    wifi_ssid: &'static str,
    #[default("")]
    wifi_psk: &'static str,
    #[default([0;4])]
    ip: [u8; 4],
    #[default([0,0,0,0])]
    gateway: [u8; 4],
    #[default([0,0,0,0])]
    mqtt_server: [u8; 4],
}

pub fn get_config() -> Config {
    return CONFIG;
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct SensorData {
    #[serde(skip_serializing_if = "Option::is_none",rename = "t")]
    pub temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none",rename = "t_core")]
    pub core_temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none",rename = "hrel")]
    pub humidity: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none",rename = "V_bat")]
    pub battery_voltage: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none",rename = "V_psu")]
    pub system_voltage: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none",rename = "p")]
    pub pressure: Option<f32>,
    #[serde(default = "default_version",rename = "ver")]
    pub version: (u8, u8, u8),
}

fn default_version() -> (u8, u8, u8) {
    (0, 0, 0)
}

impl Default for SensorData {
    fn default() -> Self {
        Self {
            temperature: None,
            humidity: None,
            core_temperature: None,
            battery_voltage: None,
            system_voltage: None,
            pressure: None,
            version: (0, 0, 0),
        }
    }
}

pub fn round_to_n_places(input:f32,n:usize)->f32{
   let factor= libm::powf(10.0, n as f32);
   libm::floorf(input*factor+0.5)/factor
}