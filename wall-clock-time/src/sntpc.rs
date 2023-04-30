use core::fmt::Formatter;
use core::fmt::{Debug, Display};
use core::marker::Copy;
use core::mem;
use embassy_net::udp::UdpSocket;
use embassy_net::Ipv4Address;
use embassy_time::Instant;

use crate::clock_time::UnixTimeStamp;

/// SNTP mode value bit mask
const MODE_MASK: u8 = 0b0000_0111;
/// SNTP mode bit mask shift value
const MODE_SHIFT: u8 = 0;
/// SNTP version value bit mask
const VERSION_MASK: u8 = 0b0011_1000;
/// SNTP mode bit mask shift value
const VERSION_SHIFT: u8 = 3;
/// SNTP LI (leap indicator) bit mask value
const LI_MASK: u8 = 0b1100_0000;
/// SNTP LI bit mask shift value
const LI_SHIFT: u8 = 6;
/// SNTP nanoseconds in second constant
#[allow(dead_code)]
const NSEC_IN_SEC: u32 = 1_000_000_000;
/// SNTP microseconds in second constant
const USEC_IN_SEC: u32 = 1_000_000;
/// SNTP milliseconds in second constant
const MSEC_IN_SEC: u32 = 1_000;
/// SNTP seconds mask
const SECONDS_MASK: u64 = 0xffff_ffff_0000_0000;
/// SNTP seconds fraction mask
const SECONDS_FRAC_MASK: u64 = 0xffff_ffff;
/// SNTP library result type
pub type Result<T> = core::result::Result<T, Error>;

pub struct NtpTime<'a> {
    socket: UdpSocket<'a>,
    adress: (Ipv4Address, u16),
}

impl NtpTime<'_> {
    pub fn new<'a>(socket: UdpSocket<'a>, adress: (Ipv4Address, u16)) -> NtpTime<'a> {
        return NtpTime { socket, adress };
    }

    async fn sntp_send_request(&mut self, timestamp: &UnixTimeStamp) -> Result<SendRequestResult> {
        let request = NtpPacket::new(timestamp);
        if let Err(err) = self.send_request(&request).await {
            return Err(err);
        }
        Ok(SendRequestResult::from(request))
    }

    async fn send_request(&mut self, req: &NtpPacket) -> core::result::Result<(), Error> {
        let buf = RawNtpPacket::from(req);

        return match self.socket.send_to(&buf.0, self.adress).await {
            Ok(_) => Ok(()),
            Err(_) => Err(Error::Network),
        };
    }

    pub async fn update_time(&mut self, timestamp: UnixTimeStamp) -> Option<Instant> {
        let result = self.sntp_send_request(&timestamp).await.unwrap();
        let result = self.sntp_process_response(result, timestamp).await;
        if let Ok(data) = result {
            let ntp_time =
                (data.sec() as u64) * 1_000_000 + (data.sec_fraction() as u64) % 1_000_000;

            return Some(Instant::from_micros(ntp_time));
        }
        None
    }

    async fn sntp_process_response(
        &mut self,
        send_req_result: SendRequestResult,
        timestamp: UnixTimeStamp,
    ) -> Result<NtpResult> {
        let mut response_buf = RawNtpPacket::default();
        let (response, _) = self
            .socket
            .recv_from(response_buf.0.as_mut())
            .await
            .unwrap();
        let recv_timestamp = get_ntp_timestamp(&timestamp);
        if response != mem::size_of::<NtpPacket>() {
            return Err(Error::IncorrectPayload);
        }
        let result = process_response(send_req_result, response_buf, recv_timestamp);
        return match result {
            Ok(result) => Ok(result),
            Err(err) => Err(err),
        };
    }
}

#[derive(Debug)]
struct NtpPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    ref_id: u32,
    ref_timestamp: u64,
    origin_timestamp: u64,
    recv_timestamp: u64,
    tx_timestamp: u64,
}

#[derive(Debug, Copy, Clone)]
struct NtpTimestamp {
    seconds: i64,
    seconds_fraction: i64,
}

impl From<u64> for NtpTimestamp {
    fn from(v: u64) -> Self {
        let seconds = (((v & SECONDS_MASK) >> 32) - NtpPacket::NTP_TIMESTAMP_DELTA as u64) as i64;
        let microseconds = (v & SECONDS_FRAC_MASK) as i64;

        NtpTimestamp {
            seconds,
            seconds_fraction: microseconds,
        }
    }
}

/// Helper enum for specification delay units
#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
enum Units {
    Milliseconds,
    Microseconds,
}

impl Display for Units {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let unit = match self {
            Units::Microseconds => "us",
            Units::Milliseconds => "ms",
        };

        write!(f, "{}", unit)
    }
}

/// The error type for SNTP client
/// Errors originate on network layer or during processing response from a NTP server
#[derive(Debug, PartialEq, Copy, Clone)]
#[non_exhaustive]
pub enum Error {
    /// Origin timestamp value in a NTP response differs from the value
    /// that has been sent in the NTP request
    IncorrectOriginTimestamp,
    /// Incorrect mode value in a NTP response
    IncorrectMode,
    /// Incorrect Leap Indicator (LI) value in a NTP response
    IncorrectLeapIndicator,
    /// Incorrect version in a NTP response. Currently SNTPv4 is supported
    IncorrectResponseVersion,
    /// Incorrect stratum headers in a NTP response
    IncorrectStratumHeaders,
    /// Payload size of a NTP response does not meet SNTPv4 specification
    IncorrectPayload,
    /// Network error occurred.
    Network,
    /// A NTP server address can not be resolved
    AddressResolve,
    /// A NTP server address response has been received from does not match
    /// to the address the request was sent to
    ResponseAddressMismatch,
}

/// SNTP request result representation
#[derive(Debug)]
pub struct NtpResult {
    /// NTP server seconds value
    pub seconds: u32,
    /// NTP server seconds fraction value (microseconds)
    pub seconds_fraction: u32,
    /// Request roundtrip time in microseconds
    pub roundtrip: u64,
    /// Offset of the current system time with one received from a NTP server in microseconds
    pub offset: i64,
}

impl NtpResult {
    /// Create new NTP result
    /// Args:
    /// * `seconds` - number of seconds
    /// * `seconds_fraction` - number of nanoseconds
    /// * `roundtrip` - calculated roundtrip in microseconds
    /// * `offset` - calculated system clock offset in microseconds
    pub fn new(seconds: u32, seconds_fraction: u32, roundtrip: u64, offset: i64) -> Self {
        let seconds = seconds + seconds_fraction / u32::MAX;
        let seconds_fraction = seconds_fraction % u32::MAX;

        NtpResult {
            seconds,
            seconds_fraction,
            roundtrip,
            offset,
        }
    }
    /// Returns number of seconds reported by an NTP server
    pub fn sec(&self) -> u32 {
        self.seconds
    }

    /// Returns number of seconds fraction reported by an NTP server
    pub fn sec_fraction(&self) -> u32 {
        self.seconds_fraction
    }

    /// Returns request's roundtrip time (client -> server -> client) in microseconds
    pub fn roundtrip(&self) -> u64 {
        self.roundtrip
    }

    /// Returns system clock offset value in microseconds
    pub fn offset(&self) -> i64 {
        self.offset
    }
}

impl NtpPacket {
    // First day UNIX era offset https://www.rfc-editor.org/rfc/rfc5905
    const NTP_TIMESTAMP_DELTA: u32 = 2_208_988_800u32;
    const SNTP_CLIENT_MODE: u8 = 3;
    const SNTP_VERSION: u8 = 4 << 3;

    pub fn new(timestamp_gen: &UnixTimeStamp) -> NtpPacket {
        let tx_timestamp = get_ntp_timestamp(timestamp_gen);

        #[cfg(feature = "log")]
        debug!(target: "NtpPacket::new", "{}", tx_timestamp);

        NtpPacket {
            li_vn_mode: NtpPacket::SNTP_CLIENT_MODE | NtpPacket::SNTP_VERSION,
            stratum: 0,
            poll: 0,
            precision: 0,
            root_delay: 0,
            root_dispersion: 0,
            ref_id: 0,
            ref_timestamp: 0,
            origin_timestamp: 0,
            recv_timestamp: 0,
            tx_timestamp,
        }
    }
}

/// Preserve SNTP request sending operation result required during receiving and processing
/// state
#[derive(Copy, Clone, Debug)]
pub struct SendRequestResult {
    originate_timestamp: u64,
    version: u8,
}

impl From<NtpPacket> for SendRequestResult {
    fn from(ntp_packet: NtpPacket) -> Self {
        SendRequestResult {
            originate_timestamp: ntp_packet.tx_timestamp,
            version: ntp_packet.li_vn_mode,
        }
    }
}

impl From<&NtpPacket> for SendRequestResult {
    fn from(ntp_packet: &NtpPacket) -> Self {
        SendRequestResult {
            originate_timestamp: ntp_packet.tx_timestamp,
            version: ntp_packet.li_vn_mode,
        }
    }
}

trait NtpNum {
    type Type;

    fn ntohl(&self) -> Self::Type;
}

impl NtpNum for u32 {
    type Type = u32;

    fn ntohl(&self) -> Self::Type {
        self.to_be()
    }
}

impl NtpNum for u64 {
    type Type = u64;

    fn ntohl(&self) -> Self::Type {
        self.to_be()
    }
}

struct RawNtpPacket([u8; mem::size_of::<NtpPacket>()]);

impl Default for RawNtpPacket {
    fn default() -> Self {
        RawNtpPacket([0u8; mem::size_of::<NtpPacket>()])
    }
}

impl From<RawNtpPacket> for NtpPacket {
    fn from(val: RawNtpPacket) -> Self {
        // left it here for a while, maybe in future Rust releases there
        // will be a way to use such a generic function with compile-time
        // size determination
        // const fn to_array<T: Sized>(x: &[u8]) -> [u8; mem::size_of::<T>()] {
        //     let mut temp_buf = [0u8; mem::size_of::<T>()];
        //
        //     temp_buf.copy_from_slice(x);
        //     temp_buf
        // }
        let to_array_u32 = |x: &[u8]| {
            let mut temp_buf = [0u8; mem::size_of::<u32>()];
            temp_buf.copy_from_slice(x);
            temp_buf
        };
        let to_array_u64 = |x: &[u8]| {
            let mut temp_buf = [0u8; mem::size_of::<u64>()];
            temp_buf.copy_from_slice(x);
            temp_buf
        };

        NtpPacket {
            li_vn_mode: val.0[0],
            stratum: val.0[1],
            poll: val.0[2] as i8,
            precision: val.0[3] as i8,
            root_delay: u32::from_le_bytes(to_array_u32(&val.0[4..8])),
            root_dispersion: u32::from_le_bytes(to_array_u32(&val.0[8..12])),
            ref_id: u32::from_le_bytes(to_array_u32(&val.0[12..16])),
            ref_timestamp: u64::from_le_bytes(to_array_u64(&val.0[16..24])),
            origin_timestamp: u64::from_le_bytes(to_array_u64(&val.0[24..32])),
            recv_timestamp: u64::from_le_bytes(to_array_u64(&val.0[32..40])),
            tx_timestamp: u64::from_le_bytes(to_array_u64(&val.0[40..48])),
        }
    }
}

impl From<&NtpPacket> for RawNtpPacket {
    fn from(val: &NtpPacket) -> Self {
        let mut tmp_buf = [0u8; mem::size_of::<NtpPacket>()];

        tmp_buf[0] = val.li_vn_mode;
        tmp_buf[1] = val.stratum;
        tmp_buf[2] = val.poll as u8;
        tmp_buf[3] = val.precision as u8;
        tmp_buf[4..8].copy_from_slice(&val.root_delay.to_be_bytes());
        tmp_buf[8..12].copy_from_slice(&val.root_dispersion.to_be_bytes());
        tmp_buf[12..16].copy_from_slice(&val.ref_id.to_be_bytes());
        tmp_buf[16..24].copy_from_slice(&val.ref_timestamp.to_be_bytes());
        tmp_buf[24..32].copy_from_slice(&val.origin_timestamp.to_be_bytes());
        tmp_buf[32..40].copy_from_slice(&val.recv_timestamp.to_be_bytes());
        tmp_buf[40..48].copy_from_slice(&val.tx_timestamp.to_be_bytes());

        RawNtpPacket(tmp_buf)
    }
}

fn process_response(
    send_req_result: SendRequestResult,
    resp: RawNtpPacket,
    recv_timestamp: u64,
) -> Result<NtpResult> {
    const SNTP_UNICAST: u8 = 4;
    const SNTP_BROADCAST: u8 = 5;
    const LI_MAX_VALUE: u8 = 3;
    let shifter = |val, mask, shift| (val & mask) >> shift;
    let mut packet = NtpPacket::from(resp);

    convert_from_network(&mut packet);

    if send_req_result.originate_timestamp != packet.origin_timestamp {
        return Err(Error::IncorrectOriginTimestamp);
    }
    // Shift is 0
    let mode = shifter(packet.li_vn_mode, MODE_MASK, MODE_SHIFT);
    let li = shifter(packet.li_vn_mode, LI_MASK, LI_SHIFT);
    let resp_version = shifter(packet.li_vn_mode, VERSION_MASK, VERSION_SHIFT);
    let req_version = shifter(send_req_result.version, VERSION_MASK, VERSION_SHIFT);

    if mode != SNTP_UNICAST && mode != SNTP_BROADCAST {
        return Err(Error::IncorrectMode);
    }

    if li > LI_MAX_VALUE {
        return Err(Error::IncorrectLeapIndicator);
    }

    if req_version != resp_version {
        return Err(Error::IncorrectResponseVersion);
    }

    if packet.stratum == 0 {
        return Err(Error::IncorrectStratumHeaders);
    }
    // System clock offset:
    // theta = T(B) - T(A) = 1/2 * [(T2-T1) + (T3-T4)]
    // Round-trip delay:
    // delta = T(ABA) = (T4-T1) - (T3-T2).
    // where:
    // - T1 = client's TX timestamp
    // - T2 = server's RX timestamp
    // - T3 = server's TX timestamp
    // - T4 = client's RX timestamp
    let t1 = packet.origin_timestamp;
    let t2 = packet.recv_timestamp;
    let t3 = packet.tx_timestamp;
    let t4 = recv_timestamp;
    let units = Units::Microseconds;
    let roundtrip = roundtrip_calculate(t1, t2, t3, t4, units);
    let offset = offset_calculate(t1, t2, t3, t4, units);
    let timestamp = NtpTimestamp::from(packet.tx_timestamp);

    Ok(NtpResult::new(
        timestamp.seconds as u32,
        timestamp.seconds_fraction as u32,
        roundtrip,
        offset,
    ))
}

fn convert_from_network(packet: &mut NtpPacket) {
    fn ntohl<T: NtpNum>(val: T) -> T::Type {
        val.ntohl()
    }

    packet.root_delay = ntohl(packet.root_delay);
    packet.root_dispersion = ntohl(packet.root_dispersion);
    packet.ref_id = ntohl(packet.ref_id);
    packet.ref_timestamp = ntohl(packet.ref_timestamp);
    packet.origin_timestamp = ntohl(packet.origin_timestamp);
    packet.recv_timestamp = ntohl(packet.recv_timestamp);
    packet.tx_timestamp = ntohl(packet.tx_timestamp);
}

fn convert_delays(sec: u64, fraction: u64, units: u64) -> u64 {
    sec * units + fraction * units / u32::MAX as u64
}

fn roundtrip_calculate(t1: u64, t2: u64, t3: u64, t4: u64, units: Units) -> u64 {
    let delta = t4.wrapping_sub(t1).wrapping_sub(t3.wrapping_sub(t2));
    let delta_sec = (delta & SECONDS_MASK) >> 32;
    let delta_sec_fraction = delta & SECONDS_FRAC_MASK;

    match units {
        Units::Milliseconds => convert_delays(delta_sec, delta_sec_fraction, MSEC_IN_SEC as u64),
        Units::Microseconds => convert_delays(delta_sec, delta_sec_fraction, USEC_IN_SEC as u64),
    }
}

fn offset_calculate(t1: u64, t2: u64, t3: u64, t4: u64, units: Units) -> i64 {
    let theta = (t2.wrapping_sub(t1) as i64).wrapping_add(t3.wrapping_sub(t4) as i64) / 2;
    let theta_sec = (theta.abs() as u64 & SECONDS_MASK) >> 32;
    let theta_sec_fraction = theta.abs() as u64 & SECONDS_FRAC_MASK;

    match units {
        Units::Milliseconds => {
            convert_delays(theta_sec, theta_sec_fraction, MSEC_IN_SEC as u64) as i64
                * theta.signum()
        }
        Units::Microseconds => {
            convert_delays(theta_sec, theta_sec_fraction, USEC_IN_SEC as u64) as i64
                * theta.signum()
        }
    }
}

fn get_ntp_timestamp(unix_time: &UnixTimeStamp) -> u64 {
    let timestamp = ((unix_time.secs + (u64::from(NtpPacket::NTP_TIMESTAMP_DELTA))) << 32)
        + u64::from(unix_time.micro_secs as u64 * u32::MAX as u64 / USEC_IN_SEC as u64);

    timestamp
}
