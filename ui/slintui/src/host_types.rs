use derive_more::Constructor;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

#[derive(Serialize, Deserialize, Debug)]
pub struct ATTPacket {
    repr: String,
    op: u8,
    handle: u16,
    data: Vec<u8>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AttOperation {
    conn_handle: u16,
    op: u8,
    data: Vec<u8>,
}

// TODO can json support rust enums?
#[derive(Deserialize, Debug)]
pub struct Attribute {
    handle: u16,
    att_type: u8,
    uuid16: u16,
    uuid128: u128,
}

#[derive(Deserialize, Debug)]
pub struct GattTable {
    attributes: Vec<Attribute>,
}

#[derive(Serialize, Deserialize, Constructor, Clone, Debug)]
pub struct Address {
    address_type: u8,
    address: u64,
}

#[derive(Deserialize, Clone, Debug)]
pub struct ScanResult {
    pub address: Address,
    pub rssi: i8,
    pub name: String,
    pub data: Vec<u8>,
    pub decoded: String,
}

#[derive(Deserialize, Debug)]
pub struct ScanResults {
    results: Vec<ScanResult>,
}

#[derive(Deserialize, Debug)]
pub struct ConnComplete {
    pub conn_handle: u16,
    pub address: Address,
}

#[derive(Deserialize, Debug)]
pub struct Disconnected {
    pub conn_handle: u16,
}

#[derive(Deserialize, Debug)]
pub struct EncryptionChange {
    status: u8,
    conn_handle: u16,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "snake_case")]
pub enum RemoteEvent {
    ScanResult(ScanResult),
    ConnComplete(ConnComplete),
    Disconnected(Disconnected),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "method", content = "params")]
#[serde(rename_all = "snake_case")]
pub enum RemoteMethod {
    GetEvent,
    Connect { address: Address },
    Disconnect { conn: u16 },
}

// ------ Impl ------

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bytes = self.address.to_be_bytes();

        write!(
            f,
            "{:02X}:{:02X}:{:02X}:{:02X}:{:02X}:{:02X} ({:02X})",
            bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7], self.address_type,
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseAddressError {
    InvalidFormat,
    ParseIntError,
}

impl FromStr for Address {
    type Err = ParseAddressError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // 1. Split the string into the MAC part and the Type part
        // Format: "XX:XX:XX:XX:XX:XX (YY)"
        let parts: Vec<&str> = s.split(|c| c == ' ' || c == '(' || c == ')').filter(|s| !s.is_empty()).collect();

        if parts.len() != 2 {
            return Err(ParseAddressError::InvalidFormat);
        }

        // 2. Parse the 6 hex bytes
        let hex_digits: Vec<&str> = parts[0].split(':').collect();
        if hex_digits.len() != 6 {
            return Err(ParseAddressError::InvalidFormat);
        }

        let mut address_bytes = [0u8; 8];
        for (i, hex) in hex_digits.iter().enumerate() {
            address_bytes[i + 2] = u8::from_str_radix(hex, 16)
                .map_err(|_| ParseAddressError::ParseIntError)?;
        }

        // 3. Convert bytes back to u64 (Big Endian as per your Display impl)
        let address = u64::from_be_bytes(address_bytes);

        // 4. Parse the address_type
        let address_type = u8::from_str_radix(parts[1], 16)
            .map_err(|_| ParseAddressError::ParseIntError)?;

        Ok(Address { address, address_type })
    }
}
