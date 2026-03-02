use serde::{Deserialize, Serialize};
use derive_more::{Constructor};
use std::fmt;

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

#[derive(Deserialize, Debug)]
pub struct ScanResult {
    address: Address,
    rssi: i8,
    name: String,
    data: Vec<u8>,
    decoded: String,
}

#[derive(Deserialize, Debug)]
pub struct ScanResults {
    results: Vec<ScanResult>,
}

#[derive(Deserialize, Debug)]
pub struct ConnComplete {
    conn_handle: u16,
    address: Address,
}

#[derive(Deserialize, Debug)]
pub struct EncryptionChange {
    status: u8,
    conn_handle: u16,
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
