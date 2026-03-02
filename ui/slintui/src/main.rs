mod rpc;
mod host_types;
use crate::rpc::{LispClient};
use crate::host_types::*;

slint::include_modules!();
use slint::{ModelRc, SharedString, StandardListViewItem, VecModel};
use std::rc::Rc;

use tokio::task;
use tokio::sync::mpsc;

#[derive(Clone)]
struct Device {
    address: Address,
    rssi: i32,
    name: String,
    data: String,
    _private_field: u32,
}

fn create_row_from_device(device: &Device) -> ModelRc<StandardListViewItem> {
    let addr = format!("{}", device.address);
    let row_data = vec![
        StandardListViewItem::from(SharedString::from(&addr)),
        StandardListViewItem::from(SharedString::from(device.rssi.to_string())),
        StandardListViewItem::from(SharedString::from(&device.name)),
        StandardListViewItem::from(SharedString::from(&device.data)),
    ];

    ModelRc::from(Rc::new(VecModel::from(row_data)))
}

fn ui_main(tx_chan: mpsc::Sender<RemoteMethod>) {
    let ui = AppWindow::new().unwrap();

    let ui_handle = ui.as_weak();
    let devices_: Box<Vec<Device>> = Box::new(Vec::new());
    let devices = Box::leak(devices_);

    // Set up button commands
    ui.on_button(move |id| {
        println!("CALLBACK: {:?}", id);
        match id {
            Command::Connect => {
                let address = Address::new(1, 0xC1234567890A);
                let cmd = RemoteMethod::Connect {address} ;
                tx_chan.blocking_send(cmd).unwrap();
            },
            Command::Disconnect => {
                let address = Address::new(1, 0xC1234567890A);
                let cmd = RemoteMethod::Disconnect {conn: 1};
                tx_chan.blocking_send(cmd).unwrap();
            },
            _ => {
                println!("UNHANDLED");
            },
        }
    });

    ui.run().unwrap();
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = LispClient::new("127.0.0.1:55000").await?;

    let msg: String = client.call(RemoteMethod::Echo {
        message: "Refactored!".into()
    }).await?;
    println!("Lisp said: {}", msg);

    let rsp: String = client.call(RemoteMethod::Connect {
        address : Address::new(1, 0xC1234567890A),
    }).await?;
    println!("Lisp said: {:?}", rsp);

    let (to_tokio_tx, mut to_tokio_rx) = mpsc::channel(10);

    let res = task::spawn_blocking(|| {
        ui_main(to_tokio_tx);
    });

    while let Some(res) = to_tokio_rx.recv().await {
        println!("JRPC THREAD: {:?}", res);
    }

    println!("Exiting..");

    Ok(())
}
