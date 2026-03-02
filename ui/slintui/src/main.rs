mod rpc;
mod host_types;
use crate::rpc::{LispClient};
use crate::host_types::*;

slint::include_modules!();
use slint::{ModelRc, SharedString, StandardListViewItem, VecModel};
use std::rc::Rc;

use tokio::task;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

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
    let (to_tokio_tx, mut to_tokio_rx) = mpsc::channel(10);

    let res = task::spawn_blocking(|| {
        ui_main(to_tokio_tx);
    });

    let token = CancellationToken::new();
    let cloned_token = token.clone();

    let events_tid = task::spawn(async move {
        let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

        println!("startin events");

        while let Some(evt) =
        tokio::select! {
            _ = cloned_token.cancelled() => {
                println!("cancelled");
                None
            }
            Ok(evt) = client.call::<String>(RemoteMethod::GetEvent) => {Some(evt)}
        } {
            println!("Got event: {:?}", evt);
        }

        println!("quitting events");
    });

    let cmds_tid = task::spawn(async move {
        let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

        while let Some(res) = to_tokio_rx.recv().await {
            println!("JRPC THREAD: {:?}", res);
            match res {
                RemoteMethod::Connect { address: _ } => {
                    let rsp: String = client.call(res).await.unwrap();
                    println!("Lisp response {:?}", rsp);
                },
                _ => {},
            }
        }
        token.cancel();
    });

    tokio::join!(events_tid, cmds_tid);

    println!("Exiting..");

    Ok(())
}
