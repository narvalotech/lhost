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
use async_compat;

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

fn main() {
    let ui = AppWindow::new().unwrap();
    let ui_handle = ui.as_weak();

    let (tx_chan, rx_chan) = mpsc::channel(10);

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

    let slint_future = async_main(rx_chan, ui_handle);
    slint::spawn_local(async_compat::Compat::new(slint_future)).unwrap();

    ui.run().unwrap();
}

async fn backend_event_task(cancel: CancellationToken, ui_handle: slint::Weak<AppWindow>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    println!("startin events");

    let devices_: Box<Vec<Device>> = Box::new(Vec::new());
    let devices = Box::leak(devices_); // mom can we have 'static

    while let Some(evt) =
        tokio::select! {
            _ = cancel.cancelled() => {
                println!("cancelled");
                None
            }
            Ok(evt) = client.call::<ScanResult>(RemoteMethod::GetEvent) => {Some(evt)}
        } {
            println!("Got event: {:?}", evt);
        }

    println!("quitting events");
}

async fn backend_cmd_task(cancel: CancellationToken, mut rx_chan: mpsc::Receiver<RemoteMethod>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    while let Some(res) = rx_chan.recv().await {
        println!("JRPC THREAD: {:?}", res);
        match res {
            RemoteMethod::Connect { address: _ } => {
                let rsp: String = client.call(res).await.unwrap();
                println!("Lisp response {:?}", rsp);
            },
            _ => {},
        }
    }
    cancel.cancel();
}

async fn async_main(mut rx_chan: mpsc::Receiver<RemoteMethod>, ui_handle: slint::Weak<AppWindow>) -> Result<(), Box<dyn std::error::Error>> {
    let token = CancellationToken::new();
    let cloned_token = token.clone();

    let events_tid = task::spawn(backend_event_task(cloned_token, ui_handle));
    let cmds_tid = task::spawn(backend_cmd_task(token, rx_chan));

    tokio::join!(events_tid, cmds_tid);

    println!("Exiting..");

    Ok(())
}
