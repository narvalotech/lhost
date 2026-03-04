mod host_types;
mod rpc;
use crate::host_types::*;
use crate::rpc::LispClient;

slint::include_modules!();
use slint::{ModelRc, SharedString, StandardListViewItem, VecModel};
use std::rc::Rc;

use async_compat;
use tokio::sync::mpsc;
use tokio::task;
use tokio_util::sync::CancellationToken;

fn scan_result_to_row(device: &ScanResult) -> ModelRc<StandardListViewItem> {
    let addr = format!("{}", device.address);
    let data = format!("{:?}", device.data);
    let row_data = vec![
        StandardListViewItem::from(SharedString::from(&addr)),
        StandardListViewItem::from(SharedString::from(device.rssi.to_string())),
        StandardListViewItem::from(SharedString::from(&device.name)),
        StandardListViewItem::from(SharedString::from(&data)),
    ];

    ModelRc::from(Rc::new(VecModel::from(row_data)))
}

fn ui_update_scan_results(devices: &Vec<ScanResult>, ui_handle: &slint::Weak<AppWindow>) {
    let devs = devices.clone();

    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let device_rows = Rc::new(VecModel::<ModelRc<StandardListViewItem>>::default());
            ui.set_scan_results(ModelRc::from(device_rows.clone()));

            let device_rows_copy = device_rows.clone();
            for device in devs {
                let row = scan_result_to_row(&device);
                device_rows_copy.push(row);
            }
        })
        .unwrap();
}

async fn backend_event_task(cancel: CancellationToken, ui_handle: slint::Weak<AppWindow>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    println!("startin events");

    let devices_: Box<Vec<ScanResult>> = Box::new(Vec::new());
    let devices = Box::leak(devices_); // mom can we have 'static

    while let Some(evt) = tokio::select! {
        _ = cancel.cancelled() => {
            println!("cancelled");
            None
        }
        Ok(evt) = client.call::<ScanResult>(RemoteMethod::GetEvent) => {Some(evt)}
    } {
        println!("Got event: {:?}", evt);
        devices.push(evt);
        ui_update_scan_results(devices, &ui_handle);
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
            }
            _ => {}
        }
    }
    cancel.cancel();
}

async fn async_main(
    mut rx_chan: mpsc::Receiver<RemoteMethod>,
    ui_handle: slint::Weak<AppWindow>,
) -> Result<(), Box<dyn std::error::Error>> {
    let token = CancellationToken::new();
    let cloned_token = token.clone();

    let events_tid = task::spawn(backend_event_task(cloned_token, ui_handle));
    let cmds_tid = task::spawn(backend_cmd_task(token, rx_chan));

    tokio::join!(events_tid, cmds_tid);

    println!("Exiting..");

    Ok(())
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
                let cmd = RemoteMethod::Connect { address };
                tx_chan.blocking_send(cmd).unwrap();
            }
            Command::Disconnect => {
                let cmd = RemoteMethod::Disconnect { conn: 1 };
                tx_chan.blocking_send(cmd).unwrap();
            }
            _ => {
                println!("UNHANDLED");
            }
        }
    });

    let slint_future = async_main(rx_chan, ui_handle);
    slint::spawn_local(async_compat::Compat::new(slint_future)).unwrap();

    ui.run().unwrap();
}
