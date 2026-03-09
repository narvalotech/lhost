mod host_types;
mod rpc;
use crate::host_types::*;
use crate::rpc::LispClient;

slint::include_modules!();
use slint::{ModelRc, Model, SharedString, StandardListViewItem, VecModel};
use std::rc::Rc;
use std::str::FromStr;

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

fn ui_update_devices(devices: &Vec<DeviceData>, ui_handle: &slint::Weak<AppWindow>) {
    let devs = devices.clone();

    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let device_list = Rc::new(VecModel::from(devs));
            ui.set_all_devices(device_list.clone().into());
        })
        .unwrap();
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

fn get_current_row(ui_handle: &slint::Weak<AppWindow>) -> Option<Address> {
    if let Some(ui) = ui_handle.upgrade() {
        let current_row: i32 = ui.get_selected_device().try_into().unwrap();
        if current_row < 0 {
            return None;
        }

        let device_rows = ui.get_scan_results();
        let the_model = device_rows.as_any().downcast_ref::<VecModel<ModelRc<StandardListViewItem>>>()
            .expect("Wrong row type");
        if let Some(data) = the_model.row_data(current_row as usize) {
            let the_model = data.as_any().downcast_ref::<VecModel<StandardListViewItem>>().unwrap();
            if let Some( StandardListViewItem { text, .. }) = the_model.row_data(0) {
                if let Ok(address) = Address::from_str(text.as_str()) {
                    return Some(address);
                }
            }
        }
    }
    None
}

async fn backend_event_task(cancel: CancellationToken, ui_handle: slint::Weak<AppWindow>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    println!("startin events");

    let devices_: Box<Vec<ScanResult>> = Box::new(Vec::new());
    let devices = Box::leak(devices_); // mom can we have 'static

    let conns_: Box<Vec<DeviceData>> = Box::new(Vec::new());
    let conns = Box::leak(conns_);

    while let Some(evt) = tokio::select! {
        _ = cancel.cancelled() => {
            println!("cancelled");
            None
        }
        Ok(evt) = client.call::<RemoteEvent>(RemoteMethod::GetEvent) => {Some(evt)}
    } {
        println!("Got event: {:?}", evt);
        match evt {
            RemoteEvent::ScanResult(res) => {
                devices.push(res);
                ui_update_scan_results(devices, &ui_handle);
            }
            RemoteEvent::ConnComplete(res) => {
                println!("Got event: {:?}", res);
                let res = res.address;
                let c = DeviceData { address: res.to_string().into(), text: "hello".into() };
                conns.push(c);
                ui_update_devices(conns, &ui_handle);
            }
        }
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
    rx_chan: mpsc::Receiver<RemoteMethod>,
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
    let ui_handle_in_cb = ui_handle.clone();
    ui.on_button(move |id| {
        println!("CALLBACK: {:?}", id);
        match id {
            Command::Connect => {
                if let Some(address) = get_current_row(&ui_handle_in_cb) {
                    let cmd = RemoteMethod::Connect { address };
                    tx_chan.blocking_send(cmd).unwrap();
                }
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
