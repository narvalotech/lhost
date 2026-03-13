mod host_types;
mod rpc;
use crate::host_types::*;
use crate::rpc::LispClient;

slint::include_modules!();
use slint::{ModelRc, Model, SharedString, StandardListViewItem, FilterModel, VecModel};
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

fn ui_add_scan_result(ui_handle: &slint::Weak<AppWindow>, item: ScanResult) {
    // TODO
    // make to and from row converters
    let item = item.clone();
    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let device_rows = ui.get_scan_results_storage();
            let the_model = device_rows.as_any().downcast_ref::<VecModel<ModelRc<StandardListViewItem>>>()
                .expect("Wrong row type");
            the_model.push(scan_result_to_row(&item));
        })
        .unwrap();
}

fn get_current_row(ui_handle: &slint::Weak<AppWindow>) -> Option<Address> {
    let ui = ui_handle.upgrade()?;

    let selected_index: i32 = ui.get_selected_device();
    if selected_index < 0 {
        return None;
    }

    let model = ui.get_scan_results();

    if let Some(row) = model.row_data(selected_index as usize) {
        if let Some(col_item) = row.row_data(0) {
            if let Ok(address) = Address::from_str(col_item.text.as_str()) {
                return Some(address);
            }
        }
    }

    None
}

async fn backend_event_task(cancel: CancellationToken, ui_handle: slint::Weak<AppWindow>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    println!("startin events");

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
                ui_add_scan_result(&ui_handle, res);
            }
            RemoteEvent::ConnComplete(res) => {
                let a = res.address;
                let c = DeviceData { address: a.to_string().into(), conn: res.conn_handle as i32, text: "hello".into() };
                conns.push(c);
                ui_update_devices(conns, &ui_handle);
            }
            RemoteEvent::Disconnected(res) => {
                let conn = res.conn_handle;
                let _ = conns.extract_if(.., |c| c.conn as u16 == conn).collect::<Vec<_>>();
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
            RemoteMethod::Disconnect { conn: _ } => {
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

    let events_ui_handle = ui_handle.clone();
    let events_token = token.clone();
    let events_tid = task::spawn(backend_event_task(events_token, events_ui_handle));

    let cmds_token = token.clone();
    let cmds_tid = task::spawn(backend_cmd_task(cmds_token, rx_chan));

    let log_ui_handle = ui_handle.clone();
    let log_token = token.clone();
    let log_tid = tokio::spawn(async move {
        let mut muxer = linemux::MuxedLines::new().expect("Failed to initialize Muxer");

        if let Err(e) = muxer.add_file("frontend.log").await {
            eprintln!("Log file doesn't exist: {}", e);
            return;
        }

        loop {
            tokio::select! {
                _ = log_token.cancelled() => break,
                Ok(Some(line)) = muxer.next_line() => {
                    let log_line = format!("{}\n", line.line());
                    let _ = log_ui_handle.upgrade_in_event_loop(move |ui| {
                        let mut current = ui.get_log_text().to_string();
                        current.push_str(&log_line);
                        ui.set_log_text(current.into());
                        ui.invoke_log_go_to_bottom();
                    });
                }
            }
        }
    });

    tokio::join!(events_tid, cmds_tid, log_tid);

    println!("Exiting..");

    Ok(())
}

fn main() {
    let ui = AppWindow::new().unwrap();

    let (tx_chan, rx_chan) = mpsc::channel(10);

    // Set up button commands
    let ui_handle = ui.as_weak();
    ui.on_button(move |id| {
        println!("CALLBACK: {:?}", id);
        match id {
            Command::Connect => {
                if let Some(address) = get_current_row(&ui_handle) {
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

    let device_rows = Rc::new(VecModel::<ModelRc<StandardListViewItem>>::default());
    ui.set_scan_results_storage(ModelRc::from(device_rows.clone()));
    ui.set_scan_results(device_rows.clone().into());

    let ui_handle = ui.as_weak();
    let base_model_handle = device_rows.clone();
    ui.on_filter_scan_results(move |filter| {
        let col_index = filter.column;
        let ascending = filter.ascending;
        let text = filter.text;

        let ui = ui_handle.unwrap();
        let base_model = base_model_handle.clone();

        let filtered_model = FilterModel::new(base_model, move |row| {
            let filter = text.as_str();
            let address = row.row_data(0).map(|i| i.text).unwrap_or_default();
            let name = row.row_data(2).map(|i| i.text).unwrap_or_default();
            address.contains(filter) || name.contains(filter)
        });

        let sorted_model = filtered_model.sort_by(move |row_a, row_b| {
            let val_a = row_a.row_data(col_index as usize).map(|i| i.text).unwrap_or_default();
            let val_b = row_b.row_data(col_index as usize).map(|i| i.text).unwrap_or_default();

            let ord = if col_index == 1 {
                let n_a: i32 = val_a.parse().unwrap_or(0);
                let n_b: i32 = val_b.parse().unwrap_or(0);
                n_a.cmp(&n_b)
            } else {
                val_a.cmp(&val_b)
            };

            if ascending { ord } else { ord.reverse() }
        });

        ui.set_scan_results(ModelRc::from(Rc::new(sorted_model)));
    });

    let ui_handle = ui.as_weak();
    let slint_future = async_main(rx_chan, ui_handle);
    slint::spawn_local(async_compat::Compat::new(slint_future)).unwrap();

    ui.run().unwrap();
}

// Road to feature-parity
// - scan
//   - [x] scanned device view
//   - [ ] display merged AD
//   - [x] sort by rssi / name
//   - [x] filter addr/name
// - connect
//   - [-] log view (gatt operations)
//     - [ ] use proper logger and log to file
//   - [ ] gatt listview
//   - [x] add/delete tab on connected/disconnected events
//   - [ ] encrypt / bond management
// - gatt
//   - [ ] discovery
//   - [ ] read/write
//   - [ ] notify
