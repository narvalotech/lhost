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

use std::fs::OpenOptions;
use tracing::{info, error, Level};

fn scan_result_to_row(device: &ScanResult) -> ModelRc<StandardListViewItem> {
    let addr = format!("{}", device.address);
    let data = format!("{:?}", device.decoded);
    let row_data = vec![
        StandardListViewItem::from(SharedString::from(&addr)),
        StandardListViewItem::from(SharedString::from(device.rssi.to_string())),
        StandardListViewItem::from(SharedString::from(&device.name)),
        StandardListViewItem::from(SharedString::from(&data)),
    ];

    ModelRc::from(Rc::new(VecModel::from(row_data)))
}

fn device_to_device_data(device: &PeerDevice) -> DeviceData { // wow what a bad name
    let attributes = Rc::new(VecModel::<ModelRc<StandardListViewItem>>::default());

    for att in device.gatt.attributes.clone() {
        let handle = format!("{:04X}", att.handle);
        let attt = format!("{:?}", att.att_type);
        let uu = if att.uuid128 != 0 { att.uuid128 } else { att.uuid16 as u128 };
        let uuid = format!("{:X}", uu);

        attributes.push(
            ModelRc::from(Rc::new(VecModel::from(
            vec![
                StandardListViewItem::from(SharedString::from(&handle)),
                StandardListViewItem::from(SharedString::from(&attt)),
                StandardListViewItem::from(SharedString::from(&uuid)),
                StandardListViewItem::from(SharedString::from("")),
            ]))));
    }

    DeviceData {
        address: SharedString::from(format!("{}", device.address)),
        conn: device.conn_handle as i32,
        gatt: ModelRc::from(attributes),
    }
}

fn ui_update_devices(devices: &Vec<PeerDevice>, ui_handle: &slint::Weak<AppWindow>) {
    let devs = devices.clone();

    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let device_list = ui.get_all_devices();
            let the_model = device_list.as_any().downcast_ref::<VecModel<DeviceData>>().expect("Wrong type");
            the_model.clear();
            for dev in devs {
                the_model.push(device_to_device_data(&dev));
            }
        })
        .unwrap();
}

fn ui_update_gatt_server(own_gatt: &PeerDevice, ui_handle: &slint::Weak<AppWindow>) {
    let gatt = own_gatt.clone();

    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let gatt_server = ui.get_gatt_server();
            ui.set_gatt_server(device_to_device_data(&gatt));
        })
        .unwrap();
}

fn ui_update_scan_results(ui_handle: &slint::Weak<AppWindow>, evt: ScanResults) {
    // TODO
    // make to and from row converters
    let evt = evt.clone();
    ui_handle
        .upgrade_in_event_loop(move |ui| {
            let device_rows = ui.get_scan_results_storage();
            let the_model = device_rows.as_any().downcast_ref::<VecModel<ModelRc<StandardListViewItem>>>()
                .expect("Wrong row type");
            the_model.clear();
            for result in evt.results {
                the_model.push(scan_result_to_row(&result));
            }
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

fn get_focused_conn_handle(ui_handle: &slint::Weak<AppWindow>) -> Option<u16> {
    let ui = ui_handle.upgrade()?;

    let selected_index: i32 = ui.get_active_tab() - 1;
    if selected_index < 0 {
        return None;
    }

    let device_list = ui.get_all_devices();
    let the_model = device_list.as_any().downcast_ref::<VecModel<DeviceData>>().expect("Wrong type");

    if let Some(device_data) = the_model.row_data(selected_index as usize) {
        Some(device_data.conn as u16)
    } else {
        None
    }
}

// 1. get connected-evt -> build PeerDevice
// 2. get discovered-evt -> build GattTable
//   -> rebuild peerdevice tab w/ gatt & replace
// 3. ...
// 4. profit!
async fn backend_event_task(cancel: CancellationToken, ui_handle: slint::Weak<AppWindow>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    info!("startin events");

    let conns_: Box<Vec<PeerDevice>> = Box::new(Vec::new());
    let conns = Box::leak(conns_);

    while let Some(evt) = tokio::select! {
        _ = cancel.cancelled() => {
            info!("cancelled");
            None
        }
        Ok(evt) = client.call::<RemoteEvent>(RemoteMethod::GetEvent) => {Some(evt)}
    } {
        info!("Got event: {:?}", evt);
        match evt {
            RemoteEvent::ScanResults(res) => {
                ui_update_scan_results(&ui_handle, res);
            }
            RemoteEvent::ConnComplete(res) => {
                let a = res.address;
                let c = PeerDevice::new(a, res.conn_handle, GattTable::default());
                conns.push(c);
                ui_update_devices(conns, &ui_handle);
            }
            RemoteEvent::Discovered(res) => {
                // TODO: better way?
                let conn = res.conn_handle;
                let prev = conns.extract_if(.., |c| c.conn_handle as u16 == conn).collect::<Vec<_>>();
                if prev.len() > 0 {
                    let mut edited = prev[0].clone();
                    edited.gatt = res.gatt;
                    conns.push(edited);
                    ui_update_devices(conns, &ui_handle);
                }
            }
            RemoteEvent::ServerDiscovered(res) => {
                ui_update_gatt_server(&res, &ui_handle);
            }
            RemoteEvent::Disconnected(res) => {
                let conn = res.conn_handle;
                let _ = conns.extract_if(.., |c| c.conn_handle as u16 == conn).collect::<Vec<_>>();
                ui_update_devices(conns, &ui_handle);
            }
        }
    }

    info!("quitting events");
}

async fn backend_cmd_task(cancel: CancellationToken, mut rx_chan: mpsc::Receiver<RemoteCommand>) {
    let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();

    while let Some(res) = rx_chan.recv().await {
        info!("JRPC THREAD: {:?}", res);
        match res {
            RemoteCommand::StartScan => {
                let rsp: String = client.call(RemoteMethod::Command(res)).await.unwrap();
                info!("Lisp response {:?}", rsp);
            }
            RemoteCommand::StopScan => {
                let rsp: String = client.call(RemoteMethod::Command(res)).await.unwrap();
                info!("Lisp response {:?}", rsp);
            }
            RemoteCommand::Connect { address: _ } => {
                let rsp: String = client.call(RemoteMethod::Command(res)).await.unwrap();
                info!("Lisp response {:?}", rsp);
            }
            RemoteCommand::Disconnect { conn: _ } => {
                let rsp: String = client.call(RemoteMethod::Command(res)).await.unwrap();
                info!("Lisp response {:?}", rsp);
            }
            _ => {}
        }
    }

    cancel.cancel();
}

async fn async_main(
    rx_chan: mpsc::Receiver<RemoteCommand>,
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
            error!("Log file doesn't exist: {}", e);
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

                        let lines_vec: Vec<&str> = current.lines().collect();
                        let truncation = 10; // textview in debug builds are sssloowwww
                        if lines_vec.len() > truncation {
                            let truncated = lines_vec[lines_vec.len() - truncation..].join("\n");
                            ui.set_log_text((truncated + "\n").into());
                        } else {
                            ui.set_log_text(current.into());
                        }

                        ui.invoke_log_go_to_bottom();
                    });
                }
            }
        }
    });

    tokio::join!(events_tid, cmds_tid, log_tid);

    info!("Exiting..");

    Ok(())
}

fn start_server(start: bool) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let command = if start { RemoteCommand::Open } else { RemoteCommand::Close };
    rt.block_on(async {
        info!("{} server", if start { "spawning" } else { "killing" });
        let mut client = LispClient::new("127.0.0.1:55000").await.unwrap();
        let rsp: String = client.call(RemoteMethod::Command(command)).await.unwrap();
        info!("Lisp response {:?}", rsp);
    });
}

fn main() {
    let file = OpenOptions::new().append(true).create(true).open("frontend.log").unwrap();
    let (non_blocking, _guard) = tracing_appender::non_blocking(file);

    tracing_subscriber::fmt()
        .with_writer(non_blocking)
        .with_ansi(false)
        .with_max_level(Level::INFO)
        .init();

    info!("App startup");

    let ui = AppWindow::new().unwrap();

    let (tx_chan, rx_chan) = mpsc::channel(10);

    // Set up button commands
    let ui_handle = ui.as_weak();
    ui.on_button(move |id| {
        info!("CALLBACK: {:?}", id);
        match id {
            Command::StartScan => {
                let cmd = RemoteCommand::StartScan;
                tx_chan.blocking_send(cmd).unwrap();
            }
            Command::StopScan => {
                let cmd = RemoteCommand::StopScan;
                tx_chan.blocking_send(cmd).unwrap();
            }
            Command::Connect => {
                if let Some(address) = get_current_row(&ui_handle) {
                    let cmd = RemoteCommand::Connect { address };
                    tx_chan.blocking_send(cmd).unwrap();
                }
            }
            Command::Disconnect => {
                if let Some(conn_handle) = get_focused_conn_handle(&ui_handle) {
                    info!("Disconnecting handle {}", conn_handle);
                    let cmd = RemoteCommand::Disconnect { conn: conn_handle };
                    tx_chan.blocking_send(cmd).unwrap();
                } else {
                    error!("Unable to determine conn handle");
                }
            }
            _ => {
                info!("UNHANDLED");
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

    let all_devices = Rc::new(VecModel::<DeviceData>::default());
    ui.set_all_devices(all_devices.clone().into());

    let gatt_server = DeviceData::default();
    ui.set_gatt_server(gatt_server);

    start_server(true);

    let ui_handle = ui.as_weak();
    let slint_future = async_main(rx_chan, ui_handle);
    slint::spawn_local(async_compat::Compat::new(slint_future)).unwrap();

    ui.run().unwrap();

    start_server(false);
}

// Road to feature-parity
// - scan
//   - [x] scanned device view
//   - [x] display merged AD
//   - [x] sort by rssi / name
//   - [x] filter addr/name
//   - [x] deduplicate (in jrpc)
// - connect
//   - [x] log view (gatt operations)
//   - [x] gatt listview
//     - later: use tree https://github.com/slint-ui/slint/discussions/1042
//   - [x] add/delete tab on connected/disconnected events
//   - [ ] encrypt / bond management
//     - needs window menus
// - gatt
//   - [x] show own table
//   - [x] discovery
//   - [ ] read/write
//   - [ ] notify
// - misc
//   - [x] real jsonrpc server
//   - [ ] keyboard shortcuts
