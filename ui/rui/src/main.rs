use iced::event::{self, Event};
use iced::keyboard;
use iced::keyboard::key;
use iced::keyboard::key::Key::Named;

use iced;
use iced::widget::{Row, button, column, container, row};
use iced::{Center, Element, Fill, Left, Subscription};

// --------------------------
use serde::{Deserialize, Serialize};

use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tokio::sync::mpsc;

struct State {
    tx: mpsc::Sender<UICommand>,
    _rt: tokio::runtime::Runtime,
}

#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
enum UICommand {
    Unknown,
    StartScan,
    StopScan,
    Connect,
    Disconnect,
}

#[derive(Clone, Debug)]
enum UIEvent {
    Command(UICommand),
    Iced(Event),
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct HostEvent {
    command: UICommand,
}

use tokio::runtime::Builder;

impl State {
    pub fn new() -> Self {
        let runtime = Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();

        let (send, recv) = mpsc::channel(16);

        let _handle = runtime.spawn(tcp_client(recv));

        State {
            tx: send,
            _rt: runtime,
        }
    }

    fn subscription(&self) -> Subscription<UIEvent> {
        event::listen().map(UIEvent::Iced)
    }

    fn update(&mut self, message: UIEvent) {
        let data = match message {
            UIEvent::Command(code) => code,
            UIEvent::Iced(event) => match event {
                Event::Keyboard(keyboard::Event::KeyPressed {
                    key: Named(code), ..
                }) => {
                    let converted = match code {
                        // key::Named::ArrowDown => UICommand::KeyDown,
                        // key::Named::ArrowUp => UICommand::KeyUp,
                        // key::Named::ArrowLeft => UICommand::KeyLeft,
                        // key::Named::ArrowRight => UICommand::KeyRight,
                        // key::Named::Enter => UICommand::KeySelect,
                        // key::Named::Backspace => UICommand::KeyBack,
                        // key::Named::Escape => UICommand::KeyPower,
                        _ => UICommand::Unknown,
                    };
                    // println!("key: {:?}", event);
                    // self.update(UIEvent::Key(converted))
                    converted
                }
                _ => UICommand::Unknown,
            },
        };
        if data != UICommand::Unknown {
            println!("send: {:?}", data);
            self.tx.blocking_send(data).unwrap();
        }
    }

    fn view(&self) -> Element<'_, UIEvent> {
        container(
            column![
                button("Start scan").on_press(UIEvent::Command(UICommand::StartScan)),
                button("Stop scan").on_press(UIEvent::Command(UICommand::StopScan)),
                button("Connect").on_press(UIEvent::Command(UICommand::Connect)),
                button("Disconnect").on_press(UIEvent::Command(UICommand::Disconnect)),
            ]
            .spacing(10)
            .padding(20)
            .align_x(Left),
        )
        .padding(10)
        // .center_x(Fill)
        // .center_y(Fill)
        .into()
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

async fn write_event_to_stream(stream: &mut TcpStream, event: HostEvent) {
    // let output: Vec<u8> = to_allocvec_cobs(&event).unwrap();

    // stream.write_all(&output).await.unwrap();

    println!("written {:?}", event);

    // let decoded: HostEvent = from_bytes_cobs(&mut output.clone()).unwrap();
    // println!("Written: {:?}", decoded);
}

async fn tcp_client(mut rx: mpsc::Receiver<UICommand>) {
    let mut stream = TcpStream::connect("127.0.0.1:9999").await.unwrap();

    loop {
        if let Some(keycode) = rx.recv().await {
            let e = HostEvent { command: keycode };
            write_event_to_stream(&mut stream, e).await;
        }
    }
}

pub fn main() -> iced::Result {
    let settings = iced::window::Settings {
        // size: iced::Size{width: 240.0, height: 120.0},
        resizable: false,
        ..Default::default()
    };

    // iced::run(State::update, State::view)
    iced::application(State::default, State::update, State::view)
        .window(settings)
        .subscription(State::subscription)
        .run()
}
