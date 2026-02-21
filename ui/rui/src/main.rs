use iced::event::{self, Event};
use iced::keyboard;
use iced::keyboard::key;
use iced::keyboard::key::Key::Named;

use iced::{Center, Subscription};
use iced::widget::{Row, button, row, column};
use iced;

// --------------------------
use serde::{Serialize, Deserialize};

use tokio::net::TcpStream;
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc;

struct State {
    tx: mpsc::Sender<KeyEvent>,
    _rt: tokio::runtime::Runtime,
}

#[derive(Clone, Serialize, Deserialize, Debug, Eq, PartialEq)]
enum KeyEvent {
    Unknown,
    KeyDown,
    KeyUp,
    KeyLeft,
    KeyRight,
    KeySelect,
    KeyBack,
    KeyPower,
}

#[derive(Clone, Debug)]
enum UIEvent {
    Key(KeyEvent),
    Raw(Event)
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct InputEvent {
    key: KeyEvent,
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
        event::listen().map(UIEvent::Raw)
    }

    fn update(&mut self, message: UIEvent) {
        let data = match message {
            UIEvent::Key(code) => code,
            UIEvent::Raw(event) => match event {
                Event::Keyboard(keyboard::Event::KeyPressed{key: Named(code), ..}) => {
                    let converted = match code {
                        key::Named::ArrowDown => KeyEvent::KeyDown,
                        key::Named::ArrowUp => KeyEvent::KeyUp,
                        key::Named::ArrowLeft => KeyEvent::KeyLeft,
                        key::Named::ArrowRight => KeyEvent::KeyRight,
                        key::Named::Enter => KeyEvent::KeySelect,
                        key::Named::Backspace => KeyEvent::KeyBack,
                        key::Named::Escape => KeyEvent::KeyPower,
                        _ => {
                            KeyEvent::Unknown
                        }
                    };
                    // println!("key: {:?}", event);
                    // self.update(UIEvent::Key(converted))
                    converted
                }
                _ => {KeyEvent::Unknown},
            },
        };
        if data != KeyEvent::Unknown {
            println!("send: {:?}", data);
            self.tx.blocking_send(data).unwrap();
        }
    }

    fn view(&self) -> Row<'_, UIEvent> {
        row![
            // D-pad
            button("<").on_press(UIEvent::Key(KeyEvent::KeyLeft)),
            column![
                button("/\\").on_press(UIEvent::Key(KeyEvent::KeyUp)),
                button("\\/").on_press(UIEvent::Key(KeyEvent::KeyDown)),
            ].spacing(10).align_x(Center),
            button(">").on_press(UIEvent::Key(KeyEvent::KeyRight)),

            // Enter/Back
            column![
                button("SELECT").on_press(UIEvent::Key(KeyEvent::KeySelect)),
                button(" BACK").on_press(UIEvent::Key(KeyEvent::KeyBack)),
            ].spacing(10).align_x(Center),
        ]
        .spacing(10)
        .padding(20)
        .align_y(Center)
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

async fn write_event_to_stream(stream: &mut TcpStream, event: InputEvent) {
    // let output: Vec<u8> = to_allocvec_cobs(&event).unwrap();

    // stream.write_all(&output).await.unwrap();

    println!("written");
    // let decoded: InputEvent = from_bytes_cobs(&mut output.clone()).unwrap();
    // println!("Written: {:?}", decoded);
}

async fn tcp_client(mut rx: mpsc::Receiver<KeyEvent>) {
    let mut stream = TcpStream::connect("127.0.0.1:9999").await.unwrap();

    loop {
        if let Some(keycode) = rx.recv().await {
            let e = InputEvent {
                key: keycode
            };
            write_event_to_stream(&mut stream, e).await;
        }
    }
}

pub fn main() -> iced::Result {
    let settings = iced::window::Settings {
        size: iced::Size{width: 240.0, height: 120.0},
        resizable: false,
        ..Default::default()
    };

    // iced::run(State::update, State::view)
    iced::application(State::default, State::update, State::view)
        .window(settings)
        .subscription(State::subscription)
        .run()
}
