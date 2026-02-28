mod rpc;
mod host_types;
use crate::rpc::{LispClient};
use crate::host_types::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = LispClient::new("127.0.0.1:55000").await?;

    // Calling 'echo'
    let msg: String = client.call(RemoteMethod::Echo {
        message: "Refactored!".into()
    }).await?;
    println!("Lisp said: {}", msg);

    // Calling 'echo'
    let rsp: String = client.call(RemoteMethod::Connect {
        address : Address::new(1, 0xC1234567890A),
    }).await?;
    println!("Lisp said: {:?}", rsp);

    Ok(())
}
