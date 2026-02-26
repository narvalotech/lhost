use tokio::net::TcpStream;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "method", content = "params")]
#[serde(rename_all = "snake_case")]
pub enum RemoteMethod {
    Echo { message: String },
    GetUser { id: u64 },
}

// Use this for complex data coming from Lisp
#[derive(Deserialize, Debug)]
pub struct UserProfile {
    pub id: u64,
    pub name: String,
    pub roles: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: u64,
    #[serde(flatten)]
    pub method: RemoteMethod,
}

#[derive(Deserialize, Debug)]
pub struct JsonRpcResponse<T> {
    pub result: Option<T>,
    pub error: Option<Value>,
    pub id: u64,
}

struct LispClient {
    reader: BufReader<tokio::net::tcp::OwnedReadHalf>,
    writer: tokio::net::tcp::OwnedWriteHalf,
    next_id: u64,
}

impl LispClient {
    async fn new(addr: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let stream = TcpStream::connect(addr).await?;
        let (read_half, write_half) = stream.into_split();
        Ok(Self {
            reader: BufReader::new(read_half),
            writer: write_half,
            next_id: 1,
        })
    }

    async fn call<T: for<'de> Deserialize<'de>>(
        &mut self,
        method: RemoteMethod
    ) -> Result<T, Box<dyn std::error::Error>>
    where T: for<'de> Deserialize<'de>{
        let id = self.next_id;
        self.next_id += 1;

        // 1. Prepare JSON Body
        let json_body = serde_json::to_string(&JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id,
            method,
        })?;

        // 2. Wrap in HTTP for the Common Lisp 'jsonrpc' lib
        let http_request = format!(
            "POST / HTTP/1.1\r\n\
             Content-Type: application/json\r\n\
             Content-Length: {}\r\n\
             Connection: keep-alive\r\n\
             \r\n\
             {}",
            json_body.len(),
            json_body
        );

        self.writer.write_all(http_request.as_bytes()).await?;
        self.writer.flush().await?;

        // 3. Parse Response Headers for Content-Length
        let mut content_length = 0;
        loop {
            let mut line = String::new();
            self.reader.read_line(&mut line).await?;
            let trimmed = line.trim();
            if trimmed.is_empty() { break; } // End of headers

            if trimmed.to_lowercase().starts_with("content-length:") {
                if let Some(val) = trimmed.split(':').nth(1) {
                    content_length = val.trim().parse::<usize>()?;
                }
            }
        }

        // 4. Read exact bytes for body (No newline required)
        let mut buffer = vec![0u8; content_length];
        self.reader.read_exact(&mut buffer).await?;

        let response: JsonRpcResponse<T> = serde_json::from_slice(&buffer)?;

        if let Some(error) = response.error {
            return Err(format!("Lisp Error: {:?}", error).into());
        }

        response.result.ok_or_else(|| "Missing result".into())
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = LispClient::new("127.0.0.1:55000").await?;

    // Calling 'echo'
    let msg: String = client.call(RemoteMethod::Echo {
        message: "Refactored!".into()
    }).await?;
    println!("Lisp said: {}", msg);

    // Calling 'echo'
    let rsp: UserProfile = client.call(RemoteMethod::GetUser {
        id: 1337
    }).await?;
    println!("Lisp said: {:?}", rsp);

    Ok(())
}
