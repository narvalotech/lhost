use tokio::net::TcpStream;
use tokio::io::{AsyncReadExt, AsyncBufReadExt, AsyncWriteExt, BufReader};
use serde_json::{json, Value};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Connect to the Lisp server
    let stream = TcpStream::connect("127.0.0.1:55000").await?;
    let (reader, mut writer) = stream.into_split();
    let mut reader = BufReader::new(reader);

    // 2. Construct the JSON-RPC Request
    let request = json!({
        "jsonrpc": "2.0",
        "method": "echo",
        "params": { "message": "Hello from Rust!" },
        "id": 1
    });

    let payload = request.to_string();

    // 3. Format with Content-Length header (The "LSP" format Lisp expects)
    // Note: \r\n is standard for these headers.
    let message = format!("Content-Length: {}\r\n\r\n{}", payload.len(), payload);

    writer.write_all(message.as_bytes()).await?;
    writer.flush().await?;
    println!("Sent: {}", payload);

    // 4. Read the response headers
    let mut content_length = 0;
    let mut line = String::new();

    // Read headers until we hit the empty line (\r\n\r\n)
    loop {
        line.clear();
        reader.read_line(&mut line).await?;
        if line == "\r\n" || line == "\n" {
            break;
        }
        if line.to_lowercase().starts_with("content-length:") {
            content_length = line
                .split(':')
                .nth(1)
                .unwrap_or("0")
                .trim()
                .parse::<usize>()?;
        }
    }

    // 5. Read exactly the amount of bytes specified in Content-Length
    if content_length > 0 {
        let mut buffer = vec![0u8; content_length];
        reader.read_exact(&mut buffer).await?;

        let response: Value = serde_json::from_slice(&buffer)?;

        // Handle Lisp's "echo" response
        // Note: Your Lisp code uses (gethash "message" args),
        // but the 'echo' method usually returns the input or a result field.
        println!("Full Response: {:?}", response);
        if let Some(res) = response.get("result") {
            println!("Result: {:?}", res);
        }
    }

    Ok(())
}
