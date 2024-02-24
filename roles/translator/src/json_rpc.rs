use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    sync::mpsc,
};

use crate::lib::proxy_request::ProxyRequest;

pub async fn run_http_server(notify: mpsc::Sender<ProxyRequest>) {
    let listener = TcpListener::bind("0.0.0.0:8080").await.unwrap();
    while let Ok((stream, addr)) = listener.accept().await {
        let Ok(req) = handle_client(stream).await else {
            continue;
        };
        notify.send(req).await.unwrap();
    }
}

async fn get_next_line(stream: &mut TcpStream) -> String {
    let mut line = String::new();
    loop {
        let mut buf = [0; 1];
        let _ = stream.read(&mut buf).await;
        for ch in buf {
            if ch == 10 {
                return line;
            }
            line.push(unsafe { char::from_u32_unchecked(buf[0] as u32) });
        }
    }
}

async fn get_content(stream: &mut TcpStream, size: usize) -> Vec<u8> {
    let mut content = Vec::with_capacity(size as usize);
    content.resize(size, 0);
    stream.read_exact(&mut content).await.unwrap();
    content
}

async fn write_ans(stream: &mut TcpStream, err: Option<String>) {
    let ans = if let Some(error) = err {
        format!(
            "HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\nerror: {}\n",
            error.len(),
            error
        )
    } else {
        String::from("HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nOK\n")
    };
    stream.write_all(&ans.as_bytes()).await.unwrap();
}

async fn handle_client(mut stream: TcpStream) -> Result<ProxyRequest, ()> {
    let mut content_len = 0;
    loop {
        let line = get_next_line(&mut stream).await;
        let value: Vec<&str> = line.split(":").collect();
        if line.len() == 1 {
            let content = get_content(&mut stream, content_len).await;
            println!("{}", String::from_utf8(content.clone()).unwrap());
            match serde_json::from_slice(&content) {
                Ok(content) => {
                    write_ans(&mut stream, None).await;
                    return Ok(content);
                }
                Err(e) => {
                    write_ans(&mut stream, Some(format!("{e}"))).await;
                    return Err(());
                }
            }
        }

        if value.len() == 1 {
            continue;
        }

        let key = value[0];
        let value = value[1];
        if key == "Content-Length" {
            content_len = value[1..].trim().parse::<usize>().unwrap();
        }
    }
}
