use std::{
    error::Error,
    io::{Read, Write},
    net::{TcpListener, TcpStream}, fmt::Debug,
};

use serde::Deserialize;
use tokio::sync::mpsc;
use tracing_subscriber::fmt::format;

#[derive(Deserialize)]
pub struct ProxyRequest {
    txid: String,
    client_upstream: String,
    duration: u32,
}

fn run_http(notify: mpsc::Sender<ProxyRequest>) {
    let listener = TcpListener::bind("0.0.0.0:8080").unwrap();
    while let Ok((stream, addr)) = listener.accept() {
        let mut req = handle_client(stream);
    }
}

fn get_next_line(stream: &mut TcpStream) -> String {
    let mut line = String::new();
    loop {
        let mut buf = [0; 1];
        let _ = stream.read(&mut buf);
        for ch in buf {
            if ch == 10 {
                return line;
            }
            line.push(unsafe { char::from_u32_unchecked(buf[0] as u32) });
        }
    }
}

fn get_content(stream: &mut TcpStream, size: usize) -> Vec<u8> {
    let mut content = Vec::with_capacity(size as usize);
    content.resize(size, 0);
    stream.read_exact(&mut content).unwrap();
    content
}

fn write_ans(stream: &mut TcpStream, err: Option<Box<dyn Debug>>) {
    let ans = if let Some(error) = err {
        let error = format!("{error:?}");
        format!("HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\nerror: {}\n", error.len(), error)
    } else {
        String::from("HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nOK\n")
    };
    stream.write_all(&ans.as_bytes()).unwrap();
}

fn handle_client(mut stream: TcpStream) -> Result<ProxyRequest, ()> {
    let mut content_len = 0;
    loop {
        let line = get_next_line(&mut stream);
        let value: Vec<&str> = line.split(":").collect();
        if line.len() == 1 {
            let content = get_content(&mut stream, content_len);
            println!("{}", String::from_utf8(content.clone()).unwrap());
            match serde_json::from_slice(&content) {
                Ok(content) => {
                    write_ans(&mut stream, None);
                    return Ok(content);
                }
                Err(e) => {
                    write_ans(&mut stream, Some(Box::new(e)));
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
