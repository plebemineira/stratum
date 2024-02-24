use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct ProxyRequest {
    pub txid: String,
    pub client_upstream: String,
    pub duration: u32,
}

