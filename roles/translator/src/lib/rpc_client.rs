use stratum_common::bitcoin::{blockdata::transaction::Transaction, consensus::Decodable};
use jsonrpc::{error::Error as JsonRpcError, Client as JosnRpcClient};
use serde::{Serialize, Deserialize};
use stratum_common::bitcoin;
use std::{io, io::Error};
/// Iterator over a hex-encoded string slice which decodes hex and yields bytes.
pub struct HexIterator<'a> {
    /// The `Bytes` iterator whose next two bytes will be decoded to yield
    /// the next byte.
    iter: std::str::Bytes<'a>,
}

impl<'a> HexIterator<'a> {
    /// Constructs a new `HexIterator` from a string slice.
    ///
    /// # Errors
    ///
    /// If the input string is of odd length.
    pub fn new(s: &'a str) -> Result<HexIterator<'a>, Error> {
        if s.len() % 2 != 0 {
            panic!("the length must be even!");
            //Err(Error::OddLengthString(s.len()))
        } else {
            Ok(HexIterator { iter: s.bytes() })
        }
    }
}
impl<'a> Iterator for HexIterator<'a> {
    type Item = Result<u8, HexError>;

    fn next(&mut self) -> Option<Result<u8, HexError>> {
        let hi = self.iter.next()?;
        let lo = self.iter.next().unwrap();
        Some(chars_to_hex(hi, lo))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (min, max) = self.iter.size_hint();
        (min / 2, max.map(|x| x / 2))
    }
}

impl<'a> io::Read for HexIterator<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut bytes_read = 0usize;
        for dst in buf {
            match self.next() {
                Some(Ok(src)) => {
                    *dst = src;
                    bytes_read += 1;
                }
                _ => break,
            }
        }
        Ok(bytes_read)
    }
}

fn chars_to_hex(hi: u8, lo: u8) -> Result<u8, HexError> {
    let hih = (hi as char).to_digit(16).ok_or(HexError::InvalidChar(hi))?;
    let loh = (lo as char).to_digit(16).ok_or(HexError::InvalidChar(lo))?;

    let ret = (hih << 4) + loh;
    Ok(ret as u8)
}

pub enum HexError {
    /// Non-hexadecimal character.
    InvalidChar(u8),
    // Purported hex string had odd length.
    //OddLengthString(usize),
    // Tried to parse fixed-length hash from a string with the wrong type (expected, got).
    //InvalidLength(usize, usize),
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hash([u8; 32]);

#[derive(Clone, Deserialize)]
pub struct Amount(f64);

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockHash(Hash);

#[derive(Clone, Debug)]
pub enum Auth {
    //None,
    UserPass(String, String),
    //CookieFile(PathBuf),
}

impl Auth {
    pub fn get_user_pass(self) -> (Option<String>, Option<String>) {
        match self {
            Auth::UserPass(u, p) => (Some(u), Some(p)),
        }
    }
}

pub struct RpcClient {
    client: JosnRpcClient, //jsonrpc::client::Client,
}

impl RpcClient {
    pub fn new(url: &str, auth: Auth) -> Result<Self, BitcoincoreRpcError> {
        let (user, pass) = auth.get_user_pass();
        jsonrpc::client::Client::simple_http(url, user, pass)
            .map(|client| RpcClient { client })
            .map_err(|e| BitcoincoreRpcError::JsonRpc(e.into()))
    }
    pub fn submit_block(
        &self,
        submit_block: String,
    ) -> Result<Option<String>, BitcoincoreRpcError> {
        self.call(
            "submitblock",
            &[serde_json::to_value(submit_block).unwrap()],
        )
    }
}

pub trait RpcApi: Sized {
    /// Call a `cmd` rpc with given `args` list
    fn call<T: for<'a> serde::de::Deserialize<'a>>(
        &self,
        cmd: &str,
        args: &[serde_json::Value],
    ) -> Result<T, BitcoincoreRpcError>;

    fn get_raw_mempool_verbose(&self) -> Result<Vec<String>, BitcoincoreRpcError> {
        self.call("getrawmempool", &[])
    }

    fn get_raw_transaction(
        &self,
        txid: &String,
        block_hash: Option<&BlockHash>,
    ) -> Result<Transaction, JsonRpcError> {
        let mut args = [
            into_json(txid)?,
            into_json(false)?,
            opt_into_json(block_hash)?,
        ];
        let hex: String = self
            .call(
                "getrawtransaction",
                handle_defaults(&mut args, &[serde_json::Value::Null]),
            )
            .map_err(|_| JsonRpcError::EmptyBatch)?;
        let mut reader =
            HexIterator::new(&hex).unwrap_or_else(|_| panic!("Can not decode hex  {}", hex));
        let object = Decodable::consensus_decode(&mut reader).expect("Can not decode transaction");
        Ok(object)
    }
}

/// Shorthand for converting a variable into a serde_json::Value.
fn into_json<T>(val: T) -> Result<serde_json::Value, JsonRpcError>
where
    T: serde::ser::Serialize,
{
    Ok(serde_json::to_value(val)?)
}

/// Shorthand for converting an Option into an Option<serde_json::Value>.
fn opt_into_json<T>(opt: Option<T>) -> Result<serde_json::Value, JsonRpcError>
where
    T: serde::ser::Serialize,
{
    match opt {
        Some(val) => Ok(into_json(val)?),
        None => Ok(serde_json::Value::Null),
    }
}

impl RpcApi for RpcClient {
    /// Call an `cmd` rpc with given `args` list
    fn call<T: for<'a> serde::de::Deserialize<'a>>(
        &self,
        cmd: &str,
        args: &[serde_json::Value],
    ) -> RResult<T> {
        let raw_args: Vec<_> = args
            .iter()
            .map(|a| {
                let json_string = serde_json::to_string(a)?;
                serde_json::value::RawValue::from_string(json_string) // we can't use to_raw_value here due to compat with Rust 1.29
            })
            .map(|a| a.map_err(BitcoincoreRpcError::Json))
            .collect::<RResult<Vec<_>>>()?;
        let req = self.client.build_request(cmd, &raw_args);

        let resp = self.client.send_request(req).map_err(JsonRpcError::from);
        Ok(resp?.result()?)
    }
}

pub type RResult<T> = Result<T, BitcoincoreRpcError>;

/// The error type for errors produced in this library.
#[derive(Debug)]
pub enum BitcoincoreRpcError {
    JsonRpc(jsonrpc::error::Error),
    //Hex(hex::Error),
    Json(serde_json::error::Error),
    //BitcoinSerialization(bitcoin::consensus::encode::Error),
    //Secp256k1(secp256k1::Error),
    //Io(io::Error),
    //InvalidAmount(bitcoin::util::amount::ParseAmountError),
    //InvalidCookieFile,
    // The JSON result had an unexpected structure.
    //UnexpectedStructure,
    // The daemon returned an error string.
    //ReturnedError(String),
}

impl From<jsonrpc::error::Error> for BitcoincoreRpcError {
    fn from(e: jsonrpc::error::Error) -> BitcoincoreRpcError {
        BitcoincoreRpcError::JsonRpc(e)
    }
}

/// Handle default values in the argument list
///
/// Substitute `Value::Null`s with corresponding values from `defaults` table,
/// except when they are trailing, in which case just skip them altogether
/// in returned list.
///
/// Note, that `defaults` corresponds to the last elements of `args`.
///
/// ```norust
/// arg1 arg2 arg3 arg4
///           def1 def2
/// ```
///
/// Elements of `args` without corresponding `defaults` value, won't
/// be substituted, because they are required.
fn handle_defaults<'a>(
    args: &'a mut [serde_json::Value],
    defaults: &[serde_json::Value],
) -> &'a [serde_json::Value] {
    assert!(args.len() >= defaults.len());

    // Pass over the optional arguments in backwards order, filling in defaults after the first
    // non-null optional argument has been observed.
    let mut first_non_null_optional_idx = None;
    for i in 0..defaults.len() {
        let args_i = args.len() - 1 - i;
        let defaults_i = defaults.len() - 1 - i;
        if args[args_i] == serde_json::Value::Null {
            if first_non_null_optional_idx.is_some() {
                if defaults[defaults_i] == serde_json::Value::Null {
                    panic!("Missing `default` for argument idx {}", args_i);
                }
                args[args_i] = defaults[defaults_i].clone();
            }
        } else if first_non_null_optional_idx.is_none() {
            first_non_null_optional_idx = Some(args_i);
        }
    }

    let required_num = args.len() - defaults.len();

    if let Some(i) = first_non_null_optional_idx {
        &args[..i + 1]
    } else {
        &args[..required_num]
    }
}

// #[derive(Deserialize)]
// pub struct GetMempoolEntryResultFees {
//     /// Transaction fee in BTC
//     //#[serde(with = "bitcoin::amount::serde::as_btc")]
//     pub base: Amount,
//     /// Transaction fee with fee deltas used for mining priority in BTC
//     //#[serde(with = "bitcoin::amount::serde::as_btc")]
//     pub modified: Amount,
//     /// Modified fees (see above) of in-mempool ancestors (including this one) in BTC
//     //#[serde(with = "bitcoin::amount::serde::as_btc")]
//     pub ancestor: Amount,
//     /// Modified fees (see above) of in-mempool descendants (including this one) in BTC
//     //#[serde(with = "bitcoin::amount::serde::as_btc")]
//     pub descendant: Amount,
// }
