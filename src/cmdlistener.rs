
#![allow(dead_code)]

use crate::imports::*;

//use std::os::unix::prelude;
use std::os::unix::io::AsRawFd;

pub use std::os::unix::net::UnixStream;
use std::os::unix::net::UnixListener;
use uds::UnixStreamExt;
//use uds::UnixListenerExt;
use pwd::Passwd;

//use serde_json::ser::Serializer;
//use serde_json::de::{IoRead,StreamDeserializer};

const SOCKET_PATH : &str = "command.socket"; // xxx

pub struct CommandListener {
  listener : UnixListener,
}

type CSWrite = BufWriter<UnixStream>;

struct CommandStream {
  euid : Result<u32, anyhow::Error>,
  read : io::Lines<BufReader<UnixStream>>,
  write : CSWrite,
}

type CSE = anyhow::Error;

impl CommandStream {
  #[throws(CSE)]
  pub fn mainloop(mut self) {
    for l in &mut self.read {
      let l = l.context("read")?;
      decode_and_process(&l, &mut self.write)?;
      write!(&mut self.write, "\n")?;
      self.write.flush()?;
    }
  }
}

impl From<serde_lexpr::Error> for MgmtError {
  fn from(je: serde_lexpr::Error) -> ME {
    ParseFailed(format!("{}", &je))
  }
}

use MgmtCommand::*;
use MgmtResponse::*;
use MgmtError::*;

type ME = MgmtError;

#[throws(CSE)]
pub fn decode_and_process(s: &str, w: &mut CSWrite) {
  let resp = self::decode_process_inner(s)
    .unwrap_or_else(|e| MgmtResponse::Error(format!("{}", e)));
  serde_lexpr::to_writer(w, &resp)?;
}

#[throws(ME)]
fn decode_process_inner(s: &str)-> MgmtResponse {
  let cmd : MgmtCommand = serde_lexpr::from_str(s)?;
  execute(cmd)?
}

#[throws(ME)]
fn execute(cmd: MgmtCommand) -> MgmtResponse {
  match cmd {
    Noop { } => Fine { },
  }
}

impl CommandListener {
  #[throws(StartupError)]
  pub fn new() -> Self {
    let path = SOCKET_PATH;
    match fs::remove_file(path) {
      Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
      r => r,
    }
    .with_context(|| format!("remove socket {:?} before we bind", &path))?;
    let listener = UnixListener::bind(path)
      .with_context(|| format!("bind command socket {:?}", &path))?;
    CommandListener { listener }
  }

  #[throws(StartupError)]
  pub fn spawn(mut self) {
    thread::spawn(move ||{
      loop {
        self.accept_one().unwrap_or_else(
          |e| eprintln!("accept/spawn failed: {:?}", e)
        );
      }
    })
  }

  #[throws(CSE)]
  fn accept_one(&mut self) {
    let (conn, _caller) = self.listener.accept().context("accept")?;
    let mut desc = format!("{:>5}", conn.as_raw_fd());
    eprintln!("command connection {}: accepted", &desc);
    thread::spawn(move||{
      match (||{
        let euid = conn.initial_peer_credentials()
          .context("initial_peer_credentials")
          .map(|creds| creds.euid());

        #[derive(Error,Debug)]
        struct EuidLookupError(String);
        display_as_debug!{EuidLookupError}
        impl From<&E> for EuidLookupError where E : Display {
          fn from(e: &E) -> Self { EuidLookupError(format!("{}",e)) }
        }

        let user_desc : String = (||{
          let euid = *(euid.as_ref()?);
          let pwent = Passwd::from_uid(euid);
          let show_username =
            pwent.map_or_else(|| format!("<euid {}>", euid),
                              |p| p.name);
          <Result<String,EuidLookupError>>::Ok(show_username)
        })().unwrap_or_else(|e| format!("<error: {}>", e.0));
        write!(&mut desc, " user={}", user_desc)?;

        let read = conn.try_clone().context("dup the command stream")?;
        let read = BufReader::new(read);
        let read = read.lines();
        let write = conn;
        let write = BufWriter::new(write);

        let cs = CommandStream { read, write, euid };
        cs.mainloop()?;
        
        <Result<_,StartupError>>::Ok(())
      })() {
        Ok(()) => eprintln!("command connection {}: disconnected", &desc),
        Err(e) => eprintln!("command connection {}: error: {:?}", &desc, e),
      }
    });
  }
}
