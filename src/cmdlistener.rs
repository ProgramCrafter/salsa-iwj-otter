
#![allow(dead_code)]

use crate::imports::*;

//use std::os::unix::prelude;
use std::os::unix::io::AsRawFd;

pub use std::os::unix::net::UnixStream;
use std::os::unix::net::UnixListener;
use uds::UnixStreamExt;
//use uds::UnixListenerExt;
use pwd::Passwd;

use serde_json::ser::Serializer;
use serde_json::de::{IoRead,Deserializer,StreamDeserializer};

const SOCKET_PATH : &str = "command.socket"; // xxx

pub struct CommandListener {
  listener : UnixListener,
}

struct CommandStream<'de> {
  euid : Result<u32, anyhow::Error>,
  read : StreamDeserializer<'de,IoRead<BufReader<UnixStream>>,
                            MgmtCommand>,
  write : UnixStream,
}

type CSE = anyhow::Error;

impl CommandStream<'_> {
  pub fn mainloop(self) -> Result<(),CSE> {
    loop {
      
    }
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
        let write = conn;
        let read = BufReader::new(read);
        let read = IoRead::new(read);
        let read = StreamDeserializer::new(read);

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
