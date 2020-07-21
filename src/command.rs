
#![allow(dead_code)]

use crate::imports::*;

//use std::os::unix::prelude;

pub use std::os::unix::net::UnixStream;
use std::os::unix::net::UnixListener;
use uds::UnixStreamExt;
//use uds::UnixListenerExt;
use pwd::Passwd;

const SOCKET_PATH : &str = "command.socket"; // xxx

pub struct CommandListener {
  listener : UnixListener,
}

struct CommandStream {
  conn : UnixStream,
  euid : Result<u32, anyhow::Error>,
}

type CSE = anyhow::Error;

impl CommandStream {
  #[throws(CSE)]
  pub fn mainloop(&mut self) {
  }
}

impl CommandListener {
  #[throws(StartupError)]
  pub fn new() -> Self {
    let listener = UnixListener::bind(SOCKET_PATH)?;
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
    let (conn, caller) = self.listener.accept().context("accept")?;
    let mut desc = format!("conn={:?} peer={:?}", &conn, &caller);
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

        let mut cs = CommandStream { conn, euid };
        cs.mainloop()?;
        
        <Result<_,StartupError>>::Ok(())
      })() {
        Ok(()) => eprintln!("command connection {}: disconnected", &desc),
        Err(e) => eprintln!("command connection {}: error: {:?}", &desc, e),
      }
    });
  }
}
