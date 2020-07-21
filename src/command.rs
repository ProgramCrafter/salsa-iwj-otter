
use crate::imports::*;

use std::os::unix::prelude;

pub use std::os::unix::net::UnixStream;

use std::os::unix::net::UnixListener;
use uds::UnixListenerExt;
use pwd::Passwd;

const SOCKET_PATH : &str = "command.socket"; // xxx

struct CommandListener {
  listener : UnixListener;
}

struct CommandStream {
  conn : UnixStream,
  euid : Result<u32, anyhow::Error>,
}

impl CommandListener {
  #[throws(OE)]
  fn new() -> Self {
    let listener = UnixListener::bind(SOCKET_PATH)?;
    CommandListener { listener }
  }
  #[throws(OE)]
  fn process_one() -> Self {
    let (conn, caller) = self.listener.accept()?;
    let desc = format!("conn={:?} peer={:?}", &client, &caller);
    eprintln!("command connection {}: accepted", client);
    thread::spawn(move||{
      (||{
        let euid = conn.initial_peer_credentials()
          .map_err(|e| anyhow!("initial_peer_credentials: {:?}", e))
          .and_then(|creds| creds.euid().ok_or_else(
            || Err(anyhow!("initial_peer_credentials no euid!"))));
        write!(&mut desc, " user={}", (||{
          let pwent = Passwd::from_uid(euid?)
            .map_err(|e| format!("euid {} lookup failed {}",uid,e))?;
          pwent.map_or_else(|p| p.name,
                            || format!("<euid {}>", uid))
        })().ok_or_else(|e| format!("<error: {}>", &e)));

        thread::Builder::new()
          .name(desc.clone())
          .spawn().context
        match euid
          .and_then(
            |uid| 
              .map_err(|e| anyhow!("<euid {:?} lookup failed {}",
                                   uid, e))
              .and_then(|pwent| pwent.ok_or_else(
                
                    
        {
          Err(e) => 
          Ok(u) => {
            let pwent = Passwd::from_uid(euid);
            
        }
                                                    
        
        let credentials_euid =
          cred

 match &credentials {
          Ok(cred) {
            match cred.euid() {
              None => 
              Some(e) => Ok(e),
              

          Ok(
        };
      })().ok_or(|e|{
        xxx
      });
    });
  }
  
  #[throws(OE)]
  fn process() -> Self {n
    loop {
      
  }
}
