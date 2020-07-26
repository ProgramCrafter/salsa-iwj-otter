
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

#[derive(Debug,Error,Clone)]
#[error("connection euid lookup failed (at connection initiation): {0}")]
pub struct ConnectionEuidDiscoverEerror(String);

struct CommandStream<'d> {
  euid : Result<u32, ConnectionEuidDiscoverEerror>,
  read : io::Lines<BufReader<UnixStream>>,
  write : CSWrite,
  scope : Option<ManagementScope>,
  desc : &'d str,
}

type CSE = anyhow::Error;

impl CommandStream<'_> {
  #[throws(CSE)]
  pub fn mainloop(mut self) {
    while let Some(l) = self.read.next() {
      let l = l.context("read")?;
      decode_and_process(&mut self, &l)?;
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
fn decode_and_process(cs: &mut CommandStream, s: &str) {
  let resp = self::decode_process_inner(cs, s)
    .unwrap_or_else(|e| MgmtResponse::Error(format!("{}", e)));
  serde_lexpr::to_writer(&mut cs.write, &resp)?;
}

#[throws(ME)]
fn decode_process_inner(cs: &mut CommandStream, s: &str)-> MgmtResponse {
  let cmd : MgmtCommand = serde_lexpr::from_str(s)?;
  execute(cs, cmd)?
}

const USERLIST : &str = "/etc/userlist";

impl CommandStream<'_> {
  #[throws(AuthorisationError)]
  fn authorised_uid(&self, wanted: Option<uid_t>, xinfo: Option<&str>)
                    -> Authorised<(Passwd,uid_t),> {
    let client_euid = *self.euid.as_ref().map_err(|e| e.clone())?;
    let server_euid = unsafe { libc::getuid() };
    if client_euid == 0 ||
       client_euid == server_euid ||
       Some(client_euid) == wanted
    {
      return Authorised::authorise();
    }
    Err(anyhow!("{}: euid mismatch: client={:?} server={:?} wanted={:?}{}",
                &self.desc, client_euid, server_euid, wanted,
                xinfo.unwrap_or("")))?
  }

  fn map_auth_err(&self, ae: AuthorisationError) -> MgmtError {
    eprintln!("command connection {}: authorisation error: {}",
              self.desc, ae.0);
    return MgmtError::AuthorisationError;
  }
}

#[throws(AuthorisationError)]
fn authorise_scope(cs: &CommandStream, wanted: &ManagementScope)
                   -> AuthorisedSatisfactory {
  type AS<T> = (T, ManagementScope);
//  fn AS<T>(a:T, s:ManagementScope) -> AuthorisedScope<T>
//  { AuthorisedScope(a,s) }
  use fs::File;

  match &wanted {
    ManagementScope::XXX => {
      let y : AS<
        Authorised<(Passwd,uid_t)>,
      > = {
        let ok = cs.authorised_uid(None,None)?;
        (ok,
         ManagementScope::XXX)
      };
      return y.into()
    },
    ManagementScope::Unix { user: wanted } => {
      let y : AS<
        Authorised<(Passwd,uid_t)>,
      > = {
        struct AuthorisedIf { authorised_for : Option<uid_t> };

        let pwent = Passwd::from_name(&wanted)
          .map_err(
            |e| anyhow!("looking up requested username {:?}: {:?}",
                        &wanted, &e)
          )?
          .ok_or_else(
            || AuthorisationError(format!(
              "requested username {:?} not found", &wanted
            ))
          )?;

        let (in_userlist, xinfo) = (||{ <Result<(_,Option<String>),anyhow::Error>>::Ok({
          let allowed = BufReader::new(match File::open(USERLIST) {
            Err(e) if e.kind() == ErrorKind::NotFound => {
              return Ok((
                AuthorisedIf{ authorised_for: None },
                Some(format!(" user list {} does not exist", USERLIST))
              ))
            },
            r => r,            
          }?);
          allowed
            .lines()
            .filter_map(|le| match le {
              Ok(l) if l.trim() == wanted => Some(
                Ok((
                  AuthorisedIf{ authorised_for: Some(pwent.uid) },
                  None
                ))
              ),
              Ok(_) => None,
              Err(e) => Some(<Result<_,anyhow::Error>>::Err(e.into())),
            })
            .next()
            .unwrap_or_else(
              || Ok((
                AuthorisedIf{ authorised_for: None },
                Some(format!(" requested username {:?} not in {}",
                             &wanted, USERLIST)),
              ))
            )?
        })})()?;

        let AuthorisedIf{ authorised_for } = in_userlist;
        let info = xinfo.as_ref().map(|s| s.as_str());
        let ok = cs.authorised_uid(authorised_for, info)?;
        (ok,
         ManagementScope::Unix { user: pwent.name })
      };
      y.into()
    }
  }
}

#[throws(ME)]
fn execute(cs: &mut CommandStream, cmd: MgmtCommand) -> MgmtResponse {
//  use MgmtError::*;

  match cmd {
    Noop { } => Fine { },

    SetScope(wanted_scope) => {
      let authorised : AuthorisedSatisfactory =
        authorise_scope(cs, &wanted_scope)
        .map_err(|e| cs.map_auth_err(e))
        ?;
      cs.scope = Some(authorised.into_inner());
      Fine { }
    }
/*
    CreateGame(game) => {
      
    },
    AddPiece(game, { pos,count,name,info }) => {
      let game = cs.lookup_game(&game)?;
      let count = spec.count.unwrap_or(1);
      let pos = spec.ok_or(XXU("missing piece pos"))?;
      let _xxx_name = spec.name;
      let pc = info.load()?;
      
    }
    }, // xxx*/
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
          .map(|creds| creds.euid())
          .map_err(|e| ConnectionEuidDiscoverEerror(format!("{}", e)));

        #[derive(Error,Debug)]
        struct EuidLookupError(String);
        display_as_debug!{EuidLookupError}
        impl From<&E> for EuidLookupError where E : Display {
          fn from(e: &E) -> Self { EuidLookupError(format!("{}",e)) }
        }

        let user_desc : String = (||{
          let euid = euid.clone()?;
          let pwent = Passwd::from_uid(euid);
          let show_username =
            pwent.map_or_else(|| format!("<euid {}>", euid),
                              |p| p.name);
          <Result<String,anyhow::Error>>::Ok(show_username)
        })().unwrap_or_else(|e| format!("<error: {}>", e));
        write!(&mut desc, " user={}", user_desc)?;

        let read = conn.try_clone().context("dup the command stream")?;
        let read = BufReader::new(read);
        let read = read.lines();
        let write = conn;
        let write = BufWriter::new(write);

        let cs = CommandStream {
          scope: None, desc: &desc,
          read, write, euid,
        };
        cs.mainloop()?;
        
        <Result<_,StartupError>>::Ok(())
      })() {
        Ok(()) => eprintln!("command connection {}: disconnected", &desc),
        Err(e) => eprintln!("command connection {}: error: {:?}", &desc, e),
      }
    });
  }
}

use authproofs::*;
use authproofs::AuthorisationError;

mod authproofs {
  use crate::imports::*;

  #[derive(Error,Debug)]
  #[error("internal AuthorisationError {0}")]
  pub struct AuthorisationError(pub String);

  pub struct Authorised<A> (PhantomData<A>);
  //struct AuthorisedScope<A> (Authorised<A>, ManagementScope);
  pub struct AuthorisedSatisfactory (ManagementScope);

  impl AuthorisedSatisfactory {
    pub fn into_inner(self) -> ManagementScope { self.0 }
  }

  impl<T> Authorised<T> {
    pub fn authorise() -> Authorised<T> { Authorised(PhantomData) }
  }

  impl<T> From<(Authorised<T>, ManagementScope)> for AuthorisedSatisfactory {
    fn from((_,s): (Authorised<T>, ManagementScope)) -> Self { Self(s) }
  }
  impl<T,U> From<((Authorised<T>, Authorised<U>), ManagementScope)> for AuthorisedSatisfactory {
    fn from(((..),s): ((Authorised<T>, Authorised<U>), ManagementScope)) -> Self { Self(s) }
  }

  impl From<anyhow::Error> for AuthorisationError {
    fn from(a: anyhow::Error) -> AuthorisationError {
      AuthorisationError(format!("{}",a))
    }
  }
  impl From<ConnectionEuidDiscoverEerror> for AuthorisationError {
    fn from(e: ConnectionEuidDiscoverEerror) -> AuthorisationError {
      AuthorisationError(format!("{}",e))
    }
  }
}
