
#![allow(dead_code)]

pub use crate::from_instance_lock_error;

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

pub const SOCKET_PATH : &str = "command.socket"; // xxx

pub struct CommandListener {
  listener : UnixListener,
}

#[derive(Debug,Error,Clone)]
#[error("connection euid lookup failed (at connection initiation): {0}")]
pub struct ConnectionEuidDiscoverEerror(String);

struct CommandStream<'d> {
  euid : Result<u32, ConnectionEuidDiscoverEerror>,
  desc : &'d str,
  scope : Option<ManagementScope>,
  amu : Option<InstanceRef>,
  chan : MgmtChannel,
}

type CSE = anyhow::Error;

impl CommandStream<'_> {
  #[throws(CSE)]
  pub fn mainloop(mut self) {
    loop {
      use MgmtChannelReadError::*;
      let resp = match self.chan.read() {
        Ok(cmd) => match execute(&mut self, cmd) {
          Ok(resp) => resp,
          Err(error) => MgmtResponse::Error { error },
        },
        Err(EOF) => break,
        Err(IO(e)) => Err(e).context("read command stream")?,
        Err(Parse(s)) => MgmtResponse::Error { error : ParseFailed(s) },
      };
      self.chan.write(&resp).context("swrite command stream")?;
    }
  }

  #[throws(MgmtError)]
  fn get_scope(&self) -> &ManagementScope {
    self.scope.as_ref().ok_or(NoScope)?
  }
}

/*
impl From<serde_lexpr::Error> for MgmtError {
  fn from(je: serde_lexpr::Error) -> ME {
    ParseFailed(format!("{}", &je))
  }
}
*/

use MgmtCommand::*;
use MgmtResponse::*;
use MgmtError::*;

type ME = MgmtError;
from_instance_lock_error!{MgmtError}

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
    throw!(anyhow!("{}: euid mismatch: client={:?} server={:?} wanted={:?}{}",
                   &self.desc, client_euid, server_euid, wanted,
                   xinfo.unwrap_or("")));
  }

  fn map_auth_err(&self, ae: AuthorisationError) -> MgmtError {
    eprintln!("command connection {}: authorisation error: {}",
              self.desc, ae.0);
    MgmtError::AuthorisationError
  }
}

#[throws(MgmtError)]
fn authorise_scope(cs: &CommandStream, wanted: &ManagementScope)
                   -> AuthorisedSatisfactory {
  do_authorise_scope(cs, wanted)
    .map_err(|e| cs.map_auth_err(e))?
}

#[throws(AuthorisationError)]
fn do_authorise_scope(cs: &CommandStream, wanted: &ManagementScope)
                   -> AuthorisedSatisfactory {
  type AS<T> = (T, ManagementScope);

  match &wanted {

    ManagementScope::Server => {
      let y : AS<
        Authorised<(Passwd,uid_t)>,
      > = {
        let ok = cs.authorised_uid(None,None)?;
        (ok,
         ManagementScope::Server)
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

        let (in_userlist, xinfo) = (||{ <Result<_,AE>>::Ok({
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
              Err(e) => Some(<Result<_,AE>>::Err(e.into())),
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
        let info = xinfo.as_deref();
        let ok = cs.authorised_uid(authorised_for, info)?;
        (ok,
         ManagementScope::Unix { user: pwent.name })
      };
      y.into()
    },

  }
}

#[throws(ME)]
fn execute(cs: &mut CommandStream, cmd: MgmtCommand) -> MgmtResponse {
  eprintln!("command connection {}: executing {:?}", &cs.desc, &cmd);

  match cmd {
    Noop => Fine,

    SetScope(wanted_scope) => {
      let authorised : AuthorisedSatisfactory =
        authorise_scope(cs, &wanted_scope)?;
      cs.scope = Some(authorised.into_inner());
      Fine
    },

    CreateGame { name, insns } => {
      let gs = crate::gamestate::GameState {
        table_size : DEFAULT_TABLE_SIZE,
        pieces : Default::default(),
        players : Default::default(),
        log : Default::default(),
        gen : Generation(0),
        max_z: ZCoord(0.),
      };

      let name = InstanceName {
        scope : cs.get_scope()?.clone(),
        scoped_name : name,
      };

      let gref = Instance::new(name, gs)?;
      let mut ig = gref.lock()?;

      execute_for_game(cs, &mut ig, insns, MgmtGameUpdateMode::Bulk)
        .map_err(|e|{
          let name = ig.name.clone();
          Instance::destroy_game(ig)
            .unwrap_or_else(|e|
 eprintln!("failed to tidy up failecd creation of {:?}: {:?}",
                                      &name, &e));
          e
        })?;

      Fine
    },

    ListGames { all } => {
      let scope = if all == Some(true) {
        let _authorise : AuthorisedSatisfactory =
          authorise_scope(cs, &ManagementScope::Server)?;
        None
      } else {
        let scope = cs.get_scope()?;
        Some(scope)
      };
      let mut games = Instance::list_names(scope);
      games.sort_unstable();
      GamesList(games)
    },

    MgmtCommand::AlterGame { name, insns, how} => {
      let name = InstanceName {
        scope: cs.get_scope()?.clone(),
        scoped_name: name
      };
      let gref = Instance::lookup_by_name(&name)?;
      let mut g = gref.lock()?;
      execute_for_game(cs, &mut g, insns, how)?
    },
  }
}

#[derive(Debug,Default)]
struct UpdateHandlerBulk {
  pieces : slotmap::SparseSecondaryMap<PieceId, PieceUpdateOp<()>>,
  logs : bool,
  raw : Vec<PreparedUpdateEntry>,
}

#[derive(Debug)]
enum UpdateHandler {
  Bulk(UpdateHandlerBulk),
  Online,
}

impl UpdateHandler {
  fn from_how(how: MgmtGameUpdateMode) -> Self {
    use UpdateHandler::*;
    match how {
      MgmtGameUpdateMode::Bulk => Bulk(Default::default()),
      MgmtGameUpdateMode::Online => Online,
    }
  }

  #[throws(SVGProcessingError)]
  fn accumulate(&mut self, g: &mut Instance,
                updates: ExecuteGameChangeUpdates) {
    let mut raw = updates.raw.unwrap_or_default();
    use UpdateHandler::*;
    match self {
      Bulk(bulk) => {
        for (upiece, uuop) in updates.pcs {
          use PieceUpdateOp::*;
          let ne = match (bulk.pieces.get(upiece), uuop) {
            ( None               , e        ) => Some( e          ),
            ( Some( Insert(()) ) , Delete() ) => None,
            ( Some( Insert(()) ) , _        ) => Some( Insert(()) ),
            ( Some( Delete(  ) ) , _        ) => Some( Modify(()) ),
            ( _                  , _        ) => Some( Modify(()) ),
          };
          match ne {
            Some(ne) => { bulk.pieces.insert(upiece, ne); },
            None     => { bulk.pieces.remove(upiece); },
          };
        }
        bulk.logs |= updates.log.len() != 0;
        bulk.raw.append(&mut raw);
      },
      Online => {
        let estimate = updates.pcs.len() + updates.log.len();
        let mut buf = PrepareUpdatesBuffer::new(g, None, Some(estimate));
        for (upiece, uuop) in updates.pcs {
          let lens = TransparentLens { };
          buf.piece_update(upiece, uuop, &lens);
        }
        buf.log_updates(updates.log);
        buf.raw_updates(raw);
      },
    }
  }

  #[throws(SVGProcessingError)]
  fn complete(self, _cs: &CommandStream, g: &mut InstanceGuard) {
    use UpdateHandler::*;
    match self {
      Bulk(bulk) => {
        let mut buf = PrepareUpdatesBuffer::new(g, None, None);
        for (upiece, uuop) in bulk.pieces {
          let lens = TransparentLens { };
          buf.piece_update(upiece, uuop, &lens);
        }

        if bulk.logs {
          buf.log_updates(vec![LogEntry {
            html: "The facilitator (re)configured the game".to_owned(),
            // xxx use cs.desc
          }]);
        }

        buf.raw_updates(bulk.raw);
      },
      Online => { },
    }
  }
}

#[throws(ME)]
fn execute_for_game(cs: &CommandStream, ig: &mut InstanceGuard,
                    mut insns: Vec<MgmtGameInstruction>,
                    how: MgmtGameUpdateMode) -> MgmtResponse {
  let mut uh = UpdateHandler::from_how(how);
  let mut responses = Vec::with_capacity(insns.len());
  let ok = (||{
    for insn in insns.drain(0..) {
      let (updates, resp) = execute_game_insn(cs, ig, insn)?;
      uh.accumulate(ig, updates)?;
      responses.push(resp);
    }
    uh.complete(cs,ig)?;
    Ok(None)
  })();
  MgmtResponse::AlterGame {
    responses,
    error: ok.unwrap_or_else(Some)
  }
}

const XXX_START_POS : Pos = [20,20];
const XXX_DEFAULT_POSD : Pos = [5,5];

const CREATE_PIECES_MAX : u32 = 300;

type ExecuteGameInsnResults = (
  ExecuteGameChangeUpdates,
  MgmtGameResponse,
);

#[throws(ME)]
fn execute_game_insn(cs: &CommandStream,
                     ig: &mut InstanceGuard, update: MgmtGameInstruction)
                     -> ExecuteGameInsnResults {
  type U = ExecuteGameChangeUpdates;
  use MgmtGameInstruction::*;
  use MgmtGameResponse::*;
  type Insn = MgmtGameInstruction;
  type Resp = MgmtGameResponse;
  fn readonly(_ig: &InstanceGuard, resp: Resp) -> ExecuteGameInsnResults {
    (U{ pcs: vec![], log: vec![], raw: None }, resp)
  }

  match update {
    Noop { } => readonly(ig, Fine),

    Insn::SetTableSize(size) => {
      ig.gs.table_size = size;
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: format!("The table was resized to {}x{}", size[0], size[1]),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableSize(size) ]) },
       Fine)
    }

    Insn::AddPlayer(pl) => {
      if ig.gs.players.values().any(|p| p.nick == pl.nick) {
        Err(ME::AlreadyExists)?;
      }
      let player = ig.player_new(pl)?;
      #[allow(clippy::useless_format)] // xxx below
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: format!("The facilitator added a player xxx"),
          } ],
          raw: None },
       Resp::AddPlayer(player))
    },

    Insn::ListPieces => readonly(ig, {
      let pieces = ig.gs.pieces.iter().map(|(piece,p)|{
        let &PieceState { pos, face, .. } = p;
        let desc_html = p.p.describe_html(None);
        MgmtGamePieceInfo { piece, pos, face, desc_html }
      }).collect();
      Resp::Pieces(pieces)
    }),

    RemovePlayer(player) => {
      ig.player_remove(player)?;
      #[allow(clippy::useless_format)] // xxx below
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: format!("The facilitator removed a player xxx"),
          }],
          raw: None},
       Fine)
    },

    Insn::Info => readonly(ig, {
      let players = ig.gs.players.clone();
      let table_size = ig.gs.table_size;
      let info = MgmtGameResponseGameInfo { table_size, players };
      Resp::Info(info)
    }),

    ResetPlayerAccesses { players } => {
      let tokens = ig.players_access_reset(&players)?
        .drain(0..).map(|token| vec![token]).collect();
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       PlayerAccessTokens(tokens))
    }

    ReportPlayerAccesses { players } => {
      let tokens = ig.players_access_report(&players)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       PlayerAccessTokens(tokens))
    },

    SetFixedPlayerAccess { player, token } => {
      let authorised : AuthorisedSatisfactory =
        authorise_scope(cs, &ManagementScope::Server)?;
      let authorised = match authorised.into_inner() {
        ManagementScope::Server => Authorised::<RawToken>::authorise(),
        _ => panic!(),
      };
      ig.player_access_register_fixed(
        player, token, authorised
      )?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None},
       Fine)
    }

    DeletePiece(piece) => {
      let gs = &mut ig.gs;
      let p = gs.pieces.remove(piece).ok_or(ME::PieceNotFound)?;
      let desc_html = p.p.describe_html(Some(Default::default()));
      p.p.delete_hook(&p, gs);
      (U{ pcs: vec![(piece, PieceUpdateOp::Delete())],
          log: vec![ LogEntry {
            html: format!("A piece {} was removed from the game",
                          desc_html),
          }],
          raw: None },
       Fine)
    },

    AddPieces(PiecesSpec{ pos,posd,count,face,info }) => {
      let gs = &mut ig.gs;
      let count = count.unwrap_or(1);
      if count > CREATE_PIECES_MAX { throw!(LimitExceeded) }
      let posd = posd.unwrap_or(XXX_DEFAULT_POSD);
      let face = info.resolve_spec_face(face)?;

      let mut updates = Vec::with_capacity(count as usize);
      let mut pos = pos.unwrap_or(XXX_START_POS);
      for _ in 0..count {
        let p = info.load()?;
        let z = ZCoord(gs.max_z.0 + (count + 1) as f64);
        let pc = PieceState {
          held: None,
          zlevel: ZLevel { z, zg: gs.gen },
          lastclient: Default::default(),
          gen_before_lastclient: Generation(0),
          gen: gs.gen,
          pos, p, face,
        };
        let piece = gs.pieces.insert(pc);
        updates.push((piece, PieceUpdateOp::Insert(())));
        pos[0] += posd[0];
        pos[1] += posd[1];
      }

      (U{ pcs: updates,
          log: vec![ LogEntry {
            html: format!("The facilitaror added {} pieces", count),
          }],
          raw: None },
       Fine)
    },
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

    fs::set_permissions(path, unix::fs::PermissionsExt::from_mode(0o666))
      .with_context(|| format!("chmod sommand socket {:?}", &path))?;

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
          <Result<_,AE>>::Ok(show_username)
        })().unwrap_or_else(|e| format!("<error: {}>", e));
        write!(&mut desc, " user={}", user_desc)?;

        let chan = MgmtChannel::new(conn)?;

        let cs = CommandStream {
          scope: None, amu: None, desc: &desc,
          chan, euid,
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

pub use authproofs::Authorised;

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
