// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// management API implementation

use crate::imports::*;

// ---------- newtypes, type aliases, basic definitions ----------

use std::os::unix::io::AsRawFd;
use std::os::unix::net::UnixListener;
use uds::UnixStreamExt;
use pwd::Passwd;

pub use crate::from_instance_lock_error;
pub use std::os::unix::net::UnixStream;

type CSE = anyhow::Error;

use MgmtCommand::*;
use MgmtResponse::*;
use MgmtError::*;

type ME = MgmtError;
from_instance_lock_error!{MgmtError}

type AS = AccountScope;

const USERLIST : &str = "/etc/userlist";
const CREATE_PIECES_MAX : u32 = 300;

const DEFAULT_POS_START : Pos = PosC([20,20]);
const DEFAULT_POS_DELTA : Pos = PosC([5,5]);

pub struct CommandListener {
  listener : UnixListener,
}

struct Who;
impl Display for Who {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut Formatter) { write!(f, "The facilitator")? }
}

// ========== management API ==========

// ---------- management command implementations

#[throws(ME)]
fn execute(cs: &mut CommandStream, cmd: MgmtCommand) -> MgmtResponse {
  info!("command connection {}: executing {:?}", &cs.desc, &cmd);

  match cmd {
    Noop => Fine,

    SetAccount(wanted_account) => {
      let authorised = authorise_scope(cs, &wanted_account.scope)?;
      cs.account = ScopedName {
        scope: Some(authorised.into_inner()),
        scoped_nmae: wanted_account.scoped_name,
      };
      Fine
    },

    CreateGame { game, insns } => {
      let authorised = authorise_scope(cs, &game.scope)?;

      let gs = crate::gamestate::GameState {
        table_size : DEFAULT_TABLE_SIZE,
        pieces : Default::default(),
        players : Default::default(),
        log : Default::default(),
        gen : Generation(0),
        max_z: default(),
      };

      let gref = Instance::new(game, gs, authorised)?;
      let mut ig = gref.lock()?;

      execute_for_game(cs, &mut ig, insns, MgmtGameUpdateMode::Bulk)
        .map_err(|e|{
          let name = ig.name.clone();
          Instance::destroy_game(ig)
            .unwrap_or_else(|e| warn!(
              "failed to tidy up failecd creation of {:?}: {:?}",
              &name, &e
            ));
          e
        })?;

      Fine
    },

    ListGames { all } => {
      let (scope, authorised) = if all == Some(true) {
        let auth = authorise_scope(cs, &AS::Server)?;
        (None, auth)
      } else {
        cs.current_account()?;
      };
      let mut games = Instance::list_names(scope, authorised);
      games.sort_unstable();
      GamesList(games)
    },

    MgmtCommand::AlterGame { name, insns, how} => {
      let name = ScopedName {
        scope: cs.get_scope()?.clone(),
        scoped_name: name
      };
      let gref = Instance::lookup_by_name(&name)?;
      let mut g = gref.lock()?;
      execute_for_game(cs, &mut g, insns, how)?
    },

    LibraryListByGlob { glob: spec } => {
      let lib = shapelib::libs_lookup(&spec.lib)?;
      let results = lib.list_glob(&spec.item)?;
      LibraryItems(results)
    },
  }
}

// ---------- game command implementations ----------

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
  let who = &cs.who;
  fn readonly(_ig: &InstanceGuard, resp: Resp) -> ExecuteGameInsnResults {
    (U{ pcs: vec![], log: vec![], raw: None }, resp)
  }

  match update {
    Noop { } => readonly(ig, Fine),

    Insn::SetTableSize(size) => {
      ig.gs.table_size = size;
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: Html(format!("The table was resized to {}x{}",
                               size.0[0], size.0[1])),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableSize(size) ]) },
       Fine)
    }

    Insn::AddPlayer(pl) => {
      if ig.gs.players.values().any(|p| p.nick == pl.st.nick) {
        Err(ME::AlreadyExists)?;
      }
      let logentry = LogEntry {
        html: Html(format!("{} added a player: {}", &who,
                      htmlescape::encode_minimal(&pl.st.nick))),
      };
      let timezone = pl.timezone.as_ref().map(String::as_str)
        .unwrap_or("");
      let tz = match Timezone::from_str(timezone) {
        Ok(tz) => tz,
        Err(x) => match x { },
      };
      let (player, logentry) = ig.player_new(pl.st, tz, logentry)?;
      (U{ pcs: vec![],
          log: vec![ logentry ],
          raw: None },
       Resp::AddPlayer(player))
    },

    Insn::ListPieces => readonly(ig, {
      // xxx put something in log
      let pieces = ig.gs.pieces.iter().map(|(piece,p)|{
        let &PieceState { pos, face, .. } = p;
        let pinfo = ig.pieces.get(piece)?;
        let desc_html = pinfo.describe_html(None);
        let itemname = pinfo.itemname().to_string();
        let bbox = pinfo.bbox_approx();
        Some(MgmtGamePieceInfo { piece, pos, face, desc_html, bbox, itemname })
      }).flatten().collect();
      Resp::Pieces(pieces)
    }),

    RemovePlayer(player) => {
      let old_state = ig.player_remove(player)?;
      (U{ pcs: vec![],
          log: old_state.iter().map(|pl| LogEntry {
            html: Html(format!("{} removed a player: {}", &who,
                          htmlescape::encode_minimal(&pl.nick))),
          }).collect(),
          raw: None},
       Fine)
    },

    Insn::Info => readonly(ig, {
      let players = ig.gs.players.clone();
      let table_size = ig.gs.table_size;
      let info = MgmtGameResponseGameInfo { table_size, players };
      Resp::Info(info)
    }),

    ResetPlayerAccess(player) => {
      let token = ig.players_access_reset(player)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       PlayerAccessToken(token))
    }

    RedeliverPlayerAccess(player) => {
      let token = ig.players_access_redeliver(player)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       PlayerAccessToken(token))
    },
/*
    SetFixedPlayerAccess { player, token } => {
      let authorised : AuthorisedSatisfactory =
        authorise_scope(cs, &AS::Server)?;
      let authorised = match authorised.into_inner() {
        AS::Server => Authorised::<RawToken>::authorise(),
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
*/

    DeletePiece(piece) => {
      let modperm = ig.modify_pieces();
      let p = ig.pieces.as_mut(modperm)
        .remove(piece).ok_or(ME::PieceNotFound)?;
      let gs = &mut ig.gs;
      let pc = gs.pieces.as_mut(modperm).remove(piece);
      let desc_html = p.describe_html(Some(Default::default()));
      if let Some(pc) = pc { p.delete_hook(&pc, gs); }
      (U{ pcs: vec![(piece, PieceUpdateOp::Delete())],
          log: vec![ LogEntry {
            html: Html(format!("A piece {} was removed from the game",
                          desc_html.0)),
          }],
          raw: None },
       Fine)
    },

    AddPieces(PiecesSpec{ pos,posd,count,face,pinned,info }) => {
      let modperm = ig.modify_pieces();
      let ig = &mut **ig;
      let gs = &mut ig.gs;
      let count = count.unwrap_or(1);
      if count > CREATE_PIECES_MAX { throw!(LimitExceeded) }
      let posd = posd.unwrap_or(DEFAULT_POS_DELTA);

      let mut updates = Vec::with_capacity(count as usize);
      let mut pos = pos.unwrap_or(DEFAULT_POS_START);
      let mut z = gs.max_z.clone_mut();
      for _ in 0..count {
        let p = info.load()?;
        let face = face.unwrap_or_default();
        if p.nfaces() <= face.into() {
          throw!(SpecError::FaceNotFound);
        }
        let pc = PieceState {
          held: None,
          zlevel: ZLevel { z: z.increment()?, zg: gs.gen },
          lastclient: Default::default(),
          gen_before_lastclient: Generation(0),
          pinned: pinned.unwrap_or(false),
          gen: gs.gen,
          pos, face,
        };
        if let (_, true) = pc.pos.clamped(gs.table_size) {
          throw!(SpecError::PosOffTable);
        }
        let piece = gs.pieces.as_mut(modperm).insert(pc);
        ig.pieces.as_mut(modperm).insert(piece, p);
        updates.push((piece, PieceUpdateOp::Insert(())));
        pos += posd;
      }

      (U{ pcs: updates,
          log: vec![ LogEntry {
            html: Html(format!("{} added {} pieces", &who, count)),
          }],
          raw: None },
       Fine)
    },
  }
}

// ---------- how to execute game commands & handle their updates ----------

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
  ig.save_game_now()?;
  MgmtResponse::AlterGame {
    responses,
    error: ok.unwrap_or_else(Some)
  }
}


#[derive(Debug,Default)]
struct UpdateHandlerBulk {
  pieces : slotmap::SparseSecondaryMap<PieceId, PieceUpdateOp<(),()>>,
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
  fn complete(self, cs: &CommandStream, g: &mut InstanceGuard) {
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
            html: Html(format!("{} (re)configured the game", &cs.who))
          }]);
        }

        buf.raw_updates(bulk.raw);
      },
      Online => { },
    }
  }
}

// ========== general implementation ==========

// ---------- core listener implementation ----------

struct CommandStream<'d> {
  euid : Result<Uid, ConnectionEuidDiscoverEerror>,
  desc : &'d str,
  account : Option<(AccountName, Authorised<AccountName>)>,
  chan : MgmtChannel,
  who: Who,
}

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
  fn current_account(&self) -> (&AccountName, Authorised<AccountName>) {
    self.scope.as_ref()
      .ok_or(ME::SpecifyAccount)?
      .map(|(account,auth)| (account,*auth))
  }
}

impl CommandListener {
  #[throws(StartupError)]
  pub fn new() -> Self {
    let path = &config().command_socket;
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
          |e| error!("accept/spawn failed: {:?}", e)
        );
      }
    })
  }

  #[throws(CSE)]
  fn accept_one(&mut self) {
    let (conn, _caller) = self.listener.accept().context("accept")?;
    let mut desc = format!("{:>5}", conn.as_raw_fd());
    info!("command connection {}: accepted", &desc);
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
          scope: None, desc: &desc,
          chan, euid: euid.map(Uid::from_raw),
          who: Who,
        };
        cs.mainloop()?;
        
        <Result<_,StartupError>>::Ok(())
      })() {
        Ok(()) => info!("command connection {}: disconnected", &desc),
        Err(e) => warn!("command connection {}: error: {:?}", &desc, e),
      }
    });
  }
}

//---------- authorisation ----------

#[derive(Debug,Error,Clone)]
#[error("connection euid lookup failed (at connection initiation): {0}")]
pub struct ConnectionEuidDiscoverEerror(String);

impl CommandStream<'_> {
  #[throws(AuthorisationError)]
  fn authorised_uid(&self, wanted: Option<Uid>, xinfo: Option<&str>)
                    -> Authorised<(Passwd,Uid),> {
    let client_euid = *self.euid.as_ref().map_err(|e| e.clone())?;
    let server_uid = Uid::current();
    if client_euid.is_root() ||
       client_euid == server_uid ||
       Some(client_euid) == wanted
    {
      return Authorised::authorise();
    }
    throw!(anyhow!("{}: euid mismatch: client={:?} server={:?} wanted={:?}{}",
                   &self.desc, client_euid, server_uid, wanted,
                   xinfo.unwrap_or("")));
  }

  fn map_auth_err(&self, ae: AuthorisationError) -> MgmtError {
    warn!("command connection {}: authorisation error: {}",
          self.desc, ae.0);
    MgmtError::AuthorisationError
  }
}

#[throws(MgmtError)]
fn authorise_scope(cs: &CommandStream, wanted: &AccountScope)
                   -> Authorised<AccountScope> {
  do_authorise_scope(cs, wanted)
    .map_err(|e| cs.map_auth_err(e))?
}

#[throws(AuthorisationError)]
fn do_authorise_scope(cs: &CommandStream, wanted: &AccountScope)
                   -> Authorised<AccountScope> {
  match &wanted {

    AccountScope::Server => {
      let y : Authorised<(Passwd,Uid)> = {
        cs.authorised_uid(None,None)?;
      };
      y.therefore_ok()
    },

    AccountScope::Unix { user: wanted } => {
      struct InUserList;

      let y : Authorised<(Passwd,Uid,InUserList)> = {

        struct AuthorisedIf { authorised_for : Option<Uid> };

        const SERVER_ONLY : (AuthorisedIf, Authorised<InUserList>) = (
          AuthorisedIf { authorised_for: None },
          Authorised::authorised(InUserList),
        );

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
        let pwent_ok = Authorised::authorised(pwent);

        let ((uid, in_userlist_ok), xinfo) = (||{ <Result<_,AE>>::Ok({
          let allowed = BufReader::new(match File::open(USERLIST) {
            Err(e) if e.kind() == ErrorKind::NotFound => {
              return Ok((
                SERVER_ONLY,
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
                  (AuthorisedIf{ authorised_for: Some(
                    Uid::from_raw(pwent.uid)
                  )},
                   Authorised::authorised(InUserList),
                  ),
                  None
                ))
              ),
              Ok(_) => None,
              Err(e) => Some(<Result<_,AE>>::Err(e.into())),
            })
            .next()
            .unwrap_or_else(
              || Ok((
                SERVER_ONLY,
                Some(format!(" requested username {:?} not in {}",
                             &wanted, USERLIST)),
              ))
            )?
        })})()?;

        let info = xinfo.as_deref();
        let uid_ok = cs.authorised_uid(uid.authorised_for, info)?;
      
        (pwent_ok, uid_ok, in_userlist_ok).combine()
      };
      y.therefore_ok()
    },

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

  impl<T> Authorised<T> {
    pub const fn authorise_any() -> Authorised<T> { Authorised(PhantomData) }
    pub const fn authorised(v: &T) -> Authorised<T> { Authorised(PhantomData) }
  }

  impl<T> Authorised<T> {
    pub fn therefore_ok<U>(self) -> Authorised<U> { Authorised(PhantomData) }
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

  pub trait AuthorisedCombine {
    type Output;
    fn combine(self) -> Authorised<Self::Output> { Authorised(PhantomData) }
  }
  impl<A,B> AuthorisedCombine
    for (Authorised<A>, Authorised<B>) {
    type Output = Authorised<(A, B)>;
  }
  impl<A,B,C> AuthorisedCombine
    for (Authorised<A>, Authorised<B>, Authorised<C>) {
    type Output = Authorised<(A, B, C)>;
  }
}
