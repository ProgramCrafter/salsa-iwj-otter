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
type TP = TablePermission;

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

struct CommandStream<'d> {
  euid : Result<Uid, ConnectionEuidDiscoverEerror>,
  desc : &'d str,
  account : Option<AccountSpecified>,
  chan : MgmtChannel,
  who: Who,
}

#[derive(Debug,Clone)]
struct AccountSpecified {
  account: AccountName,
  cooked: String, // account.to_string()
  auth: Authorisation<AccountName>,
}

enum PermissionCheckHow<'p> {
  Instance,
  InstanceOrOnlyAffectedAccount(&'p AccountName),
  InstanceOrOnlyAffectedPlayer(PlayerId),
}

type PCH<'p> = PermissionCheckHow<'p>;

// ========== management API ==========

// ---------- management command implementations

#[throws(ME)]
fn execute(cs: &mut CommandStream, cmd: MgmtCommand) -> MgmtResponse {
  info!("command connection {}: executing {:?}", &cs.desc, &cmd);

  match cmd {
    Noop => Fine,

    SetAccount(wanted_account) => {
      let auth = authorise_scope_direct(cs, &wanted_account.scope)?;
      cs.account = Some(AccountSpecified {
        account: wanted_account,
        cooked: wanted_account.to_string(),
        auth: auth.therefore_ok(),
      });
      Fine
    },

    CreateGame { game, insns } => {
      let authorised = authorise_by_account(cs, &game)?;

      let gs = crate::gamestate::GameState {
        table_size : DEFAULT_TABLE_SIZE,
        pieces : Default::default(),
        players : Default::default(),
        log : Default::default(),
        gen : Generation(0),
        max_z: default(),
      };

      let acl = default();
      let gref = Instance::new(game, gs, acl, authorised)?;
      let ig = gref.lock()?;

      execute_for_game(cs, &mut Unauthorised::of(ig),
                       insns, MgmtGameUpdateMode::Bulk)
        .map_err(|e|{
          let name = ig.name.clone();
          Instance::destroy_game(ig, authorised)
            .unwrap_or_else(|e| warn!(
              "failed to tidy up failecd creation of {:?}: {:?}",
              &name, &e
            ));
          e
        })?;

      Fine
    },

    ListGames { all } => {
      let (scope, auth) = if all == Some(true) {
        let auth = authorise_scope_direct(cs, &AS::Server)?;
        (None, auth.therefore_ok())
      } else {
        let AccountSpecified { account, auth, .. } = cs.account.as_ref()
          .ok_or(ME::SpecifyAccount)?;
        (Some(account), *auth)
      };
      let mut games = Instance::list_names(scope, auth);
      games.sort_unstable();
      GamesList(games)
    },

    MgmtCommand::AlterGame { game, insns, how } => {
      let gref = Instance::lookup_by_name_unauth(&game)?;
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

//#[throws(ME)]
fn execute_game_insn(cs: &CommandStream,
                     ig: &mut Unauthorised<InstanceGuard, InstanceName>,
                     update: MgmtGameInstruction)
                     -> Result<ExecuteGameInsnResults,ME> {
  type U = ExecuteGameChangeUpdates;
  use MgmtGameInstruction::*;
  use MgmtGameResponse::*;
  type Insn = MgmtGameInstruction;
  type Resp = MgmtGameResponse;
  let who = &cs.who;

  fn readonly<F: FnOnce(&InstanceGuard) -> MgmtGameResponse,
              P: Into<PermSet<TablePermission>>>
  
    (
      cs: &CommandStream,
      ig: &Unauthorised<InstanceGuard, InstanceName>,
      p: P,
      f: F
    ) -> ExecuteGameInsnResults
  {
    let ig = cs.check_acl(p)?;
    let resp = f(ig);
    (U{ pcs: vec![], log: vec![], raw: None }, resp)
  }

  impl<'cs> CommandStream<'cs> {
    #[throws(MgmtError)]
    fn check_acl_manip_player_access(
      &mut self,
      ig: &mut Unauthorised<InstanceGuard, InstanceName>,
      player: PlayerId,
      perm: TablePermission,
    ) -> (&mut InstanceGuard, Authorisation<AccountName>) {
      let (ig, auth) = self.check_acl(ig,
                            PCH::InstanceOrOnlyAffectedPlayer(player),
                                      &[perm])?;
      fn auth_map(n: &InstanceName) -> &AccountName { &n.account }
      let auth = auth.map(&auth_map);
      (ig, auth)
    }
  }

  let y = match update {
    Noop { } => readonly(cs,ig, &[], |ig| Fine),

    Insn::SetTableSize(size) => {
      let ig = cs.check_acl(ig, PCH::Instance, &[TP::ChangePieces])?.0;
      ig.gs.table_size = size;
      (U{ pcs: vec![],
          log: vec![ LogEntry {
            html: Html(format!("The table was resized to {}x{}",
                               size.0[0], size.0[1])),
          }],
          raw: Some(vec![ PreparedUpdateEntry::SetTableSize(size) ]) },
       Fine)
    }

    Insn::AddPlayer(add) => {
      // todo some kind of permissions check for player too
      let ig = cs.check_acl(ig, PCH::Instance, &[TP::AddPlayer])?.0;
      let nick = add.nick.ok_or(ME::ParameterMissing)?;
      let logentry = LogEntry {
        html: Html(format!("{} added a player: {}", &who,
                      htmlescape::encode_minimal(&nick))),
      };
      let timezone = add.timezone.as_ref().map(String::as_str)
        .unwrap_or("");
      let tz = match Timezone::from_str(timezone) {
        Ok(tz) => tz,
        Err(x) => match x { },
      };
      let st = PlayerState {
        tz,
        account: add.account,
        nick: nick.to_string(),
      };
      let (player, logentry) = ig.player_new(st, tz, logentry)?;
      (U{ pcs: vec![],
          log: vec![ logentry ],
          raw: None },
       Resp::AddPlayer(player))
    },

    Insn::ListPieces => readonly(cs,ig, &[TP::ViewPublic], |ig|{
      // xxx put something in log
      let pieces = ig.gs.pieces.iter().map(|(piece,p)|{
        let &PieceState { pos, face, .. } = p;
        let pinfo = ig.ipieces.get(piece)?;
        let desc_html = pinfo.describe_html(None);
        let itemname = pinfo.itemname().to_string();
        let bbox = pinfo.bbox_approx();
        let lens = TransparentLens { };
        Some(MgmtGamePieceInfo {
          piece, itemname,
          visible:
          if let TransparentLens { } = lens {
            Some(MgmtGamePieceVisibleInfo {
              pos, face, desc_html, bbox
            })
          } else {
            None
          }
        })
      }).flatten().collect();
      Resp::Pieces(pieces)
    }),

    RemovePlayer(account) => {
      // todo let you remove yourself unconditionally
      let ig = cs.check_acl(ig,
                            PCH::InstanceOrOnlyAffectedAccount(&account),
                            &[TP::RemovePlayer])?.0;
      let player = ig.gs.players.iter()
        .filter_map(|(k,v)| if v == &account { Some(k) } else { None })
        .next()
        .ok_or(ME::PlayerNotFound)?;
      let (_, old_state) = ig.player_remove(player)?;
      (U{ pcs: vec![],
          log: old_state.iter().map(|pl| LogEntry {
            html: Html(format!("{} removed a player: {}", &who,
                          htmlescape::encode_minimal(&pl.nick))),
          }).collect(),
          raw: None},
       Fine)
    },

    Insn::Info => readonly(cs,ig, &[TP::ViewPublic], |ig|{
      let players = ig.gs.players.iter().map(
        |(player, &account)| {
          let account = account.clone();
          let info = match ig.iplayers.get(player) {
            Some(pl) => MgmtPlayerInfo {
              account,
              nick: pl.pst.nick.clone(),
            },
            None => MgmtPlayerInfo {
              account,
              nick: format!("<missing from iplayers table!>"),
            },
          };
          (player, info)
        }).collect();
      let table_size = ig.gs.table_size;
      let info = MgmtGameResponseGameInfo { table_size, players };
      Resp::Info(info)
    }),

    ResetPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ig, player, TP::ResetOthersAccess)?;

      let token = ig.player_access_reset(player, auth)?;
      (U{ pcs: vec![],
          log: vec![],
          raw: None },
       PlayerAccessToken(token))
    }

    RedeliverPlayerAccess(player) => {
      let (ig, auth) = cs.check_acl_manip_player_access
        (ig, player, TP::RedeliverOthersAccess)?;

      let token = ig.player_access_redeliver(player)?;
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
        AS::Server => Authorisation::<RawToken>::authorise(),
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
  };
  Ok(y)
}

// ---------- how to execute game commands & handle their updates ----------

#[throws(ME)]
fn execute_for_game(cs: &CommandStream,
                    ig: &mut Unauthorised<InstanceGuard, InstanceName>,
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
  fn current_account(&self) -> (&AccountName, Authorisation<AccountName>) {
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
                    -> Authorisation<(Passwd,Uid),> {
    let client_euid = *self.euid.as_ref().map_err(|e| e.clone())?;
    let server_uid = Uid::current();
    if client_euid.is_root() ||
       client_euid == server_uid ||
       Some(client_euid) == wanted
    {
      return Authorisation::authorise();
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


impl CommandStream<'_> {
  #[throws(MgmtError)]
  pub fn check_acl<P: Into<PermSet<TablePermission>>>(
    &mut self,
    ig: &mut Unauthorised<InstanceGuard, InstanceName>,
    how: PermissionCheckHow<'_>,
    p: P,
  ) -> (&mut InstanceGuard, Authorisation<InstanceName>) {

    let subject_is = |object_account|{
      if let Some(ref subject_account_spec) = self.account {
        if subject_account_spec.account == object_account {
          let auth : Authorisation<InstanceName>
            = Authorisation::authorise_any();
          return auth;
        }
      }
      return None;
    };
      
    if let Some(auth) = match how {
      PCH::InstanceOrOnlyAffectedAccount(object_account) => {
        subject_is(object_account) 
      },
      PCH::InstanceOrOnlyAffectedPlayer(object_player) => {
        if let Some(object_account) = ig.gs.players.get(object_player) {
          subject_is(object_account)
        } else {
          None
        }
      },
    } {
      return self.by_mut(auth);
    }

    let p = p.into();
    let auth = {
      let subject = &self.account.as_ref()?.cooked;
      let acl = self.by(Authorisation::authorise_any()).acl;
      let eacl = EffectiveAcl {
        owner_account : &self.account?.to_string(),
        acl : &acl,
      };
      eacl.check(subject, p)?
    };
    (self.by_mut(auth), auth)
  }
}

#[throws(MgmtError)]
fn authorise_by_account(cs: &CommandStream, wanted: &InstanceName)
                        -> Authorisation<InstanceName> {
  let currently = &cs.account.as_ref()?.account;
  if currently == wanted.account {
    return Authorisation::authorised(wanted);
  }
  throw!(MgmtError::AuthorisationError)
}

#[throws(MgmtError)]
fn authorise_scope_direct(cs: &CommandStream, wanted: &AccountScope)
                          -> Authorisation<AccountScope> {
  // Usually, use authorise_by_account
  do_authorise_scope(cs, wanted)
    .map_err(|e| cs.map_auth_err(e))?
}

#[throws(AuthorisationError)]
fn do_authorise_scope(cs: &CommandStream, wanted: &AccountScope)
                   -> Authorisation<AccountScope> {
  match &wanted {

    AccountScope::Server => {
      let y : Authorisation<(Passwd,Uid)> = {
        cs.authorised_uid(None,None)?;
      };
      y.therefore_ok()
    },

    AccountScope::Unix { user: wanted } => {
      struct InUserList;

      let y : Authorisation<(Passwd,Uid,InUserList)> = {

        struct AuthorisedIf { authorised_for : Option<Uid> };

        const SERVER_ONLY : (AuthorisedIf, Authorisation<InUserList>) = (
          AuthorisedIf { authorised_for: None },
          Authorisation::authorised(&InUserList),
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
        let pwent_ok = Authorisation::authorised(pwent);

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
                   Authorisation::authorised(InUserList),
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

pub use authproofs::Authorisation;
pub use authproofs::Unauthorised;

mod authproofs {
  use crate::imports::*;

  #[derive(Debug,Copy,Clone)]
  pub struct Unauthorised<T,A> (T, PhantomData<A>);
  impl<T,A> Unauthorised<T,A> {
    pub fn of(t: T) -> Self { Unauthorised(t, PhantomData) }
    pub fn by(self, _auth: Authorisation<A>) -> T { self.0 }
  }
  impl<T,A> From<T> for Unauthorised<T,A> {
    fn from(t: T) -> Self { Self::of(t) }
  }

  #[derive(Error,Debug)]
  #[error("internal AuthorisationError {0}")]
  pub struct AuthorisationError(pub String);

  #[derive(Debug,Copy,Clone)]
  pub struct Authorisation<A> (PhantomData<A>);

  impl<T> Authorisation<T> {
    pub const fn authorise_any() -> Authorisation<T> {
      Authorisation(PhantomData)
    }
    pub const fn authorised(v: &T) -> Authorisation<T> {
      Authorisation(PhantomData)
    }
    pub fn therefore_ok<U>(self) -> Authorisation<U> {
      Authorisation(PhantomData)
    }

    pub fn map<U>(self, f: &fn(&T) -> &U) -> Authorisation<U> {
    }
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

  pub trait AuthorisationCombine {
    type Output;
    fn combine(self) -> Authorisation<Self::Output> {
      Authorisation(PhantomData)
    }
  }
  impl<A,B> AuthorisationCombine
    for (Authorisation<A>, Authorisation<B>) {
    type Output = Authorisation<(A, B)>;
  }
  impl<A,B,C> AuthorisationCombine
    for (Authorisation<A>, Authorisation<B>, Authorisation<C>) {
    type Output = Authorisation<(A, B, C)>;
  }
}
