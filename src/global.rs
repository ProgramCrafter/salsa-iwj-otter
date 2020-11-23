// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::let_and_return)]

use crate::imports::*;

use std::sync::PoisonError;
use slotmap::dense as sm;

type ME = MgmtError;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

const MAX_CLIENT_INACTIVITY : Duration = Duration::from_secs(200);

const GAME_SAVE_LAG : Duration = Duration::from_millis(500);

const MAX_LOG_AGE : Duration = Duration::from_secs(10 * 86400);

#[derive(Hash,Ord,PartialOrd,Eq,PartialEq,Serialize)]
#[repr(transparent)]
pub struct RawTokenVal(str);

// ---------- public data structure ----------

#[derive(Clone,Debug,Hash,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
pub struct InstanceName {
  pub account: AccountName,
  pub game: String,
}

#[derive(Debug,Clone)]
pub struct InstanceRef (Arc<Mutex<InstanceContainer>>);

pub struct Instance {
  pub name : Arc<InstanceName>,
  pub gs : GameState,
  pub ipieces : PiecesLoaded,
  pub clients : DenseSlotMap<ClientId,Client>,
  pub iplayers : SecondarySlotMap<PlayerId, PlayerRecord>,
  pub tokens_players : TokenRegistry<PlayerId>,
  pub tokens_clients : TokenRegistry<ClientId>,
  pub acl: LoadedAcl<TablePermission>,
}

pub struct PlayerRecord {
  pub u: PlayerUpdates,
  pub ipl: IPlayerState,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct IPlayerState {
  pub acctid: AccountId,
  pub tokens_revealed: HashMap<TokenRevelationKey, TokenRevelationValue>,
  pub tz: Timezone,
}

#[derive(Debug,Serialize,Deserialize)]
#[serde(transparent)]
pub struct PiecesLoaded (ActualPiecesLoaded);
pub type ActualPiecesLoaded = SecondarySlotMap<PieceId,Box<dyn Piece>>;
#[derive(Copy,Clone,Debug)]
pub struct ModifyingPieces(());

#[derive(Debug,Serialize,Deserialize,Default)]
#[serde(transparent)]
pub struct Pieces (pub(in crate::global) ActualPieces);
type ActualPieces = DenseSlotMap<PieceId,PieceState>;

#[derive(Debug)] 
pub struct Client {
  pub player : PlayerId,
  pub lastseen : Instant,
}

// KINDS OF PERSISTENT STATE
//
//               TokenTable   TokenTable    GameState    Instance GameState
//                <ClientId>   <PlayerId>    .players    .pieces  .pieces
//
//   Saved        No           a-*           g-*         a-*      g-*
//   Spec TOML    Absent       table, ish    table       game     game
//
//
// UPDATE RELIABILITY/PERSISTENCE RULES
//
// From the caller's point of view
//
// We offer atomic creation/modification/destruction of:
//
//    * Games (roughtly, a map from InstanceName to GameState;
//             includes any player)
//
//    * Player access tokens, for an existing (game, player)
//
//    * Clients (for an existing (game, player)
//
// We also offer atomic destruction of:
//
//    * Games
//
//    * Players
//
// All of the above, except clients, are persistent, in the sense
// that a server restart will preserve them.  See above.
//
// The general code gets mutable access to the GameState.  We offer
// post-hoc saving of a modified game.  This should not be used for
// player changes.  For other changes, if the save fails, a server
// restart may be a rewind.  This relaxation of the rules is
// necessary avoid complicated three-phase commit on GameState update
// for routine game events.
//
// IMPLEMENTATION
//
// Games are created in this order:
//
//  save/a-<nameforfs>    MessagePack of InstanceSaveAccess
//     if, on reload, this does not exist, the game is considered
//     not to exist, so at this stage the game conclusively does
//     not exist
//
//  save/g-<nameforfs>    MessagePack of GameState
//
//  games
//     can always be done right after g-<nameforfs>
//     since games update is infallible (barring unexpected panic)
//
// For the access elements such as player tokens and client
// tokents, we
//   1. check constraints against existing state
//   2. save to disk
//   3. modify in memory (infallibly)
//
// A consequence is that game creation or deletion means running
// much of this code (including game save/load) with a write lock
// onto `games`.
//
// The Instance object is inside Arc<Mutex>.  Because of Arc the Mutex
// may persist beyond the lifetime of the actual game.  So within the
// Mutex we have a field `live' that tells us if the thing is dead.
// This allows a lockholder to infallibly declare the thing dead,
// atomically from the pov of anyone else who has got a reference
// to it.  We prevent the caller from ever seeing an Instance whosae
// `live` is `false`.
#[derive(Debug)]
pub struct InstanceGuard<'g> {
  pub c : MutexGuard<'g,InstanceContainer>,
  pub gref : InstanceRef,
}

#[derive(Debug,Default)]
pub struct TokenRegistry<Id: AccessId> {
  tr : HashSet<RawToken>,
  id : PhantomData<Id>,
}

#[derive(Clone,Debug)]
pub struct InstanceAccessDetails<Id> {
  pub gref : InstanceRef,
  pub ident : Id,
  pub acctid : AccountId,
}

#[derive(Clone,Debug)]
pub struct InstanceAccess<'i, Id> {
  pub raw_token : &'i RawTokenVal,
  pub i : InstanceAccessDetails<Id>,
}

// ========== internal data structures ==========

lazy_static! {
  pub static ref GLOBAL : Global = Default::default();
}

#[derive(Default)]
pub struct Global {
  // lock hierarchy: all of these are in order of acquisition
  // (in order of lock acquisition (L first), so also in order of criticality
  // (perf impact); outermost first, innermost last)

  // slow global locks:
  save_area_lock : Mutex<Option<File>>,
  // <- accounts::accounts ->
  games_table: RwLock<GamesTable>,

  // per-game lock:
  // <- InstanceContainer ->
  pub shapelibs : RwLock<shapelib::Registry>,

  // inner locks which the game needs:
  dirty   : Mutex<VecDeque<InstanceRef>>,
  pub config: RwLock<Arc<ServerConfig>>,

  // fast global lookups
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
}

pub type GamesGuard = RwLockWriteGuard<'static, GamesTable>;
pub type GamesTable = HashMap<Arc<InstanceName>,InstanceRef>;

#[derive(Debug)]
pub struct InstanceContainer {
  live : bool,
  game_dirty : bool,
  access_dirty : bool,
  g : Instance,
}

#[derive(Debug,Default,Serialize,Deserialize)]
struct InstanceSaveAccesses<RawTokenStr, PiecesLoadedRef> {
  ipieces: PiecesLoadedRef,
  tokens_players: Vec<(RawTokenStr, PlayerId)>,
  aplayers: SecondarySlotMap<PlayerId, IPlayerState>,
  acl: Acl<TablePermission>,
}

display_as_debug!{InstanceLockError}
impl<X> From<PoisonError<X>> for InstanceLockError {
  fn from(_: PoisonError<X>) -> Self { Self::GameCorrupted }
}

pub struct PrivateCaller(());
// outsiders cannot construct this
// workaround for inability to have private trait methods
const PRIVATE_Y : PrivateCaller = PrivateCaller(());

// ========== implementations ==========

impl RawTokenVal {
  // str is [u8] with a funny hat on, so &str is pointer + byte count.
  // nomicon says &SomeStruct([T]) is pointer plus number of elements.
  // So &str and &SomeStruct(str) have the same layout
  fn from_str(s: &str) -> &RawTokenVal { unsafe { mem::transmute(s) } }
}

impl Borrow<RawTokenVal> for RawToken {
  fn borrow(&self) -> &RawTokenVal { RawTokenVal::from_str(&self.0) }
}

impl Debug for RawTokenVal {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    crate::spec::implementation::raw_token_debug_as_str(&self.0, f)
  }
}

impl Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Instance {{ name: {:?}, .. }}", &self.name)
  }
}

// ---------- Main API for instance lifecycle ----------

impl InstanceRef {
  #[throws(InstanceLockError)]
  pub fn lock(&self) -> InstanceGuard<'_> {
    let c = self.0.lock()?;
    if !c.live { throw!(InstanceLockError::GameBeingDestroyed) }
    InstanceGuard { c, gref: self.clone() }
  }

  pub fn lock_even_poisoned(&self) -> MutexGuard<InstanceContainer> {
    match self.0.lock() {
      Ok(g) => g,
      Err(poison) => poison.into_inner(),
    }
  }
}

impl<A> Unauthorised<InstanceRef, A> {
  #[throws(InstanceLockError)]
  pub fn lock<'r>(&'r self) -> Unauthorised<InstanceGuard<'r>, A> {
    let must_not_escape = self.by_ref(Authorisation::authorise_any());
    Unauthorised::of(must_not_escape.lock()?)
  }
}

impl Instance {
  /// Returns `None` if a game with this name already exists
  #[allow(clippy::new_ret_no_self)]
  #[throws(MgmtError)]
  pub fn new(name: InstanceName, gs: GameState,
             games: &mut GamesGuard,
             acl: LoadedAcl<TablePermission>, _: Authorisation<InstanceName>)
             -> InstanceRef {
    let name = Arc::new(name);

    let g = Instance {
      name : name.clone(),
      gs, acl,
      ipieces : PiecesLoaded(Default::default()),
      clients : Default::default(),
      iplayers : Default::default(),
      tokens_players : Default::default(),
      tokens_clients : Default::default(),
    };

    let cont = InstanceContainer {
      live : true,
      game_dirty : false,
      access_dirty : false,
      g,
    };

    let gref = InstanceRef(Arc::new(Mutex::new(cont)));
    let mut ig = gref.lock()?;

    let entry = games.entry(name);

    use hash_map::Entry::*;
    let entry = match entry {
      Vacant(ve) => ve,
      Occupied(_) => throw!(MgmtError::AlreadyExists),
    };

    ig.save_access_now()?;
    ig.save_game_now()?;

    (||{
      entry.insert(gref.clone());
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    ig.gref
  }

  #[throws(MgmtError)]
  pub fn lookup_by_name_locked_unauth
    (games_table: &GamesTable, name: &InstanceName)
     -> Unauthorised<InstanceRef, InstanceName>
  {
    Unauthorised::of(
      games_table
        .get(name)
        .ok_or(MgmtError::GameNotFound)?
        .clone()
        .into()
    )
  }

  #[throws(MgmtError)]
  pub fn lookup_by_name_locked(games: &GamesTable,
                               name: &InstanceName,
                               auth: Authorisation<InstanceName>)
                               -> InstanceRef {
    Self::lookup_by_name_locked_unauth(games, name)?.by(auth)
  }

  #[throws(MgmtError)]
  pub fn lookup_by_name_unauth(name: &InstanceName)
      -> Unauthorised<InstanceRef, InstanceName>
  {
    let games = GLOBAL.games_table.read().unwrap();
    Self::lookup_by_name_locked_unauth(&games, name)?
  }

  #[throws(MgmtError)]
  pub fn lookup_by_name(name: &InstanceName, auth: Authorisation<InstanceName>)
                        -> InstanceRef {
    Self::lookup_by_name_unauth(name)?.by(auth)
  }

  #[throws(InternalError)]
  pub fn destroy_game(games: &mut GamesGuard,
                      mut g: MutexGuard<InstanceContainer>,
                      _: Authorisation<InstanceName>) {
    let a_savefile = savefilename(&g.g.name, "a-", "");

    let g_file = savefilename(&g.g.name, "g-", "");
    fs::remove_file(&g_file).context("remove").context(g_file)?;

    (||{ // Infallible:
      g.live = false;
      games.remove(&g.g.name);
      InstanceGuard::forget_all_tokens(&mut g.g.tokens_clients);
      InstanceGuard::forget_all_tokens(&mut g.g.tokens_players);
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    (||{ // Best effort:
      fs::remove_file(&a_savefile)
    })()
      .unwrap_or_else(
        |e| warn!("failed to delete stale auth file {:?}: {:?}",
                  &a_savefile, e)
        // apart from that, ignore the error
        // load_games will clean it up later.
      );
  }

  pub fn list_names(account: Option<&AccountName>,
                    _: Authorisation<AccountName>)
                    -> Vec<Arc<InstanceName>> {
    let games = GLOBAL.games_table.read().unwrap();
    let out : Vec<Arc<InstanceName>> =
      games.keys()
      .filter(|k| account == None || account == Some(&k.account))
      .cloned()
      .collect();
    out
  }
}

pub fn games_lock() -> RwLockWriteGuard<'static, GamesTable> {
  GLOBAL.games_table.write().unwrap()
}

// ---------- Simple trait implementations ----------

impl Deref for InstanceGuard<'_> {
  type Target = Instance;
  fn deref(&self) -> &Instance { &self.c.g }
}
impl DerefMut for InstanceGuard<'_> {
  fn deref_mut(&mut self) -> &mut Instance { &mut self.c.g }
}

impl FromStr for AccountScope {
  type Err = InvalidScopedName;
  #[throws(InvalidScopedName)]
  fn from_str(s: &str) -> Self {
    let scope = AccountScope::parse_name(s, &mut [])?;
    scope
  }
}

impl FromStr for InstanceName {
  type Err = InvalidScopedName;
  #[throws(InvalidScopedName)]
  fn from_str(s: &str) -> Self {
    let mut names : [_;2] = Default::default();
    let scope = AccountScope::parse_name(s, &mut names)?;
    let [subaccount, game] = names;
    InstanceName {
      account: AccountName { scope, subaccount },
      game,
    }
  }
}

impl Display for InstanceName {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    self.account.scope.display_name(
      &[ self.account.subaccount.as_str(), self.game.as_str() ],
      |s| f.write_str(s)
    )?
  }
}

// ---------- Player and token functionality ----------

impl<'ig> InstanceGuard<'ig> {
  /// caller is responsible for logging; threading it through
  /// proves the caller has a log entry.
  #[throws(MgmtError)]
  pub fn player_new(&mut self, gnew: GPlayerState, inew: IPlayerState,
                    logentry: LogEntry)
                    -> (PlayerId, /* todo some game update,*/ LogEntry) {
    // saving is fallible, but we can't attempt to save unless
    // we have a thing to serialise with the player in it
    self.check_new_nick(&gnew.nick)?;
    if self.c.g.iplayers.values().any(|r| r.ipl.acctid == inew.acctid) {
      Err(MgmtError::AlreadyExists)?;
    }
    let player = self.c.g.gs.players.insert(gnew);
    let u = PlayerUpdates::new_begin(&self.c.g.gs).new();
    let record = PlayerRecord { u, ipl: inew };
    self.c.g.iplayers.insert(player, record);

    (||{
      self.save_game_now()?;
      self.save_access_now()?;
      Ok::<_,InternalError>(())
    })().map_err(|e|{
      self.c.g.iplayers.remove(player);
      self.c.g.gs.players.remove(player);
      // Perhaps we leave the g-* file with this player recorded,
      // but this will be ignored when we load.
      e
    })?;
    (||{
      
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
    (player, logentry)
  }

  #[throws(MgmtError)]
  pub fn check_new_nick(&mut self, new_nick: &str) {
    if self.c.g.gs.players.values().any(|old| old.nick == new_nick) {
      Err(MgmtError::NickCollision)?;
    }
  }

  pub fn remove_clients(&mut self,
                        players: &HashSet<PlayerId>,
                        signal: ErrorSignaledViaUpdate) {
    let mut clients_to_remove = HashSet::new();
    self.clients.retain(|k,v| {
      let remove = players.contains(&v.player);
      if remove { clients_to_remove.insert(k); }
      !remove
    });

    let gen = self.c.g.gs.gen;
    for &player in players {
      if let Some(iplayer) = self.iplayers.get_mut(player) {
        iplayer.u.push(PreparedUpdate {
          gen,
          when: Instant::now(),
          us : vec![ PreparedUpdateEntry::Error(
            None,
            signal.clone(),
          )],
        });
      };
    }
    self.tokens_deregister_for_id(|id| clients_to_remove.contains(&id));
  }

  //  #[throws(InternalError)]
  //  https://github.com/withoutboats/fehler/issues/62
  pub fn players_remove(&mut self, oldplayers: &HashSet<PlayerId>)
                        ->
    Result<Vec<
        (Option<GPlayerState>, Option<IPlayerState>)
        >, InternalError>
  {
    // We have to filter this player out of everything
    // Then save
    // Then send updates
    // We make a copy so if the save fails, we can put everything back

    let mut players = self.c.g.gs.players.clone();
    let old_gpls : Vec<_> = oldplayers.iter().cloned().map(|oldplayer| {
      players.remove(oldplayer)
    }).collect();

    // New state
    let mut gs = GameState {
      // These parts are straightforward and correct
      table_size: self.c.g.gs.table_size,
      gen: self.c.g.gs.gen,
      max_z: self.gs.max_z.clone(),
      players,
      // These have special handling
      log: default(),
      pieces: default(),
    };

    let held_by_old = |p: &PieceState| if_chain! {
      if let Some(held) = p.held;
      if oldplayers.contains(&held);
      then { true }
      else { false }
    };

    let mut updated_pieces = vec![];
    
    // drop order is reverse of creation order, so create undo
    // after all the things it will reference
    let mut undo : Vec<Box<dyn FnOnce(&mut InstanceGuard)>> = vec![];

    // Arrange gs.pieces
    for (piece,p) in &mut self.c.g.gs.pieces {
      if held_by_old(p) {
        p.held = None;
        updated_pieces.push(piece);
      }
    }
    undo.push(Box::new(|ig| for &piece in &updated_pieces {
      (||Some({
        held_by_old(ig.c.g.gs.pieces.get_mut(piece)?);
      }))();
    }));

    // Handle gs.log:
    // Installs gs as the new game state, stealing the log and pieces
    let mut swap_things = |ig: &mut InstanceGuard| {
      mem::swap(&mut ig.c.g.gs.log,    &mut gs.log   );
      mem::swap(&mut ig.c.g.gs.pieces, &mut gs.pieces);
      mem::swap(&mut ig.c.g.gs,        &mut gs,   );
    };
    swap_things(self);
    undo.push(Box::new(swap_things));

    self.save_game_now().map_err(|e|{
      // oof
      for u in undo.drain(..).rev() {
        u(self);
      }
      e
    })?;

    // point of no return
    mem::drop(undo);

    let old_ipls = (||{
      for &piece in &updated_pieces {
        (||Some({
          self.c.g.gs.pieces.get_mut(piece)?.gen = self.c.g.gs.gen;
        }))();
      }

      let lens = TransparentLens { };
      let estimate = updated_pieces.len() + 1;
      let mut buf = PrepareUpdatesBuffer::new(self, None , Some(estimate));
      for &piece in &updated_pieces {
        buf.piece_update(piece, PieceUpdateOp::Modify(()), &lens);
      }
      buf.finish();

      self.remove_clients(oldplayers, ErrorSignaledViaUpdate::PlayerRemoved);
      self.tokens_deregister_for_id(|id:PlayerId| oldplayers.contains(&id));
      let old_ipls : Vec<_> = oldplayers.iter().cloned().map(
        |oldplayer| self.iplayers.remove(oldplayer)
          .map(|ipr| ipr.ipl)
      ).collect();
      self.save_access_now().unwrap_or_else(
        |e| warn!(
          "trouble garbage collecting accesses for deleted player: {:?}",
          &e)
      );
      old_ipls
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    let old = itertools::zip(
      old_gpls,
      old_ipls,
    ).collect();
    Ok(old)
  }

  #[throws(InternalError)]
  pub fn invalidate_tokens(&mut self, player: PlayerId) {
    let old_tokens = TokenRegistry {
      tr: self.tokens_players.tr.clone(),
      id: self.tokens_players.id,
    };
    self.tokens_deregister_for_id(|id:PlayerId| id==player);
    self.save_access_now().map_err(|e|{
      // oof, the tokens are already out of the global map, but
      // not saved, so they might come back.  We need to leave
      // them here so they can be deleted later.
      self.tokens_players = old_tokens;
      e
    })?;
    // ppoint of no return
    (||{
      self.remove_clients(&[player].iter().cloned().collect(),
                          ErrorSignaledViaUpdate::TokenRevoked);
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
  }

  #[throws(MgmtError)]
  fn player_access_reset_redeliver(&mut self,
                                   accounts: &mut AccountsGuard,
                                   player: PlayerId,
                                   _auth: Authorisation<AccountName>,
                                   reset: bool)
                                   -> AccessTokenReport {
    let acctid = self.iplayers.byid(player)?.ipl.acctid;

    let access = {
      let (acct, _) = accounts.lookup(acctid)?;
      let access = acct.access.clone();
      let desc = access.describe_html();
      let now = Timestamp::now();
      let revk = TokenRevelationKey {
        account: (*acct.account).clone(),
        desc,
      };
      self.iplayers.byid_mut(player)?.ipl.tokens_revealed.entry(revk)
        .or_insert(TokenRevelationValue {
          latest: now,
          earliest: now,
        })
        .latest = now;
      access
    };
    
    if reset {
      self.invalidate_tokens(player)?;
      self.save_access_now()?;
    }

    let token : RawToken = if reset {

      let token = access
        .override_token()
        .cloned()
        .unwrap_or_else(||{
          RawToken::new_random()
        });
        
      let iad = InstanceAccessDetails {
        gref : self.gref.clone(),
        ident : player,
        acctid
      };
      self.token_register(token.clone(), iad);
      self.save_access_now()?;

      token

    } else {

      let tokens : ArrayVec<[&RawToken;2]> = {
        let players = GLOBAL.players.read().unwrap();
        self.tokens_players.tr.iter().
          filter(|&token| (||{
            let iad = players.get(token)?;
            if iad.ident != player { return None }
            if ! Arc::ptr_eq(&iad.gref.0, &self.gref.0) { return None }
            Some(())
          })() == Some(()))
          .take(2)
          .collect()
      };

      let token = match tokens.as_slice() {
        [] => throw!(ME::AuthorisationUninitialised),
        [token] => token,
        _ => {
          warn!("duplicate token for {}", player);
          throw!(ME::ServerFailure("duplicate token".to_string()));
        },
      };

      (*token).clone()
    };

    let ipl = &self.c.g.iplayers.byid(player)?.ipl;
    let gpl = self.c.g.gs.players.byid(player)?;

    let url = format!("{}/{}",
                      &config().public_url.trim_start_matches("/"),
                      token.0);
    let info = AccessTokenInfo { url };
    let report = access.deliver(&gpl, &ipl, info)?;
    report
  }

  #[throws(MgmtError)]
  pub fn player_access_reset(&mut self,
                             accounts: &mut AccountsGuard,
                             player: PlayerId,
                             auth: Authorisation<AccountName>)
                             -> AccessTokenReport {
    self.player_access_reset_redeliver(accounts, player, auth, true)?
  }

  #[throws(MgmtError)]
  pub fn player_access_redeliver(&mut self,
                                 accounts: &mut AccountsGuard,
                                 player: PlayerId,
                                 auth: Authorisation<AccountName>)
                                 -> AccessTokenReport {
    self.player_access_reset_redeliver(accounts, player, auth, false)?
  }

  pub fn modify_pieces(&mut self) -> ModifyingPieces {
    self.save_game_and_access_later();
    // want this to be borrowed from self, so that we tie it properly
    // to the same game.  But in practice we don't expect to write
    // bugs where we get different games mixed up.  Borrowing self
    // from the caller's pov is troublesome beczuse ultimately the
    // caller will need to manipulate various fields of Instance (so
    // we mustn't have a borrow of it).
    ModifyingPieces(())
  }

  fn token_register<Id:AccessId>(
    &mut self,
    token: RawToken,
    iad: InstanceAccessDetails<Id>
  ) {
    Id::tokens_registry(&mut self.c.g, PRIVATE_Y).tr.insert(token.clone());
    Id::global_tokens(PRIVATE_Y).write().unwrap().insert(token, iad);
  }

  fn forget_all_tokens<Id:AccessId>(tokens: &mut TokenRegistry<Id>) {
    let global : &RwLock<TokenTable<Id>> = AccessId::global_tokens(PRIVATE_Y);
    let mut global = global.write().unwrap();
    for t in tokens.tr.drain() { global.remove(&t); }
  }

  fn tokens_deregister_for_id<Id:AccessId, F: Fn(Id) -> bool
                              > (&mut self, oldid: F) {
    let mut tokens = AccessId::global_tokens(PRIVATE_Y).write().unwrap();
    tokens.retain(|k,v| {
      let remove = oldid(v.ident);
      if remove { Id::tokens_registry(self, PRIVATE_Y).tr.remove(k); }
      !remove
    });
  }

}

// ---------- save/load ----------

enum SavefilenameParseResult {
  NotGameFile,
  AccessFile,
  TempToDelete,
  GameFile {
    access_leaf : Vec<u8>,
    name : InstanceName,
  },
}

fn savefilename(name: &InstanceName, prefix: &str, suffix: &str) -> String {
  [ config().save_dir.as_str(), &"/", prefix ]
    .iter().map(Deref::deref)
    .chain(iter::once( name.to_string().as_str() ))
    .chain([ suffix ].iter().map(Deref::deref))
    .collect()
}

#[throws(anyhow::Error)]
fn savefilename_parse(leaf: &[u8]) -> SavefilenameParseResult {
  use SavefilenameParseResult::*;

  if leaf.starts_with(b"a-") { return AccessFile }
  let rhs = match leaf.strip_prefix(b"g-") {
    Some(rhs) => rhs,
    None => return NotGameFile,
  };
  let after_ftype_prefix = rhs;
  let rhs = str::from_utf8(rhs)?;
  let rcomp = rhs.rsplitn(2, ':').next().unwrap();
  if rcomp.find('.').is_some() { return TempToDelete }

  let name = InstanceName::from_str(&rhs)?;

  GameFile {
    access_leaf : [ b"a-", after_ftype_prefix ].concat(),
    name,
  }
}

impl InstanceGuard<'_> {
  #[throws(InternalError)]
  fn save_something(
    &self, prefix: &str,
    w: fn(s: &Self, w: &mut BufWriter<fs::File>)
          -> Result<(),rmp_serde::encode::Error>
  ) {
    let tmp = savefilename(&self.name, prefix,".tmp");
    let f = fs::File::create(&tmp)
      .with_context(||format!("save: create {:?}",&tmp))?;
    let mut f = BufWriter::new(f);
    w(self, &mut f)?;
    f.flush()
      .with_context(||format!("save: flush {:?}",&tmp))?;
    drop(
      f.into_inner().map_err(|e| { let e : io::Error = e.into(); e })
        .with_context(||format!("save: close {:?}",&tmp))?
    );
    let out = savefilename(&self.name, prefix,"");
    fs::rename(&tmp, &out).context("install")
      .with_context(||format!("save: install {:?} as {:?}", &tmp, &out))?;
    debug!("saved to {}", &out);
  }

  #[throws(InternalError)]
  pub fn save_game_now(&mut self) {
    if self.c.access_dirty {
      self.save_access_now()?;
    }
    self.save_something("g-", |s,w| {
      rmp_serde::encode::write_named(w, &s.c.g.gs)
    })?;
    self.c.game_dirty = false;
    debug!("saved (now) {}", &self.name);
  }

  #[throws(InternalError)]
  fn save_access_now(&mut self) {
    self.save_something("a-", |s,w| {
      let ipieces = &s.c.g.ipieces;
      let tokens_players : Vec<(&str, PlayerId)> = {
        let global_players = GLOBAL.players.read().unwrap();
        s.c.g.tokens_players.tr
          .iter()
          .map(|token|
               global_players.get(token)
               .map(|player| (token.0.as_str(), player.ident)))
          .flatten()
          .collect()
      };
      let aplayers = s.c.g.iplayers.iter().map(
        |(player, PlayerRecord { ipl, .. })|
        (player, ipl.clone())
      ).collect();
      let acl = s.c.g.acl.clone().into();
      let isa = InstanceSaveAccesses {
        ipieces, tokens_players, aplayers, acl
      };
      rmp_serde::encode::write_named(w, &isa)
    })?;
    self.c.access_dirty = false;
    info!("saved accesses for {}", &self.name);
  }

  #[throws(InternalError)]
  fn load_something<T:DeserializeOwned>(name: &InstanceName, prefix: &str)
                                        -> T {
    let inp = savefilename(name, prefix, "");
    let f = fs::File::open(&inp).with_context(|| inp.clone())?;
    let mut f = BufReader::new(f);
    let thing = rmp_serde::decode::from_read(&mut f)?;
    debug!("loaded from {:?}", &inp);
    thing
  }

  #[throws(StartupError)]
  fn load_game(accounts: &AccountsGuard,
               games: &mut GamesGuard,
               name: InstanceName) -> Option<InstanceRef> {
    {
      let mut st = GLOBAL.save_area_lock.lock().unwrap();
      let st = &mut *st;
      if st.is_none() {
        let lockfile = format!("{}/lock", config().save_dir);
        *st = Some((||{
          let file = File::create(&lockfile).context("open")?;
          file.try_lock_exclusive().context("lock")?;
          Ok::<_,AE>(file)
        })().context(lockfile).context("lock global save area")?);
      }
    }

    let InstanceSaveAccesses::<String,ActualPiecesLoaded>
    { tokens_players, mut ipieces, mut aplayers, acl }
    = match Self::load_something(&name, "a-") {
      Ok(data) => data,
      Err(e) => if (||{
        let ae = match &e { InternalError::Anyhow(ae) => Some(ae), _=>None }?;
        let ioe = ae.downcast_ref::<io::Error>()?;
        let is_enoent = ioe.kind() == io::ErrorKind::NotFound;
        is_enoent.as_option()
      })().is_some() {
        return None;
      } else {
        throw!(e);
      },
    };

    let mut gs : GameState = Self::load_something(&name, "g-")?;

    fn discard_mismatches<K:slotmap::Key, V1, V2>(
      primary:   &mut DenseSlotMap<K, V1>,
      secondary: &mut SecondarySlotMap<K, V2>,
    ) {
      primary.retain(|k,_v| secondary.contains_key(k));
      secondary.retain(|k,_v| primary.contains_key(k));
    }

    discard_mismatches(&mut gs.players,  &mut aplayers);
    discard_mismatches(&mut gs.pieces.0, &mut ipieces);
  
    let pu_bc = PlayerUpdates::new_begin(&gs);

    let iplayers : SecondarySlotMap<PlayerId, PlayerRecord> = {
      let a = aplayers;
      a.into_iter()
    }.map(|(player, ipl)| {
      let u = pu_bc.new();
      (player, PlayerRecord { u, ipl })
    }).collect();

    for mut p in gs.pieces.values_mut() {
      p.lastclient = Default::default();
      if let Some(held) = p.held {
        if !gs.players.contains_key(held) { p.held = None }
      }
    }

    let name = Arc::new(name);
    let (mut tokens_players, acctids_players) = {
      let mut tokens = Vec::with_capacity(tokens_players.len());
      let mut acctids = Vec::with_capacity(tokens_players.len());
      for (token, player) in tokens_players { if_chain! {
        if let Some(record) = iplayers.get(player);
        then {
          tokens.push((token, player));
          acctids.push(record.ipl.acctid);
        }
      }}
      (tokens, accounts.bulk_check(&acctids))
    };

    let g = Instance {
      gs, iplayers,
      acl: acl.into(),
      ipieces: PiecesLoaded(ipieces),
      name: name.clone(),
      clients : Default::default(),
      tokens_clients : Default::default(),
      tokens_players : Default::default(),
    };
    let cont = InstanceContainer {
      live: true,
      game_dirty: false,
      access_dirty: false,
      g,
    };
    let gref = InstanceRef(Arc::new(Mutex::new(cont)));
    let mut g = gref.lock().unwrap();
    for (token, _) in &tokens_players {
      g.tokens_players.tr.insert(RawToken(token.clone()));
    }
    let mut global = GLOBAL.players.write().unwrap();
    for ((token, player), acctid) in
      tokens_players.drain(0..)
      .zip(acctids_players)
    { if_chain!{
      if let Some(acctid) = acctid;
      let iad = InstanceAccessDetails {
        acctid,
        gref : gref.clone(),
        ident : player,
      };
      then { global.insert(RawToken(token), iad); }
    } }
    drop(global);
    drop(g);
    games.insert(name.clone(), gref.clone());
    info!("loadewd {:?}", &name);
    Some(gref)
  }
}

#[throws(anyhow::Error)]
pub fn load_games(accounts: &mut AccountsGuard,
                  games: &mut GamesGuard) {
  enum AFState { Found(PathBuf), Used };
  use AFState::*;
  use SavefilenameParseResult::*;
  let mut a_leaves = HashMap::new();
  for de in fs::read_dir(&config().save_dir)? {
    let de = de?;
    let leaf = de.file_name();
    (||{
      let leaf = leaf.as_bytes();
      match savefilename_parse(leaf)? {
        NotGameFile => {
        },
        TempToDelete => {
          fs::remove_file(de.path())
            .context("stale temporary file")?;
        },
        AccessFile => {
          a_leaves.entry(leaf.to_owned()).or_insert_with(
            || Found(de.path())
          );
        },
        GameFile { access_leaf, name } => {
          InstanceGuard::load_game(accounts, games, name)?;
          a_leaves.insert(access_leaf, Used);
        },
      }
      <Result<_,anyhow::Error>>::Ok(())
    })().with_context(|| format!("leaf={:?}", leaf))?;
  }
  (||{
    for (leaf, state) in &a_leaves {
      if let Found(path) = state {
        fs::remove_file(&path)
          .with_context(|| format!("leaf={:?}", leaf))?;
      }
    }
    <Result<_,anyhow::Error>>::Ok(())
  })().context("cleaning up stale files")?;
  info!("loaded games");
}

// ---------- Tokens / TokenTable / AccessId ----------

pub type TokenTable<Id> = HashMap<RawToken, InstanceAccessDetails<Id>>;

pub trait AccessId : Copy + Clone + 'static {
  type Error : Into<OnlineError>;
  const ERROR : Self::Error;
  fn global_tokens(_:PrivateCaller) -> &'static RwLock<TokenTable<Self>>;
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self>;
}

#[derive(Debug,Copy,Clone,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[derive(Error)]
#[error("Player not found")]
pub struct PlayerNotFound;

impl AccessId for PlayerId {
  type Error = PlayerNotFound;
  const ERROR : PlayerNotFound = PlayerNotFound;
  fn global_tokens(_: PrivateCaller) -> &'static RwLock<TokenTable<Self>> {
    &GLOBAL.players
  }
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self> {
    &mut ig.tokens_players
  }
}
impl AccessId for ClientId {
  type Error = OnlineError;
  const ERROR : OnlineError = NoClient;
  fn global_tokens(_: PrivateCaller) -> &'static RwLock<TokenTable<Self>> {
    &GLOBAL.clients
  }
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self> {
    &mut ig.tokens_clients
  }
}

impl RawToken {
  fn new_random() -> Self {
    let mut rng = thread_rng();
    let token = RawToken (
      repeat_with(|| rng.sample(Alphanumeric))
        .take(64).collect()
    );
    token
  }
}

pub fn lookup_token<Id : AccessId>(s : &RawTokenVal)
      -> Result<InstanceAccessDetails<Id>, Id::Error> {
  Id::global_tokens(PRIVATE_Y).read().unwrap().get(s).cloned()
    .ok_or(Id::ERROR)
}

impl<'r, Id> FromParam<'r> for InstanceAccess<'r, Id>
  where Id : AccessId, OE : From<Id::Error>
{
  type Error = OE;
  #[throws(OE)]
  fn from_param(param: &'r RawStr) -> Self {
    let g = Id::global_tokens(PRIVATE_Y).read().unwrap();
    let token = RawTokenVal::from_str(param.as_str());
    let i = g.get(token).ok_or(Id::ERROR)?;
    InstanceAccess { raw_token : token, i : i.clone() }
  }
}

#[throws(OE)]
pub fn record_token<Id : AccessId> (
  ig : &mut InstanceGuard,
  iad : InstanceAccessDetails<Id>
) -> RawToken {
  let token = RawToken::new_random();
  ig.token_register(token.clone(), iad);
  token
}

#[throws(E)]
pub fn process_all_players_for_account<
    E: Error,
    F: FnMut(&mut InstanceGuard<'_>, PlayerId) -> Result<(),E>
    >
  (games: &mut GamesGuard, acctid: AccountId, mut f: F)
{
  for gref in games.values() {
    let c = gref.lock_even_poisoned();
    let remove : Vec<_> = c.g.iplayers.iter().filter_map(|(player,pr)| {
      if pr.ipl.acctid == acctid { Some(player) } else { None }
    }).collect();
    let mut ig = InstanceGuard { gref: gref.clone(), c };
    for player in remove.into_iter() {
      f(&mut ig, player)?;
    }
  }
}

// ========== instance pieces data access ==========

impl PiecesLoaded {
  pub fn get(&self, piece: PieceId) -> Option<&Box<dyn Piece>> {
    self.0.get(piece)
  }

  pub fn as_mut(&mut self, _: ModifyingPieces) -> &mut ActualPiecesLoaded {
    &mut self.0
  }
}

// ---------- gamestate pieces table ----------

impl Deref for Pieces {
  type Target = ActualPieces;
  fn deref(&self) -> &ActualPieces { &self.0 }
}

impl Pieces {
  pub fn get_mut(&mut self, piece: PieceId) -> Option<&mut PieceState> {
    self.0.get_mut(piece)
  }
  pub fn values_mut(&mut self) -> sm::ValuesMut<PieceId, PieceState> {
    self.0.values_mut()
  }
  pub fn as_mut(&mut self, _: ModifyingPieces) -> &mut ActualPieces {
    &mut self.0
  }
}

impl ById for Pieces {
  type Id = PieceId;
  type Entry = PieceState;
  type Error = OnlineError;
  #[throws(OE)]
  fn byid(&self, piece: PieceId) -> &PieceState {
    self.get(piece).ok_or(OE::PieceGone)?
  }
  #[throws(OE)]
  fn byid_mut(&mut self, piece: PieceId) -> &mut PieceState {
    self.get_mut(piece).ok_or(OE::PieceGone)?
  }
}

/*impl<'p> IntoIterator for &'p Pieces {
  type Item = (PieceId, &'p PieceState);
  type IntoIter = sm::Iter<'p, PieceId, PieceState>;
  fn into_iter(self) -> Self::IntoIter { (&self.0).into_iter() }
}*/
impl<'p> IntoIterator for &'p mut Pieces {
  type Item = (PieceId, &'p mut PieceState);
  type IntoIter = sm::IterMut<'p, PieceId, PieceState>;
  fn into_iter(self) -> Self::IntoIter { (&mut self.0).into_iter() }
}

// ========== background maintenance ==========

// ---------- delayed game save ----------

impl InstanceGuard<'_> {
  pub fn save_game_later(&mut self) {
    if self.c.game_dirty { return }
    GLOBAL.dirty.lock().unwrap().push_back(self.gref.clone());
    self.c.game_dirty = true;
  }

  pub fn save_game_and_access_later(&mut self) {
    if self.c.access_dirty { return }
    self.save_game_later();
    self.c.access_dirty = true;
  }
}

pub fn game_flush_task() {
  let mut inner_queue = VecDeque::new();
  loop {
    {
      mem::swap(&mut inner_queue, &mut *GLOBAL.dirty.lock().unwrap());
    }
    thread::sleep(GAME_SAVE_LAG);
    for _ in 0..inner_queue.len() {
      let ent = inner_queue.pop_front().unwrap();
      let mut ig = match ent.lock() { Ok(ig) => ig, _ => continue/*ah well*/ };
      if !ig.c.game_dirty { continue }
      match ig.save_game_now() {
        Ok(_) => {
          assert!(!ig.c.game_dirty);
        },
        Err(e) => {
          // todo: notify the players
          error!("save error! name={:?}: {}", &ig.name, &e);
          mem::drop(ig);
          inner_queue.push_back(ent); // oof
        }
      }
    }
  }
}

// ---------- client expiry ----------

fn client_expire_old_clients() {
  let mut expire = vec![];
  let max_age = Instant::now() - MAX_CLIENT_INACTIVITY;

  trait ClientIterator {
    type Ret;
    fn iter<'g>(&mut self, gref: &'g InstanceRef, max_age: Instant)
            -> (MutexGuard<'g, InstanceContainer>, Option<Self::Ret>) {
      let c = gref.lock_even_poisoned();
      let ret = 'ret: loop {
        for (client, cl) in &c.g.clients {
          if cl.lastseen > max_age { continue }
          let ret = self.old(client);
          if ret.is_some() { break 'ret ret }
        }
        break 'ret None;
      };
      (c,ret)
    }
    fn old(&mut self, client: ClientId) -> Option<Self::Ret>;
  }

  for gref in GLOBAL.games_table.read().unwrap().values() {
    struct Any;
    impl ClientIterator for Any {
      type Ret = ();
      fn old(&mut self, _client: ClientId) -> Option<()> {
        return Some(())
      }
    }
    if let (_, Some(())) = Any.iter(&gref, max_age) {
      expire.push(gref.clone());
    }
  }
  for gref in expire.drain(..) {
    #[derive(Debug)]
    struct Now(HashSet<ClientId>);
    impl ClientIterator for Now {
      type Ret = Impossible;
      fn old(&mut self, client: ClientId)
             -> Option<Impossible> {
        self.0.insert(client);
        None
      }
    }

    let mut now = Now(Default::default());
    let (mut c, _) = now.iter(&gref, max_age);
    c.g.clients.retain(|c,_| !now.0.contains(&c));
    let mut gref = InstanceGuard { c, gref: gref.clone() };
    debug!("expiring client {:?}", &now);
    gref.tokens_deregister_for_id::<ClientId,_>(|c| now.0.contains(&c));
  }
}

pub fn client_periodic_expiry() {
  loop {
    sleep(MAX_CLIENT_INACTIVITY);
    client_expire_old_clients();
  }
}

// ---------- log expiry ----------

fn global_expire_old_logs() {
  let cutoff = Timestamp(Timestamp::now().0 - MAX_LOG_AGE.as_secs());

  let mut want_expire = vec![];

  let read = GLOBAL.games_table.read().unwrap();
  for gref in read.values() {
    if gref.lock_even_poisoned().g.gs.want_expire_some_logs(cutoff) {
      want_expire.push(gref.clone())
    }
  }
  drop(read);

  for gref in want_expire.drain(..) {
    let mut g = gref.lock_even_poisoned();
    info!("expiring old log entries in {:?}", &g.g.name);
    g.g.gs.do_expire_old_logs(cutoff);
  }
}

pub fn logs_periodic_expiry() {
  loop {
    sleep(MAX_LOG_AGE/10);
    global_expire_old_logs();
  }
}
