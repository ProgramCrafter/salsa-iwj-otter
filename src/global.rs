// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![allow(clippy::let_and_return)]

use crate::imports::*;

use std::sync::PoisonError;
use slotmap::dense as sm;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

const MAX_CLIENT_INACTIVITY : Duration = Duration::from_secs(200);

const GAME_SAVE_LAG : Duration = Duration::from_millis(500);

const MAX_LOG_AGE : Duration = Duration::from_secs(10 * 86400);

#[derive(Hash,Ord,PartialOrd,Eq,PartialEq,Serialize)]
#[repr(transparent)]
pub struct RawTokenVal(str);

// ---------- public data structure ----------

pub type InstanceName = ScopedName;

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
}

pub struct PlayerRecord {
  pub u: PlayerUpdates,
  pub pst: PlayerState,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PlayerState {
  pub account: AccountName,
  pub nick: String,
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
  // lock hierarchy: InstanceContainer < games < {players, clients}
  // (in order of criticality (perf impact); outermost first, innermost last)
  games   : RwLock<HashMap<Arc<InstanceName>,InstanceRef>>,
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
  config  : RwLock<Arc<ServerConfig>>,
  dirty   : Mutex<VecDeque<InstanceRef>>,
  save_area_lock : Mutex<Option<File>>,
  pub shapelibs : RwLock<shapelib::Registry>,
}

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
  aplayers: SecondarySlotMap<PlayerId, PlayerState>,
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

  fn lock_even_poisoned(&self) -> MutexGuard<InstanceContainer> {
    match self.0.lock() {
      Ok(g) => g,
      Err(poison) => poison.into_inner(),
    }
  }
}

impl Instance {
  /// Returns `None` if a game with this name already exists
  #[allow(clippy::new_ret_no_self)]
  #[throws(MgmtError)]
  pub fn new(name: InstanceName, gs: GameState)
             -> InstanceRef {
    let name = Arc::new(name);

    let g = Instance {
      name : name.clone(),
      gs,
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

    // We hold the GLOBAL.games lock while we save the new game.
    // That lock is not on the hot path.
    let mut games = GLOBAL.games.write().unwrap();
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
  pub fn lookup_by_name(name: &InstanceName) -> InstanceRef {
    GLOBAL.games.read().unwrap()
      .get(name)
      .ok_or(MgmtError::GameNotFound)?
      .clone()
  }

  #[throws(InternalError)]
  pub fn destroy_game(mut g: InstanceGuard) {
    let a_savefile = savefilename(&g.name, "a-", "");

    let mut gw = GLOBAL.games.write().unwrap();
    let g_file = savefilename(&g.name, "g-", "");
    fs::remove_file(&g_file).context("remove").context(g_file)?;

    (||{ // Infallible:
      g.c.live = false;
      gw.remove(&g.name);
      InstanceGuard::forget_all_tokens(&mut g.tokens_clients);
      InstanceGuard::forget_all_tokens(&mut g.tokens_players);
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

  pub fn list_names(scope: Option<&AccountScope>)
                    -> Vec<Arc<InstanceName>> {
    let games = GLOBAL.games.read().unwrap();
    let out : Vec<Arc<InstanceName>> =
      games.keys()
      .filter(|k| scope == None || scope == Some(&k.scope))
      .cloned()
      .collect();
    out
  }
}

// ---------- Simple trait implementations ----------

impl Deref for InstanceGuard<'_> {
  type Target = Instance;
  fn deref(&self) -> &Instance { &self.c.g }
}
impl DerefMut for InstanceGuard<'_> {
  fn deref_mut(&mut self) -> &mut Instance { &mut self.c.g }
}

// ---------- Player and token functionality ----------

impl InstanceGuard<'_> {
  /// caller is responsible for logging; threading it through
  /// proves the caller has a log entry.
  #[throws(MgmtError)]
  pub fn player_new(&mut self, newplayer: PlayerState, tz: Timezone,
                    logentry: LogEntry) -> (PlayerId, LogEntry) {
    // saving is fallible, but we can't attempt to save unless
    // we have a thing to serialise with the player in it
    if self.c.g.gs.players.values().any(|a| a == &newplayer.account) {
      Err(MgmtError::AlreadyExists)?;
    }
    if self.c.g.iplayers.values().any(|pl| pl.pst.nick == newplayer.nick) {
      Err(MgmtError::NickCollision)?;
    }
    let player = self.c.g.gs.players.insert(newplayer.account.clone());
    let u = PlayerUpdates::new_begin(&self.c.g.gs).new();
    let record = PlayerRecord { u, pst: newplayer };
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

  //  #[throws(ServerFailure)]
  //  https://github.com/withoutboats/fehler/issues/62
  pub fn player_remove(&mut self, oldplayer: PlayerId)
                       -> Result<(Option<AccountName>, Option<PlayerState>),
                                 InternalError> {
    // We have to filter this player out of everything
    // Then save
    // Then send updates
    // We make a copy so if the save fails, we can put everything back

    let mut players = self.c.g.gs.players.clone();
    let old_account = players.remove(oldplayer);

    // New state
    let mut gs = GameState {
      // These parts are straightforward and correct
      table_size : self.c.g.gs.table_size,
      gen : self.c.g.gs.gen,
      max_z : self.gs.max_z.clone(),
      players,
      // These have special handling
      log : Default::default(),
      pieces : Default::default(),
    };

    let mut updated_pieces = vec![];
    
    // drop order is reverse of creation order, so create undo
    // after all the things it will reference
    let mut undo : Vec<Box<dyn FnOnce(&mut InstanceGuard)>> = vec![];

    // Arrange gs.pieces
    for (piece,p) in &mut self.c.g.gs.pieces {
      if p.held == Some(oldplayer) {
        p.held = None;
        updated_pieces.push(piece);
      }
    }
    undo.push(Box::new(|ig| for &piece in &updated_pieces {
      (||Some({
        ig.c.g.gs.pieces.get_mut(piece)?.held = Some(oldplayer);
      }))();
    }));

    // Handle gs.log:
    // Installs gs as the new game state, stealing the log
    let mut swap_things = |ig: &mut InstanceGuard| {
      mem::swap(&mut ig.c.g.gs.log, &mut gs.log);
      mem::swap(&mut ig.c.g.gs,     &mut gs,   );
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

    let old_pst = (||{
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

      let mut clients_to_remove = HashSet::new();
      self.clients.retain(|k,v| {
        let remove = v.player == oldplayer;
        if remove { clients_to_remove.insert(k); }
        !remove
      });
      let pst = if let Some(PlayerRecord { u: mut updates, pst })
        = self.iplayers.remove(oldplayer)
      {
        updates.push(PreparedUpdate {
          gen: self.c.g.gs.gen,
          when: Instant::now(),
          us : vec![ PreparedUpdateEntry::Error(
            None,
            ErrorSignaledViaUpdate::PlayerRemoved
          )],
        });
        Some(pst)
      } else {
        None
      };
      self.tokens_deregister_for_id(|id:PlayerId| id==oldplayer);
      self.tokens_deregister_for_id(|id| clients_to_remove.contains(&id));
      self.save_access_now().unwrap_or_else(
        |e| warn!(
          "trouble garbage collecting accesses for deleted player: {:?}",
          &e)
      );
      pst
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    Ok((old_account, old_pst))
  }

  #[throws(MgmtError)]
  pub fn player_access_register_fixed(&mut self,
                                      player: PlayerId, token: RawToken,
                                      _safe: Authorised<RawToken>
  ) {
    // xxx get rid of this or something ?
    self.tokens_deregister_for_id(|id:PlayerId| id==player);
    let iad = InstanceAccessDetails {
      gref : self.gref.clone(),
      ident : player
    };
    self.token_register(token, iad);
    self.save_access_now()?;
  }

  #[throws(MgmtError)]
  pub fn player_access_reset(&mut self, player: PlayerId)
                             -> Option<AccessTokenReport> {
    // xxx call this function when access changes

    let pst = self.c.g.iplayers.get(player)
      .ok_or(MgmtError::PlayerNotFound)?
      .pst;
    self.save_access_now()?;

    let access = AccountRecord::with_entry_mut(&pst.account, |acct|{
      let acct = acct.ok_or(MgmtError::AccountNotFound)?;
      let access = acct.access;
      let desc = access.describe_html();
      let now = Timestamp::now();
      acct.tokens_revealed.entry(desc)
        .or_insert(TokenRevelation {
          latest: now,
          earliest: now,
        })
        .latest = now;
      acct.expire_tokens_revealed();
      Ok::<_,MgmtError>(access.clone())
    }).map_err(|(e,_)|e)??;

    let token = access
      .override_token()
      .cloned()
      .unwrap_or_else(||{
        RawToken::new_random()
        // xxx disconnect everyone else
      });

    let iad = InstanceAccessDetails {
      gref : self.gref.clone(),
      ident : player
    };
    self.token_register(token.clone(), iad);

    let report = AccessTokenReport {
      url: format!("http://localhost:8000/{}", token.0), // xxx
    };
    let report = access
      .server_deliver(&pst, &report)?;
    report.cloned()
  }

  #[throws(MgmtError)]
  pub fn players_access_report(&mut self, players: &[PlayerId])
                               -> Vec<Vec<RawToken>> {
    let mut wanted = {
      let mut wanted = SecondarySlotMap::new();
      for &player in players {
        wanted.insert(player, vec![]);
      }
      let global = GLOBAL.players.read().unwrap();
      for token in &self.tokens_players.tr {
        (||{
          let iad = global.get(token)?;
          let e = wanted.get_mut(iad.ident)?;
          e.push(token.clone());
          Some(())
        })();
      }
      wanted
    };
    #[allow(clippy::or_fun_call)]
    let out = players.iter().map(|&player| {
      let mut tokens = wanted.remove(player)
        .unwrap_or(vec![] /* dupe, somehow */);
      tokens.sort_unstable();
      tokens
    }).collect();
    out
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
  const ENCODE : percent_encoding::AsciiSet =
    percent_encoding::NON_ALPHANUMERIC.remove(b':');

  [ config().save_directory.as_str(), &"/", prefix ]
    .iter().map(Deref::deref)
    .chain( utf8_percent_encode(&format!("{}", name), &ENCODE ))
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
  if rhs.rfind('.').is_some() { return TempToDelete }

  let name : String = percent_decode_str(rhs).decode_utf8()?.into();
  let name = ScopedName::from_str(&name)?;

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
    debug!("saved (now) {:?}", &self.name);
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
        |(player, PlayerRecord { pst, .. })|
        (player, pst.clone())
      ).collect();
      let isa = InstanceSaveAccesses { ipieces, tokens_players, aplayers };
      rmp_serde::encode::write_named(w, &isa)
    })?;
    self.c.access_dirty = false;
    info!("saved accesses for {:?}", &self.name);
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
  fn load_game(name: InstanceName) -> InstanceRef {
    {
      let mut st = GLOBAL.save_area_lock.lock().unwrap();
      let st = &mut *st;
      if st.is_none() {
        let lockfile = format!("{}/lock", config().save_directory);
        *st = Some((||{
          let file = File::create(&lockfile).context("open")?;
          file.try_lock_exclusive().context("lock")?;
          Ok::<_,AE>(file)
        })().context(lockfile).context("lock global save area")?);
      }
    }
    let InstanceSaveAccesses::<String,ActualPiecesLoaded>
    { mut tokens_players, mut ipieces, mut aplayers }
    = Self::load_something(&name, "a-")
      .or_else(|e| {
        if let InternalError::Anyhow(ae) = &e {
          if let Some(ioe) = ae.downcast_ref::<io::Error>() {
            if ioe.kind() == io::ErrorKind::NotFound {
              return Ok(Default::default())
            }
          }
        }
        Err(e)
      })?;

    let mut gs : GameState = Self::load_something(&name, "g-")?;

    fn discard_mismatches<K:slotmap::Key, V1, V2>(
      primary:   &mut DenseSlotMap<K, V1>,
      secondary: &mut SecondarySlotMap<K, V2>,
    ) {
      primary.retain(|k,_v| secondary.contains_key(k));
      secondary.retain(|k,_v| primary.contains_key(k));
    }

    discard_mismatches(&mut gs.players, &mut aplayers);
    discard_mismatches(&mut gs.pieces,  &mut ipieces);
  
    let pu_bc = PlayerUpdates::new_begin(&gs);

    let iplayers = {
      let a = aplayers;
      a.drain()
    }.map(|(player, pst)| {
      let u = pu_bc.new();
      (player, PlayerRecord { u, pst })
    }).collect();

    for mut p in gs.pieces.values_mut() {
      p.lastclient = Default::default();
      if let Some(held) = p.held {
        if !gs.players.contains_key(held) { p.held = None }
      }
    }

    let name = Arc::new(name);
    tokens_players.retain(
      |&(_,player)| gs.players.contains_key(player)
    );

    let g = Instance {
      gs, iplayers,
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
    for (token, player) in tokens_players.drain(0..) {
      let iad = InstanceAccessDetails {
        gref : gref.clone(),
        ident : player,
      };
      global.insert(RawToken(token), iad);
    }
    drop(global);
    drop(g);
    GLOBAL.games.write().unwrap().insert(name.clone(), gref.clone());
    info!("loadewd {:?}", &name);
    gref
  }
}

#[throws(anyhow::Error)]
pub fn load_games() {
  enum AFState { Found(PathBuf), Used };
  use AFState::*;
  use SavefilenameParseResult::*;
  let mut a_leaves = HashMap::new();
  for de in fs::read_dir(&config().save_directory)? {
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
          InstanceGuard::load_game(name)?;
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
  fn global_tokens(_:PrivateCaller) -> &'static RwLock<TokenTable<Self>>;
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self>;
  const ERROR : OnlineError;
}

impl AccessId for PlayerId {
  const ERROR : OnlineError = NoPlayer;
  fn global_tokens(_: PrivateCaller) -> &'static RwLock<TokenTable<Self>> {
    &GLOBAL.players
  }
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self> {
    &mut ig.tokens_players
  }
}
impl AccessId for ClientId {
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
      -> Result<InstanceAccessDetails<Id>, OE> {
  Id::global_tokens(PRIVATE_Y).read().unwrap().get(s).cloned()
    .ok_or(Id::ERROR)
}

impl<'r, Id> FromParam<'r> for InstanceAccess<'r, Id>
  where Id : AccessId
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

  for gref in GLOBAL.games.read().unwrap().values() {
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

  let read = GLOBAL.games.read().unwrap();
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

// ========== server config ==========

const DEFAULT_CONFIG_FILENAME : &str = "server.toml";

const DEFAULT_SAVE_DIRECTORY : &str = "save";
const DEFAULT_COMMAND_SOCKET : &str = "command.socket"; // in save dir
const DEFAULT_TEMPLATE_DIR : &str = "templates";
const DEFAULT_LIBRARY_DIR : &str = "library";
const DEFAULT_WASM_DIR : &str = "target/packed-wasm";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub save_directory: Option<String>,
  pub command_socket: Option<String>,
  pub debug: Option<bool>,
  pub http_port: Option<u16>,
  pub rocket_workers: Option<u16>,
  pub template_dir: Option<String>,
  pub wasm_dir: Option<String>,
  pub log: Option<toml::Value>,
  pub bundled_sources: Option<String>,
  pub shapelibs: Option<Vec<shapelib::Config1>>,
}

#[derive(Debug,Clone)]
pub struct ServerConfig {
  pub save_directory: String,
  pub command_socket: String,
  pub debug: bool,
  pub http_port: Option<u16>,
  pub rocket_workers: u16,
  pub template_dir: String,
  pub wasm_dir: String,
  pub log: LogSpecification,
  pub bundled_sources: String,
  pub shapelibs: Vec<shapelib::Config1>,
}

impl TryFrom<ServerConfigSpec> for ServerConfig {
  type Error = AE;
  #[throws(Self::Error)]
  fn try_from(spec: ServerConfigSpec) -> ServerConfig {
    let ServerConfigSpec {
      save_directory, command_socket, debug,
      http_port, rocket_workers, template_dir, wasm_dir,
      log, bundled_sources, shapelibs,
    } = spec;

    let save_directory = save_directory
      .unwrap_or_else(|| DEFAULT_SAVE_DIRECTORY.to_owned());

    let mut command_socket = command_socket
      .unwrap_or_else(|| DEFAULT_COMMAND_SOCKET.to_owned());
    if !command_socket.starts_with('/') {
      command_socket = format!("{}/{}", save_directory, command_socket);
    }

    let debug = debug.unwrap_or(cfg!(debug_assertions));
    let rocket_workers = rocket_workers.unwrap_or(
      if debug { 20 } else { 1000 });

    let template_dir = template_dir
      .unwrap_or_else(|| DEFAULT_TEMPLATE_DIR.to_owned());

    let log = {
      use toml::Value::Table;
      match log {
        None => Table(Default::default()),
        Some(log @Table(_)) => log,
        Some(x) => throw!(anyhow!(
          r#"wanted table for "log" config key, not {}"#,
          x.type_str())
        ),
      }
    };
    let log = toml::to_string(&log)?;
    let log = LogSpecification::from_toml(&log)
      .context("log specification")?;

    let bundled_sources = bundled_sources
      .unwrap_or_else(|| save_directory.clone());

    let shapelibs = shapelibs.unwrap_or_else(
      ||vec![ shapelib::Config1::PathGlob(
        format!("{}/*.toml", DEFAULT_LIBRARY_DIR)
      )]);

    let wasm_dir = wasm_dir.unwrap_or_else(|| DEFAULT_WASM_DIR.to_owned());

    ServerConfig {
      save_directory, command_socket, debug,
      http_port, rocket_workers, template_dir, wasm_dir,
      log, bundled_sources, shapelibs,
    }
  }
}

pub fn config() -> Arc<ServerConfig> {
  GLOBAL.config.read().unwrap().clone()
}

fn set_config(config: ServerConfig) {
  *GLOBAL.config.write().unwrap() = Arc::new(config)
}

impl ServerConfig {
  #[throws(StartupError)]
  pub fn read(config_filename: Option<&str>) {
    let config_filename = config_filename
      .unwrap_or_else(|| DEFAULT_CONFIG_FILENAME);
    let mut buf = String::new();
    File::open(&config_filename).with_context(||config_filename.to_string())?
      .read_to_string(&mut buf)?;
    let config : ServerConfigSpec = toml::de::from_str(&buf)?;
    let config = config.try_into()?;
    set_config(config);
  }
}

impl Default for ServerConfig {
  fn default() -> ServerConfig {
    let spec : ServerConfigSpec = toml::de::from_str("")
      .expect("parse empty string as ServerConfigSpec");
    spec.try_into().expect("empty spec into config")
  }
}
