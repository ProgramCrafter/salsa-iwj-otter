
use crate::imports::*;
use lazy_static::lazy_static;

use std::sync::PoisonError;

#[allow(dead_code)]
const SAVE_DIRECTORY : &str = "save";

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken (pub String);

// ---------- public data structure ----------

#[derive(Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct InstanceName {
  pub scope: ManagementScope,
  pub scoped_name: String,
}

#[derive(Debug,Clone)]
pub struct InstanceRef (Arc<Mutex<InstanceContainer>>);

#[derive(Debug)]
pub struct Instance {
  pub name : Arc<InstanceName>,
  pub gs : GameState,
  pub clients : DenseSlotMap<ClientId,Client>,
  pub updates : SecondarySlotMap<PlayerId, PlayerUpdates>,
  pub tokens_players : TokenRegistry<PlayerId>,
  pub tokens_clients : TokenRegistry<ClientId>,
}

#[derive(Debug,Clone,Deserialize,Serialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
pub enum ManagementScope {
  Server,
  Unix { user : String /* username, so filename-safe */ },
}

#[derive(Debug)] 
pub struct Client {
  pub player : PlayerId,
}

pub type PlayerMap = DenseSlotMap<PlayerId,PlayerState>;
/* xxx
#[derive(Serialize,Deserialize)]
#[repr(transparent)]
pub struct PlayerMap(ActualPlayerMap);
impl Deref for PlayerMap {
  type Target = ActualPlayerMap;
  fn deref(&self) -> &ActualPlayerMap { return self.0 }
}
// No DerefMut: callers in this module should access .0 directly
// This prevents accidental modification of Players without appropriate
// synchrnoisation.
*/

/// UPDATE RELIABILITY/PERSISTENCE RULES
///
/// From the caller's point of view
///
/// We offer atomic creation/modification/destruction of:
///
///    * Games (roughtly, a map from InstanceName to GameState;
///             includes any player)
///
///    * Player access tokens, for an existing (game, player)
///
///    * Clients (for an existing (game, player)
///
/// We also offer atomic destruction of:
///
///    * Games
///
///    * Players
///
/// All of the above, except clients, are persistent, in the sense
/// that a server restart will preserve them.
///
/// The general code gets mutable access to the GameState.  We offer
/// post-hoc saving of a modified game.  This should not be used for
/// player changes.  For other changes, if the save fails, a server
/// restart may be a rewind.  This relaxation of the rules is
/// necessary avoid complicated three-phase commit on GameState update
/// for routine game events.
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
  pub raw_token : &'i str,
  pub i : InstanceAccessDetails<Id>,
}

// ========== internal data structures ==========

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

#[derive(Default)]
struct Global {
  // lock hierarchy: InstanceContainer < games < {players, clients}
  // (in order of criticality (perf impact); outermost first, innermost last)
  games   : RwLock<HashMap<Arc<InstanceName>,InstanceRef>>,
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
  // xxx delete instances at some point!
}

#[derive(Debug)]
pub struct InstanceContainer {
  live : bool,
  g : Instance,
}

#[derive(Debug,Serialize,Deserialize)]
struct InstanceSaveAccesses<RawTokenStr> {
  tokens_players : Vec<(RawTokenStr, PlayerId)>,
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

impl Borrow<str> for RawToken {
  fn borrow(&self) -> &str { &self.0 }
}

// ---------- Main API for instance lifecycle ----------

impl InstanceRef {
  #[throws(InstanceLockError)]
  pub fn lock<'g>(&'g self) -> InstanceGuard<'g> {
    let c = self.0.lock()?;
    if !c.live { throw!(InstanceLockError::GameBeingDestroyed) }
    InstanceGuard { c, gref: self.clone() }
  }
}

impl Instance {
  /// Returns `None` if a game with this name already exists
  #[throws(MgmtError)]
  pub fn new(name: InstanceName, gs: GameState) -> InstanceRef {
    let name = Arc::new(name);

    let g = Instance {
      name : name.clone(),
      gs,
      clients : Default::default(),
      updates : Default::default(),
      tokens_players : Default::default(),
      tokens_clients : Default::default(),
    };

    let cont = InstanceContainer {
      live : true,
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

  #[throws(ServerFailure)]
  pub fn destroy_game(mut g: InstanceGuard) {
    let a_savefile = InstanceGuard::savefile(&g.name, "a-", "");

    let mut gw = GLOBAL.games.write().unwrap();
    fs::remove_file(InstanceGuard::savefile(&g.name, "g-", ""))?;

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
        |e| eprintln!("warning: failed to delete stale auth file {:?}: {:?}",
                      &a_savefile, e)
        // apart from that, ignore the error.  someone will clean it up
        // later.   xxx periodic cleanup ?
      );
  }

  pub fn list_names(scope: Option<&ManagementScope>)
                    -> Vec<Arc<InstanceName>> {
    let games = GLOBAL.games.read().unwrap();
    let out : Vec<Arc<InstanceName>> =
      games.keys()
      .filter(|k| scope == None || scope == Some(&k.scope))
      .map(|k| k.clone())
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
  #[throws(ServerFailure)]
  pub fn player_new(&mut self, newplayer: PlayerState) -> PlayerId {
    // saving is fallible, but we can't attempt to save unless
    // we have a thing to serialise with the player in it
    let player = self.c.g.gs.players.insert(newplayer);
    self.save_game_now().map_err(|e|{
      self.c.g.gs.players.remove(player);
      e
    })?;
    (||{
      self.c.g.updates.insert(player, Default::default());
      // xxx send log message, should be provided by caller
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
    player
  }

//  #[throws(ServerFailure)]
  pub fn player_remove(&mut self, oldplayer: PlayerId) -> Result<(),ServerFailure> {
    // We have to filter this player out of everything
    // Then save
    // Then send updates
    // We make a copy so if the save fails, we can put everything back

    let mut players = self.c.g.gs.players.clone();
    players.remove(oldplayer);

    // New state
    let mut gs = GameState {
      // These parts are straightforward and correct
      gen : self.c.g.gs.gen,
      max_z : self.gs.max_z,
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
      ig.c.g.gs.pieces[piece].held = Some(oldplayer)
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

    (||{
      for &piece in &updated_pieces {
        self.c.g.gs.pieces[piece].gen = self.c.g.gs.gen;
      }

      let lens = TransparentLens { };
      let estimate = updated_pieces.len() + 1;
      let mut buf = PrepareUpdatesBuffer::new(self, None, Some(estimate));
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
      if let Some(mut updates) = self.updates.remove(oldplayer) {
        updates. push(PreparedUpdate {
          gen: self.c.g.gs.gen,
          us : vec![ PreparedUpdateEntry::Error(
            ErrorSignaledViaUpdate::PlayerRemoved
          )],
        });
      }
      self.tokens_deregister_for_id(|id:PlayerId| id==oldplayer);
      self.tokens_deregister_for_id(|id| clients_to_remove.contains(&id));
      self.save_access_now().unwrap_or_else(
        |e| eprintln!(
          "trouble garbage collecting accesses for deleted player: {:?}",
          &e)
      );
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    Ok(())
  }

  #[throws(OE)]
  pub fn player_access_register_xxx(&mut self, token: RawToken, player: PlayerId) {
    // xxx server has to not allow even facilitators to define tokens
    // xxx this fn should become part of player_access_add
    let iad = InstanceAccessDetails {
      gref : self.gref.clone(),
      ident : player
    };
    self.token_register(token, iad);
  }

  #[throws(OE)]
  pub fn player_access_reset(&mut self, player: PlayerId) -> RawToken {
    // tokens can't persist unless game is never destroyed ?
    // so a game is like a tables, and persistent
    // xxx boxes feature maybe
    self.tokens_deregister_for_id(|id:PlayerId| id==player);
    self.save_access_now()?;
    let iad = InstanceAccessDetails {
      gref : self.gref.clone(),
      ident : player
    };
    let token = RawToken::new_random()?;
    self.token_register(token.clone(), iad);
    self.save_access_now()?;
    // If the save fails, we don't return the token so no-one can use
    // it.  Therefore we don't need to bother deleting it.
    token
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

impl InstanceGuard<'_> {
  fn savefile(name: &InstanceName, prefix: &str, suffix: &str) -> String {
    let scope_prefix = { use ManagementScope::*; match &name.scope {
      Server => format!(""),
      Unix{user} => { format!("{}:", user) },
    } };
    iter::once(prefix)
      .chain( iter::once(scope_prefix.as_ref()) )
      .chain( utf8_percent_encode(&name.scoped_name,
                                  &percent_encoding::NON_ALPHANUMERIC) )
      .chain( iter::once(suffix) )
      .collect()
  }
  #[throws(ServerFailure)]
  fn save_something(
    &self, prefix: &str,
    w: fn(s: &Self, w: &mut BufWriter<fs::File>)
          -> Result<(),rmp_serde::encode::Error>
  ) {
    let tmp = Self::savefile(&self.name, prefix,".tmp");
    let mut f = BufWriter::new(fs::File::create(&tmp)?);
    w(self, &mut f)?;
    f.flush()?;
    drop( f.into_inner().map_err(|e| { let e : io::Error = e.into(); e })? );
    let out = Self::savefile(&self.name, prefix,"");
    fs::rename(&tmp, &out)?;
    eprintln!("saved to {}", &out);
  }

  #[throws(ServerFailure)]
  pub fn save_game_now(&mut self) {
    self.save_something("g-", |s,w| {
      rmp_serde::encode::write_named(w, &s.c.g.gs)
    })?;
  }

  #[throws(ServerFailure)]
  fn save_access_now(&mut self) {
    self.save_something("a-", |s,w| {
      let global_players = GLOBAL.players.read().unwrap();
      let tokens_players : Vec<(&str, PlayerId)> =
        s.c.g.tokens_players.tr
        .iter()
        .map(|token|
             global_players.get(token)
             .map(|player| (token.0.as_str(), player.ident)))
        .flatten()
        .collect();
      let isa = InstanceSaveAccesses { tokens_players };
      rmp_serde::encode::write_named(w, &isa)
    })?;
  }

  #[throws(ServerFailure)]
  fn load_something<T:DeserializeOwned>(name: &InstanceName, prefix: &str) -> T {
    let inp = Self::savefile(name, prefix, "");
    let mut f = BufReader::new(fs::File::open(&inp)?);
    // xxx handle ENOENT specially, own OE variant
    rmp_serde::decode::from_read(&mut f)?
  }

  #[throws(OE)]
  pub fn load(name: InstanceName) -> InstanceRef {
    // xxx scan on startup, rather than asking caller to specify names
    // xxx should take a file lock on save area
    // xxx check for deleted players, throw their tokens away
    let gs : GameState = Self::load_something(&name, "g-")?;
    let mut al : InstanceSaveAccesses<String>
                       = Self::load_something(&name, "a-")?;
    let mut updates : SecondarySlotMap<_,_> = Default::default();
    for player in gs.players.keys() {
      updates.insert(player, Default::default());
    }
    let name = Arc::new(name);

    let g = Instance {
      name, gs, updates,
      clients : Default::default(),
      tokens_clients : Default::default(),
      tokens_players : Default::default(),
    };
    let cont = InstanceContainer {
      live: true,
      g,
    };
    // xxx record in GLOBAL.games
    let gref = InstanceRef(Arc::new(Mutex::new(cont)));
    let mut g = gref.lock().unwrap();
    for (token, _) in &al.tokens_players {
      g.tokens_players.tr.insert(RawToken(token.clone()));
    }
    let mut global = GLOBAL.players.write().unwrap();
    for (token, player) in al.tokens_players.drain(0..) {
      let iad = InstanceAccessDetails {
        gref : gref.clone(),
        ident : player,
      };
      global.insert(RawToken(token), iad);
    }
    drop(global);
    drop(g);
    gref
  }
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
  #[throws(OE)]
  fn new_random() -> Self {
    let mut rng = thread_rng();
    let token = RawToken (
      repeat_with(|| rng.sample(Alphanumeric))
        .take(64).collect()
    );
    token
  }
}

pub fn lookup_token<Id : AccessId>(s : &str)
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
    let token = param.as_str();
    let i = g.get(token).ok_or(Id::ERROR)?;
    InstanceAccess { raw_token : token, i : i.clone() }
  }
}

#[throws(OE)]
pub fn record_token<Id : AccessId> (
  ig : &mut InstanceGuard,
  iad : InstanceAccessDetails<Id>
) -> RawToken {
  let token = RawToken::new_random()?;
  ig.token_register(token.clone(), iad);
  token
}

// ========== ad-hoc and temporary ==========

const XXX_PLAYERS_TOKENS : &[(&str, &str)] = &[
  ("kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe", "alice"),
  ("ccg9kzoTh758QrVE1xMY7BQWB36dNJTx", "bob"),
];

#[throws(OE)]
pub fn xxx_global_setup() {
  let gs = xxx_gamestate_init();
  let gref = Instance::new(InstanceName {
    scope: ManagementScope::Server,
    scoped_name: "dummy".to_string()
  }, gs).expect("xxx create dummy");
  let mut g = gref.lock()?;
  for (token, nick) in XXX_PLAYERS_TOKENS {
    let player = g.player_new(PlayerState {
      nick : nick.to_string(),
    })?;
    g.player_access_register_xxx(RawToken(token.to_string()), player)?;
  }
}
