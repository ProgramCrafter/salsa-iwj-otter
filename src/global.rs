#![allow(clippy::let_and_return)]

use crate::imports::*;

use std::sync::PoisonError;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

const MAX_CLIENT_INACTIVITY : Duration = Duration::from_secs(200);

const GAME_SAVE_LAG : Duration = Duration::from_millis(500);

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
  pub lastseen : Instant,
}

/// KINDS OF PERSISTENT STATE
///
///               TokenTable   TokenTable    GameState    GameState
///                <ClientId>   <PlayerId>    .players     .pieces
///
///   Saved        No           a-*           g-*          g-*
///   Spec TOML    Absent       table, ish    table        game
///
///
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
/// that a server restart will preserve them.  See above.
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
  config  : RwLock<Arc<ServerConfig>>,
  dirty   : Mutex<VecDeque<InstanceRef>>,
  save_area_lock : Mutex<Option<File>>,
}

#[derive(Debug)]
pub struct InstanceContainer {
  live : bool,
  game_dirty : bool,
  g : Instance,
}

#[derive(Debug,Default,Serialize,Deserialize)]
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
  pub fn lock(&self) -> InstanceGuard<'_> {
    let c = self.0.lock()?;
    if !c.live { throw!(InstanceLockError::GameBeingDestroyed) }
    InstanceGuard { c, gref: self.clone() }
  }
}

impl Instance {
  /// Returns `None` if a game with this name already exists
  #[allow(clippy::new_ret_no_self)]
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
      game_dirty : false,
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
  pub fn player_new(&mut self, newplayer: PlayerState,
                    logentry: LogEntry) -> (PlayerId, LogEntry) {
    // saving is fallible, but we can't attempt to save unless
    // we have a thing to serialise with the player in it
    if self.c.g.gs.players.values().any(|pl| pl.nick == newplayer.nick) {
      Err(MgmtError::AlreadyExists)?;
    }
    let player = self.c.g.gs.players.insert(newplayer);
    self.save_game_now().map_err(|e|{
      self.c.g.gs.players.remove(player);
      e
    })?;
    (||{
      self.c.g.updates.insert(player, Default::default());
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
    (player, logentry)
  }

  //  #[throws(ServerFailure)]
  //  https://github.com/withoutboats/fehler/issues/62
  pub fn player_remove(&mut self, oldplayer: PlayerId)
                       -> Result<Option<PlayerState>,InternalError> {
    // We have to filter this player out of everything
    // Then save
    // Then send updates
    // We make a copy so if the save fails, we can put everything back

    let mut players = self.c.g.gs.players.clone();
    let old_data = players.remove(oldplayer);

    // New state
    let mut gs = GameState {
      // These parts are straightforward and correct
      table_size : self.c.g.gs.table_size,
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
            None,
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

    Ok(old_data)
  }

  #[throws(MgmtError)]
  pub fn player_access_register_fixed(&mut self,
                                      player: PlayerId, token: RawToken,
                                      _safe: Authorised<RawToken>
  ) {
    self.tokens_deregister_for_id(|id:PlayerId| id==player);
    let iad = InstanceAccessDetails {
      gref : self.gref.clone(),
      ident : player
    };
    self.token_register(token, iad);
    self.save_access_now()?;
  }

  #[throws(MgmtError)]
  pub fn players_access_reset(&mut self, players: &[PlayerId])
                             -> Vec<RawToken> {
    for &player in players {
      self.c.g.gs.players.get(player).ok_or(MgmtError::PlayerNotFound)?;
    }
    self.save_access_now()?;
    let mut tokens = vec![];
    for &player in players {
      let iad = InstanceAccessDetails {
        gref : self.gref.clone(),
        ident : player
      };
      let token = RawToken::new_random();
      self.token_register(token.clone(), iad);
      tokens.push(token);
    }
    self.save_access_now()?;
    // If the save fails, we don't return the token so no-one can use
    // it.  Therefore we don't need to bother deleting it.
    tokens
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
  let scope_prefix = { use ManagementScope::*; match &name.scope {
    Server => format!(""),
    Unix{user} => { format!("{}:", user) },
  } };
  [ config().save_directory.as_str(), &"/", prefix, scope_prefix.as_ref() ]
    .iter().map(Deref::deref)
    .chain( utf8_percent_encode(&name.scoped_name,
                                &percent_encoding::NON_ALPHANUMERIC) )
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
  let (rhs, scope) = match rhs.find(':') {
    None => {
      (rhs, ManagementScope::Server)
    },
    Some(colon) => {
      let (lhs, rhs) = rhs.split_at(colon);
      assert_eq!(rhs.chars().next(), Some(':'));
      (rhs, ManagementScope::Unix { user: lhs.to_owned() })
    },
  };
  if rhs.rfind('.').is_some() { return TempToDelete }
  let scoped_name = percent_decode_str(rhs).decode_utf8()?.into();
  GameFile {
    access_leaf : [ b"a-", after_ftype_prefix ].concat(),
    name : InstanceName { scope, scoped_name },
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
    eprintln!("saved to {}", &out);
  }

  #[throws(InternalError)]
  pub fn save_game_now(&mut self) {
    self.save_something("g-", |s,w| {
      rmp_serde::encode::write_named(w, &s.c.g.gs)
    })?;
    self.c.game_dirty = false;
  }

  #[throws(InternalError)]
  fn save_access_now(&mut self) {
    self.save_something("a-", |s,w| {
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
      let isa = InstanceSaveAccesses { tokens_players };
      rmp_serde::encode::write_named(w, &isa)
    })?;
  }

  #[throws(InternalError)]
  fn load_something<T:DeserializeOwned>(name: &InstanceName, prefix: &str)
                                        -> T {
    let inp = savefilename(name, prefix, "");
    let mut f = BufReader::new(fs::File::open(&inp).context(inp)?);
    rmp_serde::decode::from_read(&mut f)?
  }

  #[throws(StartupError)]
  pub fn load(name: InstanceName) -> InstanceRef {
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
    let gs = {
      let mut gs : GameState = Self::load_something(&name, "g-")?;
      for mut p in gs.pieces.values_mut() {
        p.lastclient = Default::default();
      }
      gs
    };

    let mut access_load : InstanceSaveAccesses<String>
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
    let mut updates : SecondarySlotMap<_,_> = Default::default();
    for player in gs.players.keys() {
      updates.insert(player, Default::default());
    }
    let name = Arc::new(name);
    access_load.tokens_players.retain(
      |&(_,player)| gs.players.contains_key(player)
    );

    let g = Instance {
      gs, updates,
      name: name.clone(),
      clients : Default::default(),
      tokens_clients : Default::default(),
      tokens_players : Default::default(),
    };
    let cont = InstanceContainer {
      live: true,
      game_dirty: false,
      g,
    };
    let gref = InstanceRef(Arc::new(Mutex::new(cont)));
    let mut g = gref.lock().unwrap();
    for (token, _) in &access_load.tokens_players {
      g.tokens_players.tr.insert(RawToken(token.clone()));
    }
    let mut global = GLOBAL.players.write().unwrap();
    for (token, player) in access_load.tokens_players.drain(0..) {
      let iad = InstanceAccessDetails {
        gref : gref.clone(),
        ident : player,
      };
      global.insert(RawToken(token), iad);
    }
    drop(global);
    drop(g);
    GLOBAL.games.write().unwrap().insert(name, gref.clone());
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
          InstanceGuard::load(name)?;
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
  let token = RawToken::new_random();
  ig.token_register(token.clone(), iad);
  token
}

// ========== background maintenance ==========

// ---------- delayed game save ----------

impl InstanceGuard<'_> {
  pub fn save_game_later(&mut self) {
    if self.c.game_dirty { return }
    GLOBAL.dirty.lock().unwrap().push_back(self.gref.clone());
    self.c.game_dirty = true;
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
          eprintln!("saved {:?}", &ig.name);
        },
        Err(e) => {
          eprintln!("save error! name={:?}: {}", &ig.name, &e);
          mem::drop(ig);
          inner_queue.push_back(ent); // oof
        }
      }
    }
  }
}

// ---------- client expiry ----------

pub fn client_expire_old_clients() {
  fn lock_even_poisoned(gref: &InstanceRef) -> MutexGuard<InstanceContainer> {
    match gref.0.lock() {
      Ok(g) => g,
      Err(poison) => poison.into_inner(),
    }
  }

  let mut expire = vec![];
  let max_age = Instant::now() - MAX_CLIENT_INACTIVITY;

  trait ClientIterator {
    type Ret;
    fn iter<'g>(&mut self, gref: &'g InstanceRef, max_age: Instant)
            -> (MutexGuard<'g, InstanceContainer>, Option<Self::Ret>) {
      let c = lock_even_poisoned(gref);
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
    struct Now(HashSet<ClientId>);
    enum Impossible { }
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
    gref.tokens_deregister_for_id::<ClientId,_>(|c| now.0.contains(&c));
  }
}

// ========== server config ==========

const DEFAULT_CONFIG_FILENAME : &str = "server.toml";

const DEFAULT_SAVE_DIRECTORY : &str = "save";
const DEFAULT_COMMAND_SOCKET : &str = "command.socket"; // in save dir
const DEFAULT_TEMPLATE_DIR : &str = "templates";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub save_directory: Option<String>,
  pub command_socket: Option<String>,
  pub debug: Option<bool>,
  pub http_port: Option<u16>,
  pub rocket_workers: Option<u16>,
  pub template_dir: Option<String>,
  pub log: toml::Value,
}

#[derive(Debug,Clone)]
pub struct ServerConfig {
  pub save_directory: String,
  pub command_socket: String,
  pub debug: bool,
  pub http_port: Option<u16>,
  pub rocket_workers: u16,
  pub template_dir: String,
  pub log: LogSpecification,
}

impl TryFrom<ServerConfigSpec> for ServerConfig {
  type Error = AE;
  #[throws(Self::Error)]
  fn try_from(spec: ServerConfigSpec) -> ServerConfig {
    let ServerConfigSpec {
      save_directory, command_socket, debug,
      http_port, rocket_workers, template_dir, log,
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

    let log = toml::to_string(&log)?;
    let log = LogSpecification::from_toml(&log)
      .context("log specification")?;

    ServerConfig {
      save_directory, command_socket, debug,
      http_port, rocket_workers, template_dir, log,
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
