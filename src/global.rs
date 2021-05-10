#![allow(clippy::let_and_return)]

use crate::prelude::*;

use slotmap::dense as sm;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId(b'C') }

const MAX_CLIENT_INACTIVITY: Duration = Duration::from_secs(200);

const GAME_SAVE_LAG: Duration = Duration::from_millis(500);

const MAX_LOG_AGE: Duration = Duration::from_secs(10 * 86400);

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
pub struct InstanceRef(Arc<InstanceOuter>);

#[derive(Debug,Clone)]
pub struct InstanceWeakRef(std::sync::Weak<InstanceOuter>);

#[derive(Debug)]
pub struct InstanceOuter {
  c: Mutex<InstanceContainer>,
  b: Mutex<InstanceBundles>,
}

#[derive(Debug,Clone,Serialize,Deserialize,Default)]
#[serde(transparent)]
pub struct LinksTable(pub EnumMap<LinkKind, Option<String>>);
deref_to_field_mut!{LinksTable, EnumMap<LinkKind, Option<String>>, 0}

pub struct Instance {
  pub name: Arc<InstanceName>,
  pub gs: GameState,
  pub ipieces: IPieces,
  pub pcaliases: PieceAliases,
  pub ioccults: IOccults,
  pub clients: DenseSlotMap<ClientId, Client>,
  pub iplayers: SecondarySlotMap<PlayerId, PlayerRecord>,
  pub tokens_players: TokenRegistry<PlayerId>,
  pub tokens_clients: TokenRegistry<ClientId>,
  pub acl: LoadedAcl<TablePermission>,
  pub links: Arc<LinksTable>,
  pub bundle_list: MgmtBundleList, // copy for easy access
  pub asset_url_key: AssetUrlKey,
}

pub struct PlayerRecord {
  pub u: PlayerUpdates,
  pub ipl: IPlayer,
  pub account: Arc<AccountName>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct IPlayer { // usual variable: ipl
  pub acctid: AccountId,
  pub tokens_revealed: HashMap<TokenRevelationKey, TokenRevelationValue>,
  pub tz: Timezone,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct IPiece {
  pub p: IPieceTraitObj,
  #[serde(default)] pub loaded_via_alias: Option<String>,
  pub occilk: Option<OccultIlkOwningId>,
}
deref_to_field!{IPiece, IPieceTraitObj, p}

#[derive(Debug,Serialize,Deserialize)]
#[serde(transparent)]
pub struct IPieces(ActualIPieces);
pub type ActualIPieces = SecondarySlotMap<PieceId, IPiece>;
#[derive(Copy,Clone,Debug)]
pub struct ModifyingPieces(());

#[derive(Debug,Serialize,Deserialize,Default)]
pub struct IOccults {
  pub ilks: OccultIlks,
}

#[derive(Debug,Serialize,Deserialize,Default)]
#[serde(transparent)]
pub struct GPieces(pub(in crate::global) ActualGPieces);
type ActualGPieces = DenseSlotMap<PieceId, GPiece>;

#[derive(Debug)]
pub struct Client {
  pub player: PlayerId,
  pub lastseen: Instant,
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
//  save/a-<nameforfs>    MessagePack of InstanceSaveAuxiliary
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
  pub c: MutexGuard<'g, InstanceContainer>,
  pub gref: InstanceRef,
}

#[derive(Debug,Default)]
pub struct TokenRegistry<Id: AccessId> {
  tr: HashSet<RawToken>,
  id: PhantomData<Id>,
}

#[derive(Clone,Debug)]
pub struct InstanceAccessDetails<Id> {
  pub gref: InstanceRef,
  pub ident: Id,
  pub acctid: AccountId,
}

// ========== internal data structures ==========

lazy_static! {
  pub static ref GLOBAL: Global = default();
}

#[derive(Default)]
pub struct Global {
  // lock hierarchy: all of these are in order of acquisition
  // (in order of lock acquisition (L first), so also in order of criticality
  // (perf impact); outermost first, innermost last)

  // slow global locks:
  pub save_area_lock: Mutex<Option<File>>,
  // <- accounts::accounts ->
  games_table: RwLock<GamesTable>,

  // per-game lock:
  // <- InstanceContainer ->
  // inner locks which the game needs:
  dirty: Mutex<VecDeque<InstanceRef>>,
  pub config: RwLock<WholeServerConfig>,

  // fast global lookups
  players: RwLock<TokenTable<PlayerId>>,
  clients: RwLock<TokenTable<ClientId>>,
}

pub type GamesGuard = RwLockWriteGuard<'static, GamesTable>;
pub type GamesTable = HashMap<Arc<InstanceName>, InstanceRef>;

#[derive(Debug)]
pub struct InstanceContainer {
  live: bool,
  game_dirty: bool,
  aux_dirty: bool,
  g: Instance,
}

#[derive(Debug,Default,Serialize,Deserialize)]
struct InstanceSaveAuxiliary<RawTokenStr, PiecesLoadedRef, OccultIlksRef,
                             PieceAliasesRef> {
  ipieces: PiecesLoadedRef,
  ioccults: OccultIlksRef,
  pcaliases: PieceAliasesRef,
  tokens_players: Vec<(RawTokenStr, PlayerId)>,
  aplayers: SecondarySlotMap<PlayerId, IPlayer>,
  acl: Acl<TablePermission>,
  pub links: Arc<LinksTable>,
  asset_url_key: AssetUrlKey,
}

pub struct PrivateCaller(());
// outsiders cannot construct this
// workaround for inability to have private trait methods
const PRIVATE_Y: PrivateCaller = PrivateCaller(());

// ========== implementations ==========

impl RawTokenVal {
  // str is [u8] with a funny hat on, so &str is pointer + byte count.
  // nomicon says &SomeStruct([T]) is pointer plus number of elements.
  // So &str and &SomeStruct(str) have the same layout
  pub fn from_str(s: &str) -> &RawTokenVal { unsafe { mem::transmute(s) } }
}

impl Borrow<RawTokenVal> for RawToken {
  fn borrow(&self) -> &RawTokenVal { RawTokenVal::from_str(&self.0) }
}

impl Debug for RawTokenVal {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    crate::spec::imp::raw_token_debug_as_str(&self.0, f)
  }
}

impl Debug for Instance {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Instance {{ name: {:?}, .. }}", &self.name)
  }
}

// ---------- Main API for instance lifecycle ----------

impl InstanceRef {
  #[throws(GameBeingDestroyed)]
  pub fn lock(&self) -> InstanceGuard<'_> {
    let c = self.0.c.lock();
    if !c.live { throw!(GameBeingDestroyed) }
    InstanceGuard { c, gref: self.clone() }
  }

  pub fn lock_even_destroying(&self) -> MutexGuard<InstanceContainer> {
    self.0.c.lock()
  }

  pub fn downgrade_to_weak(&self) -> InstanceWeakRef {
    InstanceWeakRef(Arc::downgrade(&self.0))
  }

  pub fn lock_bundles(&self) -> MutexGuard<'_, InstanceBundles> {
    self.0.b.lock()
  }
}

impl InstanceWeakRef {
  pub fn upgrade(&self) -> Option<InstanceRef> {
    Some(InstanceRef(self.0.upgrade()?))
  }
}

impl<A> Unauthorised<InstanceRef, A> {
  #[throws(GameBeingDestroyed)]
  pub fn lock<'r>(&'r self) -> Unauthorised<InstanceGuard<'r>, A> {
    let must_not_escape = self.by_ref(Authorisation::authorise_any());
    Unauthorised::of(must_not_escape.lock()?)
  }

  pub fn lock_even_destroying<'r>(&'r self) -> Unauthorised<InstanceGuard<'r>, A> {
    let must_not_escape = self.by_ref(Authorisation::authorise_any());
    Unauthorised::of(InstanceGuard {
      c: must_not_escape.lock_even_destroying(),
      gref: must_not_escape.clone(),
    })
  }

  pub fn lock_bundles<'r>(&'r self) -> Unauthorised<MutexGuard<'r, InstanceBundles>, A> {
    let must_not_escape = self.by_ref(Authorisation::authorise_any());
    Unauthorised::of(must_not_escape.lock_bundles())
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
      name: name.clone(),
      gs, acl,
      ipieces: IPieces(default()),
      pcaliases: default(),
      ioccults: default(),
      clients: default(),
      iplayers: default(),
      tokens_players: default(),
      tokens_clients: default(),
      links: default(),
      bundle_list: default(),
      asset_url_key: AssetUrlKey::new_random()?,
    };

    let c = InstanceContainer {
      live: true,
      game_dirty: false,
      aux_dirty: false,
      g,
    };

    let c = Mutex::new(c);
    let b = Mutex::new(InstanceBundles::new());
    let gref = InstanceRef(Arc::new(InstanceOuter { c, b }));
    let mut ig = gref.lock()?;

    let entry = games.entry(name);

    use hash_map::Entry::*;
    let entry = match entry {
      Vacant(ve) => ve,
      Occupied(_) => throw!(ME::AlreadyExists),
    };

    ig.save_aux_now()?;
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
        .ok_or(ME::GameNotFound)?
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
    let games = GLOBAL.games_table.read();
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
    let b_dir = bundles::b_dir(&g.g.name);

    let g_file = savefilename(&g.g.name, "g-", "");
    fs::remove_file(&g_file).context("remove").context(g_file)?;

    (||{ // Infallible:
      g.live = false;
      games.remove(&g.g.name);
      InstanceGuard::forget_all_tokens(&mut g.g.tokens_clients);
      InstanceGuard::forget_all_tokens(&mut g.g.tokens_players);

      // xxx truncate bundles, part of ClearBundles too!
      fn best_effort<F>(rm: F, path: &str, desc: &str)
      where F: FnOnce(&str) -> Result<(), io::Error>
      {
        rm(path)
          .unwrap_or_else(
            |e| warn!("failed to delete stale {} {:?}: {:?}",
                      desc, path, e)
            // apart from that, ignore the error
            // load_games will clean it up later.
          );
      }
      best_effort(|f| fs::remove_file(f), &a_savefile, "auth file");
      best_effort(|f| fs::remove_dir_all(f), &b_dir, "bundles dir");

    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
  }

  pub fn list_names(account: Option<&AccountName>,
                    _: Authorisation<AccountName>)
                    -> Vec<Arc<InstanceName>> {
    let games = GLOBAL.games_table.read();
    let out: Vec<Arc<InstanceName>> =
      games.keys()
      .filter(|k| account == None || account == Some(&k.account))
      .cloned()
      .collect();
    out
  }

  #[throws(InternalError)]
  pub fn player_info_pane(&self) -> Html {
    #[derive(Serialize,Debug)]
    struct RenderContext<'r> {
      players: Vec<RenderPlayer<'r>>,
    }
    #[derive(Serialize,Debug)]
    struct RenderPlayer<'r> {
      player_num: u32,
      nick: &'r str,
      account: &'r AccountName,
    }
    let players = self.gs.players.iter().filter_map(|(player, gpl)| {
      let ipl = self.iplayers.get(player)?;
      let (idx, _) = player.data().get_idx_version();
      Some(RenderPlayer {
        player_num: idx,
        nick: &gpl.nick,
        account: &ipl.account,
      })
    }).collect::<Vec<_>>();
    let render = RenderContext { players };
    let html = Html::from_html_string(
      nwtemplates::render("player-info-pane.tera", &render)
        .context("render player info template")?
    );
    html
  }
}

pub fn games_lock() -> RwLockWriteGuard<'static, GamesTable> {
  GLOBAL.games_table.write()
}

// ---------- Simple trait implementations ----------

deref_to_field_mut!{InstanceGuard<'_>, Instance, c.g}

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
    let mut names: [_;2] = default();
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
      |s| f.write_str(s),
    )?
  }
}
hformat_as_display!{InstanceName}

fn link_a_href(k: &HtmlStr, v: &str) -> Html {
  hformat!("<a href={}>{}</a>", v, k)
}
#[ext(pub)]
impl (LinkKind, &str) {
  fn to_html(self) -> Html {
    let (k, v) = self;
    link_a_href(&k.to_html(), v)
  }
}

impl From<&LinksTable> for Html {
  fn from(links: &LinksTable) -> Html {
    let mut s = links.iter()
      .filter_map(|(k,v)| {
        let v = v.as_ref()?;
        Some((k, v.as_str()).to_html())
      })
      .chain(iter::once(
        link_a_href(Html::lit("Shapelib").into(), "/_/shapelib.html")
      ))
      .collect::<Vec<Html>>()
      .iter()
      .hjoin(&Html::lit(" "));
    if s.len() != 0 { s = hformat!("links: {}", s) }
    s
  }
}

// ---------- Player and token functionality ----------

impl<Id> InstanceAccessDetails<Id>
  where Id: AccessId, OE: From<Id::Error>
{
  #[throws(OE)]
  pub fn from_token(token: &RawTokenVal) -> InstanceAccessDetails<Id> {
    let g = Id::global_tokens(PRIVATE_Y).read();
    let i = g.get(token).ok_or(Id::ERROR)?;
    i.clone()
  }
}

impl<'ig> InstanceGuard<'ig> {
  /// caller is responsible for logging; threading it through
  /// proves the caller has a log entry.
  #[throws(MgmtError)]
  pub fn player_new(&mut self, gnew: GPlayer, inew: IPlayer,
                    account: Arc<AccountName>, logentry: LogEntry)
                    -> (PlayerId, PreparedUpdateEntry, LogEntry) {
    // saving is fallible, but we can't attempt to save unless
    // we have a thing to serialise with the player in it
    self.check_new_nick(&gnew.nick)?;
    if self.c.g.iplayers.values().any(|r| r.ipl.acctid == inew.acctid) {
      Err(ME::AlreadyExists)?;
    }
    let player = self.c.g.gs.players.insert(gnew);
    let u = PlayerUpdates::new_begin(&self.c.g.gs).new();
    let record = PlayerRecord { u, account, ipl: inew, };

    self.c.g.iplayers.insert(player, record);

    let update = (||{
      let new_info_pane = Arc::new(self.player_info_pane()?);

      let update = PreparedUpdateEntry::AddPlayer {
        player, new_info_pane,
        data: DataLoadPlayer::from_player(self, player),
      };

      self.save_game_now()?;
      self.save_aux_now()?;
      Ok::<_,InternalError>(update)
    })().map_err(|e|{
      self.c.g.iplayers.remove(player);
      self.c.g.gs.players.remove(player);
      // Perhaps we leave the g-* file with this player recorded,
      // but this will be ignored when we load.
      e
    })?;
    (||{
      
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)
    (player, update, logentry)
  }

  #[throws(MgmtError)]
  pub fn check_new_nick(&mut self, new_nick: &str) {
    if self.c.g.gs.players.values().any(|old| old.nick == new_nick) {
      Err(ME::NickCollision)?;
    }
  }

  pub fn remove_clients(&mut self,
                        players: &HashSet<PlayerId>,
                        signal: ErrorSignaledViaUpdate<PUE_P>) {
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
          us: vec![ PreparedUpdateEntry::Error(
            signal.clone(),
          )],
        });
      };
    }
    self.tokens_deregister_for_id(|id| clients_to_remove.contains(&id));
  }

  //  #[throws(InternalError)]
  //  https://github.com/withoutboats/fehler/issues/62
  pub fn players_remove(&mut self, old_players_set: &HashSet<PlayerId>)
                        ->
    Result<Vec<
        (Option<GPlayer>, Option<IPlayer>, PreparedUpdateEntry)
        >, InternalError>
  {
    // We have to filter this player out of everything
    // Then save
    // Then send updates
    // We make a copy so if the save fails, we can put everything back

    let mut players = self.c.g.gs.players.clone();
    let old_players: Vec<_> = old_players_set.iter().cloned().collect();
    let old_gpls: Vec<_> = old_players.iter().cloned().map(|oldplayer| {
      players.remove(oldplayer)
    }).collect();

    // New state
    let mut gs = GameState {
      // These parts are straightforward and correct
      table_colour: self.c.g.gs.table_colour.clone(),
      table_size: self.c.g.gs.table_size,
      gen: self.c.g.gs.gen,
      max_z: self.gs.max_z.clone(),
      players,
      // These have special handling
      log: default(),
      pieces: default(),
      occults: default(),
    };

    let held_by_old = |p: &GPiece| if_chain! {
      if let Some(held) = p.held;
      if old_players_set.contains(&held);
      then { true }
      else { false }
    };

    let mut updated_pieces = vec![];

    // drop order is reverse of creation order, so create undo
    // after all the things it will reference
    let mut undo: Vec<Box<dyn FnOnce(&mut InstanceGuard)>> = vec![];

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
      mem::swap(&mut ig.c.g.gs.occults,&mut gs.occults);
      mem::swap(&mut ig.c.g.gs,        &mut gs,   );
    };
    swap_things(self);
    undo.push(Box::new(swap_things));

    let new_info_pane = Arc::new(self.player_info_pane()?);

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

      let estimate = updated_pieces.len() + 1;
      let mut buf = PrepareUpdatesBuffer::new(self, Some(estimate));
      for &piece in &updated_pieces {
        buf.piece_update(piece, &None, PieceUpdateOp::Modify(()).into());
      }
      buf.finish();

      self.remove_clients(old_players_set, ESVU::PlayerRemoved);
      self.tokens_deregister_for_id(
        |id:PlayerId| old_players_set.contains(&id)
      );
      let old_ipls: Vec<_> = old_players.iter().cloned().map(
        |oldplayer| self.iplayers.remove(oldplayer)
          .map(|ipr| ipr.ipl)
      ).collect();
      self.save_aux_now().unwrap_or_else(
        |e| warn!(
          "trouble garbage collecting accesses for deleted player: {:?}",
          &e)
      );
      old_ipls
    })(); // <- No ?, ensures that IEFE is infallible (barring panics)

    let updates = old_players.iter().cloned().map(
      |player| PreparedUpdateEntry::RemovePlayer {
        player,
        new_info_pane: new_info_pane.clone(),
      }
    );

    let old = izip!(
      old_gpls,
      old_ipls,
      updates
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
    self.save_aux_now().map_err(|e|{
      // oof, the tokens are already out of the global map, but
      // not saved, so they might come back.  We need to leave
      // them here so they can be deleted later.
      self.tokens_players = old_tokens;
      e
    })?;
    // ppoint of no return
    (||{
      self.remove_clients(&[player].iter().cloned().collect(),
                          ESVU::TokenRevoked);
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

    let current_tokens: ArrayVec<[&RawToken;2]> = {
      let players = GLOBAL.players.read();
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

    let reset = reset || current_tokens.is_empty();

    let token: RawToken = if reset {
      drop(current_tokens);

      self.invalidate_tokens(player)?;
      self.save_aux_now()?;

      let token = access
        .override_token()
        .cloned()
        .unwrap_or_else(||{
          RawToken::new_random()
        });
        
      let iad = InstanceAccessDetails {
        gref: self.gref.clone(),
        ident: player,
        acctid
      };
      self.token_register(token.clone(), iad);
      self.save_aux_now()?;

      token

    } else {

      let token = match current_tokens.as_slice() {
        [] => panic!(), // this possibility was excluded earlier
        [token] => token,
        _ => {
          warn!("duplicate token for {}", player);
          throw!(ME::ServerFailure("duplicate token".to_string()));
        }
      };

      (*token).clone()
    };

    let ipl = &self.c.g.iplayers.byid(player)?.ipl;
    let gpl = self.c.g.gs.players.byid(player)?;

    let url = format!("{}/?{}",
                      &config().public_url.trim_end_matches("/"),
                      token.0);
    let info = AccessTokenInfo { url };
    let report = access.deliver(accounts, &self.c.g, &gpl, &ipl, info)?;
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
    self.save_game_and_aux_later();
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
    Id::global_tokens(PRIVATE_Y).write().insert(token, iad);
  }

  fn forget_all_tokens<Id:AccessId>(tokens: &mut TokenRegistry<Id>) {
    let global: &RwLock<TokenTable<Id>> = AccessId::global_tokens(PRIVATE_Y);
    let mut global = global.write();
    for t in tokens.tr.drain() { global.remove(&t); }
  }

  fn tokens_deregister_for_id<Id:AccessId, F: Fn(Id) -> bool
                              > (&mut self, oldid: F) {
    let mut tokens = AccessId::global_tokens(PRIVATE_Y).write();
    tokens.retain(|k,v| if_chain! {
      if oldid(v.ident);
      if Id::tokens_registry(self, PRIVATE_Y).tr.remove(k);
      then { false }
      else { true }
    });
  }

}

// ---------- save/load ----------

#[derive(Copy,Clone,Debug)]
enum SaveFileOrDir { File, Dir }

impl SaveFileOrDir {
  #[throws(io::Error)]
  fn remove<P:AsRef<std::path::Path>>(self, path: P) {
    match self {
      SaveFileOrDir::File => fs::remove_file(path)?,
      SaveFileOrDir::Dir  => fs::remove_dir_all(path)?,
    }
  }
}

#[derive(Debug)]
enum SavefilenameParseResult {
  NotGameFile,
  Auxiliary(SaveFileOrDir),
  TempToDelete,
  GameFile {
    aux_leaves: Vec<Vec<u8>>,
    name: InstanceName,
  },
}

pub fn savefilename(name: &InstanceName, prefix: &str, suffix: &str)
                    -> String {
  [ config().save_dir().as_str(), &"/", prefix ]
    .iter().map(Deref::deref)
    .chain(iter::once( name.to_string().as_str() ))
    .chain([ suffix ].iter().map(Deref::deref))
    .collect()
}

#[throws(anyhow::Error)]
fn savefilename_parse(leaf: &[u8]) -> SavefilenameParseResult {
  use SavefilenameParseResult::*;

  if leaf.starts_with(b"a-") { return Auxiliary(SaveFileOrDir::File) }
  if leaf.starts_with(b"b-") { return Auxiliary(SaveFileOrDir::Dir ) }
  let rhs = match leaf.strip_prefix(b"g-") {
    Some(rhs) => rhs,
    None => return NotGameFile,
  };
  let after_ftype_prefix = rhs;
  let rhs = str::from_utf8(rhs)?;
  let rcomp = rhs.rsplitn(2, ':').next().unwrap();
  if rcomp.find('.').is_some() { return TempToDelete }

  let name = InstanceName::from_str(&rhs)?;

  let aux_leaves = [ b"a-", b"b-" ].iter().map(|prefix| {
    let mut s: Vec<_> = (prefix[..]).into(); s.extend(after_ftype_prefix); s
  }).collect();
  GameFile { name, aux_leaves }
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
      f.into_inner().map_err(|e| { let e: io::Error = e.into(); e })
        .with_context(||format!("save: close {:?}",&tmp))?
    );
    let out = savefilename(&self.name, prefix,"");
    fs::rename(&tmp, &out).context("install")
      .with_context(||format!("save: install {:?} as {:?}", &tmp, &out))?;
    debug!("saved to {}", &out);
  }

  #[throws(InternalError)]
  pub fn save_game_now(&mut self) {
    if self.c.aux_dirty {
      self.save_aux_now()?;
    }
    self.save_something("g-", |s,w| {
      rmp_serde::encode::write_named(w, &s.c.g.gs)
    })?;
    self.c.game_dirty = false;
    debug!("saved (now) {}", &self.name);
  }

  #[throws(InternalError)]
  pub fn save_aux_now(&mut self) {
    self.save_something("a-", |s, w| {
      let ipieces = &s.c.g.ipieces;
      let ioccults = &s.c.g.ioccults;
      let pcaliases = &s.c.g.pcaliases;
      let tokens_players: Vec<(&str, PlayerId)> = {
        let global_players = GLOBAL.players.read();
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
      let links = s.c.g.links.clone();
      let asset_url_key = s.c.g.asset_url_key.clone();
      let isa = InstanceSaveAuxiliary {
        ipieces, ioccults, tokens_players, aplayers, acl, links,
        pcaliases, asset_url_key,
      };
      rmp_serde::encode::write_named(w, &isa)
    })?;
    self.c.aux_dirty = false;
    info!("saved aux for {}", &self.name);
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
    let InstanceSaveAuxiliary::<String,ActualIPieces,IOccults,PieceAliases> {
      tokens_players, mut ipieces, ioccults, mut aplayers, acl, links,
      pcaliases, asset_url_key,
    } = match Self::load_something(&name, "a-") {
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

    let mut gs: GameState = Self::load_something(&name, "g-")?;

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

    let iplayers: SecondarySlotMap<PlayerId, PlayerRecord> = {
      let a = aplayers;
      a.into_iter()
    }.filter_map(|(player, ipl)| {
      let u = pu_bc.new();
      let account = accounts.lookup(ipl.acctid).ok()?.0.account.clone();
      Some((player, PlayerRecord { u, ipl, account }))
    }).collect();

    for mut p in gs.pieces.values_mut() {
      p.lastclient = default();
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

    let mut g = Instance {
      gs, iplayers, links,
      acl: acl.into(),
      ipieces: IPieces(ipieces),
      pcaliases,
      ioccults,
      name: name.clone(),
      clients: default(),
      tokens_clients: default(),
      tokens_players: default(),
      bundle_list: default(), // set by load_game_bundles
      asset_url_key,
    };

    let b = InstanceBundles::reload_game_bundles(&mut g)?;
    let b = Mutex::new(b);

    let c = InstanceContainer {
      live: true,
      game_dirty: false,
      aux_dirty: false,
      g,
    };
    let c = Mutex::new(c);
    let gref = InstanceRef(Arc::new(InstanceOuter { c, b }));
    let mut g = gref.lock().unwrap();

    let ig = &mut *g;
    for (piece, ipc) in ig.ipieces.0.iter() {
      ipc.direct_trait_access().loaded_hook(piece, &mut ig.gs, &gref)?;
    }

    for (token, _) in &tokens_players {
      g.tokens_players.tr.insert(RawToken(token.clone()));
    }
    let mut global = GLOBAL.players.write();
    for ((token, player), acctid) in
      tokens_players.drain(0..)
      .zip(acctids_players)
    { if_chain!{
      if let Some(acctid) = acctid;
      let iad = InstanceAccessDetails {
        acctid,
        gref: gref.clone(),
        ident: player,
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
  enum AFState { Found(PathBuf, SaveFileOrDir), Used }
  use AFState::*;
  use SavefilenameParseResult::*;
  let mut a_leaves = HashMap::new();
  let save_dir = config().save_dir().clone();
  for de in fs::read_dir(&save_dir).context(save_dir)? {
    let de = de?;
    let leaf = de.file_name();
    (||{
      let leaf = leaf.as_bytes();
      match savefilename_parse(leaf)? {
        NotGameFile => {
        }
        TempToDelete => {
          fs::remove_file(de.path())
            .context("stale temporary file")?;
        }
        Auxiliary(fd) => {
          a_leaves.entry(leaf.to_owned()).or_insert_with(
            || Found(de.path(), fd)
          );
        }
        GameFile { aux_leaves, name } => {
          InstanceGuard::load_game(accounts, games, name)?;
          for aux_leaf in aux_leaves {
            a_leaves.insert(aux_leaf, Used);
          }
        }
      }
      <Result<_,anyhow::Error>>::Ok(())
    })().with_context(|| format!("leaf={:?}", leaf))?;
  }
  (||{
    for (leaf, state) in &a_leaves {
      if let Found(path, fd) = state {
        fd.remove(&path)
          .with_context(|| format!("leaf={:?}", leaf))?;
      }
    }
    <Result<_,anyhow::Error>>::Ok(())
  })().context("cleaning up stale files")?;
  info!("loaded games");
}

// ---------- Tokens / TokenTable / AccessId ----------

pub type TokenTable<Id> = HashMap<RawToken, InstanceAccessDetails<Id>>;

pub trait AccessId: Copy + Clone + 'static {
  type Error: Into<OnlineError>;
  const ERROR: Self::Error;
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
  const ERROR: PlayerNotFound = PlayerNotFound;
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
  const ERROR: OnlineError = NoClient;
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
        .map(char::from)
        .take(64).collect()
    );
    token
  }
}

pub fn lookup_token<Id:AccessId>(s: &RawTokenVal)
      -> Result<InstanceAccessDetails<Id>, Id::Error> {
  Id::global_tokens(PRIVATE_Y).read().get(s).cloned()
    .ok_or(Id::ERROR)
}

#[throws(OE)]
pub fn record_token<Id:AccessId> (
  ig: &mut InstanceGuard,
  iad: InstanceAccessDetails<Id>
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
    let c = gref.lock_even_destroying();
    let remove: Vec<_> = c.g.iplayers.iter().filter_map(|(player,pr)| {
      if pr.ipl.acctid == acctid { Some(player) } else { None }
    }).collect();
    let mut ig = InstanceGuard { gref: gref.clone(), c };
    for player in remove.into_iter() {
      f(&mut ig, player)?;
    }
  }
}

// ========== instance pieces data access ==========

impl IPieces {
  pub fn get(&self, piece: PieceId) -> Option<&IPiece> {
    self.0.get(piece)
  }

  pub fn as_mut(&mut self, _: ModifyingPieces) -> &mut ActualIPieces {
    &mut self.0
  }
}

// ---------- gamestate pieces table ----------

// No DerefMut to make sure we send updates, save, etc.
deref_to_field!{GPieces, ActualGPieces, 0}

impl GPieces {
  pub fn get_mut(&mut self, piece: PieceId) -> Option<&mut GPiece> {
    self.0.get_mut(piece)
  }
  pub fn values_mut(&mut self) -> sm::ValuesMut<PieceId, GPiece> {
    self.0.values_mut()
  }
  pub fn as_mut(&mut self, _: ModifyingPieces) -> &mut ActualGPieces {
    &mut self.0
  }
}

impl ById for GPieces {
  type Id = PieceId;
  type Entry = GPiece;
  type Error = PieceOpError;
  #[throws(POE)]
  fn byid(&self, piece: PieceId) -> &GPiece {
    self.get(piece).ok_or(POE::PieceGone)?
  }
  #[throws(POE)]
  fn byid_mut(&mut self, piece: PieceId) -> &mut GPiece {
    self.get_mut(piece).ok_or(POE::PieceGone)?
  }
}

/*impl<'p> IntoIterator for &'p Pieces {
  type Item = (PieceId, &'p PieceState);
  type IntoIter = sm::Iter<'p, PieceId, PieceState>;
  fn into_iter(self) -> Self::IntoIter { (&self.0).into_iter() }
}*/
impl<'p> IntoIterator for &'p mut GPieces {
  type Item = (PieceId, &'p mut GPiece);
  type IntoIter = sm::IterMut<'p, PieceId, GPiece>;
  fn into_iter(self) -> Self::IntoIter { (&mut self.0).into_iter() }
}

// ========== background maintenance ==========

// ---------- delayed game save ----------

impl InstanceGuard<'_> {
  pub fn save_game_later(&mut self) {
    if self.c.game_dirty { return }
    GLOBAL.dirty.lock().push_back(self.gref.clone());
    self.c.game_dirty = true;
  }

  pub fn save_game_and_aux_later(&mut self) {
    if self.c.aux_dirty { return }
    self.save_game_later();
    self.c.aux_dirty = true;
  }
}

pub fn game_flush_task() {
  let mut inner_queue = VecDeque::new();
  loop {
    {
      mem::swap(&mut inner_queue, &mut *GLOBAL.dirty.lock());
    }
    thread::sleep(GAME_SAVE_LAG);
    for _ in 0..inner_queue.len() {
      let ent = inner_queue.pop_front().unwrap();
      let mut ig = match ent.lock() { Ok(ig) => ig, _ => continue/*ah well*/ };
      if !ig.c.game_dirty { continue }
      match ig.save_game_now() {
        Ok(_) => {
          assert!(!ig.c.game_dirty);
        }
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
      let c = gref.lock_even_destroying();
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

  for gref in GLOBAL.games_table.read().values() {
    struct Any;
    impl ClientIterator for Any {
      type Ret = ();
      fn old(&mut self, _client: ClientId) -> Option<()> {
        Some(())
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
      type Ret = Void;
      fn old(&mut self, client: ClientId) -> Option<Void> {
        self.0.insert(client);
        None
      }
    }

    let mut now = Now(default());
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

  let read = GLOBAL.games_table.read();
  for gref in read.values() {
    if gref.lock_even_destroying().g.gs.want_expire_some_logs(cutoff) {
      want_expire.push(gref.clone())
    }
  }
  drop(read);

  for gref in want_expire.drain(..) {
    let mut g = gref.lock_even_destroying();
    info!("expiring old log entries in {:?}", &g.g.name);
    g.g.gs.do_expire_old_logs(cutoff);
  }
}

pub fn logs_periodic_expiry() {
  loop {
    sleep(MAX_LOG_AGE / 10);
    global_expire_old_logs();
  }
}
