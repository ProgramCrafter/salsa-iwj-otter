
use crate::imports::*;
use lazy_static::lazy_static;

#[allow(dead_code)]
const SAVE_DIRECTORY : &str = "save";

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Deserialize)]
#[serde(transparent)]
pub struct RawToken (pub String);

// ---------- data structure ----------

#[derive(Debug,Serialize,Deserialize)]
#[derive(Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct InstanceName {
  pub scope: ManagementScope,
  pub scoped_name: String,
}
pub type InstanceRef = Arc<Mutex<Instance>>;

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
  XXX,
  Unix { user : String /* username, so filename-safe */ },
}

#[derive(Debug)] 
pub struct Client {
  pub player : PlayerId,
}

#[derive(Debug)]
pub struct InstanceGuard<'g> {
  ig : MutexGuard<'g,Instance>,
  amu : Arc<Mutex<Instance>>,
}

#[derive(Default)]
struct Global {
  // lock hierarchy: this is the innermost lock
  games   : RwLock<HashMap<Arc<InstanceName>,InstanceRef>>,
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
  // xxx delete instances at some point!
}

#[derive(Debug,Default)]
pub struct TokenRegistry<Id: AccessId> {
  tr : HashSet<RawToken>,
  id : PhantomData<Id>,
}

#[derive(Debug,Serialize,Deserialize)]
struct InstanceSaveAccesses<RawTokenStr> {
  tokens_players : Vec<(RawTokenStr, PlayerId)>,
}

// ---------- API ----------

#[derive(Clone,Debug)]
pub struct InstanceAccessDetails<Id> {
  pub g : Arc<Mutex<Instance>>,
  pub ident : Id,
}

#[derive(Clone,Debug)]
pub struct InstanceAccess<'i, Id> {
  pub raw_token : &'i str,
  pub i : InstanceAccessDetails<Id>,
}

pub type TokenTable<Id> = HashMap<RawToken, InstanceAccessDetails<Id>>;

pub trait AccessId : Copy + Clone + 'static {
  fn global_tokens(_:PrivateCaller) -> &'static RwLock<TokenTable<Self>>;
  fn tokens_registry(ig: &mut Instance, _:PrivateCaller)
                     -> &mut TokenRegistry<Self>;
  const ERROR : OnlineError;
}

pub struct PrivateCaller(());
// outsiders cannot construct this
// workaround for inability to have private trait methods
const PRIVATE_Y : PrivateCaller = PrivateCaller(());

// ========== implementations ==========

// ---------- newtypes and type aliases ----------

impl Borrow<str> for RawToken {
  fn borrow(&self) -> &str { &self.0 }
}

// ---------- data structure ----------

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

// ---------- Player and instance API ----------

impl Instance {
  /// Returns `None` if a game with this name already exists
  #[throws(MgmtError)]
  pub fn new(name: InstanceName, gs: GameState) -> InstanceRef {
    let name = Arc::new(name);

    let inst = Instance {
      name : name.clone(),
      gs,
      clients : Default::default(),
      updates : Default::default(),
      tokens_players : Default::default(),
      tokens_clients : Default::default(),
    };

    let mut games = GLOBAL.games.write().unwrap();
    let entry = games.entry(name);

    use hash_map::Entry::*;
    let entry = match entry {
      Vacant(ve) => ve,
      Occupied(_) => throw!(MgmtError::AlreadyExists),
    };

    let ami = Arc::new(Mutex::new(inst));
    entry.insert(ami.clone());
    ami
  }

  #[throws(OE)]
  pub fn lock<'g>(amu : &'g Arc<Mutex<Instance>>) -> InstanceGuard<'g> {
    let ig = amu.lock()?;
    InstanceGuard { ig, amu: amu.clone() }
  }
}

impl Deref for InstanceGuard<'_> {
  type Target = Instance;
  fn deref(&self) -> &Instance { &self.ig }
}
impl DerefMut for InstanceGuard<'_> {
  fn deref_mut(&mut self) -> &mut Instance { &mut self.ig }
}

impl InstanceGuard<'_> {
  #[throws(OE)]
  pub fn player_new(&mut self, newplayer: PlayerState) -> PlayerId {
    let player = self.ig.gs.players.insert(newplayer);
    self.ig.updates.insert(player, Default::default());
    self.save_game_now()?;
    player
  }

  #[throws(OE)]
  pub fn player_access_register(&mut self, token: RawToken, player: PlayerId) {
    let iad = InstanceAccessDetails { g : self.amu.clone(), ident : player };
    self.token_register(token, iad);
    self.save_access_now()?;
  }

  fn token_register<Id:AccessId>(
    &mut self,
    token: RawToken,
    iad: InstanceAccessDetails<Id>
  ) {
    Id::tokens_registry(&mut *self.ig, PRIVATE_Y).tr.insert(token.clone());
    Id::global_tokens(PRIVATE_Y).write().unwrap().insert(token, iad);
  }

  pub fn game_destroy(mut self) {
    Self::forget_all_tokens(&mut self.ig.tokens_players);
    Self::forget_all_tokens(&mut self.ig.tokens_clients);
  }
  fn forget_all_tokens<Id:AccessId>(tokens: &mut TokenRegistry<Id>) {
    let global : &RwLock<TokenTable<Id>> = AccessId::global_tokens(PRIVATE_Y);
    let mut global = global.write().unwrap();
    for t in tokens.tr.drain() { global.remove(&t); }
  }

  fn savefile(name: &InstanceName, prefix: &str, suffix: &str) -> String {
    let scope_prefix = { use ManagementScope::*; match &name.scope {
      XXX => format!(""),
      Unix{user} => { format!("{}:", user) },
    } };
    iter::once(prefix)
      .chain( iter::once(scope_prefix.as_ref()) )
      .chain( utf8_percent_encode(&name.scoped_name,
                                  &percent_encoding::NON_ALPHANUMERIC) )
      .chain( iter::once(suffix) )
      .collect()
  }
  #[throws(OE)]
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

  #[throws(OE)]
  fn save_game_now(&mut self) {
    self.save_something("g-", |s,w| {
      rmp_serde::encode::write_named(w, &s.ig.gs)
    })?;
  }

  #[throws(OE)]
  fn save_access_now(&mut self) {
    self.save_something("a-", |s,w| {
      let global_players = GLOBAL.players.read().unwrap();
      let tokens_players : Vec<(&str, PlayerId)> =
        s.ig.tokens_players.tr
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

  #[throws(OE)]
  fn load_something<T:DeserializeOwned>(name: &InstanceName, prefix: &str) -> T {
    let inp = Self::savefile(name, prefix, "");
    let mut f = BufReader::new(fs::File::open(&inp)?);
    // xxx handle ENOENT specially, own OE variant
    rmp_serde::decode::from_read(&mut f)?
  }

  #[throws(OE)]
  pub fn load(name: InstanceName) -> Arc<Mutex<Instance>> {
    // xxx scan on startup, rather than asking caller to specify names
    // xxx should take a file lock on save area
    let gs : GameState = Self::load_something(&name, "g-")?;
    let mut al : InstanceSaveAccesses<String>
                       = Self::load_something(&name, "a-")?;
    let mut updates : SecondarySlotMap<_,_> = Default::default();
    for player in gs.players.keys() {
      updates.insert(player, Default::default());
    }
    let name = Arc::new(name);

    let inst = Instance {
      name, gs, updates,
      clients : Default::default(),
      tokens_clients : Default::default(),
      tokens_players : Default::default(),
    };
    // xxx record in GLOBAL.games
    let amu = Arc::new(Mutex::new(inst));
    let mut ig = amu.lock().unwrap();
    for (token, _) in &al.tokens_players {
      ig.tokens_players.tr.insert(RawToken(token.clone()));
    }
    let mut global = GLOBAL.players.write().unwrap();
    for (token, player) in al.tokens_players.drain(0..) {
      let iad = InstanceAccessDetails {
        g : amu.clone(),
        ident : player,
      };
      global.insert(RawToken(token), iad);
    }
    drop(global);
    drop(ig);
    amu
  }
}

// ---------- Lookup and token API ----------

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
  pub fn new_random() -> Self {
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
  let ami = Instance::new(InstanceName {
    scope: ManagementScope::XXX,
    scoped_name: "dummy".to_string()
  }, gs).expect("xxx create dummy");
  let mut ig = Instance::lock(&ami)?;
  for (token, nick) in XXX_PLAYERS_TOKENS {
    let player = ig.player_new(PlayerState {
      nick : nick.to_string(),
    })?;
    ig.player_access_register(RawToken(token.to_string()), player)?;
  }
}
