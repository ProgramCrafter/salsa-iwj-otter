
use crate::imports::*;
use lazy_static::lazy_static;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct RawToken (pub String);

// ---------- data structure ----------

#[derive(Debug)]
pub struct Instance {
  pub gs : GameState,
  pub clients : DenseSlotMap<ClientId,Client>,
  pub updates : SecondarySlotMap<PlayerId, PlayerUpdates>,
  pub tokens_players : TokenRegistry<PlayerId>,
  pub tokens_clients : TokenRegistry<ClientId>,
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
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
  // xxx delete instances at some point!
}

#[derive(Debug,Default)]
pub struct TokenRegistry<Id: AccessId> {
  tr : HashSet<RawToken>,
  id : PhantomData<Id>,
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
  fn global_tokens() -> &'static RwLock<TokenTable<Self>>;
  fn tokens_registry(ig: &mut Instance) -> &mut TokenRegistry<Self>;
  const ERROR : OnlineError;
}

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
  #[throws(OE)]
  pub fn new(gs: GameState) -> Instance {
    Instance {
      gs,
      clients : Default::default(),
      updates : Default::default(),
      tokens_players : Default::default(),
      tokens_clients : Default::default(),
    }
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
    Id::tokens_registry(&mut *self.ig).tr.insert(token.clone());
    Id::global_tokens().write().unwrap().insert(token, iad);
  }

  #[throws(OE)]
  fn save_game_now(&mut self) { eprintln!("xxx would save!"); }
  #[throws(OE)]
  fn save_access_now(&mut self) { eprintln!("xxx would save!"); }
}

// ---------- Lookup and token API ----------

impl AccessId for PlayerId {
  const ERROR : OnlineError = NoPlayer;
  fn global_tokens() -> &'static RwLock<TokenTable<Self>> { &GLOBAL.players }
  fn tokens_registry(ig: &mut Instance) -> &mut TokenRegistry<Self> {
    &mut ig.tokens_players
  }
}
impl AccessId for ClientId {
  const ERROR : OnlineError = NoClient;
  fn global_tokens() -> &'static RwLock<TokenTable<Self>> { &GLOBAL.clients }
  fn tokens_registry(ig: &mut Instance) -> &mut TokenRegistry<Self> {
    &mut ig.tokens_clients
  }
}

pub fn lookup_token<Id : AccessId>(s : &str)
      -> Result<InstanceAccessDetails<Id>, OE> {
  Id::global_tokens().read().unwrap().get(s).cloned()
    .ok_or(Id::ERROR)
}

impl<'r, Id> FromParam<'r> for InstanceAccess<'r, Id>
  where Id : AccessId
{
  type Error = OE;
  #[throws(OE)]
  fn from_param(param: &'r RawStr) -> Self {
    let g = Id::global_tokens().read().unwrap();
    let token = param.as_str();
    let i = g.get(token).ok_or(Id::ERROR)?;
    InstanceAccess { raw_token : token, i : i.clone() }
  }
}
  
pub fn record_token<Id : AccessId>(
  ig : &mut InstanceGuard,
  iad : InstanceAccessDetails<Id>
) -> RawToken {
  let mut rng = thread_rng();
  let token = RawToken (
    repeat_with(|| rng.sample(Alphanumeric))
      .take(64).collect()
  );
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
  let gi = Instance::new(gs)?;
  let amu = Arc::new(Mutex::new(gi));
  let mut ig = Instance::lock(&amu)?;
  for (token, nick) in XXX_PLAYERS_TOKENS {
    let player = ig.player_new(PlayerState {
      nick : nick.to_string(),
    })?;
    ig.player_access_register(RawToken(token.to_string()), player)?;
  }
}
