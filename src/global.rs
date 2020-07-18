
use crate::imports::*;
use lazy_static::lazy_static;

#[allow(dead_code)]
const SAVE_DIRECTORY : &str = "save";

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ ClientId('C') }

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct RawToken (pub String);

// ---------- data structure ----------

#[derive(Debug)]
pub struct Instance {
  pub name : String,
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
  #[throws(OE)]
  pub fn new(gs: GameState, instance_name: String) -> Instance {
    Instance {
      name : instance_name,
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

  fn savefile(&self, prefix: &str, suffix: &str) -> String {
    iter::once(prefix)
      .chain( utf8_percent_encode(&self.name,
                                  &percent_encoding::NON_ALPHANUMERIC) )
      .chain( iter::once(suffix) )
      .collect()
  }

  #[throws(OE)]
  fn save_game_now(&mut self) {
    let savefile = self.savefile("g-","tmp");
    let mut f = BufWriter::new(fs::File::create(&savefile)?);
    rmp_serde::encode::write_named(&mut f, &self.ig.gs)?;
    f.flush()?;
    drop( f.into_inner().map_err(|e| { let e : io::Error = e.into(); e })? );
    fs::rename(&savefile, &self.savefile("g-",""))?;
    eprintln!("xxx saved {} to {}!", self.name, &savefile);
  }

  #[throws(OE)]
  fn save_access_now(&mut self) { eprintln!("xxx would save!"); }
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
  let gi = Instance::new(gs, "dummy".to_string())?;
  let amu = Arc::new(Mutex::new(gi));
  let mut ig = Instance::lock(&amu)?;
  for (token, nick) in XXX_PLAYERS_TOKENS {
    let player = ig.player_new(PlayerState {
      nick : nick.to_string(),
    })?;
    ig.player_access_register(RawToken(token.to_string()), player)?;
  }
}
