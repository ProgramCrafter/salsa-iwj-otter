
#![allow(dead_code)]

use crate::imports::*;
use lazy_static::lazy_static;

visible_slotmap_key!{ ClientId('C') }
visible_slotmap_key!{ PlayerId('#') }

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
pub struct RawToken (pub String);
impl Borrow<str> for RawToken {
  fn borrow(&self) -> &str { &self.0 }
}

#[derive(Debug)] 
pub struct Client {
  pub player : PlayerId,
}

#[derive(Debug)]
pub struct PreparedUpdate {
  pub gen : Generation,
  pub us : Vec<PreparedUpdateEntry>,
}
#[derive(Debug)]
pub enum PreparedUpdateEntry {
  Piece {
    client : ClientId,
    sameclient_cseq : ClientSequence,
    piece : VisiblePieceId,
    op : PieceUpdateOp<PreparedPieceState>,
  },
  Log (Arc<LogEntry>),
}
impl PreparedUpdateEntry {
  pub fn json_len(&self) -> usize {
    use PreparedUpdateEntry::*;
    match self {
      Piece { ref op, .. } => {
        50 +
        op.new_state().map(|x| x.svg.len()).unwrap_or(0)
      },
      Log(logent) => {
        logent.html.as_bytes().len() * 3
      }
    }
  }
}
impl PreparedUpdate {
  pub fn json_len(&self) -> usize {
    self.us.iter().map(|u| 20 + u.json_len()).sum()
  }
}

#[derive(Debug,Serialize)]
pub enum PieceUpdateOp<NS> {
  Delete(),
  Insert(NS),
  Modify(NS),
  Move(Pos),
  SetZLevel(ZLevel),
}
impl<NS> PieceUpdateOp<NS> {
  pub fn new_state(&self) -> Option<&NS> {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(ns) => Some(ns),
      Modify(ns) => Some(ns),
      Move(_) => None,
      SetZLevel(_) => None,
    }
  }
  pub fn map_new_state<NS2,F: FnOnce(NS) -> NS2>(self, f:F)
                            -> PieceUpdateOp<NS2> {
    use PieceUpdateOp::*;
    match self {
      Delete() => Delete(),
      Insert(ns) => Insert(f(ns)),
      Modify(ns) => Modify(f(ns)),
      Move(pos) => Move(pos),
      SetZLevel(zl) => SetZLevel(zl),
    }
  }
  pub fn new_z_generation(&self) -> Option<Generation> {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(_) => None,
      Modify(_) => None,
      Move(_) => None,
      SetZLevel(ZLevel{zg,..}) => Some(*zg),
    }
  }
}

#[derive(Debug)]
pub struct PlayerUpdates {
  pub log : StableIndexVecDeque<Arc<PreparedUpdate>,sse::UpdateId>,
  pub cv : Arc<Condvar>,
}

const RECENT_BUFFER : usize = 50;

impl Default for PlayerUpdates {
  fn default() -> PlayerUpdates { PlayerUpdates {
    log : StableIndexVecDeque::with_capacity(RECENT_BUFFER),
    cv : Default::default(),
  } }
}

#[derive(Debug)]
pub struct Instance {
  pub gs : GameState,
  pub clients : DenseSlotMap<ClientId,Client>,
  pub updates : SecondarySlotMap<PlayerId, PlayerUpdates>,
}

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

#[derive(Default)]
struct Global {
  // lock hierarchy: this is the innermost lock
  players : RwLock<TokenTable<PlayerId>>,
  clients : RwLock<TokenTable<ClientId>>,
  // xxx delete instances at some point!
}

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

pub trait AccessId : Copy + Clone + 'static {
  fn global_tokens() -> &'static RwLock<TokenTable<Self>>;
  const ERROR : OnlineError;
}

impl AccessId for PlayerId {
  const ERROR : OnlineError = NoPlayer;
  fn global_tokens() -> &'static RwLock<TokenTable<Self>> { &GLOBAL.players }
}
impl AccessId for ClientId {
  const ERROR : OnlineError = NoClient;
  fn global_tokens() -> &'static RwLock<TokenTable<Self>> { &GLOBAL.clients }
}

pub fn lookup_token<Id : AccessId>(s : &str)
      -> Result<InstanceAccessDetails<Id>, OE> {
  Id::global_tokens().read().unwrap().get(s).cloned()
    .ok_or(Id::ERROR)
}

pub fn record_token<Id : AccessId>(iad : InstanceAccessDetails<Id>)
                                   -> RawToken {
  let mut rng = thread_rng();
  let token = RawToken (
    repeat_with(|| rng.sample(Alphanumeric))
      .take(64).collect()
  );
  Id::global_tokens().write().unwrap().insert(token.clone(), iad);
  token
}

const XXX_PLAYERS_TOKENS : &[(&str, &str)] = &[
  ("kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe", "alice"),
  ("ccg9kzoTh758QrVE1xMY7BQWB36dNJTx", "bob"),
];

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

pub fn xxx_global_setup() {
  let gi = Instance {
    gs : xxx_gamestate_init(),
    clients : Default::default(),
    updates : Default::default(),
  };
  let amu = Arc::new(Mutex::new(gi));
  let mut ig = amu.lock().unwrap();
  for (token, nick) in XXX_PLAYERS_TOKENS {
    let np = Player {
      nick : nick.to_string(),
    };
    let player = ig.gs.players.insert(np);
    let ia = InstanceAccessDetails { g : amu.clone(), ident : player };
    GLOBAL.players.write().unwrap().insert(
      RawToken(token.to_string()), ia
    );
    ig.updates.insert(player, Default::default());
  }
}
