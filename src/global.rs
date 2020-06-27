
#![allow(dead_code)]

use crate::imports::*;
use lazy_static::lazy_static;

slotmap::new_key_type!{
  pub struct ClientId;
}

visible_slotmap_key!{PlayerId}
display_consequential_impls!{PlayerId}

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
struct RawToken (String);
impl Borrow<str> for RawToken {
  fn borrow(&self) -> &str { &self.0 }
}

pub struct Client {
}

pub struct Player {
  pub nick : String,
  pub clients : DenseSlotMap<ClientId,Client>,
}

pub struct Instance {
  /* game state goes here */
  pub users : DenseSlotMap<PlayerId,Player>,
  pub gs : GameState,
}

#[derive(Clone)]
pub struct InstanceAccessDetails {
  pub i : Arc<Mutex<Instance>>,
  pub user : PlayerId,
}

#[derive(Clone)]
pub struct InstanceAccess<'i> {
  pub raw_token : &'i str,
  pub i : InstanceAccessDetails,
}

#[derive(Default)]
struct Global {
  // lock hierarchy: this is the innermost lock
  tokens : RwLock<HashMap<RawToken, InstanceAccessDetails>>,
  // xxx delete instances at some point!
}

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

pub fn lookup_token(s : &str) -> Option<InstanceAccessDetails> {
  GLOBAL.tokens.read().unwrap().get(s).cloned()
}

const XXX_USERS_TOKENS : &[(&str, &str)] = &[
  ("kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe", "alice"),
  ("ccg9kzoTh758QrVE1xMY7BQWB36dNJTx", "bob"),
];

impl<'r> FromParam<'r> for InstanceAccess<'r> {
  type Error = E;
  #[throws(E)]
  fn from_param(param: &'r RawStr) -> Self {
    let g = GLOBAL.tokens.read().unwrap();
    let token = param.as_str();
    let i = g.get(token).ok_or_else(|| anyhow!("unknown token"))?;
    InstanceAccess { raw_token : token, i : i.clone() }
  }
}

pub fn xxx_global_setup() {
  let i = Instance {
    users : Default::default(),
    gs : xxx_gamestate_init(),
  };
  let i = Arc::new(Mutex::new(i));
  let mut ig = i.lock().unwrap();
  for (token, nick) in XXX_USERS_TOKENS {
    let nu = Player {
      nick : nick.to_string(),
      clients : Default::default(),
    };
    let user = ig.users.insert(nu);
    let ia = InstanceAccessDetails { i : i.clone(), user };
    GLOBAL.tokens.write().unwrap().insert(
      RawToken(token.to_string()), ia
    );
  }
}
