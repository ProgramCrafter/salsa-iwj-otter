
use crate::imports::*;
use lazy_static::lazy_static;

slotmap::new_key_type!{
  struct UserId;
  struct ClientId;
}

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
struct RawToken (String);
impl Borrow<str> for RawToken {
  fn borrow(&self) -> &str { &self.0 }
}

struct Client {
  user : UserId,
}

struct User {
  nick : String,
}

struct Instance {
  /* game state goes here */
  users : DenseSlotMap<UserId,User>,
  clients : DenseSlotMap<ClientId,Client>,
}

#[derive(Clone)]
struct InstanceAccess {
  i : Arc<Mutex<Instance>>,
  user : UserId,
}

#[derive(Default)]
struct Global {
  // lock hierarchy: this is the innermost lock
  tokens : RwLock<HashMap<RawToken, InstanceAccess>>,
  // xxx delete instances at some point!
}

lazy_static! {
  static ref GLOBAL : Global = Default::default();
}

fn lookup_token(s : &str) -> Option<InstanceAccess> {
  GLOBAL.tokens.read().unwrap().get(s).cloned()
}

const XXX_USERS_TOKENS : &[(&str, &str)] = &[
  ("kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe", "alice"),
  ("ccg9kzoTh758QrVE1xMY7BQWB36dNJTx", "bob"),
];

pub fn xxx_global_setup() {
  let i = Instance {
    users : Default::default(),
    clients : Default::default(),
  };
  let i = Arc::new(Mutex::new(i));
  let mut ig = i.lock().unwrap();
  for (token, nick) in XXX_USERS_TOKENS {
    let nu = User { nick : nick.to_string() };
    let user = ig.users.insert(nu);
    let ia = InstanceAccess { i : i.clone(), user };
    GLOBAL.tokens.write().unwrap().insert(
      RawToken(token.to_string()), ia
    );
  }
}
