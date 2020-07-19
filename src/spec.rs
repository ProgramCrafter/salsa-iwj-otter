
#![allow(dead_code)]

use crate::imports::*;

struct GameSpec {
  players : Vec<PlayerSpec>
}

struct PlayerSpec {
  nick: String,
  access: Box<dyn PlayerAccessSpec>,
}

trait PlayerAccessSpec {
  #[throws(OE)]
  fn make_token(&self) -> RawToken { RawToken::new_random()? }
}
