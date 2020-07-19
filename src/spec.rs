
#![allow(dead_code)]

use crate::imports::*;

#[derive(Deserialize)]
struct GameSpec {
  table : Pos,
  players : Vec<PlayerSpec>,
  pieces : Vec<PiecesSpec>,
}

#[derive(Deserialize)]
struct PlayerSpec {
  nick: String,
  #[serde(flatten)]
  access: Box<dyn PlayerAccessSpec>,
}

#[derive(Deserialize)]
struct PiecesSpec {
  pos : Option<Pos>,
  count : Option<u32>,
  name : Option<String>,
  #[serde(flatten)]
  info : Box<dyn PieceSpec>,
}

#[typetag::deserialize(tag="access")]
trait PlayerAccessSpec {
  #[throws(OE)]
  fn make_token(&self) -> RawToken { RawToken::new_random()? }
  fn deliver_token(&mut self) -> Result<(),OE>;
}

#[typetag::deserialize]
impl PlayerAccessSpec for RawToken {
  #[throws(OE)]
  fn make_token(&self) -> RawToken { self.clone() }
  #[throws(OE)]
  fn deliver_token(&mut self) { }
}
