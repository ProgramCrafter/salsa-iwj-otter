
#![allow(dead_code)]

use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub struct GameSpec {
  pub table : Pos,
  pub players : Vec<PlayerSpec>,
  pub pieces : Vec<PiecesSpec>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PlayerSpec {
  pub nick: String,
  #[serde(flatten)]
  pub access: Box<dyn PlayerAccessSpec>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PiecesSpec {
  pub pos : Option<Pos>,
  pub posd : Option<Pos>,
  pub count : Option<u32>,
  pub face : Option<FaceId>,
  #[serde(flatten)]
  pub info : Box<dyn PieceSpec>,
}

#[typetag::serde(tag="access")]
pub trait PlayerAccessSpec : Debug {
  #[throws(OE)]
  fn deliver_token(&mut self) -> Result<(),OE>;
}
