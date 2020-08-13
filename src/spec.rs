
#![allow(dead_code)]

use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub struct TableSpec {
  pub players : Vec<PlayerSpec>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PlayerSpec {
  pub nick: String,
  #[serde(flatten)]
  pub access: Option<Box<dyn PlayerAccessSpec>>,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct GameSpec {
  pub table : Pos,
  pub pieces : Vec<PiecesSpec>,
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
  fn deliver_token_client(&self, conn: &mut MgmtConnection, nick: &str)
                          -> Result<(),anyhow::Error>;
}

#[derive(Debug,Serialize,Deserialize)]
struct UrlOnStdout;

#[typetag::serde]
impl PlayerAccessSpec for UrlOnStdout {
  fn deliver_token_client(&self, conn: &mut MgmtConnection, nick: &str)
                          -> Result<(),anyhow::Error> {
    todo!()
  }
}
