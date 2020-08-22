
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
  fn token_mgi(&self, player: PlayerId) -> Option<MgmtGameInstructions> {
    None
  }
  #[throws(AE)]
  fn deliver_token(&self, ps: &PlayerSpec, tokens: &[RawToken]) { }
}

#[derive(Debug,Serialize,Deserialize)]
struct FixedToken(RawToken);

#[typetag::serde]
impl PlayerAccessSpec for FixedToken {
  fn token_mgi(&self, player: PlayerId) -> Option<MgmtGameInstructions> {
    Some(MgmtGmmeInstruction::SetFixedPlayerAccess {
      player,
      token: self.0.clone(),
    })
  }
}

#[derive(Debug,Serialize,Deserialize)]
struct TokenOnStdout;

#[typetag::serde]
impl PlayerAccessSpec for FixedToken {
  #[throws(AE)]
  fn deliver_tokens(&self, ps: &PlayerSpec, token: &[RawToken]) {
                   -> Result<(),anyhow::Error> {
    println!("access nick={:?} token={}", &ps.nick, token);
  }
}
