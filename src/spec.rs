
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

#[typetag::serde(tag="deliver")]
pub trait PlayerAccessSpec : Debug {
  fn token_mgi(&self, _player: PlayerId) -> Option<MgmtGameInstruction> {
    None
  }
  fn deliver_tokens(&self, ps: &PlayerSpec, tokens: &[RawToken])
    -> Result<(),AE>;
}

#[derive(Debug,Serialize,Deserialize)]
struct FixedToken { token: RawToken }

#[typetag::serde]
impl PlayerAccessSpec for FixedToken {
  fn token_mgi(&self, player: PlayerId) -> Option<MgmtGameInstruction> {
    Some(MgmtGameInstruction::SetFixedPlayerAccess {
      player,
      token: self.token.clone(),
    })
  }
  #[throws(AE)]
  fn deliver_tokens(&self, _ps: &PlayerSpec, _tokens: &[RawToken]) { }
}

#[derive(Debug,Serialize,Deserialize)]
struct TokenOnStdout;

#[typetag::serde]
impl PlayerAccessSpec for TokenOnStdout {
  #[throws(AE)]
  fn deliver_tokens(&self, ps: &PlayerSpec, tokens: &[RawToken]) {
    for token in tokens {
      println!("access nick={:?} token={}", &ps.nick, token.0);
    }
  }
}
