
#![allow(dead_code)]

use crate::imports::*;

//---------- Table TOML file ----------

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
struct FixedToken { token: RawToken }

#[derive(Debug,Serialize,Deserialize)]
struct TokenOnStdout;

//---------- Game TOML file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameSpec {
  pub table_size : Option<Pos>,
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

//----------  Implementation ----------

#[typetag::serde(tag="access")]
pub trait PlayerAccessSpec : Debug {
  fn token_mgi(&self, _player: PlayerId) -> Option<MgmtGameInstruction> {
    None
  }
  fn deliver_tokens(&self, ps: &PlayerSpec, tokens: &[RawToken])
    -> Result<(),AE>;
}

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

#[typetag::serde]
impl PlayerAccessSpec for TokenOnStdout {
  #[throws(AE)]
  fn deliver_tokens(&self, ps: &PlayerSpec, tokens: &[RawToken]) {
    for token in tokens {
      println!("access nick={:?} token={}", &ps.nick, token.0);
    }
  }
}
