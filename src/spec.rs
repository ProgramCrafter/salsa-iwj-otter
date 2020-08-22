// game specs

use serde::{Serialize,Deserialize};
use fehler::throws;
use index_vec::define_index_type;
use crate::gamestate::PieceSpec;
use std::fmt::Debug;
use implementation::PlayerAccessSpec;

//---------- common types ----------

pub type Coord = isize;

pub type Pos = [Coord; 2];

#[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken (pub String);

define_index_type! {
  #[derive(Default)]
  pub struct FaceId = u8;
}

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
  pub info : Box<dyn PieceSpec>, // see pieces.rs
}

//----------  Implementation ----------

mod implementation {
  use super::*;
  use crate::imports::*;
  type Insn = crate::commands::MgmtGameInstruction;

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
      Some(Insn::SetFixedPlayerAccess {
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
}
