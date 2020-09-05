// game specs

use serde::{Serialize,Deserialize};
use fehler::throws;
use index_vec::{define_index_type,IndexVec};
use crate::gamestate::PieceSpec;
use std::fmt::Debug;
use implementation::PlayerAccessSpec;
use thiserror::Error;
use crate::error::display_as_debug;

//---------- common types ----------

pub type Coord = isize;

pub type Pos = [Coord; 2];

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken (pub String);

define_index_type! {
  #[derive(Default)]
  pub struct FaceId = u8;
}

#[derive(Serialize,Deserialize)]
#[derive(Debug,Default)]
#[repr(transparent)]
pub struct ColourSpec(String);

#[derive(Error,Clone,Serialize,Deserialize,Debug)]
pub enum SpecError {
  ImproperSizeSpec,
  UnsupportedColourSpec,
  FaceNotFound,
  InternalError(String),
  PosOffTable,
}
display_as_debug!{SpecError}

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

//---------- Piece specs ----------
// the implementations are in pieces.rs

pub mod piece_specs {
  use super::*;

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Disc {
    pub diam : Coord,
    pub faces : IndexVec<FaceId,ColourSpec>,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Square {
    pub size : Vec<Coord>,
    pub faces : IndexVec<FaceId,ColourSpec>,
  }

}

//---------- Implementation ----------

mod implementation {
  use super::*;
  use crate::imports::*;
  type Insn = crate::commands::MgmtGameInstruction;

  impl Debug for RawToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let s = &self.0;
      let len = min(5, s.len() / 2);
      write!(f, "{:?}...", &s[0..len])
    }
  }

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

  impl TryFrom<&ColourSpec> for Colour {
    type Error = SpecError;
    #[throws(SpecError)]
    fn try_from(spec: &ColourSpec) -> Colour {
      lazy_static! {
        static ref RE: Regex = Regex::new(concat!(
          r"^(?:", r"[[:alpha:]]{1,50}",
             r"|", r"#[[:xdigit:]]{3}{1,2}",
             r"|", r"(?:rgba?|hsla?)\([-.%\t 0-9]{1,50}\)",
            r")$"
        )).unwrap();
      }
      let s = &spec.0;
      if !RE.is_match(s) {
        throw!(SpecError::UnsupportedColourSpec);
      }
      spec.0.clone()
    }
  }
}
