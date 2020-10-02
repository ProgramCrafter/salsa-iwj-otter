// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

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

#[derive(Clone,Copy,Debug,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[serde(transparent)]
pub struct PosC<T> (pub [T; 2]);
pub type Pos = PosC<Coord>;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken (pub String);

pub type RawFaceId = u8;
define_index_type! {
  #[derive(Default)]
  pub struct FaceId = RawFaceId;
  IMPL_RAW_CONVERSIONS = true;
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
  LibraryNotFound,
  LibraryItemNotFound,
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
  pub pinned: Option<bool>,
  #[serde(flatten)]
  pub info : Box<dyn PieceSpec>,
}

//---------- Piece specs ----------
// the implementations are in pieces.rs

pub mod piece_specs {
  use super::*;

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Disc {
    pub itemname: Option<String>,
    pub diam : Coord,
    pub faces : IndexVec<FaceId,ColourSpec>,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Square {
    pub itemname: Option<String>,
    pub size : Vec<Coord>,
    pub faces : IndexVec<FaceId,ColourSpec>,
  }

}

//---------- Pos ----------

pub mod pos_traits {
  use std::ops::{Add,Sub,Mul,Neg,AddAssign,SubAssign};
  use crate::imports::*;

  impl<T:Add<T,Output=T>+Copy+Clone+Debug> Add<PosC<T>> for PosC<T> {
    type Output = PosC<T>;
    fn add(self, rhs: PosC<T>) -> PosC<T> {
      PosC(
        itertools::zip_eq(
          self.0.iter().cloned(),
          rhs .0.iter().cloned(),
        ).map(|(a,b)| a + b)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Sub<T,Output=T>+Copy+Clone+Debug> Sub<PosC<T>> for PosC<T> {
    type Output = PosC<T>;
    fn sub(self, rhs: PosC<T>) -> PosC<T> {
      PosC(
        itertools::zip_eq(
          self.0.iter().cloned(),
          rhs .0.iter().cloned(),
        ).map(|(a,b)| a - b)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Add<T,Output=T>+Copy+Clone+Debug> AddAssign<PosC<T>> for PosC<T> {
    fn add_assign(&mut self, rhs: PosC<T>) {
      *self = *self + rhs;
    }
  }

  impl<T:Sub<T,Output=T>+Copy+Clone+Debug> SubAssign<PosC<T>> for PosC<T> {
    fn sub_assign(&mut self, rhs: PosC<T>) {
      *self = *self - rhs;
    }
  }

  impl<T:Mul<T,Output=T>+Copy+Clone+Debug> Mul<T> for PosC<T> {
    type Output = PosC<T>;
    fn mul(self, rhs: T) -> PosC<T> {
      PosC(
        self.0.iter().cloned().map(|a| a * rhs)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Neg<Output=T>+Copy+Clone+Debug> Neg for PosC<T> {
    type Output = Self;
    fn neg(self) -> Self {
      PosC(
        self.0.iter().cloned().map(|a| -a)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Copy+Clone+Debug> PosC<T> {
    pub fn map<U:Copy+Clone+Debug, F: FnMut(T) -> U>(self, f: F) -> PosC<U> {
      PosC(
        self.0.iter().cloned().map(f)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl PosC<Coord> {
    pub fn promote(&self) -> PosC<f64> { self.map(|v| v as f64) }
  }
}

//---------- Implementation ----------

pub mod implementation {
  use super::*;
  use crate::imports::*;
  type Insn = crate::commands::MgmtGameInstruction;

  pub fn raw_token_debug_as_str(s: &str, f: &mut fmt::Formatter)
                                -> fmt::Result {
    let len = min(5, s.len() / 2);
    write!(f, "{:?}...", &s[0..len])
  }

  impl Debug for RawToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      raw_token_debug_as_str(&self.0, f)
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
      Html(spec.0.clone())
    }
  }
}
