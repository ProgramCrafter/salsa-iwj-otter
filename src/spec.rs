// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// game specs

use serde::{Serialize,Deserialize};
use fehler::throws;
use index_vec::{define_index_type,IndexVec};
use crate::gamestate::PieceSpec;
use std::fmt::Debug;
use std::collections::hash_set::HashSet;
use thiserror::Error;
use crate::error::display_as_debug;
use crate::accounts::AccountName;
use std::hash::Hash;
use num_derive::{ToPrimitive, FromPrimitive};

pub use implementation::PlayerAccessSpec;

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
  pub players : Vec<AccountName>,
  pub acl : Acl<TablePermission>
}

type RawAcl<Perm> = Vec<AclEntry<Perm>>;

#[derive(Debug,Clone)]
#[derive(Deserialize)]
#[serde(try_from="RawAcl<Perm>")]
pub struct Acl<Perm: Eq + Hash> { pub ents: RawAcl<Perm> }

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct AclEntry<Perm: Eq + Hash> {
  pub account_glob: String, // checked
  pub allow: HashSet<Perm>,
  pub deny: HashSet<Perm>,
}

#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
#[derive(Hash,Eq,PartialEq,Ord,PartialOrd)]
#[derive(FromPrimitive,ToPrimitive)]
pub enum TablePermission {
  TestExistence,
  ViewPublic,
  AddPlayer,
  ChangePieces,
  ResetOthersAccess,
  RedeliverOthersAccess,
  ModifyOtherPlayer,
  RemovePlayer,
  ChangeACL,
}

//---------- player accesses, should perhaps be in commands.rs ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct PlayerAccessUnset;

#[derive(Debug,Serialize,Deserialize)]
struct FixedToken { token: RawToken }

#[derive(Debug,Serialize,Deserialize)]
struct UrlOnStdout;

//#[derive(Debug,Serialize,Deserialize)]
//struct TokenByEmail { email: String };
// xxx ^ implement this
// xxx ^ 

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

  impl<P: Eq + Hash> Default for Acl<P> {
    fn default() -> Self { Acl { ents: default() } }
  }

  impl<P: Eq + Hash + Serialize> Serialize for Acl<P> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error>
    { self.ents.serialize(s) }
  }

  impl<P: Eq + Hash> From<RawAcl<P>> for Acl<P> {
    fn from(ents: RawAcl<P>) -> Self {
      // xxx check
      Acl { ents }
    }
  }

  impl loaded_acl::Perm for TablePermission {
    type Auth = InstanceName;
    const TEST_EXISTENCE : Self = TablePermission::TestExistence;
    const NOT_FOUND : MgmtError = MgmtError::GameNotFound;
  }

  type TDE = TokenDeliveryError;

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
    fn override_token(&self) -> Option<&RawToken> {
      // xxx check this on setting access
      None
    }
    fn server_deliver<'t>(&self,
                          gpl: &GPlayerState,
                          ipl: &IPlayerState,
                          token: &'t AccessTokenReport)
                          -> Result<Option<&'t AccessTokenReport>, TDE> {
      Ok(None)
    }
    fn client_deliver(&self,
                      pi: &MgmtPlayerInfo,
                      token: &AccessTokenReport)
                      -> Result<(), TDE> {
      panic!()
    }
    fn describe_html(&self) -> Html {
      let inner = Html::from_txt(&format!("{:?}", self));
      Html(format!("<code>{}</code>", inner.0))
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for PlayerAccessUnset {
  }

  #[typetag::serde]
  impl PlayerAccessSpec for FixedToken {
    fn override_token(&self) -> Option<&RawToken> {
      Some(&self.token)
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for UrlOnStdout {
    #[throws(TDE)]
    fn server_deliver<'t>(&self,
                          gpl: &GPlayerState,
                          ipl: &IPlayerState,
                          token: &'t AccessTokenReport)
                          -> Option<&'t AccessTokenReport> {
      Some(token)
    }
    #[throws(TDE)]
    fn client_deliver(&self,
                      pi: &MgmtPlayerInfo,
                      token: &AccessTokenReport) {
      println!("access account={} nick={:?} url:\n{}",
               &pi.account, &pi.nick, token.url);
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
