
use crate::imports::*;

use std::sync::PoisonError;

#[derive(Error,Debug)]
pub enum OnlineError {
  #[error("Game corrupted by previous crash - consult administrator")]
  GameCorrupted,
  #[error("client session not recognised (terminated by server?)")]
  NoClient,
  #[error("player not part of game (removed?)")]
  NoPlayer,
}

pub use OnlineError::{NoClient,NoPlayer};

use OnlineError::*;

impl<X> From<PoisonError<X>> for OnlineError {
  fn from(_: PoisonError<X>) -> OnlineError { GameCorrupted }
}


pub trait ById {
  type Id;
  type Entry;
  #[throws(OE)]
  fn byid(&self, t: Self::Id) -> &Self::Entry;
}

impl<I:AccessId+slotmap::Key, T> ById for DenseSlotMap<I,T> {
  type Id = I;
  type Entry = T;
  fn byid(&self, t: Self::Id) -> Result<&Self::Entry, OE> {
    self.get(t).ok_or(<I as AccessId>::ERROR)
  }
}
impl<I:AccessId+slotmap::Key, T> ById for SecondarySlotMap<I,T> {
  type Id = I;
  type Entry = T;
  fn byid(&self, t: Self::Id) -> Result<&Self::Entry, OE> {
    self.get(t).ok_or(<I as AccessId>::ERROR)
  }
}
