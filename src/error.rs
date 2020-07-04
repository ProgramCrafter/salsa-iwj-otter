
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
  type Error;
  #[throws(Self::Error)]
  fn byid(&self, t: Self::Id) -> &Self::Entry;
  #[throws(Self::Error)]
  fn byid_mut(&mut self, t: Self::Id) -> &mut Self::Entry;
}

pub trait IdForById {
  type Error;
  const ERROR : Self::Error;
}

impl<I:IdForById+slotmap::Key, T> ById for DenseSlotMap<I,T> {
  type Id = I;
  type Entry = T;
  type Error = <I as IdForById>::Error;
  fn byid(&self, t: Self::Id) -> Result<&Self::Entry, Self::Error> {
    self.get(t).ok_or(<I as IdForById>::ERROR)
  }
  fn byid_mut(&mut self, t: Self::Id) -> Result<&mut Self::Entry, Self::Error> {
    self.get_mut(t).ok_or(<I as IdForById>::ERROR)
  }
}
impl<I:IdForById+slotmap::Key, T> ById for SecondarySlotMap<I,T> {
  type Id = I;
  type Entry = T;
  type Error = <I as IdForById>::Error;
  fn byid(&self, t: Self::Id) -> Result<&Self::Entry, Self::Error> {
    self.get(t).ok_or(<I as IdForById>::ERROR)
  }
  fn byid_mut(&mut self, t: Self::Id) -> Result<&mut Self::Entry, Self::Error> {
    self.get_mut(t).ok_or(<I as IdForById>::ERROR)
  }
}

impl<T> IdForById for T where T : AccessId {
  type Error = OE;
  const ERROR : OE = <Self as AccessId>::ERROR;
}
