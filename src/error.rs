
use crate::imports::*;

use std::sync::PoisonError;

#[derive(Error,Debug)]
pub enum OnlineError {
  #[error("Game corrupted by previous crash - consult administrator")]
  GameCorrupted,
  #[error("client session not recognised (terminated by server?)")]
  NoClientSession,
  #[error("player not part of game (removed?)")]
  NoPlayer,
}

pub use OnlineError::{NoClientSession,NoPlayer};

use OnlineError::*;

impl<X> From<PoisonError<X>> for OnlineError {
  fn from(_: PoisonError<X>) -> OnlineError { GameCorrupted }
}
