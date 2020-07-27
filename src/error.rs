
use crate::imports::*;

#[derive(Error,Debug,Serialize,Deserialize)]
#[error("operation error {:?}",self)]
pub enum GameError {
  Conflict,
  PieceGone,
  PieceHeld,
  FaceNotFound,
  InternalErrorSVG(#[from] SVGProcessingError),
}

#[derive(Error,Debug)]
pub enum OnlineError {
  #[error("Game in process of being destroyed")]
  GameBeingDestroyed,
  #[error("Game corrupted by previous crash - consult administrator")]
  GameCorrupted,
  #[error("client session not recognised (terminated by server?)")]
  NoClient,
  #[error("player not part of game (removed?)")]
  NoPlayer,
  #[error("invalid Z coordinate")]
  InvalidZCoord,
  #[error("JSON~ serialisation error: {0:?}")]
  JSONSerializeFailed(#[from] serde_json::error::Error),
  #[error("SVG processing/generation error {0:?}")]
  SVGProcessingFailed(#[from] SVGProcessingError),
  #[error("Server operational problems: {0:?}")]
  ServerFailure(#[from] ServerFailure),
}
from_instance_lock_error!{OnlineError}

#[derive(Error,Debug)]
pub enum ServerFailure {
  #[error("Server IO error {0:?}")]
  IO(#[from] io::Error),
  #[error("Server MessagePack encoding error {0:?}")]
  MessagePackEncodeFail(#[from] rmp_serde::encode::Error),
  #[error("Server MessagePack decoding error (game load failed) {0:?}")]
  MessagePackDecodeFail(#[from] rmp_serde::decode::Error),
}

pub type StartupError = anyhow::Error;

pub use OnlineError::{NoClient,NoPlayer};

#[derive(Error,Debug)]
pub enum InstanceLockError {
  GameCorrupted,
  GameBeingDestroyed,
}
#[macro_export]
macro_rules! from_instance_lock_error {
  ($into:ident) => {
    impl From<InstanceLockError> for $into {
      fn from(e: InstanceLockError) -> $into {
        use InstanceLockError::*;
        match e {
          GameCorrupted      => $into::GameCorrupted,
          GameBeingDestroyed => $into::GameBeingDestroyed,
        }
      }
    }
  }
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

macro_rules! some_slotmap {
  ($slotmap:ident) => {
    impl<I:IdForById+slotmap::Key, T> ById for $slotmap<I,T> {
      type Id = I;
      type Entry = T;
      type Error = <I as IdForById>::Error;
      fn byid    (&    self, t: Self::Id) -> Result<&    T, Self::Error> {
        self.get    (t).ok_or(<I as IdForById>::ERROR)
      }
      fn byid_mut(&mut self, t: Self::Id) -> Result<&mut T, Self::Error> {
        self.get_mut(t).ok_or(<I as IdForById>::ERROR)
      }
    }
  }
}

some_slotmap!{DenseSlotMap}
some_slotmap!{SecondarySlotMap}

impl<T> IdForById for T where T : AccessId {
  type Error = OE;
  const ERROR : OE = <Self as AccessId>::ERROR;
}

impl IdForById for PieceId {
  type Error = GameError;
  const ERROR : GameError = GameError::PieceGone;
}

#[macro_export]
macro_rules! display_as_debug {
  {$x:ty} => {
    impl Display for $x {
      fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <Self as Debug>::fmt(self, f)
      }
    }
  }
}
pub use crate::display_as_debug;

#[macro_export]
macro_rules! error_from_losedetails {
  {$to:ty, $variant:ident, $from:ty} => {
    impl From<$from> for $to {
      fn from(_: $from) -> Self { <$to>::$variant }
    }
  }
}
pub use crate::error_from_losedetails;
