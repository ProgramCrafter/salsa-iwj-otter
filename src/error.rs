// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

type IE = InternalError;

#[derive(Error,Debug)]
pub enum InternalError {
  #[error("Game corrupted by previous crash")]
  GameCorrupted,
  #[error("Accounts file corrupted for acctid={0:?} account={1:?}")]
  AccountsCorrupted(AccountId, Arc<AccountName>),
  #[error("Error saving accounts file: {0}")]
  AccountsSaveError(#[from] AccountsSaveError),
  #[error("Server MessagePack encoding error {0}")]
  MessagePackEncodeFail(#[from] rmp_serde::encode::Error),
  #[error("Server MessagePack decoding error (game load failed) {0}")]
  MessagePackDecodeFail(#[from] rmp_serde::decode::Error),
  #[error("Server internal logic error {0}")]
  InternalLogicError(String),
  #[error("SVG processing/generation error {0:?}")]
  SVGProcessingFailed(#[from] SVGProcessingError),
  #[error("String formatting error {0}")]
  StringFormatting(#[from] fmt::Error),
  #[error("JSON deserialisation error: {0:?}")]
  JSONEncode(serde_json::Error),
  #[error("Server error {0:?}")]
  Anyhow(#[from] anyhow::Error),
  #[error("Game contains only partial data for player, or account missing")]
  PartialPlayerData,
}

#[derive(Clone,Error,Debug,Serialize,Deserialize)]
#[error("{0}")]
pub struct TokenDeliveryError(String);
impl From<anyhow::Error> for TokenDeliveryError {
  fn from(a: anyhow::Error) -> TokenDeliveryError {
    TokenDeliveryError(
      a.chain()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(": "),
    )
  }
}

impl From<InternalError> for SpecError {
  fn from(ie: InternalError) -> SpecError {
    SpecError::InternalError(format!("{:?}", ie))
  }
}

#[derive(Error,Debug)]
pub enum ApiPieceOpError {
  ReportViaResponse(#[from] OnlineError),
  ReportViaUpdate(#[from] PieceOpError),
  PartiallyProcessed(PieceOpError, Vec<LogEntry>),
}
display_as_debug!(ApiPieceOpError);

impl From<PlayerNotFound> for ApiPieceOpError {
  fn from(x: PlayerNotFound) -> ApiPieceOpError {
    ApiPieceOpError::ReportViaResponse(x.into())
  }
}

#[derive(Error,Debug,Serialize,Clone)]
pub enum ErrorSignaledViaUpdate {
  InternalError,
  PlayerRemoved,
  TokenRevoked,
  PieceOpError {
    piece: VisiblePieceId,
    error: PieceOpError,
    state: PreparedPieceState,
  },
}
display_as_debug!{ErrorSignaledViaUpdate}

#[derive(Error,Debug,Serialize,Copy,Clone)]
pub enum PieceOpError {
  Conflict,
  PosOffTable,
}
display_as_debug!{PieceOpError}

pub type StartupError = anyhow::Error;

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
          GameBeingDestroyed => $into::GameBeingDestroyed,
          GameCorrupted      => InternalError::GameCorrupted.into(),
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
  #[allow(clippy::declare_interior_mutable_const)]
  // https://github.com/rust-lang/rust-clippy/issues/3962#issuecomment-667957112
  const ERROR: Self::Error;
}

macro_rules! some_slotmap {
  ($slotmap:ident) => {
    impl<I: IdForById + slotmap::Key, T> ById for $slotmap<I,T> {
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
  type Error = T::Error;
  const ERROR : Self::Error = <Self as AccessId>::ERROR;
}

impl IdForById for PieceId {
  type Error = OE;
  const ERROR : OE = OE::PieceGone;
}

#[macro_export]
macro_rules! display_as_debug {
  {$x:ty} => {
    impl std::fmt::Display for $x {
      #[throws(std::fmt::Error)]
      fn fmt(&self, f: &mut std::fmt::Formatter) {
        <Self as Debug>::fmt(self, f)?
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

impl From<SVGProcessingError> for SpecError {
  fn from(se: SVGProcessingError) -> SpecError {
    InternalError::SVGProcessingFailed(se).into()
  }
}

impl From<AccountsSaveError> for MgmtError {
  fn from(ase: AccountsSaveError) -> MgmtError { IE::from(ase).into() }
}
