// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Error,Debug)]
pub enum Fatal { // Includes _bogus_ client updates, see PROTOCOL.md
  #[error("Game in process of being destroyed")]
  GameBeingDestroyed(#[from] GameBeingDestroyed),
  #[error("client session not recognised (terminated by server?)")]
  NoClient,
  #[error("player not part of game (removed?)")]
  NoPlayer(#[from] PlayerNotFound),
  #[error("Server operational problems - consult administrator: {0}")]
  ServerFailure(#[from] InternalError),
  #[error("JSON deserialisation error: {0}")]
  BadJSON(serde_json::Error),
  #[error("Malformed command - loose not allowed for this op")]
  BadLoose,
}

#[derive(Error,Debug)]
pub enum InternalError {
  #[error("Accounts file corrupted for acctid={0:?} account={1:?}")]
  AccountsCorrupted(AccountId, Arc<AccountName>),
  #[error("Error saving accounts file: {0}")]
  AccountsSaveError(#[from] AccountsSaveError),
  #[error("Server MessagePack encoding error {0}")]
  MessagePackEncodeFail(#[from] rmp_serde::encode::Error),
  #[error("Server MessagePack decoding error (game load failed) {0}")]
  MessagePackDecodeFail(#[from] rmp_serde::decode::Error),
  #[error("Duplicate bundle: {index} {kinds:?}")]
  DuplicateBundle { index: bundles::Index, kinds: [bundles::Kind;2] },
  #[error("Server internal logic error {0}")]
  InternalLogicError(InternalLogicError),
  #[error("SVG processing/generation error {0:?}")]
  SVGProcessingFailed(#[from] SVGProcessingError),
  #[error("String formatting error {0}")]
  StringFormatting(#[from] fmt::Error),
  #[error("JSON serialisation error: {0:?}")]
  JSONEncode(serde_json::Error),
  #[error("Server error: {}", .0.d())]
  Anyhow(#[from] anyhow::Error),
  #[error("Game contains only partial data for player, or account missing")]
  PartialPlayerData,
  #[error("Coordinate overflow")]
  CoordinateOverflow(#[from] CoordinateOverflow),
  #[error("Organised placement not possible")]
  OrganisedPlacementFailure,
  #[error("Z Coordinate overflow (game is too crufty?)")]
  ZCoordinateOverflow(#[from] zcoord::Overflow),
  #[error("Multiple errors occurred where only one could be reported")]
  Aggregated,
  #[error("{0}")]
  SshKeysManipError(#[from] sshkeys::AuthKeysManipError),
  #[error("Template rendering error")]
  TemplateRenderingError(#[from] tera::Error),
  #[error("{0}")]
  FutureInstantOutOfRange(#[from] FutureInstantOutOfRange),
}

#[derive(Error,Copy,Clone,Debug,Serialize,Deserialize)]
#[error("Unsupported colour spec")]
pub struct UnsupportedColourSpec;

#[derive(Error)]
pub struct InternalLogicError {
  desc: Cow<'static, str>,
  backtrace: parking_lot::Mutex<backtrace::Backtrace>,
}

pub fn internal_error_bydebug(desc: &dyn Debug) -> IE {
  internal_logic_error(format!("{:?}", desc))
}

impl InternalLogicError {
  pub fn new<S: Into<Cow<'static, str>>>(desc: S) -> InternalLogicError {
    let backtrace = backtrace::Backtrace::new_unresolved();
    InternalLogicError {
      desc: desc.into(),
      backtrace: parking_lot::Mutex::new(backtrace),
    }
  }
}

pub fn internal_logic_error<S: Into<Cow<'static, str>>>(desc: S) -> IE {
  IE::InternalLogicError(InternalLogicError::new(desc))
}

impl Debug for InternalLogicError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut backtrace = self.backtrace.lock();
    backtrace.resolve();
    write!(f, "internal logic error! {}\nbacktrace: {:?}",
           self.desc, &backtrace)
  }
}
display_as_debug!(InternalLogicError);

impl InternalLogicError {
  pub fn tolerate(self) { error!("tolerating {}", self); }
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
  Fatal(#[from] Fatal),
  Inapplicable(#[from] Inapplicable),

  /// This error is always generated in the context of a piece
  /// operation by a particular client.  It corresponds roughly to a
  /// PieceUpdateFromOp for other clients of (Unpredicable,
  /// PieceUpdateOp::Modify, ..).
  /// For this client it is that but also an error report.
  PartiallyProcessed(Inapplicable, Vec<LogEntry>),
}
display_as_debug!(ApiPieceOpError);

impl From<PlayerNotFound> for ApiPieceOpError {
  fn from(x: PlayerNotFound) -> ApiPieceOpError {
    ApiPieceOpError::Fatal(x.into())
  }
}
impl From<InternalError> for ApiPieceOpError {
  fn from(x: InternalError) -> ApiPieceOpError {
    ApiPieceOpError::Fatal(x.into())
  }
}

#[derive(Debug,Serialize,Clone)]
pub enum ErrorSignaledViaUpdate<POEPU: Debug, EM: Debug> {
  InternalError,
  PlayerRemoved, // appears only in streams for applicable player
  TokenRevoked, // appears only in streams for applicable player
  PieceOpError {
    error: Inapplicable,
    error_msg: EM,
    partially: PieceOpErrorPartiallyProcessed,
    state: POEPU,
  },
}
//display_as_debug!{ErrorSignaledViaUpdate<T>, <T:Debug>}

#[derive(Error,Debug,Serialize,Copy,Clone,Eq,PartialEq)]
pub enum PieceOpErrorPartiallyProcessed {
  Unprocessed,
  Partially,
}
display_as_debug!{PieceOpErrorPartiallyProcessed}

#[derive(Error,Debug,Serialize,Copy,Clone)]
pub enum Inapplicable {
  #[error("simultaneous update")]           Conflict,
  #[error("position off table")]            PosOffTable,
  #[error("piece gone")]                    PieceGone,
  #[error("piece held by another player")]  PieceHeld,
  #[error("piece not held by you")]         PieceNotHeld,
  #[error("prevented by occultation")]      Occultation,
  #[error("piece may not be rotated")]      PieceUnrotateable,
  #[error("piece may not be moved")]        PieceImmoveable,
  #[error("occulter already rotated")]      OcculterAlreadyRotated,
  #[error("overfull, cannot organise")]     OrganisedPlacementOverfull,
  #[error("overlapping occultation(s)")]    OverlappingOccultation,
  #[error("die was recently rolled, cannot flip or roll")] DieCooldown,
  #[error("UI operation not recognised")]   BadUiOperation,
  #[error("UI operation not valid in the curret piece state")]
                                            BadPieceStateForOperation,
}

pub type StartupError = anyhow::Error;

pub use Fatal::{NoClient,NoPlayer};

pub enum AggregatedIE {
  Ok,
  One(InternalError),
  Many,
}

impl Default for AggregatedIE {
  fn default() -> Self { Self::Ok }
}

impl AggregatedIE {
  pub fn new() -> Self { default() }

  pub fn handle<T>(&mut self, r: Result<T, InternalError>) -> Option<T> {
    match r {
      Ok(t) => Some(t),
      Err(e) => { self.record(e); None }
    }
  }

  pub fn record(&mut self, e: InternalError) {
    error!("error occurred in aggregating-errors contest: {}", &e);
    *self = match self {
      Self::Ok => Self::One(e),
      _ => Self::Many,
    }
  }
  pub fn ok(self) -> Result<(),IE> {
    match self {
      Self::Ok => Ok(()),
      Self::One(e) => Err(e),
      Self::Many => Err(IE::Aggregated),
    }
  }
}

#[derive(Error,Clone,Debug,Serialize,Deserialize)]
#[error("game is being destroyed")]
pub struct GameBeingDestroyed;

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

impl<T> IdForById for T where T: AccessId {
  type Error = T::Error;
  const ERROR: Self::Error = <Self as AccessId>::ERROR;
}

impl IdForById for PieceId {
  type Error = Ia;
  const ERROR: Ia = Ia::PieceGone;
}

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
