
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop(),
  SetScope(ManagementScope),
  CreateGame(String, Vec<MgmtGameUpdate>),
//  AddPiece(Box<dyn PieceSpec>),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameUpdate {
  Noop(),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine(),
  Error(MgmtError),
  ErrorAfter(usize, MgmtError),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameUpdateMode {
  Online,
  Bulk,
}

#[derive(Debug,Error,Serialize,Deserialize)]
pub enum MgmtError {
  ParseFailed(String),
  AuthorisationError,
  NoScope,
  AlreadyExists,
  GameBeingDestroyed,
  GameCorrupted,
  SVGProcessingFailed(#[from] SVGProcessingError),
}
display_as_debug!{MgmtError}

