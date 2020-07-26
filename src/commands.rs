
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { },
  SetScope(ManagementScope),
  CreateGame(String),
//  AddPiece(Box<dyn PieceSpec>),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine { },
  Error(String),
}

#[derive(Debug,Error)]
pub enum MgmtError {
  ParseFailed(String),
  AuthorisationError,
  NoScope,
  AlreadyExists,
  XXXU(&'static str),
}
display_as_debug!{MgmtError}

