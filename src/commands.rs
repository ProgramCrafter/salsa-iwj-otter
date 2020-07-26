
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { },
  SetScope(ManagementScope),
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
  XXXU(&'static str),
}
display_as_debug!{MgmtError}

