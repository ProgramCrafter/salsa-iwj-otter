
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { },
  SetScope { scope: ManagementScope },
  CreateGame { name: String, insns: Vec<MgmtGameInstruction> },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameInstruction {
  Noop { },
  //  AddPiece(Box<dyn PieceSpec>),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine { },
  Error { error: MgmtError },
  ErrorAfter { successes: usize, error: MgmtError },
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

