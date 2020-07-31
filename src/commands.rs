
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { },
  SetScope { scope: ManagementScope },
  CreateGame { name: String, insns: Vec<MgmtGameInstruction> },
  ListGames { all: Option<bool>, },
  AlterGame {
    name: String, insns: Vec<MgmtGameInstruction>,
    how: MgmtGameUpdateMode,
  },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameInstruction {
  Noop { },
  AddPiece(PiecesSpec),
  AddPlayer(PlayerState),
  RemovePlayer(PlayerId),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine { },
  Error { error: MgmtError },
  AlterGame { error: Option<MgmtError>, results: Vec<MgmtGameResult> },
  GamesList { games: Vec<Arc<InstanceName>> },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameResult {
  Fine { },
  AddPlayer { player: PlayerId },
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
  GameNotFound,
  GameCorrupted,
  LimitExceeded,
  SVGProcessingFailed(#[from] SVGProcessingError),
  GameError(#[from] GameError),
  ServerFailure(String),
}
display_as_debug!{MgmtError}

impl From<ServerFailure> for MgmtError {
  fn from(e: ServerFailure) -> MgmtError {
    MgmtError::ServerFailure(format!("ServerFailure {}\n", &e))
  }
}
