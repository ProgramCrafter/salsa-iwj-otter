
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
  // todo: RemovePiece
  AddPlayer(PlayerState),
  RemovePlayer(PlayerId),
  ResetPlayerAccesses { players: Vec<PlayerId> },
  ReportPlayerAccesses { players: Vec<PlayerId> },
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
  PlayerAccessTokens { tokens: Vec<Vec<RawToken>> },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameUpdateMode {
  Online,
  Bulk,
}

#[derive(Debug,Clone,Error,Serialize,Deserialize)]
pub enum MgmtError {
  ParseFailed(String),
  AuthorisationError,
  NoScope,
  AlreadyExists,
  GameBeingDestroyed,
  GameNotFound,
  GameCorrupted,
  PlayerNotFound,
  LimitExceeded,
  SVGProcessingFailed(#[from] SVGProcessingError),
  GameError(#[from] GameError),
  ServerFailure(String),
}
impl Display for MgmtError {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    use MgmtError::*;
    match self {
      ServerFailure(s) => write!(f, "ServerFailure: {}", &s)?,
      _ => <Self as Debug>::fmt(self,f)?,
    }
  }
}

impl From<ServerFailure> for MgmtError {
  fn from(e: ServerFailure) -> MgmtError {
    MgmtError::ServerFailure(format!("ServerFailure {}\n", &e))
  }
}
