// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop,
  SetScope(ManagementScope),
  CreateGame { name: String, insns: Vec<MgmtGameInstruction> },
  ListGames { all: Option<bool>, },
  AlterGame {
    // todo option scope, access control
    name: String, insns: Vec<MgmtGameInstruction>,
    how: MgmtGameUpdateMode,
  },
  LibraryListByGlob {
    glob: shapelib::ItemSpec,
  },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine,
  Error { error: MgmtError },
  AlterGame { error: Option<MgmtError>, responses: Vec<MgmtGameResponse> },
  GamesList(Vec<Arc<InstanceName>>),
  LibraryItems(Vec<shapelib::ItemEnquiryData>),
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameInstruction {
  Noop,
  Info,
  SetTableSize(Pos),

  ListPieces,
  AddPieces(PiecesSpec),
  DeletePiece(PieceId),

  AddPlayer(PlayerState),
  RemovePlayer(PlayerId),
  ResetPlayerAccesses { players: Vec<PlayerId> },
  ReportPlayerAccesses { players: Vec<PlayerId> },
  SetFixedPlayerAccess { player: PlayerId, token: RawToken },
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameResponse {
  Fine,
  Info(MgmtGameResponseGameInfo),

  Pieces(Vec<MgmtGamePieceInfo>),

  AddPlayer(PlayerId),
  PlayerAccessTokens(Vec<Vec<RawToken>>),
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGameResponseGameInfo {
  pub table_size: Pos,
  pub players: PlayerMap,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGamePieceInfo {
  pub piece: PieceId,
  pub itemname: String,
  pub pos: Pos,
  pub face: FaceId,
  pub desc_html: Html,
  pub bbox: [Pos;2],
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
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
  PieceNotFound,
  LimitExceeded,
  ServerFailure(String),
  BadGlob { pat: String, msg: String },
  BadSpec(#[from] SpecError),
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

impl From<InternalError> for MgmtError {
  fn from(e: InternalError) -> MgmtError {
    MgmtError::ServerFailure(format!("ServerFailure {}\n", &e))
  }
}
