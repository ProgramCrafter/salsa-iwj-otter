// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop,

  CreateAccont(AccountDetails),
  UpdateAccont(AccountDetails),
  DeleteAccount(AccountDetails),

  SetAccount(AccountName),

  CreateGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
  },
  ListGames {
    all: Option<bool>, // in same scope by default
  },
  AlterGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
    how: MgmtGameUpdateMode,
  },

  LibraryListByGlob {
    glob: shapelib::ItemSpec,
  },
}

//---------- Accounts file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct AccountDetails {
  pub account: AccountName,
  pub nick: String,
  pub timezone: Option<String>,
  #[serde(flatten)]
  pub access: Option<Box<dyn PlayerAccessSpec>>,
//  pub invite: Acl<AccountPerm>,   todo
}

//#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
//enum AccountPerm {
//  InviteToGame,
//  GameRunnerResetToken,
//}


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

  ResetPlayerAccess(PlayerId),
  // xxx ^ prevent use of Fixed when not wanted
  RedeliverPlayerAccess(PlayerId),

  AddPlayer(MgmtPlayerDetails),
  UpdatePlayer(MgmtPlayerDetails),
  RemovePlayer(AccountName),
  BlockPlayer(String),
}

// xxx facilitator name?

#[derive(Debug,Serialize,Deserialize)]
pub struct MgmtPlayerDetails {
  pub account: AccountName,
  pub timezone: Option<String>,
  pub nick: Option<String>,
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameResponse {
  Fine,
  Info(MgmtGameResponseGameInfo),

  Pieces(Vec<MgmtGamePieceInfo>),

  AddPlayer(PlayerId),
  PlayerAccessToken(Option<AccessTokenReport>),
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct AccessTokenReport {
  pub url: String,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGameResponseGameInfo {
  pub table_size: Pos,
  pub players: SecondarySlotMap<PlayerId, MgmtPlayerInfo>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtPlayerInfo {
  pub account: AccountName,
  pub nick: String,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGamePieceInfo {
  pub piece: PieceId,
  pub itemname: String,
  #[serde(flatten)]
  pub visible: Option<MgmtGamePieceVisibleInfo>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGamePieceVisibleInfo {
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
  ParameterMissing,
  SpecifyAccount,
  AlreadyExists,
  NickCollision,
  GameBeingDestroyed,
  GameNotFound,
  GameCorrupted,
  AccountNotFound,
  PlayerNotFound,
  AuthorisationUninitialised,
  PieceNotFound,
  LimitExceeded,
  ServerFailure(String),
  ZCoordinateOverflow(#[from] zcoord::Overflow),
  BadGlob { pat: String, msg: String },
  BadSpec(#[from] SpecError),
  TokenDeliveryFailed(#[from] TokenDeliveryError),
}
impl Display for MgmtError {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    use MgmtError::*;
    match self {
      ServerFailure(s) => write!(f, "ServerFailure: {}", &s)?,
      TokenDeliveryFailed(tde) =>
        write!(f, "access token delivery failed: {}", &tde)?,
      _ => <Self as Debug>::fmt(self,f)?,
    }
  }
}

impl From<InternalError> for MgmtError {
  fn from(e: InternalError) -> MgmtError {
    MgmtError::ServerFailure(format!("ServerFailure {}\n", &e))
  }
}
