// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub type MC = MgmtCommand;
pub type MGI = MgmtGameInstruction;
pub type MGR = MgmtGameResponse;
pub type MR = MgmtResponse;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop,
  SetSuperuser(bool),

  CreateAccount(AccountDetails),
  UpdateAccount(AccountDetails),
  DeleteAccount(AccountName),

  SelectAccount(AccountName), // success does not mean account exists
  CheckAccount, // success *does* mean account exists and we have access

  CreateGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
  },
  ListGames {
    all: Option<bool>, // in scope of selected account by default
    //   todo: should list games that are accessible, not just own
  },
  AlterGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
    how: MgmtGameUpdateMode,
  },
  // todo: MarkGameUncorrupted
  DestroyGame {
    game: InstanceName,
  },

  LibraryListByGlob {
    glob: shapelib::ItemSpec,
  },

  LoadFakeRng(Vec<String>),
}

//---------- Accounts file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct AccountDetails {
  pub account: AccountName,
  pub nick: Option<String>,
  pub timezone: Option<String>,
  pub layout: Option<PresentationLayout>,
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
  /// For testing, mostly.
  Synch,
  SetTableSize(Pos),
  SetTableColour(ColourSpec),

  ListPieces,
  AddPieces(PiecesSpec),
  DeletePiece(PieceId),

  ResetPlayerAccess(PlayerId),
  RedeliverPlayerAccess(PlayerId),

  JoinGame { details: MgmtPlayerDetails },
  UpdatePlayer { player: PlayerId, details: MgmtPlayerDetails },
  LeaveGame(PlayerId),

  SetLinks(HashMap<LinkKind, UrlSpec>),
  RemoveLink { kind: LinkKind },
  SetLink { kind: LinkKind, url: UrlSpec },

  ClearLog,
  SetACL { acl: Acl<TablePermission> },
  // RemovePlayer { player: PlayerId },  todo, does a special setacl
}

#[derive(Debug,Serialize,Deserialize)]
pub struct MgmtPlayerDetails {
  pub nick: Option<String>,
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameResponse {
  Fine,
  Info(MgmtGameResponseGameInfo),
  Synch(Generation),

  Pieces(Vec<MgmtGamePieceInfo>),

  JoinGame {
    nick: String,
    player: PlayerId,
    token: AccessTokenReport,
  },
  PlayerAccessToken(AccessTokenReport),
}

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct AccessTokenInfo { pub url: String }

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct AccessTokenReport { pub lines: Vec<String> }

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct MgmtGameResponseGameInfo {
  pub table_size: Pos,
  pub players: SecondarySlotMap<PlayerId, MgmtPlayerInfo>,
  pub links: Vec<(LinkKind, UrlSpec)>,
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
  pub bbox: Rect,
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
  SuperuserAuthorisationRequired,
  SpecifyAccount,
  MustSpecifyNick,
  AlreadyExists,
  NickCollision,
  GameBeingDestroyed,
  GameNotFound,
  GameCorrupted,
  AccountNotFound(#[from] AccountNotFound),
  PlayerNotFound(#[from] PlayerNotFound),
  PieceNotFound,
  LimitExceeded,
  ServerFailure(String),
  ZCoordinateOverflow(#[from] zcoord::Overflow),
  BadGlob { pat: String, msg: String },
  BadSpec(#[from] SpecError),
  TokenDeliveryFailed(#[from] TokenDeliveryError),
  CoordinateOverflow(#[from] CoordinateOverflow),
  RngIsReal,
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

from_instance_lock_error!{MgmtError}

impl AccessTokenInfo {
  pub fn report(self) -> Vec<String> {
    vec![
      "Game access url:".to_string(),
      self.url,
    ]
  }
}
