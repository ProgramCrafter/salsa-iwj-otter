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
  ListAccounts { all: Option<bool> },

  SelectAccount(AccountName), // success does not mean account exists
  CheckAccount, // success *does* mean account exists and we have access

  CreateGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
  },
  ListGames { all: Option<bool> },
  AlterGame {
    game: InstanceName,
    insns: Vec<MgmtGameInstruction>,
    how: MgmtGameUpdateMode,
  },
  DestroyGame {
    game: InstanceName,
  },

  ClearBundles {
    game: InstanceName,
  },
  UploadBundle {
    game: InstanceName,
    size: usize,
    hash: bundles::Hash,
    kind: bundles::Kind,
  },
  ListBundles {
    game: InstanceName,
  },
  DownloadBundle {
    game: InstanceName,
    id: bundles::Id,
  },

  LibraryListLibraries {
    game: InstanceName,
  },
  LibraryListByGlob {
    game: InstanceName,
    lib: Option<String>,
    pat: String,
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
  Progress(ProgressInfo<'static>),
  Error { error: MgmtError },
  AlterGame { error: Option<MgmtError>, responses: Vec<MgmtGameResponse> },
  AccountsList(Vec<Arc<AccountName>>),
  GamesList(Vec<Arc<InstanceName>>),
  Libraries(Vec<LibraryEnquiryData>),
  LibraryItems(Vec<ItemEnquiryData>),
  Bundles { bundles: MgmtBundleList },
}

pub type MgmtBundleList = BTreeMap<bundles::Id, bundles::State>;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtGameInstruction {
  Noop,
  Info,
  SetTableSize(Pos),
  SetTableColour(ColourSpec),

  InsnMark(Box<[u8]>),
  /// For testing, mostly.
  Synch, SynchLog,
  /// For testing only
  PieceIdLookupFwd { piece: PieceId, player: PlayerId, },
  /// For testing only
  PieceIdLookupRev { vpid: VisiblePieceId, player: PlayerId, },

  ListPieces,
  AddPieces(PiecesSpec),
  DeletePiece(PieceId),
  DeletePieceAlias(String),
  DefinePieceAlias { alias: String, target: Box<dyn PieceSpec> },

  ClearGame { },
  ResetFromGameSpec { spec_toml: String },

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
  InsnMark(Box<[u8]>),
  InsnExpanded,
  Synch(Generation),

  InternalPieceId(Option<PieceId>),
  VisiblePieceId(Option<VisiblePieceId>),

  Pieces {
    pieces: Vec<MgmtGamePieceInfo>,
    pcaliases: BTreeSet<String>,
  },

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
  pub itemname: shapelib::GoodItemName,
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
  #[error("failed to parse protocol command: {0}")] CommandParseFailed(String),
  #[error("not authorised")]                         AuthorisationError,
  #[error("superuse authorisation requiredr")] SuperuserAuthorisationRequired,
  #[error("command requires account specified")]     SpecifyAccount,
  #[error("command requires nick specified")]        MustSpecifyNick,
  #[error("object or item already exists")]          AlreadyExists,
  #[error("game already has player with that nick")] NickCollision,
  #[error("{0}")] GameBeingDestroyed (#[from] GameBeingDestroyed),
  #[error("game not found")]                         GameNotFound,
  #[error("{0}")] AccountNotFound     (#[from] AccountNotFound),
  #[error("{0}")] PlayerNotFound      (#[from] PlayerNotFound),
  #[error("piece not found in game")]                PieceNotFound,
  #[error("limit exceeded")]                         LimitExceeded,
  #[error("server failure: {0}")]                    ServerFailure(String),
  #[error("{0}")] ZCoordinateOverflow (#[from] zcoord::Overflow),
  #[error("bad glob pattern: {pat:?}: {msg}")]
                                           BadGlob { pat: String, msg: String },
  #[error("{0}")] BadSpec             (#[from] SpecError),
  #[error("access token delivery failed: {0}")]
                  TokenDeliveryFailed (#[from] TokenDeliveryError),
  #[error("{0}")] CoordinateOverflow  (#[from] CoordinateOverflow),
  #[error("TOML syntax error: {0}")]                 TomlSyntaxError(String),
  #[error("TOML structure error: {0}")]              TomlStructureError(String),
  #[error("RNG is real, command not supported")]     RngIsReal,
  #[error("upload truncated")]                       UploadTruncated,
  #[error("upload corrupted")]                       UploadCorrupted,
  #[error("too many bundles")]                       TooManyBundles,
  #[error("bad bundle: {0}")]                        BadBundle(String),
  #[error("bundle not found")]                       BundleNotFound,
  #[error("bundle(s) in use, cannot clear ({0})")]   BundlesInUse(String),
  #[error("idle timeout waiting for mgmt command")]  IdleTimeout,
  #[error("upload took too long (timed out)")]       UploadTimeout,
}

impl From<InternalError> for MgmtError {
  fn from(e: InternalError) -> MgmtError {
    MgmtError::ServerFailure(format!("internal error: {}\n", &e))
  }
}

impl AccessTokenInfo {
  pub fn report(self) -> Vec<String> {
    vec![
      "Game access url:".to_string(),
      self.url,
    ]
  }
}
