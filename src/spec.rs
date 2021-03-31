// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// game specs

use crate::imports::*;

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::convert::TryFrom;

use enum_map::Enum;
use fehler::{throw,throws};
use index_vec::{define_index_type, IndexVec};
use num_derive::{FromPrimitive, ToPrimitive};
use serde::{Deserialize, Serialize};
use strum::{EnumString, Display};
use thiserror::Error;

use otter_base::geometry::{Coord,Pos};
use otter_base::misc::display_as_debug;

use crate::accounts::AccountName;
use crate::error::UnsupportedColourSpec;
use crate::gamestate::PieceSpec;
use crate::hformat_as_display;
use crate::prelude::default;

pub use imp::PlayerAccessSpec;

type SpE = SpecError;

//---------- common types ----------

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken(pub String);

pub type RawFaceId = u8;
define_index_type! {
  #[derive(Default)]
  pub struct FaceId = RawFaceId;
  IMPL_RAW_CONVERSIONS = true;
}

#[derive(Serialize,Deserialize)]
#[derive(Debug,Default,Clone)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ColourSpec(pub String);

#[derive(Debug,Default,Clone,Eq,PartialEq,Hash,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct UrlSpec(pub String);

#[derive(Error,Clone,Serialize,Deserialize,Debug)]
pub enum SpecError {
  ImproperSizeSpec,
  UnsupportedColourSpec(#[from] UnsupportedColourSpec),
  FaceNotFound,
  InternalError(String),
  PosOffTable,
  LibraryNotFound,
  LibraryItemNotFound(String),
  AclInvalidAccountGlob,
  AclEntryOverlappingAllowDeny,
  InconsistentPieceCount,
  BadUrlSyntax,
  UrlTooLong,
  CompassAngleInvalid,
  ZeroFaces,
  InconsistentFacesEdgecoloursCount,
  WrongNumberOfFaces,
  SpecifiedWidthOfNoEdges,
  UnsupportedShape,
  NegativeTimeout,
  ComplexPieceWhereSimpleRequired,
  AliasNotFound,
  AliasTargetMultiSpec,
}
display_as_debug!{SpecError}

//---------- Table TOML file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct TableSpec {
  #[serde(default)] pub players: Vec<TablePlayerSpec>,
  pub player_perms: Option<HashSet<TablePermission>>,
  #[serde(default)] pub acl: Acl<TablePermission>,
  #[serde(default)] pub links: HashMap<LinkKind, UrlSpec>,
}

#[derive(Debug,Serialize,Deserialize)]
#[serde(rename_all="snake_case")]
pub enum TablePlayerSpec {
  Account(AccountName),
  AccountGlob(String),
  Local(String),
  AllLocal,
}

pub type RawAcl<Perm> = Vec<AclEntry<Perm>>;

#[derive(Debug,Clone)]
#[derive(Deserialize)]
#[serde(try_from="RawAcl<Perm>")]
pub struct Acl<Perm: Eq + Hash> { pub ents: RawAcl<Perm> }

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct AclEntry<Perm: Eq + Hash> {
  pub account_glob: String, // checked
  pub allow: HashSet<Perm>,
  pub deny: HashSet<Perm>,
}

#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
#[derive(Hash,Eq,PartialEq,Ord,PartialOrd)]
#[derive(FromPrimitive,ToPrimitive)]
pub enum TablePermission {
  TestExistence,
  ShowInList,
  ViewNotSecret,
  Play,
  ChangePieces,
  SetLinks,
  ResetOthersAccess,
  RedeliverOthersAccess,
  ModifyOtherPlayer,
  Super,
}

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
#[derive(Enum,EnumString,Display)]
#[derive(Serialize,Deserialize)]
pub enum LinkKind {
  Voice,
  Info,
}
hformat_as_display!{LinkKind}

//---------- player accesses, should perhaps be in commands.rs ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct PlayerAccessUnset;

#[derive(Debug,Serialize,Deserialize)]
pub struct FixedToken { pub token: RawToken }

#[derive(Debug,Serialize,Deserialize)]
pub struct TokenByEmail {
  /// RFC822 recipient field syntax (therefore, ASCII)
  pub addr: String,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct UrlOnStdout;

//---------- Game TOML file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameSpec {
  #[serde(default="imp::def_table_size")] pub table_size: Pos,
  pub pieces: Vec<PiecesSpec>,
  #[serde(default="imp::def_table_colour")] pub table_colour: ColourSpec,
  #[serde(default)] pub pcaliases: HashMap<String, Box<dyn PieceSpec>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PiecesSpec {
  pub pos: Option<Pos>,
  pub posd: Option<Pos>,
  pub count: Option<u32>,
  pub face: Option<FaceId>,
  pub pinned: Option<bool>,
  #[serde(default)] pub angle: PieceAngle,
  #[serde(flatten)]
  pub info: Box<dyn PieceSpec>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub enum PieceAngle {
  Compass(CompassAngle),
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
#[derive(Default,Serialize,Deserialize)]
#[serde(try_from="u8")]
#[serde(into="u8")]
/// 0 = unrotated, +ve is anticlockwise, units of 45deg
pub struct CompassAngle(u8);

//---------- Piece specs ----------
// the implementations are in pieces.rs

mod outline {
  use super::*;
  use crate::prelude::*;
  use crate::shapelib::{CircleShape, RectShape};
  #[dyn_upcast(OutlineTrait)]
  #[enum_dispatch(OutlineTrait)]
  #[derive(Clone,Debug,Serialize,Deserialize)]
  #[serde(tag="type")]
  pub enum Outline {
    #[serde(rename="Circle")] CircleShape,
    #[serde(rename="Rect")]   RectShape,
  }
}
pub use outline::*;

pub mod piece_specs {
  use super::*;

  pub type FaceColourSpecs = IndexVec<FaceId,ColourSpec>;

  #[derive(Debug,Serialize,Deserialize)]
  pub struct SimpleCommon {
    pub itemname: Option<String>,
    pub faces: IndexVec<FaceId, ColourSpec>,
    #[serde(default)] pub edges: IndexVec<FaceId, ColourSpec>,
    pub edge_width: Option<f64>,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Disc {
    pub diam: Coord,
    #[serde(flatten)]
    pub common: SimpleCommon,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Rect {
    pub size: Vec<Coord>,
    #[serde(flatten)]
    pub common: SimpleCommon,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Hand {
    #[serde(flatten)] pub c: OwnedCommon,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct PlayerLabel {
    #[serde(flatten)] pub c: OwnedCommon,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct OwnedCommon {
    pub colour: ColourSpec,
    pub edge: Option<ColourSpec>,
    pub edge_width: Option<f64>,
    pub shape: Outline,
    pub label: Option<PieceLabel>,
  }

  #[derive(Debug,Clone,Serialize,Deserialize)]
  pub struct PieceLabel {
    #[serde(default)] pub place: PieceLabelPlace,
    pub colour: Option<ColourSpec>,
  }

  #[derive(Debug,Copy,Clone,Serialize,Deserialize,Eq,PartialEq)]
  pub enum PieceLabelPlace {
    BottomLeft,        TopLeft,
    BottomLeftOutside, TopLeftOutside,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Deck {
    pub faces: IndexVec<FaceId, ColourSpec>,
    #[serde(default)] pub edges: IndexVec<FaceId, ColourSpec>,
    pub edge_width: Option<f64>,
    pub shape: Outline,
    pub label: Option<PieceLabel>,
  }
}

// ---------- Implementation - angles ----------

impl Default for PieceAngle {
  fn default() -> Self { PieceAngle::Compass(default()) }
}

impl TryFrom<u8> for CompassAngle {
  type Error = SpecError;
  #[throws(SpecError)]
  fn try_from(v: u8) -> Self {
    if v < 8 { Self(v) }
    else { throw!(SpE::CompassAngleInvalid) }
  }
}

impl From<CompassAngle> for u8 {
  fn from(a: CompassAngle) -> u8 {
    a.0
  }
}

//---------- Implementation ----------

pub mod imp {
  use super::{*, SpE};
  use crate::prelude::*;

  type AS = AccountScope;
  type TPS = TablePlayerSpec;

  pub fn def_table_size() -> Pos {
    DEFAULT_TABLE_SIZE
  }
  pub fn def_table_colour() -> ColourSpec {
    ColourSpec(DEFAULT_TABLE_COLOUR.into())
  }

  impl Default for piece_specs::PieceLabelPlace {
    fn default() -> Self { Self::BottomLeft }
  }

  impl<P: Eq + Hash> Default for Acl<P> {
    fn default() -> Self { Acl { ents: default() } }
  }

  impl<P: Eq + Hash + Serialize> Serialize for Acl<P> {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error>
    { self.ents.serialize(s) }
  }

  impl<P: Eq + Hash> TryFrom<RawAcl<P>> for Acl<P> {
    type Error = SpecError;
    #[throws(SpecError)]
    fn try_from(ents: RawAcl<P>) -> Self {
      for ent in &ents {
        glob::Pattern::new(&ent.account_glob)
          .map_err(|_| SpE::AclInvalidAccountGlob)?;
        if ! ent.deny.is_disjoint(&ent.allow) {
          throw!(SpE::AclEntryOverlappingAllowDeny);
        }
      }
      Acl { ents }
    }
  }

  impl loaded_acl::Perm for TablePermission {
    type Auth = InstanceName;
    const TEST_EXISTENCE: Self = TablePermission::TestExistence;
    const NOT_FOUND: MgmtError = ME::GameNotFound;
  }

  impl TablePlayerSpec {
    pub fn account_glob(&self) -> String {
      fn scope_glob(scope: AccountScope) -> String {
        let mut out = "".to_string();
        scope.display_name(&["*"], |s| Ok::<_,Void>(out += s)).unwrap();
        out
      }
      match self {
        TPS::Account(account) => account.to_string(),
        TPS::AccountGlob(s) => s.clone(),
        TPS::Local(user) => scope_glob(AS::Unix { user: user.clone() }),
        TPS::AllLocal => {
          // abuse that usernames are not encoded
          scope_glob(AS::Unix { user: "*".into() })
        }
      }
    }
  }

  type TDE = TokenDeliveryError;

  pub fn raw_token_debug_as_str(s: &str, f: &mut fmt::Formatter)
                                -> fmt::Result {
    let len = min(5, s.len() / 2);
    write!(f, "{:?}...", &s[0..len])
  }

  impl Debug for RawToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      raw_token_debug_as_str(&self.0, f)
    }
  }

  #[typetag::serde(tag="access")]
  pub trait PlayerAccessSpec: Debug + Sync + Send {
    fn override_token(&self) -> Option<&RawToken> {
      None
    }
    #[throws(MgmtError)]
    fn check_spec_permission(&self, _: Option<AuthorisationSuperuser>) {
    }
    fn deliver(&self,
               ag: &AccountsGuard,
               g: &Instance,
               gpl: &GPlayer,
               ipl: &IPlayer,
               token: AccessTokenInfo)
               -> Result<AccessTokenReport, TDE>;
    fn describe_html(&self) -> Html {
      let inner = Html::from_txt(&format!("{:?}", self));
      hformat!("<code>{}</code>", inner)
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for PlayerAccessUnset {
    #[throws(TokenDeliveryError)]
    fn deliver(&self,
               _ag: &AccountsGuard,
               _g: &Instance,
               _gpl: &GPlayer,
               _ipl: &IPlayer,
               _token: AccessTokenInfo) -> AccessTokenReport {
      AccessTokenReport { lines: vec![
        "Player access not set, game not accessible to this player"
          .to_string(),
      ] }
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for FixedToken {
    #[throws(MgmtError)]
    fn check_spec_permission(&self, auth: Option<AuthorisationSuperuser>) {
      auth.ok_or(ME::SuperuserAuthorisationRequired)?
    }
    fn override_token(&self) -> Option<&RawToken> {
      Some(&self.token)
    }
    #[throws(TokenDeliveryError)]
    fn deliver(&self,
               _ag: &AccountsGuard,
               _g: &Instance,
               _gpl: &GPlayer,
               _ipl: &IPlayer,
               _token: AccessTokenInfo) -> AccessTokenReport {
      AccessTokenReport { lines: vec![ "Fixed access token".to_string() ] }
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for UrlOnStdout {
    #[throws(TDE)]
    fn deliver<'t>(&self,
                   _ag: &AccountsGuard,
                   _g: &Instance,
                   _gpl: &GPlayer,
                   _ipl: &IPlayer,
                   token: AccessTokenInfo)
                   -> AccessTokenReport {
      AccessTokenReport { lines: token.report() }
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for TokenByEmail {
    #[throws(TDE)]
    fn deliver<'t>(&self,
                   ag: &AccountsGuard,
                   g: &Instance,
                   gpl: &GPlayer,
                   ipl: &IPlayer,
                   token: AccessTokenInfo)
                   -> AccessTokenReport {
      let sendmail = &config().sendmail;
      let mut command = Command::new(sendmail);

      #[derive(Debug,Serialize)]
      struct CommonData<'r> {
        player_email: &'r str,
        game_name: String,
        nick: &'r str,
        token_lines: Vec<String>,
      }
      let common = CommonData {
        player_email: &self.addr,
        game_name: g.name.to_string(),
        token_lines: token.report(),
        nick: &gpl.nick,
      };

      if self.addr.find((&['\r','\n']) as &[char]).is_some() {
        throw!(anyhow!("email address may not contain line endings"));
      }

      let (account, _) = ag.lookup(ipl.acctid).context("find account")?;
      let account = &account.account;
      let message = match &account.scope {
        AS::Unix { user } => {
          #[derive(Debug,Serialize)]
          struct Data<'r> {
            unix_user: &'r str,
            #[serde(flatten)]
            common: CommonData<'r>,
          }
          let data = Data {
            unix_user: user,
            common,
          };
          command.args(&["-f", &user]);
          nwtemplates::render("token-unix.tera", &data)
        }
        _ => {
          #[derive(Debug,Serialize)]
          struct Data<'r> {
            account: String,
            #[serde(flatten)]
            common: CommonData<'r>,
          }
          let data = Data {
            account: account.to_string(),
            common,
          };
          nwtemplates::render("token-other.tera", &data)
        },
      }
      .context("render email template")?;

      let messagefile = (||{
        let mut messagefile = tempfile::tempfile().context("tempfile")?;
        messagefile.write_all(message.as_bytes()).context("write")?;
        messagefile.flush().context("flush")?;
        messagefile.seek(SeekFrom::Start(0)).context("seek")?;
        Ok::<_,AE>(messagefile)
      })().context("write email to temporary file.")?;

      command
        .args(&["-oee","-odb","-oi","-t","--"])
        .stdin(messagefile);
      unsafe {
        command.pre_exec(|| {
          // https://github.com/rust-lang/rust/issues/79731
          match libc::dup2(2,1) {
            1 => Ok(()),
            -1 => Err(io::Error::last_os_error()),
            x => panic!("dup2(2,1) gave {}", x),
          }
        });
      }
      let st = command
        .status()
        .with_context(|| format!("run sendmail ({})", sendmail))?;
      if !st.success() {
        throw!(anyhow!("sendmail ({}) failed: {} ({})", sendmail, st, st));
      }

      AccessTokenReport { lines: vec![
        "Token sent by email.".to_string()
      ]}
    }
  }

  impl TryFrom<&ColourSpec> for Colour {
    type Error = UnsupportedColourSpec;
    #[throws(UnsupportedColourSpec)]
    fn try_from(spec: &ColourSpec) -> Colour {
      lazy_static! {
        static ref RE: Regex = Regex::new(concat!(
          r"^(?:", r"[[:alpha:]]{1,50}",
          r"|", r"#[[:xdigit:]]{3}{1,2}",
          r"|", r"(?:rgba?|hsla?)\([-.%\t 0-9]{1,50}\)",
          r")$"
        )).unwrap();
      }
      let s = &spec.0;
      if !RE.is_match(s) {
        throw!(UnsupportedColourSpec);
      }
      Html::from_html_string(spec.0.clone())
    }
  }

  impl UrlSpec {
    const MAX_LEN: usize = 200;
  }

  impl TryFrom<&UrlSpec> for Url {
    type Error = SpecError;
    #[throws(SpecError)]
    fn try_from(spec: &UrlSpec) -> Url {
      if spec.0.len() > UrlSpec::MAX_LEN {
        throw!(SpE::UrlTooLong);
      }
      let base = Url::parse(&config().public_url)
        .or_else(|_| Url::parse(
          "https://bad-otter-config-public-url.example.net/"
        )).unwrap();
      let url = Url::options()
        .base_url(Some(&base))
        .parse(&spec.0)
        .map_err(|_| SpE::BadUrlSyntax)?;
      url
    }
  }
}
