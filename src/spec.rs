// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// game specs

use crate::crates::*;
use crate::outline::*;
use otter_support::crates::*;
use otter_base::crates::*;

use std::borrow::Cow;
use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::convert::TryFrom;
use std::time::Duration;

use const_default::ConstDefault;
use enum_map::Enum;
use fehler::{throw,throws};
use index_vec::{define_index_type, IndexVec};
use num_derive::{FromPrimitive, ToPrimitive};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, EnumString};
use thiserror::Error;

use otter_base::geometry::{Coord,Pos,CoordinateOverflow};
use otter_base::hformat_as_display;

use crate::accounts::AccountName;
use crate::error::UnsupportedColourSpec;
use crate::gamestate::PieceSpec;
use crate::materials_format;
use crate::prelude::default;
use crate::utils::SVGSizeError;

pub use imp::PlayerAccessSpec;

type SpE = SpecError;

//---------- common types ----------

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Serialize,Deserialize)]
#[serde(transparent)]
pub struct RawToken(pub String);

pub type RawFaceId = u8;
define_index_type! {
  #[derive(Default,ConstDefault)]
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
  #[error("improper size specification")]    ImproperSizeSpec,
  #[error("{0}")] UnsupportedColourSpec(#[from] UnsupportedColourSpec),
  #[error("specified face not found")]       FaceNotFound,
  #[error("internal error: {0}")]            InternalError(String),
  #[error("specified position is off table")] PosOffTable,
  #[error("library not found")]              LibraryNotFound,
  #[error("item not found in library: {0:?}")] LibraryItemNotFound(ItemSpec),
  #[error("acl contains invalid account glob")] AclInvalidAccountGlob,
  #[error("acl entry allow/deny overlap")]   AclEntryOverlappingAllowDeny,
  #[error("inconsistent piece count")]       InconsistentPieceCount,
  #[error("bad url syntax")]                 BadUrlSyntax,
  #[error("url too long")]                   UrlTooLong,
  #[error("compass angle invalid")]          CompassAngleInvalid,
  #[error("piece has zero faces")]           ZeroFaces,
  #[error("inconsistent face/edge colours")] InconsistentFacesEdgecoloursCount,
  #[error("specified with of edges, but no edges")] SpecifiedWidthOfNoEdges,
  #[error("shape not supported")]            UnsupportedShape,
  #[error("negative timeout")]               NegativeTimeout,
  #[error("complex piece where inert needed")] ComplexPieceWhereInertRequired,
  #[error("piece alias not found")]          AliasNotFound,
  #[error("piece alias target is multi spec")] AliasTargetMultiSpec,
  #[error("invalid range ({0} to {1})")]     InvalidRange(usize,usize),
  #[error("piece image/alias depth exceeded")] ImageOrAliasLoop,
  #[error("invalid size scale")]             InvalidSizeScale,
  #[error("multiple faces required")]        MultipleFacesRequired,
  #[error("far too many faces ({0})")]       FarTooManyFaces(usize),
  #[error("currency quantity not multiple of minimum unit")]
  CurrencyQtyNotMultipleOfUnit,
  #[error("coordinate overflow")]
  CoordinateOverflow(#[from] CoordinateOverflow),
  #[error("SVG handling error: {item_name} for {item_for_lib} {item_for_item} {error}")] SVGError {
    item_name: String,
    item_for_lib: String,
    item_for_item: String,
    error: SVGSizeError,
  },
  #[error("image for supposedly-occultable piece \
           is not itself occultable but has multiple faces")]
  UnoccultableButRichImageForOccultation,
  #[error("timeout too large ({got:?} > {max:?})")]
  TimeoutTooLarge {
    got: Duration,
    max: Duration,
  },
  #[error("wrong number of faces {got_why}={got} != {exp_why}={exp}")]
  WrongNumberOfFaces {
    got: RawFaceId,
    exp: RawFaceId,
    got_why: Cow<'static, str>,
    exp_why: Cow<'static, str>,
  },
}

//---------- Bundle "otter.toml" file ----------

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct BundleMeta {
  pub title: String,
  #[serde(default, rename="format")]
  pub mformat: materials_format::Version,
}

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
  SameScope,
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
#[derive(FromPrimitive,ToPrimitive,EnumIter)]
pub enum TablePermission {
  TestExistence,
  ShowInList,
  ViewNotSecret,
  Play,
  ChangePieces,
  UploadBundles,
  SetLinks,
  ClearBundles,
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
  #[serde(default)] pub pieces: Vec<PiecesSpec>,
  #[serde(default="imp::def_table_colour")] pub table_colour: ColourSpec,
  #[serde(default)] pub pcaliases: HashMap<String, Box<dyn PieceSpec>>,
  #[serde(default, rename="format")] pub mformat: materials_format::Version,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PiecesSpec {
  pub pos: Option<Pos>,
  pub posd: Option<Pos>,
  pub count: Option<u32>,
  pub face: Option<FaceId>,
  pub pinned: Option<bool>,
  #[serde(default)] pub angle: Option<PieceAngleSpec>,
  #[serde(flatten)]
  pub info: Box<dyn PieceSpec>,
}

#[derive(Debug,Clone,Serialize,Deserialize)]
#[serde(untagged)]
pub enum PieceAngleSpec {
  Compass(String),
  Degrees(i32),

  CompatMF1 { #[serde(rename="Compass")] i: u8 },
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub enum PieceAngle {
  Compass(CompassAngle),
}
impl PieceAngle {
  pub fn is_default(&self) -> bool { match self {
    PieceAngle::Compass(a) => *a == CompassAngle::default(),
    #[allow(unreachable_patterns)] _ => false,
  } }
}

#[derive(Debug,Default,Clone,Serialize,Deserialize)]
pub struct TextOptionsSpec {
  pub colour: Option<ColourSpec>,
  pub size: Option<f64>,
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
#[derive(Default,Serialize,Deserialize)]
#[serde(try_from="u8")]
#[serde(into="u8")]
/// 0 = unrotated, +ve is anticlockwise, units of 45deg
pub struct CompassAngle(u8);

//---------- Piece specs ----------
// the implementations are in shapelib.rs and pieces.rs

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct ItemSpec {
  pub lib: String,
  pub item: String,
}

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
    #[serde(default)] pub stack_pos: [Coord; 2],
  }
}

// ---------- Implementation - angles ----------

impl PieceAngle {
  pub fn is_rotated(&self) -> bool { match self {
    &PieceAngle::Compass(CompassAngle(a)) => a != 0,
  } }
}

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
    pub fn account_glob(&self, instance_name: &InstanceName) -> String {
      fn scope_glob(scope: AccountScope) -> String {
        let mut out = "".to_string();
        scope.display_name(&[""], |s| Ok::<_,Void>(out += s)).unwrap();
        out += "*";
        out
      }
      match self {
        TPS::Account(account) => account.to_string(),
        TPS::AccountGlob(s) => s.clone(),
        TPS::SameScope => scope_glob(instance_name.account.scope.clone()),
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
          command.args(&["-f", user]);
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
        messagefile.rewind().context("seek")?;
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

  #[ext(pub, name=ColourSpecExt)]
  impl Option<ColourSpec> {
    #[throws(UnsupportedColourSpec)]
    fn resolve(&self) -> Colour {
      self.as_ref()
        .map(TryInto::try_into).transpose()?
        .unwrap_or_else(|| Html::lit("black").into())
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

  impl TextOptionsSpec {
    #[throws(SpecError)]
    /// Default colour is always black
    pub fn resolve(&self, default_size: f64) -> TextOptions {
      let TextOptionsSpec { colour, size } = self;
      let colour = colour.resolve()?;
      let size = size.unwrap_or(default_size);
      TextOptions { colour, size }
    }
  }

  #[ext(pub)]
  impl Option<PieceAngleSpec> {
    #[throws(SpecError)]
    fn resolve(&self) -> PieceAngle {
      use PieceAngleSpec as PAS;
      let i = match self {
        None => return default(),
        Some(PAS::Compass(s)) => {
          let (i,_) = [
            "N" , "NE", "E" , "SE", "S" , "SW", "W" , "NW",
          ].iter().enumerate().find(|(_i,&exp)| s == exp)
            .ok_or_else(|| SpE::CompassAngleInvalid)?;
          i as u8
        },
        Some(PAS::Degrees(deg)) => {
          let deg = deg.rem_euclid(360);
          if deg % 45 != 0 { throw!(SpE::CompassAngleInvalid) }
          (deg / 45) as u8
        },
        Some(PAS::CompatMF1 { i }) => *i,
      };
      PieceAngle::Compass(i.try_into()?)
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
