// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// game specs

use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

use enum_map::Enum;
use fehler::throws;
use index_vec::{define_index_type, IndexVec};
use num_derive::{FromPrimitive, ToPrimitive};
use serde::{Deserialize, Serialize};
use strum::{EnumString, Display};
use thiserror::Error;

use crate::accounts::AccountName;
use crate::error::display_as_debug;
use crate::gamestate::PieceSpec;

pub use implementation::PlayerAccessSpec;

//---------- common types ----------

pub type Coord = i32;

#[derive(Clone,Copy,Debug,Serialize,Deserialize,Hash)]
#[derive(Eq,PartialEq,Ord,PartialOrd)]
#[serde(transparent)]
pub struct PosC<T>(pub [T; 2]);
pub type Pos = PosC<Coord>;

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
#[derive(Debug,Default)]
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
  UnsupportedColourSpec,
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

//#[derive(Debug,Serialize,Deserialize)]
//struct TokenByEmail { email: String };
// todo ^ implement this

//---------- Game TOML file ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameSpec {
  pub table_size: Option<Pos>,
  pub pieces: Vec<PiecesSpec>,
  pub table_colour: Option<ColourSpec>,
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

pub mod piece_specs {
  use super::*;

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Disc {
    pub itemname: Option<String>,
    pub diam: Coord,
    pub faces: IndexVec<FaceId, ColourSpec>,
  }

  #[derive(Debug,Serialize,Deserialize)]
  pub struct Square {
    pub itemname: Option<String>,
    pub size: Vec<Coord>,
    pub faces: IndexVec<FaceId, ColourSpec>,
  }

}

//---------- Pos ----------

pub mod pos_traits {
  use std::ops::{Add,Sub,Mul,Neg,AddAssign,SubAssign};
  use crate::imports::*;

  impl<T:Add<T,Output=T>+Copy+Clone+Debug> Add<PosC<T>> for PosC<T> {
    type Output = PosC<T>;
    fn add(self, rhs: PosC<T>) -> PosC<T> {
      PosC(
        itertools::zip_eq(
          self.0.iter().cloned(),
          rhs .0.iter().cloned(),
        ).map(|(a,b)| a + b)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Sub<T,Output=T>+Copy+Clone+Debug> Sub<PosC<T>> for PosC<T> {
    type Output = PosC<T>;
    fn sub(self, rhs: PosC<T>) -> PosC<T> {
      PosC(
        itertools::zip_eq(
          self.0.iter().cloned(),
          rhs .0.iter().cloned(),
        ).map(|(a,b)| a - b)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Add<T,Output=T>+Copy+Clone+Debug> AddAssign<PosC<T>> for PosC<T> {
    fn add_assign(&mut self, rhs: PosC<T>) {
      *self = *self + rhs;
    }
  }

  impl<T:Sub<T,Output=T>+Copy+Clone+Debug> SubAssign<PosC<T>> for PosC<T> {
    fn sub_assign(&mut self, rhs: PosC<T>) {
      *self = *self - rhs;
    }
  }

  impl<T:Mul<T,Output=T>+Copy+Clone+Debug> Mul<T> for PosC<T> {
    type Output = PosC<T>;
    fn mul(self, rhs: T) -> PosC<T> {
      PosC(
        self.0.iter().cloned().map(|a| a * rhs)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Neg<Output=T>+Copy+Clone+Debug> Neg for PosC<T> {
    type Output = Self;
    fn neg(self) -> Self {
      PosC(
        self.0.iter().cloned().map(|a| -a)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl<T:Copy+Clone+Debug> PosC<T> {
    pub fn map<U:Copy+Clone+Debug, F: FnMut(T) -> U>(self, f: F) -> PosC<U> {
      PosC(
        self.0.iter().cloned().map(f)
          .collect::<ArrayVec<_>>().into_inner().unwrap()
      )
    }
  }

  impl PosC<Coord> {
    pub fn promote(&self) -> PosC<f64> { self.map(|v| v as f64) }
  }
}

//---------- Implementation ----------

pub mod implementation {
  use super::*;
  use crate::imports::*;

  type AS = AccountScope;
  type SE = SpecError;
  type TPS = TablePlayerSpec;

  impl Default for PieceAngle {
    fn default() -> Self { PieceAngle::Compass(default()) }
  }

  impl TryFrom<u8> for CompassAngle {
    type Error = SpecError;
    #[throws(SpecError)]
    fn try_from(v: u8) -> Self {
      if v < 8 { Self(v) }
      else { throw!(SE::CompassAngleInvalid) }
    }
  }

  impl From<CompassAngle> for u8 {
    fn from(a: CompassAngle) -> u8 {
      a.0
    }
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
          .map_err(|_| SE::AclInvalidAccountGlob)?;
        if ! ent.deny.is_disjoint(&ent.allow) {
          throw!(SE::AclEntryOverlappingAllowDeny);
        }
      }
      Acl { ents }
    }
  }

  impl loaded_acl::Perm for TablePermission {
    type Auth = InstanceName;
    const TEST_EXISTENCE: Self = TablePermission::TestExistence;
    const NOT_FOUND: MgmtError = MgmtError::GameNotFound;
  }

  impl TablePlayerSpec {
    pub fn account_glob(&self) -> String {
      fn scope_glob(scope: AccountScope) -> String {
        let mut out = "".to_string();
        scope.display_name(&["*"], |s| Ok::<_,Impossible>(out += s)).unwrap();
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
               gpl: &GPlayerState,
               ipl: &IPlayerState,
               token: AccessTokenInfo)
               -> Result<AccessTokenReport, TDE>;
    fn describe_html(&self) -> Html {
      let inner = Html::from_txt(&format!("{:?}", self));
      Html(format!("<code>{}</code>", inner.0))
    }
  }

  #[typetag::serde]
  impl PlayerAccessSpec for PlayerAccessUnset {
    #[throws(TokenDeliveryError)]
    fn deliver(&self,
               _ag: &AccountsGuard,
               _g: &Instance,
               _gpl: &GPlayerState,
               _ipl: &IPlayerState,
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
               _gpl: &GPlayerState,
               _ipl: &IPlayerState,
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
                   _gpl: &GPlayerState,
                   _ipl: &IPlayerState,
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
                   gpl: &GPlayerState,
                   ipl: &IPlayerState,
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
    type Error = SpecError;
    #[throws(SpecError)]
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
        throw!(SpecError::UnsupportedColourSpec);
      }
      Html(spec.0.clone())
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
        throw!(SE::UrlTooLong);
      }
      let base = Url::parse(&config().public_url)
        .or_else(|_| Url::parse(
          "https://bad-otter-config-public-url.example.net/"
        )).unwrap();
      let url = Url::options()
        .base_url(Some(&base))
        .parse(&spec.0)
        .map_err(|_| SE::BadUrlSyntax)?;
      url
    }
  }
}
