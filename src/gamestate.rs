// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ PlayerId('#') }

slotmap::new_key_type!{
  pub struct PieceId;
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct Generation (pub u64);

visible_slotmap_key!{ VisiblePieceId('.') }

#[derive(Debug,Copy,Clone,PartialEq,PartialOrd)]
#[derive(Serialize,Deserialize)]
#[serde(into="f64")]
#[serde(try_from="f64")]
pub struct ZCoord(pub f64);

#[derive(Clone,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Html (pub String);

pub const DEFAULT_TABLE_SIZE : Pos = PosC([ 400, 200 ]);

// ---------- general data types ----------

#[derive(Debug,Copy,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ZLevel {
  pub z: ZCoord,
  pub zg: Generation,
}

// ---------- game state ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameState {
  pub table_size : Pos,
  pub pieces : Pieces,
  pub players : PlayerMap,
  pub gen : Generation,
  pub log : Vec<(Generation,Arc<LogEntry>)>,
  pub max_z : ZCoord,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PieceState {
  pub pos : Pos,
  pub face : FaceId,
  pub held : Option<PlayerId>,
  pub zlevel : ZLevel,
  pub gen : Generation,
  pub lastclient : ClientId,
  pub gen_before_lastclient : Generation,
}

pub type PlayerMap = DenseSlotMap<PlayerId,PlayerState>;

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct PlayerState {
  pub nick : String,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct LogEntry {
  pub html : Html,
}

// ---------- piece trait, and rendering ----------

type IE = InternalError;
type IR = Result<(),IE>;
type SE = SVGProcessingError;

#[typetag::serde]
pub trait Outline : Send + Debug {
  fn surround_path(&self, pri : &PieceRenderInstructions) -> Result<Html, IE>;
  fn thresh_dragraise(&self, pri : &PieceRenderInstructions)
                      -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> [Pos;2];
}

#[typetag::serde]
pub trait Piece : Outline + Send + Debug {
  fn resolve_spec_face(&self, face : Option<FaceId>)
                       -> Result<FaceId,SpecError>;

  // #[throws] doesn't work here for some reason
  fn svg_piece(&self, f: &mut Html, pri: &PieceRenderInstructions) -> IR;

  fn svg_x_defs(&self, f: &mut Html, pri : &PieceRenderInstructions) -> IR;

  fn describe_html(&self, face : Option<FaceId>) -> Html;

  fn delete_hook(&self, _p: &PieceState, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }

  fn itemname(&self) -> &str;
}

#[derive(Debug,Copy,Clone)]
pub struct PieceRenderInstructions {
  pub id : VisiblePieceId,
  pub face : FaceId,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec : Debug {
  fn load(&self) -> Result<Box<dyn Piece>,SpecError>;
}

// ========== implementations ==========

// ---------- simple data types ----------

impl Generation {
  pub fn increment(&mut self) { self.0 += 1 }
}
impl Display for Generation {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    Display::fmt(&self.0,f)
  }
}

impl TryFrom<f64> for ZCoord {
  type Error = OnlineError;
  #[throws(OnlineError)]
  fn try_from(v: f64) -> ZCoord {
    if !v.is_finite() { throw!(OnlineError::InvalidZCoord) }
    ZCoord(v)
  }
}
impl From<ZCoord> for f64 {
  fn from(v: ZCoord) -> f64 { v.0 }
}
impl Ord for ZCoord {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    self.0.partial_cmp(&other.0).unwrap()
  }
}
impl Eq for ZCoord { }
impl Display for ZCoord {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    Display::fmt(&self.0,f)
  }
}

pub trait ClampTable : Sized {
  fn clamped(self, range: Self) -> (Self, bool);
}

impl ClampTable for Coord {
  fn clamped(self, range: Coord) -> (Coord, bool) {
    if self < 0     { return (0,     true) }
    if self > range { return (range, true) }
    return (self, false)
  }
}

impl ClampTable for Pos {
  fn clamped(self, range: Pos) -> (Pos, bool) {
    let mut output = ArrayVec::new();
    let mut did = false;
    for (npos, tdid) in self.0.iter().zip(range.0.iter())
      .map(|(&pos, &rng)| pos.clamped(rng)) {
      output.push(npos);
      did |= tdid;
    }
    (PosC(output.into_inner().unwrap()), did)
  }
}

impl Html {
  pub fn lit(s: &str) -> Self { Html(s.to_owned()) }
}

impl Debug for Html {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    const MAX : usize = 23;
    if self.0.len() < MAX {
      write!(f, "<{}>", &self.0)
    } else {
      write!(f, "<{}>...", &self.0[0..MAX-3])
    }
  }
}

// ---------- game state - rendering etc. ----------


impl PieceState {
  #[throws(IE)]
  pub fn prep_piecestate(&self, p: &dyn Piece, pri : &PieceRenderInstructions)
                     -> PreparedPieceState {
    PreparedPieceState {
      pos        : self.pos,
      held       : self.held,
      svg        : p.make_defs(pri)?,
      z          : self.zlevel.z,
      zg         : self.zlevel.zg,
    }
  }
}

pub trait PieceExt {
  fn make_defs(&self, pri : &PieceRenderInstructions) -> Result<Html,IE>;
  fn describe_pri(&self, pri : &PieceRenderInstructions) -> Html;
}

impl<T> PieceExt for T where T: Piece + ?Sized {
  #[throws(IE)]
  fn make_defs(&self, pri : &PieceRenderInstructions) -> Html {
    let mut defs = Html(String::new());
    let dragraise = match self.thresh_dragraise(pri)? {
      Some(n) if n < 0 => throw!(SE::NegativeDragraise),
      Some(n) => n,
      None => -1,
    };
    write!(&mut defs.0,
           r##"<g id="piece{}" data-dragraise="{}">"##,
           pri.id, dragraise)?;
    self.svg_piece(&mut defs, &pri)?;
    write!(&mut defs.0, r##"</g>"##)?;
    write!(&mut defs.0,
           r##"<path id="surround{}" d="{}"/>"##,
           pri.id, self.surround_path(&pri)?.0)?;
    self.svg_x_defs(&mut defs, &pri)?;
    defs
  }

  fn describe_pri(&self, pri : &PieceRenderInstructions) -> Html {
    self.describe_html(Some(pri.face))
  }
}

// ========== ad-hoc and temporary ==========

pub fn make_pieceid_visible(p : PieceId) -> VisiblePieceId {
  // todo-lens need to do censorship mapping here
  let kd : slotmap::KeyData = p.into();
  VisiblePieceId(kd)
}
