
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

pub const DEFAULT_TABLE_SIZE : Pos = [ 200, 100 ];

// ---------- general data types ----------

#[derive(Debug,Copy,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ZLevel {
  pub z: ZCoord,
  pub zg: Generation,
}

// ---------- game state ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameState {
  pub table_size : Pos, // xxx send to client etc.
  pub pieces : DenseSlotMap<PieceId,PieceState>,
  pub players : PlayerMap,
  pub gen : Generation,
  pub log : Vec<(Generation,Arc<LogEntry>)>,
  pub max_z : ZCoord,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PieceState {
  pub pos : Pos,
  pub p : Box<dyn Piece>,
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
  pub html : String,
}

// ---------- piece trait, and rendering ----------

type SE = SVGProcessingError;
type SR = Result<(),SE>;

#[typetag::serde]
pub trait Piece : Send + Debug {
  // #[throws] doesn't work here for some reason
  fn svg_piece(&self, f: &mut String, pri: &PieceRenderInstructions) -> SR;

  #[throws(SE)]
  fn surround_path(&self, pri : &PieceRenderInstructions) -> String;

  fn svg_x_defs(&self, f: &mut String, pri : &PieceRenderInstructions) -> SR;

  #[throws(SE)]
  fn thresh_dragraise(&self, pri : &PieceRenderInstructions)
                      -> Option<Coord>;

  fn describe_html(&self, face : Option<FaceId>) -> String;

  fn delete_hook(&self, _p: &PieceState, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }
}

#[derive(Debug,Copy,Clone)]
pub struct PieceRenderInstructions {
  pub id : VisiblePieceId,
  pub face : FaceId,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec : Debug {
  fn load(&self) -> Result<Box<dyn Piece>,SE>;
  fn resolve_spec_face(&self, face : Option<FaceId>)
                       -> Result<FaceId,GameError>;
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

// ---------- game state - rendering etc. ----------

impl PieceState {
  #[throws(SE)]
  pub fn make_defs(&self, pri : &PieceRenderInstructions) -> String {
    let pr = self;
    let mut defs = String::new();
    let dragraise = match pr.p.thresh_dragraise(pri)? {
      Some(n) if n < 0 => throw!(SE::NegativeDragraise),
      Some(n) => n,
      None => -1,
    };
    write!(defs,
           r##"<g id="piece{}" data-dragraise="{}">"##,
           pri.id, dragraise)?;
    pr.p.svg_piece(&mut defs, &pri)?;
    write!(defs, r##"</g>"##)?;
    write!(defs,
           r##"<path id="surround{}" d="{}"/>"##,
           pri.id, pr.p.surround_path(&pri)?)?;
    pr.p.svg_x_defs(&mut defs, &pri)?;
    defs
  }

  #[throws(SE)]
  pub fn prep_piecestate(&self, pri : &PieceRenderInstructions)
                         -> PreparedPieceState {
    PreparedPieceState {
      pos        : self.pos,
      held       : self.held,
      svg        : self.make_defs(pri)?,
      z          : self.zlevel.z,
      zg         : self.zlevel.zg,
    }
  }

  pub fn describe_html(&self, pri : &PieceRenderInstructions) -> String {
    self.p.describe_html(Some(pri.face))
  }
}

// ========== ad-hoc and temporary ==========

pub fn make_pieceid_visible(p : PieceId) -> VisiblePieceId {
  // todo-lens need to do censorship mapping here
  let kd : slotmap::KeyData = p.into();
  VisiblePieceId(kd)
}
