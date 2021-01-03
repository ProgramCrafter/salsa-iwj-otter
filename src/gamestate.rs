// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

type IE = InternalError;
type IR = Result<(), IE>;
type SE = SVGProcessingError;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ PlayerId('#') }

slotmap::new_key_type!{
  pub struct PieceId;
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct Generation(pub u64);

visible_slotmap_key!{ VisiblePieceId('.') }

#[derive(Clone,Serialize,Deserialize,Hash,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Html(pub String);

#[derive(Copy,Clone,Debug,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Timestamp(pub u64); /* time_t */

pub const DEFAULT_TABLE_SIZE : Pos = PosC([ 400, 200 ]);

// ---------- general data types ----------

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ZLevel {
  pub z: ZCoord,
  pub zg: Generation,
}

// ---------- game state ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameState {
  pub table_colour: Colour,
  pub table_size: Pos,
  pub pieces: Pieces,
  pub gen: Generation,
  pub log: VecDeque<(Generation, Arc<CommittedLogEntry>)>,
  pub max_z: ZCoord,
  pub players: DenseSlotMap<PlayerId, GPlayerState>,
}

#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct GPlayerState {
  pub nick: String,
  pub layout: PresentationLayout,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct PieceState {
  pub pos: Pos,
  pub face: FaceId,
  pub held: Option<PlayerId>,
  pub zlevel: ZLevel,
  pub pinned: bool,
  pub gen: Generation,
  pub lastclient: ClientId,
  pub gen_before_lastclient: Generation,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct LogEntry {
  pub html: Html,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct CommittedLogEntry {
  pub when: Timestamp,
  pub logent: LogEntry,
}

// ---------- piece trait, and rendering ----------

#[typetag::serde]
pub trait Outline: Send + Debug {
  fn surround_path(&self, pri: &PieceRenderInstructions) -> Result<Html, IE>;
  fn thresh_dragraise(&self, pri: &PieceRenderInstructions)
                      -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> [Pos;2];
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
pub enum UoKind { Global, Piece, GlobalExtra, }

pub type UoKey = char;

#[derive(Debug,Clone,Serialize,Deserialize)]
pub struct UoDescription {
  pub kind: UoKind,
  pub def_key: UoKey,
  pub opname: String,
  pub desc: Html,
  pub wrc: WhatResponseToClientOp,
}

#[typetag::serde]
pub trait Piece: Outline + Send + Debug {
  fn nfaces(&self) -> RawFaceId;

  #[throws(InternalError)]
  fn add_ui_operations(&self, _upd: &mut Vec<UoDescription>) { }

  fn ui_operation(&self,
                  _gs: &mut GameState, _player: PlayerId, _piece: PieceId,
                  _opname: &str, _wrc: WhatResponseToClientOp,
                  _lens: &dyn Lens)
                  -> PieceUpdateResult {
    throw!(OE::BadOperation)
  }

  // #[throws] doesn't work here for some reason
  fn svg_piece(&self, f: &mut Html, pri: &PieceRenderInstructions) -> IR;

  fn describe_html(&self, face: Option<FaceId>) -> Html;

  fn delete_hook(&self, _p: &PieceState, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }

  fn itemname(&self) -> &str;
}

#[derive(Debug,Copy,Clone)]
pub struct PieceRenderInstructions {
  pub id: VisiblePieceId,
  pub face: FaceId,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec: Debug {
  fn count(&self) -> usize { 1 }
  fn load(&self, i: usize) -> Result<Box<dyn Piece>, SpecError>;
}

// ========== implementations ==========

// ---------- simple data types ----------

impl Generation {
  pub fn increment(&mut self) { self.0 += 1 }
}
impl Display for Generation {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    Display::fmt(&self.0, f)
  }
}

impl Timestamp {
  /// Always >= previously
  pub fn now() -> Timestamp {
    use std::time::SystemTime;
    let now = SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
      .unwrap()
      .as_secs();
    Timestamp(now)
  }

  pub fn render(&self, tz: &Timezone) -> String {
    tz.format(*self)
  }
}

pub trait ClampTable: Sized {
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
  // todo convert to display_as but I need to write display_as::typed::Is
  pub fn lit(s: &str) -> Self { Html(s.to_owned()) }
  pub fn from_txt(s: &str) -> Self {
    Html(htmlescape::encode_minimal(&s))
  }
}

impl Debug for Html {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    const MAX: usize = 23;
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
      z          : self.zlevel.z.clone(),
      zg         : self.zlevel.zg,
      pinned     : self.pinned,
      uos        : p.ui_operations()?,
    }
  }
}

pub trait PieceExt {
  fn make_defs(&self, pri: &PieceRenderInstructions) -> Result<Html, IE>;
  fn describe_pri(&self, pri: &PieceRenderInstructions) -> Html;
  fn ui_operations(&self) -> Result<Vec<UoDescription>, IE>;
}

impl<T> PieceExt for T where T: Piece + ?Sized {
  #[throws(IE)]
  fn make_defs(&self, pri: &PieceRenderInstructions) -> Html {
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
    defs
  }

  fn describe_pri(&self, pri: &PieceRenderInstructions) -> Html {
    self.describe_html(Some(pri.face))
  }

  #[throws(InternalError)]
  fn ui_operations(&self) -> Vec<UoDescription> {
    type WRC = WhatResponseToClientOp;

    let mut out = vec![];
    if self.nfaces() > 1 {
      out.push(UoDescription {
        wrc: WRC::UpdateSvg,
        kind: UoKind::Global,
        def_key: 'f'.into(),
        opname: "flip".to_string(),
        desc: Html::lit("flip"),
      })
    }
    self.add_ui_operations(&mut out)?;
    out
  }
}

// ---------- log expiry ==========

impl GameState {
  pub fn want_expire_some_logs(&self, cutoff: Timestamp) -> bool {
    (||{
      let e = self.log.get(1)?;
      (e.1.when < cutoff).as_option()
    })().is_some()
  }

  pub fn do_expire_old_logs(&mut self, cutoff: Timestamp) {
    let want_trim = |gs: &GameState| gs.want_expire_some_logs(cutoff);

    if want_trim(self) {
      while want_trim(self) {
        self.log.pop_front();
      }
      let front = self.log.front_mut().unwrap();
      let front = &mut front.1;
      let logent = LogEntry {
        html: Html::lit("[Earlier log entries expired]"),
      };
      *front = Arc::new(CommittedLogEntry { logent, when: front.when });
    }
  }
}

// ========== ad-hoc and temporary ==========

pub fn make_pieceid_visible(p: PieceId) -> VisiblePieceId {
  // todo-lens need to do censorship mapping here
  let kd: slotmap::KeyData = p.into();
  VisiblePieceId(kd)
}
