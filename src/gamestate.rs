// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

// ---------- newtypes and type aliases ----------

visible_slotmap_key!{ PlayerId(b'#') }

slotmap::new_key_type!{
  pub struct PieceId;
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct Generation(pub u64);

visible_slotmap_key!{ VisiblePieceId(b'.') }

#[derive(Clone,Serialize,Deserialize,Hash,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Html(pub String);

#[derive(Copy,Clone,Debug,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Timestamp(pub u64); /* time_t */

pub const DEFAULT_TABLE_SIZE: Pos = PosC([ 400, 200 ]);

// ---------- general data types ----------

#[derive(Debug,Clone,Serialize,Deserialize,Eq,PartialEq,Ord,PartialOrd)]
pub struct ZLevel {
  pub z: ZCoord,
  pub zg: Generation,
}

// ---------- game state ----------

#[derive(Debug,Serialize,Deserialize)]
pub struct GameState { // usual variable: gs
  pub table_colour: Colour,
  pub table_size: Pos,
  pub pieces: GPieces,
  pub gen: Generation,
  pub log: VecDeque<(Generation, Arc<CommittedLogEntry>)>,
  pub max_z: ZCoord,
  pub players: GPlayers,
  pub occults: GameOccults,
}

pub type GPlayers = DenseSlotMap<PlayerId, GPlayer>;

#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct GPlayer { // usual variable: gpl
  pub nick: String,
  pub layout: PresentationLayout,
  pub idmap: PerPlayerIdMap,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct GPiece{  // usual variable: gpc
  pub pos: Pos,
  pub face: FaceId,
  pub held: Option<PlayerId>,
  pub zlevel: ZLevel,
  pub pinned: bool,
  pub occult: PieceOccult,
  #[serde(default)] pub angle: PieceAngle,
  pub gen: Generation,
  pub lastclient: ClientId,
  pub gen_before_lastclient: Generation,
  pub xdata: PieceXDataState,
}

pub type PieceXDataState = Option<Box<dyn PieceXData>>;

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

#[typetag::serde(tag="type")]
pub trait PieceXData: Downcast + Debug + Send + 'static {
  fn default() -> Box<dyn PieceXData> where Self: Default {
    let k = Box::new(<Self as Default>::default());
    dbg!(&k);
    k
  }
}
impl_downcast!(PieceXData);

#[enum_dispatch]
#[dyn_upcast]
pub trait OutlineTrait: Send + Debug {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn surround_path(&self) -> Result<Html, IE> {
    self.outline_path(SELECT_SCALE)
  }
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<[Pos;2], IE>;
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

#[typetag::serde] // usual variable: p
pub trait PieceTrait: OutlineTrait + Send + Debug + 'static {
  /// by convention, occult face is nfaces-1
  // xxx this is no good, we need a central definition of the occult
  // face to avoid weird behaviour with buggy gamespecs
  fn nfaces(&self) -> RawFaceId;

  #[throws(InternalError)]
  fn add_ui_operations(&self, _upd: &mut Vec<UoDescription>,
                       _gpc: &GPiece, _y: ShowUnocculted) { }

  fn ui_operation(&self, _a: ApiPieceOpArgs<'_>,
                  _opname: &str, _wrc: WhatResponseToClientOp,
                  _y: ShowUnocculted)
                  -> Result<UpdateFromOpComplex, ApiPieceOpError> {
    throw!(OE::BadOperation)
  }

  // #[throws] doesn't work here - fehler #todo
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece,
               id: VisiblePieceId, y: ShowUnocculted) -> Result<(),IE>;

  fn describe_html(&self, gpc: &GPiece) -> Result<Html,IE>;

  fn delete_hook(&self, _p: &GPiece, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }

  fn itemname(&self, _y: ShowUnocculted) -> &str;
}

#[typetag::serde]
pub trait OccultedPieceTrait: OutlineTrait + 'static {
  fn svg(&self, f: &mut Html, id: VisiblePieceId) -> Result<(),IE>;
  fn describe_html(&self) -> Result<Html,IE>;
}

#[derive(Debug)]
pub struct ApiPieceOpArgs<'a> {
  pub gs: &'a mut GameState,
  pub ipieces: &'a IPieces,
  pub ioccults: &'a IOccults,
  pub to_permute: &'a mut ToPermute,
  pub player: PlayerId,
  pub piece: PieceId,
  pub ipc: &'a IPiece,
}

#[derive(Debug)]
pub struct PieceSpecLoaded {
  pub p: Box<dyn PieceTrait>,
  pub occultable: Option<(OccultIlkName, Box<dyn OccultedPieceTrait>)>,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec: Debug {
  fn count(&self) -> usize { 1 }
  fn load(&self, i: usize) -> Result<PieceSpecLoaded, SpecError>;
}

// ========== implementations ==========

// ---------- simple data types ----------

impl Generation {
  pub fn increment(&mut self) { self.0 += 1 }
  pub fn unique_gen(&mut self) -> UniqueGenGen<'_> {
    UniqueGenGen { gen: self, none_yet: iter::once(()) }
  }
}
impl Display for Generation {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    Display::fmt(&self.0, f)
  }
}

pub struct UniqueGenGen<'g> {
  gen: &'g mut Generation,
  none_yet: iter::Once<()>,
}

impl UniqueGenGen<'_> {
  pub fn next(&mut self) -> Generation {
    if self.none_yet.next().is_some() { self.gen.increment() }
    let r = *self.gen;
    self.gen.increment();
    r
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
  fn clamped(self, range: Self) -> Result<Self, Self>;
}

impl ClampTable for Coord {
  fn clamped(self, range: Coord) -> Result<Coord, Coord> {
    if self < 0     { return Err(0,   ) }
    if self > range { return Err(range) }
    return Ok(self)
  }
}

impl ClampTable for Pos {
  fn clamped(self, range: Pos) -> Result<Pos, Pos> {
    let mut output = ArrayVec::new();
    let mut ok = true;
    for (&pos, &rng) in izip!(self.0.iter(), range.0.iter()) {
      output.push(match pos.clamped(rng) {
        Ok(pos) => pos,
        Err(pos) => { ok = false; pos },
      })
    }
    let output = PosC(output.into_inner().unwrap());
    if ok { Ok(output) } else { Err(output) }
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

impl GPiece {
  #[throws(IE)]
  pub fn xdata<T:PieceXData+Default>(&self) -> Option<&T> {
    self.xdata.get()?
  }

  #[throws(IE)]
  pub fn xdata_mut<T:PieceXData+Default>(&mut self) -> &mut T {
    self.xdata.get_mut()?
  }

  pub fn dummy() -> Self {
    let gen_dummy = Generation(1);
    GPiece {
      pos: PosC([0,0]),
      face: default(),
      held: None,
      zlevel: ZLevel { z: default(), zg: gen_dummy },
      pinned: false,
      occult: default(),
      angle: default(),
      gen: gen_dummy,
      lastclient: ClientId(default()),
      gen_before_lastclient: gen_dummy,
      xdata: None,
    }
  }
}

fn xdata_unexpected<T:PieceXData+Default>(got: &dyn PieceXData)
                                          -> InternalError {
  internal_logic_error(format!(
    "\n\
     piece xdata unexpectedly: {:?}\n\
     expected something like:  Some({:?})\n",
    &got, <T as PieceXData>::default(),
  ))
}

#[ext(pub)]
impl PieceXDataState {
  #[throws(IE)]
  fn get<T:PieceXData>(&self) -> Option<&T> where T: Default {
    let xdata = if let Some(xdata) = &self { xdata } else { return None };
    let xdata: &dyn PieceXData = xdata.as_ref();
    if let Some(y) = xdata.downcast_ref::<T>() { Some(y) }
    else { throw!(xdata_unexpected::<T>(xdata)) }
  }

  fn get_mut<T:PieceXData+Default>(&mut self) -> Result<&mut T, IE> {
    let xdata = self.get_or_insert_with(|| <T as PieceXData>::default());
    let xdata: &mut dyn PieceXData = xdata.as_mut();
    let keep: *mut dyn PieceXData = xdata;
    if let Some(y) = xdata.downcast_mut::<T>() { return Ok(y) }
    
    // Erroneous borrowck error with early returns
    // https://github.com/rust-lang/rust/issues/58910
    // https://github.com/rust-lang/rust/issues/51545
    // Safety: the `xdata` borrow that was passed to `downcast_mut`
    // is no longer present in this branch, so now we have only `keep`
    // and the things `keep` was borrowed from.
    let xdata: &dyn PieceXData = unsafe { keep.as_ref().unwrap() };
    Err(xdata_unexpected::<T>(xdata))
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
