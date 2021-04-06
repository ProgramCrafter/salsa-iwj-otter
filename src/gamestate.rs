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

#[derive(Copy,Clone,Debug,Serialize,Deserialize,Eq,Ord,PartialEq,PartialOrd)]
#[serde(transparent)]
pub struct Timestamp(pub u64); /* time_t */

pub const DEFAULT_TABLE_SIZE: Pos = PosC::new( 400, 200 );
pub const DEFAULT_TABLE_COLOUR: &str = "green";

// ---------- general data types ----------

#[derive(Debug,Clone,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
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
  pub max_z: ZLevel,
  pub players: GPlayers,
  pub occults: GameOccults,
}

pub type GPlayers = DenseSlotMap<PlayerId, GPlayer>;

#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct GPlayer { // usual variable: gpl
  pub nick: String,
  pub layout: PresentationLayout,
  pub idmap: PerPlayerIdMap,
  #[serde(default)] pub movehist: movehist::PlHist,
  #[serde(default)] pub moveheld: movehist::PlHeld,
}

#[derive(Debug,Serialize,Deserialize)]
pub struct GPiece {  // usual variable: gpc
  pub pos: Pos,
  pub face: FaceId,
  pub held: Option<PlayerId>,
  pub zlevel: ZLevel,
  pub pinned: bool,
  pub occult: PieceOccult,
  pub angle: PieceAngle,
  pub gen: Generation,
  pub lastclient: ClientId,
  pub gen_before_lastclient: Generation,
  pub xdata: PieceXDataState,
  pub moveable: PieceMoveable,
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

//---------- GoodItemName ----------

mod item_name {
  use super::*;
  use shapelib::LibraryLoadError;
  use LibraryLoadError as LLE;

  #[derive(Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
  #[derive(Serialize,Deserialize)]
  #[serde(transparent)]
  pub struct GoodItemName(String);
  impl Borrow<String> for GoodItemName {
    fn borrow(&self) -> &String { &self.0 }
  }
  impl GoodItemName {
    pub fn as_str(&self) -> &str { &self.0 }
  }
  impl From<GoodItemName> for String {
    fn from(i: GoodItemName) -> String { i.0 }
  }
  impl Display for GoodItemName {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut fmt::Formatter) { f.write_str(&self.0)? }
  }
  hformat_as_display!{ GoodItemName }

  impl TryFrom<String> for GoodItemName {
    type Error = LibraryLoadError;
    #[throws(LibraryLoadError)]
    fn try_from(s: String) -> GoodItemName {
      if s.as_bytes().iter().all(|&c:&u8| (
        c.is_ascii_alphanumeric() ||
        b"-_. ()".contains(&c)
      )) {
        GoodItemName(s)
      } else {
        throw!(LLE::BadItemName(s))
      }
    }
  }
}
pub use item_name::*;

// ---------- piece trait, and rendering ----------

#[typetag::serde(tag="type")]
pub trait PieceXData: Downcast + Debug + Send + 'static {
  fn dummy() -> Self where Self: Sized;
}
impl_downcast!(PieceXData);

#[enum_dispatch]
#[dyn_upcast]
pub trait OutlineTrait: Debug + Sync + Send + 'static {
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  fn surround_path(&self) -> Result<Html, IE> {
    self.outline_path(SELECT_SCALE)
  }
  fn thresh_dragraise(&self) -> Result<Option<Coord>, IE>;
  fn bbox_approx(&self) -> Result<Rect, IE>;
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
  fn nfaces(&self) -> RawFaceId;

  #[throws(InternalError)]
  fn add_ui_operations(&self, _upd: &mut Vec<UoDescription>,
                       _gs: &GameState, _gpc: &GPiece) { }

  fn ui_operation(&self, _a: ApiPieceOpArgs<'_>,
                  _opname: &str, _wrc: WhatResponseToClientOp)
                  -> Result<UpdateFromOpComplex, ApiPieceOpError> {
    throw!(OE::BadOperation)
  }

  // #[throws] doesn't work here - fehler #todo
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, gs: &GameState,
               id: VisiblePieceId) -> Result<(),IE>;

  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults)
                   -> Result<Html,IE>;

  #[throws(IE)]
  /// Piece is responsible for dealing with the possibility that they
  /// may be occulted!
  fn held_change_hook(&self,
                      _ig: &InstanceRef,
                      _gpieces: &mut GPieces,
                      _piece: PieceId,
                      _was_held: Option<PlayerId>)
                      -> UnpreparedUpdates { None }

  #[throws(IE)]
  fn loaded_hook(&self, _piece: PieceId,
                 _gs: &mut GameState, _ig: &InstanceRef) { }

  /// Not called if the whole game is destroyed.
  /// You can use Drop of course but it's not usually much use since
  /// you don't have a reference to the game or anything.
  fn delete_hook(&self, _p: &GPiece, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }

  fn itemname(&self) -> &str;

  fn occultation_notify_hook(&self, _piece: PieceId) -> UnpreparedUpdates {
    None
  }

  #[throws(IE)]
  fn abs_bbox(&self, p: &GPiece) -> Rect {
    Rect { corners: self.bbox_approx()?.corners.iter().map(
      |c| *c + p.pos
    )
           .collect::<Result<ArrayVec<_>,_>>()?
           .into_inner().unwrap() }
  }
}

#[typetag::serde]
pub trait OccultedPieceTrait: OutlineTrait {
  fn svg(&self, f: &mut Html, id: VisiblePieceId) -> Result<(),IE>;
  fn describe_html(&self) -> Result<Html,IE>;
}

#[derive(Debug)]
pub struct ApiPieceOpArgs<'a> {
  pub ig: &'a InstanceRef,
  pub gs: &'a mut GameState,
  pub ipieces: &'a IPieces,
  pub ioccults: &'a IOccults,
  pub to_recalculate: &'a mut ToRecalculate,
  pub player: PlayerId,
  pub piece: PieceId,
  pub ipc: &'a IPiece,
  pub cseq: ClientSequence,
  pub client: ClientId,
}

#[derive(Debug)]
pub struct PieceSpecLoaded {
  pub p: Box<dyn PieceTrait>,
  pub occultable: PieceSpecLoadedOccultable,
}
pub type PieceSpecLoadedOccultable =
  Option<(OccultIlkName, Arc<dyn OccultedPieceTrait>)>;

#[typetag::serde(tag="type")]
pub trait PieceSpec: Debug + Sync + Send + 'static {
  #[throws(SpecError)]
  fn count(&self, _pcaliases: &PieceAliases) -> usize { 1 }
  fn load(&self, i: usize, gpc: &mut GPiece,
          pcaliases: &PieceAliases, ir: &InstanceRef)
          -> Result<PieceSpecLoaded, SpecError>;
  fn load_occult(&self, _pcaliases: &PieceAliases)
                 -> Result<Box<dyn OccultedPieceTrait>, SpecError> {
    throw!(SpE::ComplexPieceWhereSimpleRequired)
  }
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
    for (&pos, &rng) in izip!(self.coords.iter(), range.coords.iter()) {
      output.push(match pos.clamped(rng) {
        Ok(pos) => pos,
        Err(pos) => { ok = false; pos },
      })
    }
    let output = PosC{ coords: output.into_inner().unwrap() };
    if ok { Ok(output) } else { Err(output) }
  }
}

// ---------- game state - rendering etc. ----------

impl GPiece {
  #[throws(IE)]
  pub fn xdata<T:PieceXData>(&self) -> Option<&T> {
    self.xdata.get()?
  }

  #[throws(IE)]
  pub fn xdata_exp<T:PieceXData>(&self) -> &T {
    self.xdata.get_exp()?
  }

  #[throws(IE)]
  pub fn xdata_mut<T:PieceXData,D:FnOnce()->T>(&mut self, def: D) -> &mut T {
    self.xdata.get_mut(def)?
  }

  #[throws(IE)]
  pub fn xdata_mut_exp<T:PieceXData>(&mut self) -> &mut T {
    self.xdata.get_mut_exp()?
  }

  pub fn moveable(&self) -> PieceMoveable {
    if self.occult.is_active() { PieceMoveable::No }
    else { self.moveable }
  }

  pub fn dummy() -> Self {
    let gen_dummy = Generation(1);
    GPiece {
      pos: PosC::zero(),
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
      moveable: default(),
    }
  }
}

fn xdata_unexpected<T:PieceXData>(got: &dyn PieceXData) -> InternalError {
  internal_logic_error(format!(
    "\n\
     piece xdata unexpectedly: {:?}\n\
     expected something like:  {:?}\n",
    &got, T::dummy(),
  ))
}
fn xdata_missing<T:PieceXData>() -> InternalError {
  internal_logic_error(format!(
    "\n\
     piece xdata unexpected missing\n\
     expected something like: {:?}\n",
    T::dummy(),
  ))
}

#[ext(pub)]
impl PieceXDataState {
  #[throws(IE)]
  fn get<T:PieceXData>(&self) -> Option<&T> {
    let xdata = if let Some(xdata) = &self { xdata } else { return None };
    let xdata: &dyn PieceXData = xdata.as_ref();
    if let Some(y) = xdata.downcast_ref::<T>() { Some(y) }
    else { throw!(xdata_unexpected::<T>(xdata)) }
  }

  #[throws(IE)]
  fn get_exp<T:PieceXData>(&self) -> &T {
    self.get()?.ok_or_else(|| xdata_missing::<T>())?
  }

  #[throws(IE)]
  fn get_mut<T:PieceXData,D:FnOnce()->T>(&mut self, def: D) -> &mut T {
    let xdata = self.get_or_insert_with(|| Box::new(def()));
    xdata_get_mut_inner(xdata)?
  }

  #[throws(IE)]
  fn get_mut_exp<T:PieceXData>(&mut self) -> &mut T {
    let xdata = self.as_mut().ok_or_else(|| xdata_missing::<T>())?;
    xdata_get_mut_inner(xdata)?
  }
}

fn xdata_get_mut_inner<
  T: PieceXData,
>(xdata: &mut Box<dyn PieceXData>) -> Result<&mut T, IE> {
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

impl GameState {
  pub fn dummy() -> Self { GameState {
    table_colour: Html::lit("green").into(),
    table_size: PosC::new(300,200),
    pieces: default(),
    gen: Generation(0),
    log: default(),
    max_z: ZLevel::zero(),
    players: default(),
    occults: default(),
  } }
}

impl ZLevel {
  pub fn zero() -> Self { ZLevel { z: default(), zg: Generation(0) } }
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
        html: Html::lit("[Earlier log entries expired]").into(),
      };
      *front = Arc::new(CommittedLogEntry { logent, when: front.when });
    }
  }
}
