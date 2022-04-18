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

#[derive(Copy,Clone,Debug,Eq,Ord,PartialEq,PartialOrd)]
pub struct SpecDepth(u16);

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
  #[serde(default)] pub last_released: ClientId,
  pub gen_before_lastclient: Generation,
  pub xdata: PieceXDataState,
  pub moveable: PieceMoveable,
  #[serde(default)] pub rotateable: bool,
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
  impl Borrow<str> for GoodItemName {
    fn borrow(&self) -> &str { &self.0 }
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
  // This is used:
  //  1. To generate the path for SimpleShape's renderings
  //  2. This call here in the default impl of surround_path
  //  3. Things that use an Outline to represent a surround (eg dice.rs)
  fn outline_path(&self, scale: f64) -> Result<Html, IE>;
  // Used for the piece selection outline
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

#[dyn_upcast]
pub trait PieceBaseTrait: OutlineTrait + Send + Debug + 'static {
  /// By convention, occult face is nfaces-1
  fn nfaces(&self) -> RawFaceId;

  fn itemname(&self) -> &str;
}

#[typetag::serde] // usual variable: p
pub trait PieceTrait: PieceBaseTrait + Send + Debug + 'static {
  #[throws(InternalError)]
  fn add_ui_operations(&self, _: ShowUnocculted,
                       _upd: &mut Vec<UoDescription>,
                       _gs: &GameState, _gpc: &GPiece) { }

  fn ui_operation(&self, _: ShowUnocculted, _a: ApiPieceOpArgs<'_>,
                  _opname: &str, _wrc: WhatResponseToClientOp)
                  -> Result<UpdateFromOpComplex, ApiPieceOpError> {
    throw!(Ia::BadUiOperation)
  }

  /// Can return `false` to mean "I will handle it in ui_operation"
  #[throws(ApiPieceOpError)]
  fn ui_permit_flip(&self, _gpc: &GPiece) -> bool {
    true
  }

  // #[throws] doesn't work here - fehler #todo
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, gs: &GameState,
               id: VisiblePieceId) -> Result<(),IE>;

  fn describe_html(&self, gpc: &GPiece, _goccults: &GameOccults)
                   -> Result<Html,IE>;

  /// Piece is responsible for dealing with the possibility that they
  /// may be occulted!
  #[throws(IE)]
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

  fn sortkey(&self) -> Option<&str> { None }

  fn occultation_notify_hook(&self, _piece: PieceId) -> UnpreparedUpdates {
    None
  }

  #[throws(IE)]
  fn abs_bbox(&self, p: &GPiece) -> Rect {
    Rect { corners: self.bbox_approx()?.corners.iter().map(
      |c| *c + p.pos
    )
           .collect::<Result<ArrayVec<_,2>,_>>()?
           .into_inner().unwrap() }
  }
}

#[typetag::serde]
pub trait InertPieceTrait: PieceBaseTrait {
  /// When used for occultated version of another object,
  /// face used is always default, regardless of nfaces.
  /// This is always the case for a piece whose `PieceSpec::load`
  /// returns `Some` for occultation, but which does not provide
  /// a nontrivial `PieceSpec::load_inert`.
  fn svg(&self, f: &mut Html, id: VisiblePieceId, face: FaceId,
         xdata: &PieceXDataState /* use with care! */) -> Result<(),IE>;
  fn describe_html(&self, face: FaceId) -> Result<Html,IE>;
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
pub struct SpecLoaded {
  pub p: Box<dyn PieceTrait>,
  pub occultable: PieceSpecLoadedOccultable,
  pub special: PieceSpecialProperties,
}
#[derive(Debug)]
pub struct SpecLoadedInert {
  pub p: Box<dyn InertPieceTrait>,
  pub occultable: PieceSpecLoadedOccultable,
}

pub type PieceSpecLoadedOccultable =
  Option<(LOccultIlk, Arc<dyn InertPieceTrait>)>;

/// Special handling instructions for this piece
///
/// These remain constant after the piece has been loaded,
/// so they are mostly "can/do we do this thing".
#[derive(Debug,Clone,Default,Serialize,Deserialize)]
pub struct PieceSpecialProperties {
  pub rendering: Option<SpecialClientRendering>,
  pub multigrab: bool,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec: Debug + Sync + Send + 'static {
  #[throws(SpecError)]
  fn count(&self, _pcaliases: &PieceAliases) -> usize { 1 }
  fn load(&self, i: usize, gpc: &mut GPiece, ig: &Instance, depth: SpecDepth)
          -> Result<SpecLoaded, SpecError>;
  /// Used when a piece wants to use another for its occulted form
  fn load_inert(&self, _ig: &Instance, _:SpecDepth)
                -> Result<SpecLoadedInert, SpecError> {
    throw!(SpE::ComplexPieceWhereInertRequired)
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
  #[allow(clippy::should_implement_trait)] // we don't return Option
  pub fn next(&mut self) -> Generation {
    if self.none_yet.next().is_some() { self.gen.increment() }
    let r = *self.gen;
    self.gen.increment();
    r
  }
}

impl SpecDepth {
  pub fn zero() -> Self { Self(0) }
  pub fn increment(self) -> Option<Self> {
    const MAX: SpecDepth = SpecDepth(5);
    if self > MAX { return None }
    Some(Self(self.0 + 1))
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

#[derive(Error,Debug,Copy,Clone)]
#[error("position off table")]
pub struct PosOffTableError<T:Debug> { pub clamped: T }

pub trait ClampTable: Sized+Debug {
  fn clamped(self, range: Self) -> Result<Self, PosOffTableError<Self>>;
}

impl ClampTable for Coord {
  #[throws(PosOffTableError<Coord>)]
  fn clamped(self, range: Coord) -> Coord {
    if self < 0     { throw!(PosOffTableError{ clamped: 0,    }) }
    if self > range { throw!(PosOffTableError{ clamped: range }) }
    self
  }
}

impl ClampTable for Pos {
  fn clamped(self, range: Pos) -> Result<Pos, PosOffTableError<Pos>> {
    let mut output = ArrayVec::new();
    let mut ok = true;
    for (&pos, &rng) in izip!(self.coords.iter(), range.coords.iter()) {
      output.push(match pos.clamped(rng) {
        Ok(pos) => pos,
        Err(e) => { ok = false; e.clamped },
      })
    }
    let output = PosC{ coords: output.into_inner().unwrap() };
    if ok { Ok(output) } else { Err(PosOffTableError { clamped: output }) }
  }
}

impl<T:Debug> From<PosOffTableError<T>> for SpecError {
  fn from(_: PosOffTableError<T>) -> SpecError { SpecError::PosOffTable }
}
impl<T:Debug> From<PosOffTableError<T>> for MgmtError {
  fn from(pote: PosOffTableError<T>) -> MgmtError { ME::BadSpec(pote.into()) }
}

// ---------- ApiPieceOpArgs ----------

impl ApiPieceOpArgs<'_> {
  #[throws(ApiPieceOpError)]
  pub fn pri(&mut self) -> PieceRenderInstructions {
    let ApiPieceOpArgs { gs,ioccults,player,piece,ipc, .. } = self;
    let gpc = gs.pieces.byid(*piece)?;
    let gpl = gs.players.byid_mut(*player)?;
    piece_pri(ioccults, &gs.occults, *player, gpl, *piece, gpc, ipc)
      .ok_or(Ia::PieceGone)?
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
  pub fn heavy(&self) -> bool {
    self.pinned ||
    self.occult.is_active() ||
    self.moveable == PieceMoveable::No
  }
  pub fn rotateable(&self) -> bool {
    if self.occult.is_active() { false }
    else { self.rotateable }
  }
  pub fn multigrab(&self, ipc: &IPiece) -> bool {
    if self.occult.is_active() { false }
    else { ipc.special.multigrab }
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
      last_released: ClientId(default()),
      gen_before_lastclient: gen_dummy,
      xdata: None,
      moveable: default(),
      rotateable: true,
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

// ---------- automatic lowering ----------

#[throws(IE)]
pub fn piece_make_heavy(gpieces: &GPieces, piece: PieceId) -> ZCoord {
  // constraints:
  //  - we would like to lower this piece below all non-heavy pieces
  //  - we don't want to lower it past any heavy piece because maybe
  //    someone is doing weird stacking deliberately
  //
  // so we make a possible range for us, which is
  //  - strictly above the highhest heavy piece below this one
  //  - strictly below the lowest non-heavy piece above that and below us
  //  - strictly below where we are now
  //
  // Hopefully the two z coordinates are different and the range is in
  // the right order. ; if not, we just return this piece's existing Z
  // coordinate.  (We don't get into messing with generations.)

  let tgpc = gpieces.get(piece)
    .ok_or_else(|| internal_error_bydebug(&piece))?;

  let find_some = |want_heavy: bool| {
    gpieces.iter()
      .filter(|(p,_)| *p != piece)
      .filter(move |(_,gpc)| gpc.heavy() == want_heavy)
      .map(|(_,gpc)| &gpc.zlevel)
  };

  let highest_heavy_below: Option<&ZLevel> =
    find_some(true)
    .filter(|z| z < &&tgpc.zlevel)
    .max();

  let lowest_nonheavy_above: Option<&ZLevel> =
    find_some(false)
    .filter(|z| {
      z < &&tgpc.zlevel &&
      if let Some(ref lim) = highest_heavy_below {
        z > lim
      } else {
        true
      }
    })
    .min();

  let maximum: &ZLevel =
    lowest_nonheavy_above.into_iter()
    .chain(iter::once( &tgpc.zlevel ))
    .min().unwrap();

  // dbgc!( &highest_heavy_below, &lowest_nonheavy_above, &maximum, );
           

  if let Ok(mut zrange) = zcoord::Mutable::some_range(
          highest_heavy_below.map(|z| z.z.clone_mut()).as_ref()  ,
   Some(& maximum                      .z.clone_mut()           ),
          1024)
  {
    zrange.next().unwrap()
  } else {
    tgpc.zlevel.z.clone()
  }
}

#[cfg(test)]
mod test {
  #[allow(unused_imports)] use super::*;

  #[test]
  #[cfg(not(miri))]
  fn make_heavy() {
    const N: usize = 5;

    let bools = [false,true].iter().cloned();

    for (targeti, for_pieces) in iproduct!(
      0..N,
      iter::repeat(
        iproduct!( bools.clone(), bools.clone() ),
      ).take(N).multi_cartesian_product()
    ) {
      let desc = (targeti, &for_pieces);

      let mut gpieces: GPieces = default();
      let mut z = ZCoord::default().clone_mut();
      let mut piece = None;
      let mut pieceids = vec![];
      for (i, &(heavy, zchange)) in for_pieces.iter().enumerate() {
        let mut gpc = GPiece::dummy();
        gpc.zlevel.zg = Generation(i.try_into().unwrap());
        gpc.pinned = heavy;
        gpc.zlevel.z = if zchange {
          z.increment().unwrap()
        } else {
          z.repack().unwrap()
        };
        let p = gpieces.as_mut_t().insert(gpc);
        if i == targeti { piece = Some(p); }
        pieceids.push(p);
      }

      eprint!("make_heavy {}", targeti);
      for &(heavy, zchange) in &for_pieces {
        eprint!(" {}{}",
                if heavy   { "H" } else { "l" },
                if zchange { "z" } else { "E" });
      }
      let targetp = piece.unwrap();
      let new_z = piece_make_heavy(&gpieces, targetp).unwrap();
      let tgpc = &gpieces[targetp];

      eprintln!(" {:?} {}", piece, &new_z);

      for (i, p, &(heavy, zchange)) in izip!(
        0..N,
        &pieceids,
        &for_pieces,
      ).rev() {
        let gpc = &gpieces[*p];
        eprintln!(" {}{} {:?}  {}{}  {:<20} {:6}  {:?}",
                  if i == targeti { "*" } else { " " },
                  i, p,
                  if heavy   { "H" } else { "l" },
                  if zchange { "z" } else { "E" },
                  &gpc.zlevel.z,
                  &gpc.zlevel.zg,
                  Ord::cmp(&gpc.zlevel.z, &new_z));
      }

      // not moved up
      assert!( new_z <= tgpc.zlevel.z );

      // if changed, distinct from every z
      if new_z != tgpc.zlevel.z {
        for (p,gpc) in &*gpieces {
          if p == targetp { continue }
          assert!( &new_z != &gpc.zlevel.z );
        }

        // not moved past any heavy
        for (p,gpc) in &*gpieces {
          if ! gpc.pinned { continue }
          if gpc.zlevel >= tgpc.zlevel { continue } // was higher
          if gpc.zlevel.z < new_z { continue } // still lower
          panic!("{:?} {:?}", &desc, p);
        }
      }

      if for_pieces.iter().all(|(_,zchange)| *zchange) {
        // any light below new postion is also below some other heavy
        // (ie, we have an excuse for not putting ourselves below it)
        // (or there were clashing ZCoords)

        let max_heavy = gpieces.values()
          .filter(|gpc| gpc.pinned)
          .map(|gpc| &gpc.zlevel)
          .max();

        for (p,gpc) in &*gpieces {
          if p == targetp { continue }
          if gpc.pinned { continue }

          if (&gpc.zlevel.z, & gpc.zlevel.zg) <
             (&new_z,        &tgpc.zlevel.zg)
          {
            assert!( &gpc.zlevel < max_heavy.as_ref().unwrap(),
                     "{:?}, {:?}", &p, &max_heavy );
          }
        }
      }
    }
  }
}

// ---------- log expiry ----------

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
