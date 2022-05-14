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

#[derive(Copy,Clone,Debug,Eq,Ord,PartialEq,PartialOrd)]
pub struct SpecDepth(u16);

// ---------- general data types ----------

/// Pieces on top have higher values than ones 
#[derive(Debug,Clone,Eq,PartialEq,Ord,PartialOrd)]
#[derive(Serialize,Deserialize)]
pub struct ZLevel {
  pub z: ZCoord,
  pub zg: Generation,
}

pub type MultigrabQty = u32;

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
  pub occults: GOccults,
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
  #[serde(default, skip_serializing_if="is_default")]
  pub face: FaceId,
  #[serde(default, skip_serializing_if="is_default")]
  pub held: Option<PlayerId>,
  pub zlevel: ZLevel,
  #[serde(default, skip_serializing_if="is_default")]
  pub pinned: bool,
  pub occult: PieceOccult,
  #[serde(default, skip_serializing_if="PieceAngle::is_default")]
  pub angle: PieceAngle,
  pub gen: Generation,
  pub lastclient: ClientId,
  #[serde(default)] pub last_released: ClientId,
  pub gen_before_lastclient: Generation,
  #[serde(default, skip_serializing_if="Option::is_none")]
  pub xdata: PieceXDataState,
  pub moveable: PieceMoveable,
  #[serde(default)] pub rotateable: bool,
  #[serde(default, skip_serializing_if="Option::is_none")]
  pub fastsplit: Option<FastSplitId>,
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

#[ambassador::delegatable_trait]
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

#[ambassador::delegatable_trait]
#[dyn_upcast]
pub trait PieceBaseTrait: OutlineTrait + Send + Debug + 'static {
  /// By convention, occult face is nfaces-1
  fn nfaces(&self) -> RawFaceId;

  fn itemname(&self) -> &str;
}

#[ambassador::delegatable_trait]
#[typetag::serde] // usual variable: p
pub trait PieceTrait: PieceBaseTrait + Downcast + Send + Debug + 'static {
  fn add_ui_operations(&self, _y: ShowUnocculted,
                       _upd: &mut Vec<UoDescription>,
                       _gs: &GameState, _gpc: &GPiece) -> Result<(),IE> {
    Ok(())
  }

  fn ui_operation(&self, _y: ShowUnocculted, _a: ApiPieceOpArgs<'_>,
                  _opname: &str, _wrc: WhatResponseToClientOp)
                  -> Result<OpOutcomeThunk, ApiPieceOpError> {
    throw!(Ia::BadUiOperation)
  }

  /// Can return `false` to mean "I will handle it in ui_operation"
  fn ui_permit_flip(&self, _gpc: &GPiece) -> Result<bool,ApiPieceOpError>  {
    Ok(true)
  }

  // #[throws] doesn't work here - fehler #todo
  fn svg_piece(&self, f: &mut Html, gpc: &GPiece, gs: &GameState,
               id: VisiblePieceId) -> Result<(),IE>;

  fn describe_html(&self, gpc: &GPiece, _goccults: &GOccults)
                   -> Result<Html,IE>;

  /// Piece is responsible for dealing with the possibility that they
  /// may be occulted!
  fn held_change_hook(&self,
                      _ig: &InstanceRef,
                      _gplayers: &GPlayers,
                      _ipieces: &IPieces,
                      _goccults: &GOccults,
                      _gpieces: &mut GPieces,
                      _tpiece: PieceId,
                      _was_held: Option<PlayerId>)
                      -> Result<OpHookThunk,IE> { Ok(default()) }

  fn loaded_hook(&self, _piece: PieceId,
                 _gs: &mut GameState, _ig: &InstanceRef) -> Result<(),IE> {
    Ok(())
  }

  /// Not called if the whole game is destroyed.
  /// You can use Drop of course but it's not usually much use since
  /// you don't have a reference to the game or anything.
  fn delete_hook(&self, _p: &GPiece, _gs: &mut GameState)
                 -> ExecuteGameChangeUpdates { 
    ExecuteGameChangeUpdates{ pcs: vec![], log: vec![], raw: None }
  }

  fn sortkey(&self) -> Option<&str> { None }

  fn occultation_notify_hook(&self, _piece: PieceId) -> UnpreparedUpdates {
    default()
  }

  // This is going to have to set the Z coordinate.  This is because
  // when we split a banknote, the note the user grabbed must end up
  // above the "change", the "new" note which remains.  So we need to
  // select Z coordinates.
  //
  // We *could* solve this by simply giving the two pieces different Z
  // generations, but the same ZCoord, but this is quite undesirable
  // because it can lead to the client having to do piece restacking.
  //
  // We can't readily find a suitable Z ZCoord for the change (ie, the
  // lower coordinate), because that's piece lowering which is very
  // complicated, because it may have to restack due to clashing
  // ZCoords.  (We have that in JS in the client.)
  //
  // So we want to set the ZCoord of the piece the client has just
  // grasped.  We don't want to disrupt the client's drag operation,
  // so we don't want to change anything server-side that's subject to
  // the update concurrency protocool.  Or to put it another way,
  // the ZCoord must be predictable (and predicted) by the client.
  // And the client can easily select a suitable ZCoord: the top
  // one will do.
  //
  // So the multigrab operation specifies a ZCoord.  The client
  // selects the top, but this is not checked centrally.  Implementations
  // of op_multigrab that have particular requirements, like the fastsplit
  // banknotes, must do whatever check is needed.
  fn op_multigrab(&self, _a: ApiPieceOpArgs, _show: ShowUnocculted,
                  _qty: MultigrabQty, _new_z: ShouldSetZLevel)
                  -> Result<OpOutcomeThunk,ApiPieceOpError>  {
    Err(Ia::BadPieceStateForOperation)?
  }

  fn abs_bbox(&self, p: &GPiece) -> Result<Rect, IE> {
    Ok(Rect { corners: self.bbox_approx()?.corners.iter().map(
      |c| *c + p.pos
    )
              .collect::<Result<ArrayVec<_,2>,_>>()?
              .into_inner().unwrap() })
  }
}
impl_downcast!(PieceTrait);

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
#[derive(Debug,Clone,ConstDefault,Default,Serialize,Deserialize)]
pub struct PieceSpecialProperties {
  #[serde(default, skip_serializing_if="Option::is_none")]
  pub rendering: Option<SpecialClientRendering>,

  #[serde(default, skip_serializing_if="is_default")]
  pub multigrab: bool,
}

pub struct PieceLoadArgs<'a> {
  pub i: usize,
  pub gpc: &'a mut GPiece,
  pub ig: &'a Instance,
  pub depth: SpecDepth,
}

#[typetag::serde(tag="type")]
pub trait PieceSpec: Debug + Sync + Send + 'static {
  #[throws(SpecError)]
  fn count(&self, _pcaliases: &PieceAliases) -> usize { 1 }
  fn load(&self, pla: PieceLoadArgs) -> Result<SpecLoaded, SpecError>;
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

#[derive(Error, Debug)]
#[error("{self:?}")]
pub struct PieceTraitDowncastFailed<'p> {
  pub p: &'p dyn PieceTrait,
  pub why: &'static str,
}

#[ext(pub)]
impl<'r> &'r dyn PieceTrait {
  #[throws(PieceTraitDowncastFailed<'r>)]
  fn downcast_piece<P: PieceTrait>(self) -> &'r P {
    self.downcast_ref::<P>().ok_or_else(
      || PieceTraitDowncastFailed { p: self, why: "piece" })?
  }
}

impl<'p> From<PieceTraitDowncastFailed<'p>> for InternalError {
  fn from(e: PieceTraitDowncastFailed<'p>) -> InternalError {
    internal_logic_error(format!(
      "downcaste_piece failure {}! got: {:?}", &e.why, &e.p))
  }
}
impl<'p> From<PieceTraitDowncastFailed<'p>> for ApiPieceOpError {
  fn from(e: PieceTraitDowncastFailed<'p>) -> ApiPieceOpError { e.into() }
}

// ---------- positions and ClampTable ----------

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

  #[throws(IE)]
  pub fn xdata_init<T:PieceXData>(&mut self, val: T) -> &mut T {
    self.xdata.init(val)?
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
      fastsplit: default(),
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
fn xdata_unexpectedly_present(got: &dyn PieceXData) -> InternalError {
  internal_logic_error(format!(
    "\n\
     piece xdata unexpectedly present: {:?}\n",
    &got,
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

  #[throws(IE)]
  fn init<T:PieceXData>(&mut self, val: T) -> &mut T {
    if let Some(xdata) = self.as_ref() {
      let xdata: &dyn PieceXData = &**xdata;
      throw!(xdata_unexpectedly_present(xdata));
    }
    let xdata = self.insert(Box::new(val) as _);
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

// ---------- 2-phase Z level setting ----------

/// Drop this only on errors; don't just forget it
#[derive(Debug, Clone)]
pub struct ShouldSetZLevel(ZLevel);

/// Prepare to set the z level of gpc to z
///
/// If this is a good idea, returns a ShouldSetZLevel.
/// That should be [`implement`](ShouldSetZLevel::implement)ed
/// along with the the rest of the operation, when committing.
///
/// This allows us to do all of an operation's checks and preparation,
/// including Z level setting, first, and then only set the Z level
/// infallibly at the end.
#[throws(ApiPieceOpError)]
pub fn api_op_set_z(gpc: &mut GPiece, gen: Generation, z: &ZCoord)
                    -> ShouldSetZLevel
{
  if gpc.occult.is_active() {
    if z >= &gpc.zlevel.z { throw!(Ia::Occultation) }
  }
  ShouldSetZLevel(ZLevel { z: z.clone(), zg: gen })
}

impl ShouldSetZLevel {
  pub fn implement(self, gpc: &mut GPiece) {
    gpc.zlevel = self.0;
  }

  pub fn inspect(&self) -> &ZLevel { &self.0 }
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
