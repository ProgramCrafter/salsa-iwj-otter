// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// update messages from server to client

use crate::prelude::*;

#[path="movehist.rs"] pub mod movehist;

#[allow(non_camel_case_types)] type TUE_P<'u> = TransmitUpdateEntry_Piece<'u>;
#[allow(non_camel_case_types)] type PUE_I = PreparedUpdateEntry_Image;
#[allow(non_camel_case_types)] type TUE_I<'u> = TransmitUpdateEntry_Image<'u>;

// ---------- newtypes, type aliases, basic definitions ----------

pub type RawClientSequence = u64;

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(RawClientSequence);

pub type UnpreparedUpdates = Option<SomeUnpreparedUpdates>;
pub type SomeUnpreparedUpdates = Box<
    dyn for<'r> FnOnce(&'r mut PrepareUpdatesBuffer)
    >;

// ---------- from manamgenet operations ----------

#[derive(Debug)] // not Default
pub struct ExecuteGameChangeUpdates {
  pub pcs: Vec<(PieceId, PieceUpdateOp<(), ()>)>,
  pub log: Vec<LogEntry>,
  pub raw: Option<Vec<PreparedUpdateEntry>>,
}

// ---------- prepared updates, queued in memory ----------

pub type PlayerUpdatesLog =
  StableIndexVecDeque<Arc<PreparedUpdate>,sse::UpdateId>;

#[derive(Debug)]
pub struct PlayerUpdates {
  log: PlayerUpdatesLog,
  cv: Arc<Condvar>,
}

#[derive(Debug)]
pub struct PreparedUpdate {
  pub gen: Generation,
  pub when: Instant,
  pub us: Vec<PreparedUpdateEntry>,
}

#[derive(Debug)]
pub enum PreparedUpdateEntry {
  Piece(PreparedUpdateEntry_Piece),
  Image(PreparedUpdateEntry_Image),
  MoveHistEnt(SecondarySlotMap<PlayerId, MoveHistEnt>),
  SetTableSize(Pos),
  SetTableColour(Colour),
  SetLinks(Arc<LinksTable>),
  AddPlayer {
    player: PlayerId,
    data: DataLoadPlayer,
    new_info_pane: Arc<Html>,
  },
  RemovePlayer {
    player: PlayerId,
    new_info_pane: Arc<Html>,
  },
  Log(Arc<CommittedLogEntry>),
  Error(ErrorSignaledViaUpdate<PUE_P>),
}

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
pub struct PreparedUpdateEntry_Piece {
  by_client: IsResponseToClientOp,
  ops: SecondarySlotMap<PlayerId, PreparedPieceUpdate>,
}

pub type PreparedPieceUpdate = PreparedPieceUpdateGeneral<
    PieceUpdateOp<PreparedPieceState, ZLevel>
    >;

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
pub struct PreparedUpdateEntry_Image {
  ims: SecondarySlotMap<PlayerId, PreparedPieceUpdateGeneral<
      PreparedPieceImage
      >>,
}

#[derive(Debug,Clone)]
pub struct PreparedPieceUpdateGeneral<U> {
  piece: VisiblePieceId,
  op: U,
}

#[derive(Debug,Clone,Serialize)]
pub struct PreparedPieceState {
  pub pos: Pos,
  pub svg: Html,
  pub facehint: Option<FaceId>,
  pub held: Option<PlayerId>,
  pub z: ZCoord,
  pub zg: Generation,
  pub angle: CompassAngle,
  pub pinned: bool,
  pub moveable: PieceMoveable,
  pub uos: Vec<UoDescription>,
  pub occregion: Option<JsonString<Region>>,
}

#[derive(Debug,Copy,Clone,Serialize,Deserialize,Eq)]
#[derive(Ord,PartialEq,PartialOrd)]
pub enum PieceMoveable {
  No,
  IfWresting,
  Yes,
}
impl Default for PieceMoveable { fn default() -> Self { PieceMoveable::Yes } }

#[derive(Debug,Clone,Serialize)]
pub struct PreparedPieceImage {
  pub svg: Html,
  pub uos: Vec<UoDescription>,
}

#[derive(Serialize,Debug)]
pub struct DataLoadPlayer {
  dasharray: Html,
  nick: Html,
}

pub trait JsonLen {
  fn json_len(&self) -> usize;
}

#[ext]
impl<T:JsonLen> SecondarySlotMap<PlayerId, T> {
  fn json_len(&self, player: PlayerId) -> usize {
    if let Some(t) = self.get(player) {
      50 + t.json_len()
    } else {
      50
    }
  }
}

impl JsonLen for Html {
  fn json_len(&self) -> usize { self.as_html_str().as_bytes().len() }
}

// ---------- piece updates ----------

#[derive(Debug,Clone,Copy,Serialize)]
pub enum PieceUpdateOp<NS,ZL> {
  Delete(),
  Insert(NS),
  Modify(NS),
  ModifyQuiet(NS),
  Move(Pos),
  SetZLevel(ZL),
}

pub type UpdateFromOpComplex = (
  PieceUpdate,
  Vec<(PieceId, PieceUpdateOps)>,
  UnpreparedUpdates,
);

pub type PieceUpdateFromOpSimple = (
  WhatResponseToClientOp,
  PieceUpdateOp<(),()>,
  Vec<LogEntry>,
);
pub type PieceUpdateResult = Result<PieceUpdate, ApiPieceOpError>;

#[derive(Debug)]
pub struct PieceUpdate {
  pub wrc: WhatResponseToClientOp,
  pub log: Vec<LogEntry>,
  pub ops: PieceUpdateOps,
}

#[derive(Debug)]
pub enum PieceUpdateOps {
  Simple(PieceUpdateOp<(),()>),
  PerPlayer(PieceUpdateOps_PerPlayer),
}

#[allow(non_camel_case_types)]
pub type PieceUpdateOps_PerPlayer =
  SecondarySlotMap<PlayerId, PieceUpdateOp<(),()>>;

impl From<PieceUpdateOp<(),()>> for PieceUpdateOps {
  fn from(op: PieceUpdateOp<(),()>) -> Self { PUOs::Simple(op) }
}

impl From<PieceUpdateFromOpSimple> for PieceUpdate {
  fn from((wrc, op, log): PieceUpdateFromOpSimple) -> Self {
    PieceUpdate { wrc, log, ops: op.into() }
  }
}

// ---------- for traansmission ----------

#[derive(Debug,Serialize)]
pub struct TransmitUpdate<'u> (
  Generation,
  Vec<TransmitUpdateEntry<'u>>,
);

#[derive(Debug,Serialize)]
enum TransmitUpdateEntry<'u> {
  Recorded {
    piece: VisiblePieceId,
    cseq: ClientSequence,
    zg: Option<Generation>,
    svg: Option<&'u Html>, // IsResponseToClientOp::UpdateSvg
  },
  Piece(TransmitUpdateEntry_Piece<'u>),
  Image(TransmitUpdateEntry_Image<'u>),
  MoveHistEnt(&'u MoveHistEnt),
  RecordedUnpredictable {
    piece: VisiblePieceId,
    cseq: ClientSequence,
    ns: &'u PreparedPieceState,
  },
  SetTableSize(Pos),
  SetTableColour(&'u Colour),
  AddPlayer {
    player: PlayerId,
    data: &'u DataLoadPlayer,
    new_info_pane: &'u Arc<Html>,
  },
  RemovePlayer {
    player: PlayerId,
    new_info_pane: &'u Arc<Html>,
  },
  SetLinks(Html),
  #[serde(serialize_with="serialize_logentry")]
  Log(TransmitUpdateLogEntry<'u>),
  Error(ErrorSignaledViaUpdate<TUE_P<'u>>),
}

type TransmitUpdateLogEntry<'u> = (&'u Timezone, &'u CommittedLogEntry);

#[allow(non_camel_case_types)]
#[derive(Debug,Serialize)]
struct TransmitUpdateEntry_Piece<'u> {
  piece: VisiblePieceId,
  op: PieceUpdateOp<&'u PreparedPieceState, &'u ZLevel>,
}

#[allow(non_camel_case_types)]
#[derive(Debug,Serialize)]
struct TransmitUpdateEntry_Image<'u> {
  piece: VisiblePieceId,
  im: &'u PreparedPieceImage,
}

#[derive(Debug,Serialize)]
struct FormattedLogEntry<'u> {
  when: String,
  logent: &'u LogEntry,
}

// ========== implementation ==========

// ---------- helpful utilities ----------

#[throws(OE)]
pub fn log_did_to_piece_whoby(ioccults: &IOccults, goccults: &GameOccults,
                              by_gpl: &GPlayer,
                              gpc: &GPiece, ipc: &IPiece, did: &str)
                              -> (Vec<LogEntry>, Option<Html>)
{
  let who_by = by_gpl.nick.to_html();
  let y = gpc.fully_visible_to_everyone();
  let desc = (||{
    Ok::<_,IE>(match ipc.show_or_instead(ioccults, y)? {
      Left(y) => ipc.show(y).describe_html(gpc, goccults)?,
      Right(instead) => instead.describe_html()?,
    })
  })().unwrap_or_else(|e|{
    error!("failed to format during logging: {:?}", e);
    Html::lit("&lt;internal error&gt;").into()
  });

  let log = vec![ LogEntry { html: hformat!(
    "{} {} {}",
    who_by,
    did,
    desc,
  )}];
  (log, Some(who_by))
}

#[throws(OE)]
pub fn log_did_to_piece(ioccults: &IOccults, goccults: &GameOccults,
                        by_gpl: &GPlayer,
                        gpc: &GPiece, ipc: &IPiece, did: &str)
                        -> Vec<LogEntry> {
  log_did_to_piece_whoby(ioccults,goccults,by_gpl,gpc,ipc,did)?.0
}

// ---------- prepared updates, queued in memory ----------

pub struct PlayerUpdatesBuildContext {
  pub(self) u1: Arc<PreparedUpdate>,
}

impl PlayerUpdatesBuildContext {
  pub fn new(&self) -> PlayerUpdates {
    let mut log = StableIndexVecDeque::with_capacity(50);
    log.push_back(self.u1.clone());
    let cv = default();
    PlayerUpdates { log, cv }
  }
}

impl PlayerUpdates {
  pub fn new_begin(gs: &GameState) -> PlayerUpdatesBuildContext {
    let u1 = Arc::new(PreparedUpdate {
      gen: gs.gen,
      when: Instant::now(),
      us: vec![],
    });
    PlayerUpdatesBuildContext { u1 }
  }

  pub fn push<U: Into<Arc<PreparedUpdate>>>(&mut self, update: U) {
    self.log.push_back(update.into());
    self.cv.notify_all();
  }
  // giving out only immutable references means no-one can
  // forget to cv.notify
  pub fn read_log(&self) -> &PlayerUpdatesLog { &self.log }
  pub fn get_cv(&self) -> Arc<Condvar> { self.cv.clone() }

  pub fn expire_upto(&mut self, before: Instant) {
    loop {
      if self.log.len() < 2 { break }

      let front = {
        if let Some(front) = self.log.front() { front }
        else { break }
      };
      if front.when >= before { break }

      trace!("update expiring #{}", self.log.front_index());
      self.log.pop_front();
    }
  }
}

impl PreparedUpdate {
  pub fn json_len(&self, player: PlayerId) -> usize {
    self.us.iter().map(|u| 20 + u.json_len(player)).sum()
  }
}

impl PreparedUpdateEntry_Piece {
  pub fn json_len(&self, player: PlayerId) -> usize {
    50 + self.ops.json_len(player)
  }
}

impl<U:JsonLen> JsonLen for PreparedPieceUpdateGeneral<U> {
  fn json_len(&self) -> usize { self.op.json_len() }
}

impl<NS:JsonLen, ZL> JsonLen for PieceUpdateOp<NS, ZL> {
  fn json_len(&self) -> usize {
    self.new_state().map(|x| x.json_len()).unwrap_or(0)
  }
}

impl PreparedUpdateEntry_Image {
  fn json_len(&self, player: PlayerId) -> usize {
    self.ims.json_len(player)
  }
}

impl JsonLen for PreparedPieceState {
  fn json_len(&self) -> usize { self.svg.json_len() + self.uos.json_len() }
}

impl JsonLen for PreparedPieceImage {
  fn json_len(&self) -> usize { self.svg.json_len() + self.uos.json_len() }
}
impl<T:JsonLen> JsonLen for Vec<T> {
  fn json_len(&self) -> usize { self.iter().map(|x| x.json_len() + 10).sum() }
}
impl JsonLen for UoDescription {
  fn json_len(&self) -> usize { self.desc.json_len() + 50 }
}

impl PreparedUpdateEntry {
  pub fn json_len(&self, player: PlayerId) -> usize {
    use PreparedUpdateEntry::*;
    match self {
      Piece(op) => {
        op.json_len(player)
      }
      Image(ims) => {
        ims.json_len(player)
      }
      MoveHistEnt(ents) => {
        match ents.get(player) { None => 0, Some(_) => 100 }
      }
      Log(logent) => {
        logent.logent.html.json_len() * 28
      }
      AddPlayer {
        player:_,
        data: DataLoadPlayer { dasharray, nick, },
        new_info_pane,
      } => {
        dasharray.json_len() +
        nick     .json_len() + 100
          + new_info_pane.json_len()
      }
      RemovePlayer { player:_, new_info_pane } => {
        new_info_pane.json_len() + 100
      }
      SetTableColour(colour) => {
        colour.json_len() + 50
      }
      SetLinks(links) => {
        links.iter().filter_map(
          |(_k,v)| Some(50 + v.as_ref()?.len())
        ).sum::<usize>() + 50
      }
      Error(ESVU::PieceOpError { state, .. }) => {
        100 + state.json_len(player)
      }
      Error(ESVU::InternalError) |
      Error(ESVU::PlayerRemoved) |
      Error(ESVU::TokenRevoked) |
      SetTableSize(_) => {
        100
      }
    }
  }
}

impl DataLoadPlayer {
  pub fn from_player(ig: &Instance, player: PlayerId) -> Self {
    let gplayers = &ig.gs.players;
    let dasharray = player_dasharray(gplayers, player);
    let nick = gplayers.get(player).map(|gpl| gpl.nick.as_str())
      .unwrap_or("<unknown-player>")
      .to_html();
    DataLoadPlayer {
      dasharray,
      nick,
    }
  }
}

// ---------- PieceUpdatesOp ----------

impl<NS,ZC> PieceUpdateOp<NS,ZC> {
  pub fn new_state(&self) -> Option<&NS> {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(ns) => Some(ns),
      Modify(ns) => Some(ns),
      ModifyQuiet(ns) => Some(ns),
      Move(_) => None,
      SetZLevel(_) => None,
    }
  }
  pub fn try_map<NS2, ZC2, E:Error,
                 F: FnOnce(NS) -> Result<NS2,E>,
                 G: FnOnce(ZC) -> Result<ZC2,E>
                 > (self, f:F, g:G) -> Result<PieceUpdateOp<NS2,ZC2>,E>
  {
    use PieceUpdateOp::*;
    Ok(match self {
      Delete() => Delete(),
      Insert(ns) => Insert(f(ns)?),
      Modify(ns) => Modify(f(ns)?),
      ModifyQuiet(ns) => ModifyQuiet(f(ns)?),
      Move(pos) => Move(pos),
      SetZLevel(zl) => SetZLevel(g(zl)?),
    })
  }
  pub fn map_ref(&self) -> PieceUpdateOp<&NS,&ZC> {
    use PieceUpdateOp::*;
    match self {
      Delete() => Delete(),
      Insert(ns) => Insert(ns),
      Modify(ns) => Modify(ns),
      ModifyQuiet(ns) => ModifyQuiet(ns),
      Move(pos) => Move(*pos),
      SetZLevel(zl) => SetZLevel(zl),
    }
  }
  pub fn map<NS2,ZC2,
             F: FnOnce(NS) -> NS2,
             G: FnOnce(ZC) -> ZC2
             > (self, f:F, g:G) -> PieceUpdateOp<NS2,ZC2>
  {
    self.try_map(
      |ns| <Result<_,Void>>::Ok(f(ns)),
      |zc| <Result<_,Void>>::Ok(g(zc)),
    ).void_unwrap()
  }
  pub fn new_z_generation(&self) -> Option<Generation>
    where ZC: Borrow<ZLevel>
  {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(_) => None,
      Modify(_) => None,
      ModifyQuiet(_) => None,
      Move(_) => None,
      SetZLevel(l) => Some(l.borrow().zg),
    }
  }
}

pub struct PrepareUpdatesBuffer<'r> {
  g: &'r mut Instance,
  us: Vec<PreparedUpdateEntry>,
  by_client: IsResponseToClientOp,
  gen: Option<Generation>,
}

/// In PROTOCOL.md terms, None is a Server update
type IsResponseToClientOp = Option<(
  WhatResponseToClientOp,
  ClientId,
  ClientSequence
)>;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum WhatResponseToClientOp {
  /// In PROTOCOL.md terms, a Client update
  Predictable,
  /// In PROTOCOL.md terms, a Client update which also updates
  /// the visible piece image (which is just server-controlled).
  UpdateSvg,
  /// In PROTOCOL.md terms, a Client update which results in
  /// an immediate Server update.  When the client knows this
  /// is going to happen it can help the user avoid conflicts.
  Unpredictable,
}

impl<'r> PrepareUpdatesBuffer<'r> {
  pub fn new(g: &'r mut Instance,
             by_client: IsResponseToClientOp,
             estimate: Option<usize>) -> Self
  {
    let us = estimate.map_or(vec![], Vec::with_capacity);

    PrepareUpdatesBuffer {
      gen: None,
      by_client, us, g,
    }
  }

  #[throws(IE)]
  pub fn spontaneous_image(g: &'r mut Instance,
                           piece: PieceId,
                           estimate: Option<usize>)
  {
    let mut updates = PrepareUpdatesBuffer::new(g, None, estimate);
    updates.piece_update_image(piece)?;
    updates.finish();
  }

  pub fn gen(&mut self) -> Generation {
    let gs = &mut self.g.gs;
    *self.gen.get_or_insert_with(||{
      gs.gen.increment();
      gs.gen
    })
  }

  pub fn piece_report_error(ig: &mut Instance,
                            error: PieceOpError, piece: PieceId,
                            logents: Vec<LogEntry>,
                            partially: PieceOpErrorPartiallyProcessed,
                            client: ClientId, cseq: ClientSequence)
                            -> Result<(),OE> {
    let by_client = (WRC::Unpredictable, client, cseq);
    let mut buf = PrepareUpdatesBuffer::new(ig, Some(by_client), None);
    let ops = PUOs::Simple(PieceUpdateOp::Modify(()));
    let state = buf.piece_update_fallible(
      piece, ops, |pc, gen, _by_client| {
        match partially {
          POEPP::Unprocessed => { }
          POEPP::Partially => { pc.gen = gen; pc.lastclient = default(); }
        }
      }
    )?;
    let update = PUE::Error(ESVU::PieceOpError {
      error, state, partially
    });
    buf.us.push(update);
    buf.log_updates(logents);
    Ok(())
  }

  #[throws(InternalError)]
  fn piece_update_player(ioccults: &IOccults,
                         gs: &GameState,
                         gpc: &GPiece,
                         ipc: &IPiece,
                         op: PieceUpdateOp<(),()>,
                         pri: &Option<PieceRenderInstructions>)
                         -> Option<PreparedPieceUpdate>
  {
    let pri = match pri { Some(pri) => pri, None => return None };


    let op = pri.map_piece_update_op(ioccults, gs, gpc, ipc, op)?;
    op.map(|op| PreparedPieceUpdate {
      piece: pri.vpid,
      op,
    })
  }


  #[throws(InternalError)]
  fn piece_update_fallible_players<U,GUF,WMZ,MUF,MPF>
    (&mut self, piece: PieceId, gen_update: GUF,
     mut with_max_z: WMZ,
     mut mk_update: MUF,
     mut missing: MPF,
    )
     -> SecondarySlotMap<PlayerId, U>
  where
    GUF: FnOnce(&mut GPiece, Generation, &IsResponseToClientOp),
    WMZ: FnMut(&mut ZCoord, &GPiece),
    MUF: FnMut(&IOccults, &GameState, &GPiece, &IPiece,
               PlayerId, &Option<PieceRenderInstructions>)
               -> Result<Option<U>,IE>,
    MPF: FnMut(&mut GPlayer) -> Option<U>,
  {
    let gen = self.gen();
    let gs = &mut self.g.gs;
    let ioccults = &self.g.ioccults;

    let mut gpc = gs.pieces.byid_mut(piece).ok();
    let ipc = self.g.ipieces.get(piece);

    if let Some(ref mut gpc) = gpc {
      gen_update(gpc, gen, &self.by_client);
    }
    let gpc = gs.pieces.byid(piece).ok();

    let mut out: SecondarySlotMap<PlayerId, U> = default();
    for player in self.g.iplayers.keys() {
      // Iterate via iplayers because we want to borrow each gpl
      // mutably and also gs immutably, at different times.  The naive
      // approach fails because the iterator must borrow the whole
      // thing, and mutably to produce mut references, so ties everything
      // in a knot.
      let gpl = match gs.players.get_mut(player) { Some(v)=>v, _=>continue };
      let upd = match (gpc, ipc) {
        (Some(gpc), Some(ipc)) => {
          let pri = piece_pri(ioccults, &gs.occults, player,
                              gpl, piece, gpc, ipc);
          drop(gpl);
          with_max_z(&mut gs.max_z, gpc);
          mk_update(ioccults,gs,gpc,ipc,player,&pri)?
        },
        _ => {
          missing(gpl)
        },
      };

      if let Some(upd) = upd {
        out.insert(player, upd);
      }
    }
    out
  }

  #[throws(InternalError)]
  fn piece_update_fallible<GUF>(&mut self, piece: PieceId,
                                ops: PieceUpdateOps,
                                gen_update: GUF)
                                -> PreparedUpdateEntry_Piece
    where GUF: FnOnce(&mut GPiece, Generation, &IsResponseToClientOp)
  {
    let ops = self.piece_update_fallible_players
      ::<PreparedPieceUpdate,_,_,_,_>
    (
      piece, gen_update,

      |max_z, gpc| max_z.update_max(&gpc.zlevel.z),

      |ioccults,gs,gpc,ipc,player,pri| {
        let ops = match ops {
          PUOs::Simple(update) => update,
          PUOs::PerPlayer(ref ops) => match ops.get(player) {
            Some(op) => *op,
            None => return Ok(None),
          }
        };
        let u = Self::piece_update_player(ioccults,gs,gpc,ipc,ops,&pri)?;
        Ok::<_,IE>(u)
      },

      |gpl| {
        gpl.idmap.fwd(piece).map(
          |vpid| PreparedPieceUpdate {
            // The piece is deleted, so we can't leak anything.
            piece: vpid,
            op: PieceUpdateOp::Delete(),
          }
        )
      }
    )?;

    PreparedUpdateEntry_Piece {
      by_client: self.by_client,
      ops
    }
  }

  pub fn piece_update(&mut self, piece: PieceId, ops: PieceUpdateOps) {
    // Caller needs us to be infallible since it is too late by
    // this point to back out a game state change.

    let update = self.piece_update_fallible(
      piece, ops,
      |pc, gen, by_client|
    {
      match *by_client {
        Some((WRC::Predictable,tclient,_)) |
        Some((WRC::UpdateSvg,  tclient,_)) => {
          if tclient != pc.lastclient {
            pc.gen_before_lastclient = pc.gen;
            pc.lastclient = tclient;
          }
        }
        Some((WRC::Unpredictable, _,_)) |
          None => {
            pc.lastclient = default();
          }
      }
      pc.gen = gen;
    })
      .map(|update| PUE::Piece(update))
      .unwrap_or_else(|e| {
        error!("piece update error! piece={:?} error={:?}",
               piece, &e);
        PreparedUpdateEntry::Error(ErrorSignaledViaUpdate::InternalError)
      });

    let movehist_update = movehist::peek_prep_update(&mut self.g.gs, &update);

    self.us.push(update);
    self.us.extend(movehist_update);
  }

  #[throws(InternalError)]
  pub fn piece_update_image(&mut self, piece: PieceId) {
    // Use this only for updates which do not change the set of valid UOs
    // or other operations or move the piece etc.
    let ims = self.piece_update_fallible_players(
      piece, |_,_,_|(), |_,_|(),

      |ioccults,gs,gpc,ipc,_player,pri| {
        let im = pri.as_ref().map(|pri| {
          let im = pri.prep_pieceimage(ioccults,gs,gpc,ipc)?;
          Ok::<_,IE>(PreparedPieceUpdateGeneral { piece: pri.vpid, op: im })
        }).transpose()?;
        Ok::<_,IE>(im)
      },

      |_gpl| None,
    )?;
    self.us.push(PUE::Image(PreparedUpdateEntry_Image { ims }))
  }

  pub fn piece_updates(&mut self, updates: Vec<(PieceId, PieceUpdateOps)>) {
    for (piece, ops) in updates {
      self.piece_update(piece, ops);
    }
  }

  pub fn raw_updates(&mut self, mut raw: Vec<PreparedUpdateEntry>) {
    self.us.append(&mut raw)
  }

  pub fn log_updates(&mut self, logents: Vec<LogEntry>) {
    let now = Timestamp::now();
    for logent in logents {
      let when = iter::once(now).chain(
        self.g.gs.log.back().map(|l| l.1.when)
      ).max().unwrap();
      let logent = Arc::new(CommittedLogEntry { when, logent });
      let gen = self.gen();
      self.g.gs.log.push_back((gen, logent.clone()));
      self.us.push(PreparedUpdateEntry::Log(logent));
    }
  }

  pub fn finish(self) { }
}

impl<'r> Drop for PrepareUpdatesBuffer<'r> {
  fn drop(&mut self) {
    if ! (self.us.is_empty() && self.gen.is_none()) {
      let gen = self.gen();
      let update = PreparedUpdate {
        when: Instant::now(),
        gen,
        us: mem::take(&mut self.us),
      };
      let update = Arc::new(update);
      trace!("PrepareUpdatesBuffer update {:?}", &update);

      for (_tplayer, PlayerRecord { u: tplupdates, .. })
        in &mut self.g.iplayers
      {
        tplupdates.push(update.clone());
      }
    }
  }
}

// ---------- for traansmission ----------

impl PreparedUpdate {
  pub fn for_transmit<'u>(&'u self, tz: &'u Timezone,
                          player: PlayerId, dest: ClientId)
                          -> TransmitUpdate<'u> {
    type ESVU<T> = ErrorSignaledViaUpdate<T>;
    type PUE = PreparedUpdateEntry;
    type TUE<'u> = TransmitUpdateEntry<'u>;
    let mut ents = vec![];

    fn pue_piece_to_tue_p(pue_p: &PUE_P, player: PlayerId)
                          -> Option<TUE_P> {
      let op = pue_p.ops.get(player)?;
      let PreparedPieceUpdate { piece, ref op } = *op;
      Some(TUE_P { piece, op: op.map_ref() })
    }

    fn pue_image_to_tue_i(pue_i: &PUE_I, player: PlayerId) -> Option<TUE_I> {
      let im = pue_i.ims.get(player)?;
      let PreparedPieceUpdateGeneral { piece, ref op } = *im;
      Some(TUE_I { piece, im: op })
    }

    fn pue_piece_to_tue(pue_p: &PUE_P, player: PlayerId, dest: ClientId)
                        -> Option<TUE> {
      let PUE_P { by_client, ref ops } = *pue_p;
      let PreparedPieceUpdate { piece, ref op } = *ops.get(player)?;
      let ns = ||op.new_state();
      enum FTG<'u> {
        Recorded(ClientSequence, Option<&'u PreparedPieceState>),
        Exactly(TransmitUpdateEntry<'u>),
        Piece,
      }
      let ftg = match by_client {
        None                                     => FTG::Piece,
        Some((_,u_client,_)) if u_client != dest => FTG::Piece,
        Some((WRC::Predictable  ,_,cseq)) => FTG::Recorded(cseq, None),
        Some((WRC::UpdateSvg    ,_,cseq)) => FTG::Recorded(cseq, ns()),
        Some((WRC::Unpredictable,_,cseq)) => {
          if let Some(ns) = ns() {
            FTG::Exactly(TUE::RecordedUnpredictable { piece, cseq, ns })
          } else {
            error!("internal error: for_transmit PreparedUpdateEntry::Piece with RecordedUnpredictable but PieceOp no NS");
            FTG::Piece
          }
        }
      };
      let tue = match ftg {
        FTG::Recorded(cseq, ns) => {
          let zg = op.new_z_generation();
          TUE::Recorded { piece, cseq, zg, svg: ns.map(|ns| &ns.svg) }
        },
        FTG::Piece => TUE::Piece(pue_piece_to_tue_p(&pue_p, player)?),
        FTG::Exactly(x) => x,
      };
      Some(tue)
    }

    for u in &self.us {
      trace!("for_transmit to={:?} {:?}", dest, &u);
      let ue = match u {
        &PUE::Piece(ref pue_p) => {
          match pue_piece_to_tue(pue_p, player,dest) {
            Some(tue) => tue,
            _ => continue,
          }
        }
        &PUE::Image(ref pue_i) => {
          match pue_image_to_tue_i(pue_i, player) {
            Some(tue) => TUE::Image(tue),
            _ => continue,
          }
        }
        PUE::MoveHistEnt(ents) => {
          match ents.get(player) {
            Some(mhe) => TUE::MoveHistEnt(mhe),
            _ => continue,
          }
        }
        PUE::Log(logent) => {
          TUE::Log((&tz, &logent))
        }
        &PUE::SetTableSize(size) => {
          TUE::SetTableSize(size)
        }
        PUE::SetTableColour(colour) => {
          TUE::SetTableColour(colour)
        }
        &PUE::AddPlayer { player, ref new_info_pane, ref data } => {
          TUE::AddPlayer { player, new_info_pane, data }
        }
        &PUE::RemovePlayer { player, ref new_info_pane } => {
          TUE::RemovePlayer { player, new_info_pane }
        }
        PUE::Error(e) => {
          match *e {
            ESVU::InternalError => TUE::Error(ESVU::InternalError),
            ESVU::PlayerRemoved => TUE::Error(ESVU::PlayerRemoved),
            ESVU::TokenRevoked  => TUE::Error(ESVU::TokenRevoked),
            ESVU::PieceOpError { error, partially, ref state } => {
              let c = state.by_client.as_ref().map(|(_,c,_)| *c);
              if c == None || c == Some(dest) {
                let state = match pue_piece_to_tue_p(state, player) {
                  Some(tue) => tue,
                  None => continue,
                };
                TUE::Error(
                  ESVU::PieceOpError { error, partially, state }
                )
              } else {
                match partially {
                  POEPP::Unprocessed => continue,
                  POEPP::Partially => {
                    match pue_piece_to_tue(&state, player, dest) {
                      Some(tue) => tue,
                      None => continue,
                    }
                  }
                }
              }
            }
          }
        }
        PUE::SetLinks(links) => {
          TUE::SetLinks((&**links).into())
        }
      };
      ents.push(ue);
    };
    TransmitUpdate(self.gen, ents)
  }
}

impl<'u> Into<FormattedLogEntry<'u>> for TransmitUpdateLogEntry<'u> {
  fn into(self) -> FormattedLogEntry<'u> {
    let (tz, logent) = self;
    let when = logent.when.render(&tz);
    FormattedLogEntry { when, logent: &logent.logent }
  }
}

fn serialize_logentry<S:Serializer>(&(tz,logent): &TransmitUpdateLogEntry,
                                    s:S) -> Result<S::Ok, S::Error> {
  let f: FormattedLogEntry = (tz, logent).into();
  f.serialize(s)
}
