// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// update messages from server to client

use crate::prelude::*;

#[allow(non_camel_case_types)] type PUE_P = PreparedUpdateEntry_Piece;
#[allow(non_camel_case_types)] type TUE_P<'u> = TransmitUpdateEntry_Piece<'u>;

// ---------- newtypes, type aliases, basic definitions ----------

pub type RawClientSequence = u64;

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(RawClientSequence);

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

#[derive(Debug,Clone)]
pub struct PreparedPieceUpdate {
  piece: VisiblePieceId,
  op: PieceUpdateOp<PreparedPieceState, ZLevel>,
}

#[derive(Debug,Clone,Serialize)]
pub struct PreparedPieceState {
  pub pos: Pos,
  pub svg: Html,
  pub held: Option<PlayerId>,
  pub z: ZCoord,
  pub zg: Generation,
  pub angle: CompassAngle,
  pub pinned: bool,
  pub uos: Vec<UoDescription>,
}

#[derive(Serialize,Debug)]
pub struct DataLoadPlayer {
  dasharray: Html,
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

pub type UpdateFromOpComplex = (PieceUpdate, Vec<(PieceId, PieceUpdateOps)>);

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

#[derive(Debug,Serialize)]
struct FormattedLogEntry<'u> {
  when: String,
  logent: &'u LogEntry,
}

// ========== implementation ==========

// ---------- helpful utilities ----------

#[throws(OE)]
pub fn log_did_to_piece_whoby(ioccults: &IOccults, by_gpl: &mut GPlayer,
                              gpc: &GPiece, ipc: &IPiece, did: &str)
                              -> (Vec<LogEntry>, Option<Html>)
{
  let who_by = Html(htmlescape::encode_minimal(&by_gpl.nick));
  let y = gpc.fully_visible_to_everyone();
  let desc = (||{
    Ok::<_,IE>(match ipc.show_or_instead(ioccults, y)? {
      Left(y) => ipc.show(y).describe_html(gpc)?,
      Right(instead) => instead.describe_html()?,
    })
  })().unwrap_or_else(|e|{
    error!("failed to format during logging: {:?}", e);
    Html::lit("<internal error>")
  });

  let log = vec![ LogEntry { html: Html(format!(
    "{} {} {}",
    &who_by.0,
    did,
    desc.0,
  ))}];
  (log, Some(who_by))
}

#[throws(OE)]
pub fn log_did_to_piece(ioccults: &IOccults, gpl_by: &mut GPlayer,
                        gpc: &GPiece, ipc: &IPiece, did: &str)
                        -> Vec<LogEntry> {
  log_did_to_piece_whoby(ioccults,gpl_by,gpc,ipc,did)?.0
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
    let PUE_P { ref ops, .. } = self;
    if let Some(op) = ops.get(player) {
      50 +
        op.op.new_state().map(|x| x.svg.0.as_bytes().len()).unwrap_or(0)
    } else {
      50
    }
  }
}

impl PreparedUpdateEntry {
  pub fn json_len(&self, player: PlayerId) -> usize {
    use PreparedUpdateEntry::*;
    match self {
      Piece(op) => {
        op.json_len(player)
      }
      Log(logent) => {
        logent.logent.html.0.as_bytes().len() * 28
      }
      AddPlayer {
        player:_,
        data: DataLoadPlayer { dasharray },
        new_info_pane,
      } => {
        dasharray.0.as_bytes().len() + 100
          + new_info_pane.0.len()
      }
      RemovePlayer { player:_, new_info_pane } => {
        new_info_pane.0.len() + 100
      }
      SetTableColour(colour) => {
        colour.0.as_bytes().len() + 50
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
    let dasharray = player_dasharray(&ig.gs.players, player);
    DataLoadPlayer {
      dasharray,
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
                 // xxx should be a function for mapping pos
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
    #[derive(Error,Debug)]
    enum Never { }
    self.try_map(
      |ns| <Result<_,Never>>::Ok(f(ns)),
      |zc| <Result<_,Never>>::Ok(g(zc)),
    ).unwrap()
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
                         max_z: &mut ZCoord,
                         gpc: &mut GPiece,
                         ipc: &IPiece,
                         op: PieceUpdateOp<(),()>,
                         pri: &Option<PieceRenderInstructions>)
                         -> Option<PreparedPieceUpdate>
  {
    let pri = match pri { Some(pri) => pri, None => return None };

    max_z.update_max(&gpc.zlevel.z);

    let op = pri.map_piece_update_op(ioccults, gpc, ipc, op)?;
    op.map(|op| PreparedPieceUpdate {
      piece: pri.vpid,
      op,
    })
  }


  #[throws(InternalError)]
  fn piece_update_fallible<GUF>(&mut self, piece: PieceId,
                                ops: PieceUpdateOps,
                                gen_update: GUF)
                                -> PreparedUpdateEntry_Piece
    where GUF: FnOnce(&mut GPiece, Generation, &IsResponseToClientOp)
  {
    let gen = self.gen();
    let gs = &mut self.g.gs;
    let ioccults = &self.g.ioccults;

    let mut gpc = gs.pieces.byid_mut(piece).ok();
    let ipc = self.g.ipieces.get(piece);

    if let Some(ref mut gpc) = gpc {
      gen_update(gpc, gen, &self.by_client);
    }
    let mut out: SecondarySlotMap<PlayerId, PreparedPieceUpdate> = default();
    for (player, gpl) in &mut gs.players {
      let ops = match ops {
        PUOs::Simple(update) => update,
        PUOs::PerPlayer(ref ops) => match ops.get(player) {
          Some(op) => *op,
          None => continue,
        }
      };
      let op = match (&mut gpc, ipc) {
        (Some(gpc), Some(ipc)) => {
          let pri = piece_pri(ioccults, &gs.occults, player,
                              gpl, piece, *gpc, ipc);
          Self::piece_update_player(
            ioccults, &mut gs.max_z, gpc, ipc, ops, &pri
          )?
        }
        _ => gpl.idmap.fwd(piece).map(
          |vpid| PreparedPieceUpdate {
            // The piece is deleted, so we can't leak anything.
            piece: vpid,
            op: PieceUpdateOp::Delete(),
          }
        )
      };

      if let Some(op) = op {
        out.insert(player, op);
      }
    }

    PreparedUpdateEntry_Piece {
      by_client: self.by_client,
      ops: out,
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
    self.us.push(update);
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
