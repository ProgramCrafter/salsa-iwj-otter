// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// update messages from server to client

use crate::imports::*;

type PUE = PreparedUpdateEntry;
type ESVU<POEPU> = ErrorSignaledViaUpdate<POEPU>;

#[allow(non_camel_case_types)] type PUE_P = PreparedUpdateEntry_Piece;
#[allow(non_camel_case_types)] type TUE_P<'u> = TransmitUpdateEntry_Piece<'u>;

// ---------- newtypes, type aliases, basic definitions ----------

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);

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
  pub pinned: bool,
  pub uos: Vec<UoDescription>,
}

#[derive(Serialize,Debug)]
pub struct DataLoadPlayer {
  dasharray: String,
}

// ---------- piece updates ----------

#[derive(Debug,Clone,Serialize)]
pub enum PieceUpdateOp<NS,ZL> {
  Delete(),
  Insert(NS),
  Modify(NS),
  ModifyQuiet(NS),
  Move(Pos),
  SetZLevel(ZL),
}

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
}

impl From<PieceUpdateOp<(),()>> for PieceUpdateOps {
  fn from(op: PieceUpdateOp<(),()>) -> Self { PUO::Simple(op) }
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

// ---------- prepared updates, queued in memory ----------

pub struct PlayerUpdatesBuildContext {
  pub(self) u1: Arc<PreparedUpdate>,
}

impl PlayerUpdatesBuildContext {
  pub fn new(&self) -> PlayerUpdates {
    let mut log = StableIndexVecDeque::with_capacity(50);
    log.push_back(self.u1.clone());
    let cv = Default::default();
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
  pub fn json_len(&self, _player: PlayerId) -> usize {
    let PUE_P { ref op, .. } = self;
    50 +
      op.new_state().map(|x| x.svg.0.as_bytes().len()).unwrap_or(0)
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
        dasharray.as_bytes().len() + 100
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
    let kd : slotmap::KeyData = player.into();
    let n = kd.get_idx_version().0;
    let n = if n != 0 { n.try_into().unwrap() }
    else { ig.gs.players.capacity() };
    let dasharray = player_dasharray(n.try_into().unwrap());
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
                            client: ClientId, cseq: ClientSequence,
                            lens: &dyn Lens) -> Result<(),OE> {
    let by_client = (WRC::Unpredictable, client, cseq);
    let mut buf = PrepareUpdatesBuffer::new(ig, Some(by_client), None);
    let ops = PUO::Simple(PieceUpdateOp::Modify(()));
    let state = buf.piece_update_fallible(
      piece, ops, lens, |pc, gen, _by_client| {
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
  fn piece_update_fallible<GUF>(&mut self, piece: PieceId,
                                ops: PieceUpdateOps,
                                lens: &dyn Lens,
                                gen_update: GUF) -> PreparedUpdateEntry_Piece
    where GUF: FnOnce(&mut PieceState, Generation, &IsResponseToClientOp)
  {
    let gen = self.gen();
    let gs = &mut self.g.gs;

    let PUO::Simple(update) = ops;

    let (update, piece) = match (
      gs.pieces.byid_mut(piece),
      self.g.ipieces.get(piece),
    ) {
      (Ok(pc), Some(p)) => {
        gs.max_z.update_max(&pc.zlevel.z);

        gen_update(pc, gen, &self.by_client);
        let pri_for_all = lens.svg_pri(piece,pc,Default::default());

        let update = update.try_map(
          |()|{
            let mut ns = pc.prep_piecestate(p.as_ref(), &pri_for_all)?;
            lens.massage_prep_piecestate(&mut ns);
            <Result<_,InternalError>>::Ok(ns)
          },
          |()|{
            Ok(pc.zlevel.clone())
          }
        )?;

        (update, pri_for_all.id)
      },
      _ => {
        (PieceUpdateOp::Delete(), lens.pieceid2visible(piece))
      }
    };

    PreparedUpdateEntry_Piece {
      piece,
      by_client : self.by_client,
      op : update,
    }
  }

  pub fn piece_update(&mut self, piece: PieceId, ops: PieceUpdateOps,
                      lens: &dyn Lens) {
    // Caller needs us to be infallible since it is too late by
    // this point to back out a game state change.

    let update = self.piece_update_fallible(
      piece, ops, lens,
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
        error!("piece update error! piece={:?} lens={:?} error={:?}",
               piece, &lens, &e);
        PreparedUpdateEntry::Error(ErrorSignaledViaUpdate::InternalError)
      });
    self.us.push(update);
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

type WRC = WhatResponseToClientOp;

impl PreparedUpdate {
  pub fn for_transmit<'u>(&'u self, tz: &'u Timezone, dest : ClientId)
                      -> TransmitUpdate<'u> {
    type ESVU<T> = ErrorSignaledViaUpdate<T>;
    type PUE = PreparedUpdateEntry;
    type TUE<'u> = TransmitUpdateEntry<'u>;
    let mut ents = vec![];

    fn pue_piece_to_tue_p(pue_p: &PUE_P) -> TUE_P {
      let PUE_P { piece, ref op, .. } = *pue_p;
      TUE_P { piece, op: op.map_ref() }
    }

    fn pue_piece_to_tue(pue_p: &PUE_P, dest: ClientId) -> TUE {
      let PUE_P { piece, by_client, ref op } = *pue_p;
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
      match ftg {
        FTG::Recorded(cseq, ns) => {
          let zg = op.new_z_generation();
          TUE::Recorded { piece, cseq, zg, svg: ns.map(|ns| &ns.svg) }
        },
        FTG::Piece => TUE::Piece(pue_piece_to_tue_p(&pue_p)),
        FTG::Exactly(x) => x,
      }
    }

    for u in &self.us {
      trace!("for_transmit to={:?} {:?}", dest, &u);
      let ue = match u {
        &PUE::Piece(ref pue_p) => {
          pue_piece_to_tue(pue_p, dest)
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
                let state = pue_piece_to_tue_p(state);
                TUE::Error(
                  ESVU::PieceOpError { error, partially, state }
                )
              } else {
                match partially {
                  POEPP::Unprocessed => continue,
                  POEPP::Partially => pue_piece_to_tue(&state, dest),
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
  let f : FormattedLogEntry = (tz, logent).into();
  f.serialize(s)
}
