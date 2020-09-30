// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// update messages from server to client

use crate::imports::*;

// ---------- newtypes, type aliases, basic definitions ----------

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);

// ---------- from manamgenet operations ----------

#[derive(Debug)] // not Default
pub struct ExecuteGameChangeUpdates {
  pub pcs: Vec<(PieceId,PieceUpdateOp<()>)>,
  pub log: Vec<LogEntry>,
  pub raw: Option<Vec<PreparedUpdateEntry>>,
}

// ---------- prepared updates, queued in memory ----------

pub type PlayerUpdatesLog =
  StableIndexVecDeque<Arc<PreparedUpdate>,sse::UpdateId>;

#[derive(Debug)]
pub struct PlayerUpdates {
  log : PlayerUpdatesLog,
  cv : Arc<Condvar>,
}

#[derive(Debug)]
pub struct PreparedUpdate {
  pub gen : Generation,
  pub when: Instant,
  pub us : Vec<PreparedUpdateEntry>,
}

#[derive(Debug)]
pub enum PreparedUpdateEntry {
  Piece {
    by_client: IsResponseToClientOp,
    piece : VisiblePieceId,
    op : PieceUpdateOp<PreparedPieceState>,
  },
  SetTableSize(Pos),
  Log (Arc<LogEntry>),
  Error (Option<ClientId> /* none: all */, ErrorSignaledViaUpdate),
}

#[derive(Debug,Clone,Serialize)]
pub struct PreparedPieceState {
  pub pos : Pos,
  pub svg : Html,
  pub held : Option<PlayerId>,
  pub z : ZCoord,
  pub zg : Generation,
  pub uos: Vec<UoDescription>,
}

// ---------- piece updates ----------

#[derive(Debug,Serialize)]
pub enum PieceUpdateOp<NS> {
  Delete(),
  Insert(NS),
  Modify(NS),
  Move(Pos),
  SetZLevel(ZLevel),
}

pub type PieceUpdateFromOp = (WhatResponseToClientOp,
                              PieceUpdateOp<()>, Vec<LogEntry>);
pub type PieceUpdateResult = Result<PieceUpdateFromOp, ApiPieceOpError>;

// ---------- for traansmission ----------

#[derive(Debug,Serialize)]
pub struct TransmitUpdate<'u> (
  Generation,
  Vec<TransmitUpdateEntry<'u>>,
);

#[derive(Debug,Serialize)]
enum TransmitUpdateEntry<'u> {
  Recorded {
    piece : VisiblePieceId,
    cseq : ClientSequence,
    zg : Option<Generation>,
    svg: Option<&'u Html>, // IsResponseToClientOp::UpdateSvg
  },
  Piece {
    piece : VisiblePieceId,
    op : PieceUpdateOp<&'u PreparedPieceState>,
  },
  RecordedUnpredictable {
    piece : VisiblePieceId,
    cseq : ClientSequence,
    ns: &'u PreparedPieceState,
  },
  SetTableSize(Pos),
  Log (&'u LogEntry),
  Error(&'u ErrorSignaledViaUpdate),
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
      gen : gs.gen,
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
  pub fn json_len(&self) -> usize {
    self.us.iter().map(|u| 20 + u.json_len()).sum()
  }
}

impl PreparedUpdateEntry {
  pub fn json_len(&self) -> usize {
    use PreparedUpdateEntry::*;
    match self {
      Piece { ref op, .. } => {
        50 +
        op.new_state().map(|x| x.svg.0.as_bytes().len()).unwrap_or(0)
      },
      Log(logent) => {
        logent.html.0.as_bytes().len() * 3
      },
      SetTableSize(_) |
      Error(_,_) => {
        100
      },
    }
  }
}

// ---------- PieceUpdatesOp ----------

impl<NS> PieceUpdateOp<NS> {
  pub fn new_state(&self) -> Option<&NS> {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(ns) => Some(ns),
      Modify(ns) => Some(ns),
      Move(_) => None,
      SetZLevel(_) => None,
    }
  }
  pub fn try_map_new_state<NS2,E:Error, F: FnOnce(NS) -> Result<NS2,E>>
    (self, f:F) -> Result<PieceUpdateOp<NS2>,E>
  {
    use PieceUpdateOp::*;
    Ok(match self {
      Delete() => Delete(),
      Insert(ns) => Insert(f(ns)?),
      Modify(ns) => Modify(f(ns)?),
      Move(pos) => Move(pos),
      SetZLevel(zl) => SetZLevel(zl),
    })
  }
  pub fn map_ref_new_state(&self) -> PieceUpdateOp<&NS> {
    use PieceUpdateOp::*;
    match self {
      Delete() => Delete(),
      Insert(ns) => Insert(ns),
      Modify(ns) => Modify(ns),
      Move(pos) => Move(*pos),
      SetZLevel(zl) => SetZLevel(*zl),
    }
  }
  pub fn map_new_state<NS2,F: FnOnce(NS) -> NS2>(self, f:F)
                            -> PieceUpdateOp<NS2> {
    #[derive(Error,Debug)]
    enum Never { }
    self.try_map_new_state(|ns| <Result<_,Never>>::Ok(f(ns))).unwrap()
  }
  pub fn new_z_generation(&self) -> Option<Generation> {
    use PieceUpdateOp::*;
    match self {
      Delete() => None,
      Insert(_) => None,
      Modify(_) => None,
      Move(_) => None,
      SetZLevel(ZLevel{zg,..}) => Some(*zg),
    }
  }
}

pub struct PrepareUpdatesBuffer<'r> {
  g : &'r mut Instance,
  us : Vec<PreparedUpdateEntry>,
  by_client : IsResponseToClientOp,
  gen : Option<Generation>,
}

/// In PROTOCOL.md terms, None is a Server update
type IsResponseToClientOp = Option<(
  WhatResponseToClientOp,
  ClientId,
  ClientSequence,
)>;
#[derive(Debug,Copy,Clone,Serialize,Deserialize)]
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

  fn new_for_error(ig: &'r mut Instance) -> Self {
    Self::new(ig, None, Some(1))
  }
  pub fn piece_report_error(ig: &mut Instance,
                            error: PieceOpError, piece: PieceId,
                            logents: Vec<LogEntry>, client: ClientId,
                            lens: &dyn Lens) -> Result<(),OE> {
    let mut buf = PrepareUpdatesBuffer::new_for_error(ig);
    let update = buf.piece_update_fallible(
      piece, PieceUpdateOp::Modify(()), lens
    )?;
    let update = match update {
      PreparedUpdateEntry::Piece {
        piece,
        op : PieceUpdateOp::Modify(state),
        ..
      } => {
        PreparedUpdateEntry::Error(
          Some(client),
          ErrorSignaledViaUpdate::PieceOpError {
            piece, error, state,
          },
        )
      },
      _ => panic!(),
    };
    buf.us.push(update);
    buf.log_updates(logents);
    Ok(())
  }

  #[throws(InternalError)]
  fn piece_update_fallible(&mut self, piece: PieceId,
                           update: PieceUpdateOp<()>,
                           lens: &dyn Lens) -> PreparedUpdateEntry {
    type WRC = WhatResponseToClientOp;

    let gen = self.gen();
    let gs = &mut self.g.gs;

    let (update, piece) = match (
      gs.pieces.byid_mut(piece),
      self.g.pieces.get(piece),
    ) {
      (Ok(pc), Some(p)) => {
        gs.max_z.update_max(pc.zlevel.z);

        if let Some(new_lastclient) = match self.by_client {
          Some((WRC::Predictable,tclient,_)) |
          Some((WRC::UpdateSvg,  tclient,_))
            => if tclient == pc.lastclient { None } else { Some(tclient) }
          _ => Some(Default::default()),
        } {
          pc.gen_before_lastclient = pc.gen;
          pc.lastclient = new_lastclient;
        }
        pc.gen = gen;
        let pri_for_all = lens.svg_pri(piece,pc,Default::default());

        let update = update.try_map_new_state(
          |_|{
            let mut ns = pc.prep_piecestate(p.as_ref(), &pri_for_all)?;
            lens.massage_prep_piecestate(&mut ns);
            <Result<_,InternalError>>::Ok(ns)
          },
        )?;

        (update, pri_for_all.id)
      },
      _ => {
        (PieceUpdateOp::Delete(), lens.pieceid2visible(piece))
      }
    };

    PreparedUpdateEntry::Piece {
      piece,
      by_client : self.by_client,
      op : update,
    }
  }

  pub fn piece_update(&mut self, piece: PieceId, update: PieceUpdateOp<()>,
                      lens: &dyn Lens) {
    // Caller needs us to be infallible since it is too late by
    // this point to back out a game state change.

    let update = self.piece_update_fallible(piece, update, lens)
      .unwrap_or_else(|e| {
        error!("piece update error! piece={:?} lens={:?} error={:?}",
               piece, &lens, &e);
        PreparedUpdateEntry::Error(None,
                                   ErrorSignaledViaUpdate::InternalError)
      });
    self.us.push(update);
  }

  pub fn raw_updates(&mut self, mut raw: Vec<PreparedUpdateEntry>) {
    self.us.append(&mut raw)
  }

  pub fn log_updates(&mut self, logents: Vec<LogEntry>) {
    for logentry in logents {
      let logentry = Arc::new(logentry);
      let gen = self.gen();
      self.g.gs.log.push((gen, logentry.clone()));
      self.us.push(PreparedUpdateEntry::Log(logentry));
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

      for (_tplayer, tplupdates) in &mut self.g.updates {
        tplupdates.push(update.clone());
      }
    }
  }
}

// ---------- for traansmission ----------

type WRC = WhatResponseToClientOp;

impl PreparedUpdate {
  pub fn for_transmit(&self, dest : ClientId) -> TransmitUpdate {
    type ESVU = ErrorSignaledViaUpdate;
    type PUE = PreparedUpdateEntry;
    type TUE<'u> = TransmitUpdateEntry<'u>;
    let mut ents = vec![];
    for u in &self.us {
      trace!("for_transmit to={:?} {:?}", dest, &u);
      let ue = match u {
        &PUE::Piece { piece, by_client, ref op } => {
          let ns = ||op.new_state();
          enum FTG<'u> {
            Recorded(ClientSequence, Option<&'u PreparedPieceState>),
            Exactly(TransmitUpdateEntry<'u>),
            Piece,
          };
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
            FTG::Piece => TUE::Piece { piece, op: op.map_ref_new_state() },
            FTG::Exactly(x) => x,
          }
        },
        PUE::Log(logent) => {
          TUE::Log(&logent)
        },
        &PUE::SetTableSize(size) => {
          TUE::SetTableSize(size)
        },
        PUE::Error(c, e) => {
          if *c == None || *c == Some(dest) {
            TUE::Error(e)
          } else if let &ESVU::PieceOpError { piece, ref state, .. } = e {
            let op = PieceUpdateOp::Modify(state);
            TUE::Piece { piece, op }
          } else {
            continue
          }
        }
      };
      ents.push(ue);
    };
    TransmitUpdate(self.gen, ents)
  }
}
