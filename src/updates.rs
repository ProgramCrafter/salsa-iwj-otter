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
// xxx delete old updates at some point

#[derive(Debug)]
pub struct PlayerUpdates {
  log : PlayerUpdatesLog,
  cv : Arc<Condvar>,
}

#[derive(Debug)]
pub struct PreparedUpdate {
  pub gen : Generation,
  pub us : Vec<PreparedUpdateEntry>,
}

#[derive(Debug)]
pub enum PreparedUpdateEntry {
  Piece {
    client : ClientId,
    sameclient_cseq : ClientSequence,
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
  },
  Piece {
    piece : VisiblePieceId,
    op : PieceUpdateOp<&'u PreparedPieceState>,
  },
  SetTableSize(Pos),
  Log (&'u LogEntry),
  Error(&'u ErrorSignaledViaUpdate),
}

// ========== implementation ==========

// ---------- prepared updates, queued in memory ----------

impl Default for PlayerUpdates {
  fn default() -> PlayerUpdates { PlayerUpdates {
    log : StableIndexVecDeque::with_capacity(50),
    cv : Default::default(),
  } }
}

impl PlayerUpdates {
  pub fn push<U: Into<Arc<PreparedUpdate>>>(&mut self, update: U) {
    self.log.push_back(update.into());
    self.cv.notify_all();
  }
  // giving out only immutable references means no-one can
  // forget to cv.notify
  pub fn read_log(&self) -> &PlayerUpdatesLog { &self.log }
  pub fn get_cv(&self) -> Arc<Condvar> { self.cv.clone() }
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
  gen : Generation,
  by_client : ClientId,
  cseq : ClientSequence,
}

impl<'r> PrepareUpdatesBuffer<'r> {
  pub fn new(g: &'r mut Instance,
             by_client: Option<(ClientId, ClientSequence)>,
             estimate: Option<usize>) -> Self
  {
    let by_client = by_client.unwrap_or(
      (Default::default(), ClientSequence(0))
    );
    let us = estimate.map_or(vec![], Vec::with_capacity);

    g.gs.gen.increment();

    PrepareUpdatesBuffer {
      gen: g.gs.gen,
      by_client: by_client.0, cseq: by_client.1,
      us, g,
    }
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
    let gs = &mut self.g.gs;

    let (update, piece) = match gs.pieces.byid_mut(piece) {
      Ok(pc) => {
        gs.max_z.update_max(pc.zlevel.z);

        if self.by_client != pc.lastclient {
          pc.gen_before_lastclient = pc.gen;
          pc.lastclient = self.by_client;
        }
        pc.gen = self.gen;
        let pri_for_all = lens.svg_pri(piece,pc,Default::default());

        let update = update.try_map_new_state(
          |_|{
            let mut ns = pc.prep_piecestate(&pri_for_all)?;
            lens.massage_prep_piecestate(&mut ns);
            <Result<_,InternalError>>::Ok(ns)
          },
        )?;

        (update, pri_for_all.id)
      },
      Err(()) => {
        (PieceUpdateOp::Delete(), lens.pieceid2visible(piece))
      }
    };

    PreparedUpdateEntry::Piece {
      piece,
      client : self.by_client,
      sameclient_cseq : self.cseq,
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
      self.g.gs.log.push((self.gen, logentry.clone()));
      self.us.push(PreparedUpdateEntry::Log(logentry));
    }
  }

  pub fn finish(self) { }
}

impl<'r> Drop for PrepareUpdatesBuffer<'r> {
  fn drop(&mut self) {
    let update = PreparedUpdate {
      gen: self.gen,
      us: mem::take(&mut self.us),
    };
    let update = Arc::new(update);
    trace!("PrepareUpdatesBuffer update {:?}", &update);

    for (_tplayer, tplupdates) in &mut self.g.updates {
      tplupdates.push(update.clone());
    }
  }
}

// ---------- for traansmission ----------

impl PreparedUpdate {
  pub fn for_transmit(&self, dest : ClientId) -> TransmitUpdate {
    type ESVU = ErrorSignaledViaUpdate;
    type PUE = PreparedUpdateEntry;
    type TUE<'u> = TransmitUpdateEntry<'u>;
    let mut ents = vec![];
    for u in &self.us {
      trace!("for_transmit to={:?} {:?}", dest, &u);
      let ue = match u {
        &PUE::Piece { piece, client, sameclient_cseq : cseq, ref op }
        if client == dest => {
          let zg = op.new_z_generation();
          TUE::Recorded { piece, cseq, zg }
        },
        &PUE::Piece { piece, ref op, .. } => {
          TUE::Piece { piece, op: op.map_ref_new_state() }
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
