// update messages from server to client

use crate::imports::*;

// ---------- newtypes, type aliases, basic definitions ----------

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);

const RECENT_BUFFER : usize = 50;

// ---------- prepared updates, queued in memory ----------

#[derive(Debug)]
pub struct PlayerUpdates {
  pub log : StableIndexVecDeque<Arc<PreparedUpdate>,sse::UpdateId>,
  pub cv : Arc<Condvar>,
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
    op : PieceUpdateOp<VisiblePieceId,PreparedPieceState>,
  },
  Log (Arc<LogEntry>),
}

#[derive(Debug,Serialize)]
pub struct PreparedPieceState {
  pub piece : VisiblePieceId,
  pub pos : Pos,
  pub svg : String,
  pub held : Option<PlayerId>,
  pub z : ZCoord,
  pub zg : Generation,
}

// ---------- piece updates ----------

#[derive(Debug,Serialize)]
pub enum PieceUpdateOp<ID,NS> {
  Delete(ID),
  Insert(NS),
  Modify(NS),
  Move(ID,Pos),
  SetZLevel(ID,ZLevel),
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
    op : &'u PieceUpdateOp<VisiblePieceId, PreparedPieceState>,
  },
  Log (&'u LogEntry),
}

// ========== implementation ==========

// ---------- prepared updates, queued in memory ----------

impl Default for PlayerUpdates {
  fn default() -> PlayerUpdates { PlayerUpdates {
    log : StableIndexVecDeque::with_capacity(RECENT_BUFFER),
    cv : Default::default(),
  } }
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
        op.new_state().map(|x| x.svg.len()).unwrap_or(0)
      },
      Log(logent) => {
        logent.html.as_bytes().len() * 3
      }
    }
  }
}

// ---------- PieceUpdatesOp ----------

impl<ID,NS> PieceUpdateOp<ID,NS> {
  pub fn new_state(&self) -> Option<&NS> {
    use PieceUpdateOp::*;
    match self {
      Delete(_) => None,
      Insert(ns) => Some(ns),
      Modify(ns) => Some(ns),
      Move(..) => None,
      SetZLevel(..) => None,
    }
  }
  pub fn try_map<ID2,NS2, E:Error,
                 IDF: FnOnce(ID) -> Result<ID2,E>,
                 NSF: FnOnce(NS) -> Result<NS2,E>>
    (self, f:NSF, idf:IDF) -> Result<PieceUpdateOp<ID2,NS2>,E>
  {
    use PieceUpdateOp::*;
    Ok(match self {
      Delete(i) => Delete(idf(i)?),
      Insert(ns) => Insert(f(ns)?),
      Modify(ns) => Modify(f(ns)?),
      Move(i,pos) => Move(idf(i)?,pos),
      SetZLevel(i,zl) => SetZLevel(idf(i)?,zl),
    })
  }
  pub fn map<ID2,NS2,
             IDF: FnOnce(ID) -> ID2,
             NSF: FnOnce(NS) -> NS2>
    (self, nsf:NSF, idf:IDF) -> PieceUpdateOp<ID2,NS2>
  {
    #[derive(Error,Debug)] enum Never { }
    self.try_map(
      |ns| <Result<_,Never>>::Ok(nsf(ns)),
      |id| <Result<_,Never>>::Ok(idf(id)),
    ).unwrap()
  }
  pub fn new_z_generation(&self) -> Option<Generation> {
    use PieceUpdateOp::*;
    match self {
      Delete(_) => None,
      Insert(_) => None,
      Modify(_) => None,
      Move(..) => None,
      SetZLevel(_,ZLevel{zg,..}) => Some(*zg),
    }
  }
  pub fn pieceid<'ns>(&'ns self) -> ID where &'ns NS : Into<ID>, ID : Copy {
    use PieceUpdateOp::*;
    match self {
      Delete(i) => *i,
      Insert(ns) | Modify(ns) => ns.into(),
      Move(i,_) => *i,
      SetZLevel(i,_) => *i,
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
  pub fn new(g: &'r mut Instance, by_client: ClientId, cseq: ClientSequence,
             estimate: usize) -> Self
  {
    g.gs.gen.increment();
    PrepareUpdatesBuffer {
      us: Vec::with_capacity(estimate),
      gen: g.gs.gen,
      g, by_client, cseq,
    }
  }

  pub fn piece_update(&mut self, piece: PieceId, update: PieceUpdateOp<(),()>,
                      lens: &dyn Lens) {
    let gs = &mut self.g.gs;

    let update = match gs.pieces.byid_mut(piece) {
      Some(pc) => {
        if self.by_client != pc.lastclient {
          pc.gen_before_lastclient = pc.gen;
          pc.lastclient = self.by_client;
        }
        pc.gen = self.gen;
        eprintln!("PC GEN_LC={:?} LC={:?}", pc.gen, pc.lastclient);
      
        let pri_for_all = lens.svg_pri(piece,pc,Default::default());

        let update = update.try_map(
          |_|{
            let mut ns = pc.prep_piecestate(&pri_for_all)?;
            lens.massage_prep_piecestate(&mut ns);
            <Result<_,SVGProcessingError>>::Ok(ns)
          },
          |_|{
            <Result<_,SVGProcessingError>>::Ok(pri_for_all.id)
          },
        )?;

        update
      },
      None => {
        PieceUpdateOp::Delete(lens.make_piece_visible(piece))
      }
    };

    self.us.push(PreparedUpdateEntry::Piece {
      client : self.by_client,
      sameclient_cseq : self.cseq,
      op : update,
    });
  }

  pub fn log_updates(&mut self, logents: Vec<LogEntry>) {
    for logentry in logents {
      let logentry = Arc::new(logentry);
      self.g.gs.log.push((self.gen, logentry.clone()));
      self.us.push(PreparedUpdateEntry::Log(logentry));
    }
  }
}

impl<'r> Drop for PrepareUpdatesBuffer<'r> {
  fn drop(&mut self) {
    let update = PreparedUpdate { gen: self.gen, us: self.us.take(), };
    let update = Arc::new(update);
    eprintln!("UPDATE {:?}", &update);

    for (_tplayer, tplupdates) in &mut self.g.updates {
      tplupdates.log.push_back(update.clone());
      tplupdates.cv.notify_all();
    }
  }
}

// ---------- for traansmission ----------

impl PreparedUpdate {
  pub fn for_transmit(&self, dest : ClientId) -> TransmitUpdate {
    let mut ents = vec![];
    for u in &self.us {
      let ue = match u {
        &PreparedUpdateEntry::Piece
        { piece, client, sameclient_cseq : cseq, ref op }
        if client == dest => {
          let zg = op.new_z_generation();
          TransmitUpdateEntry::Recorded { piece, cseq, zg }
        },
        &PreparedUpdateEntry::Piece { piece, ref op, .. } => {
          TransmitUpdateEntry::Piece { piece, op }
        },
        PreparedUpdateEntry::Log(logent) => {
          TransmitUpdateEntry::Log(&logent)
        },
      };
      ents.push(ue);
    };
    TransmitUpdate(self.gen, ents)
  }
}
