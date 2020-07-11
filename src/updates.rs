
// update messages from server to client

use crate::imports::*;

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);

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
  Log (Arc<LogEntry>),
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
impl PreparedUpdate {
  pub fn json_len(&self) -> usize {
    self.us.iter().map(|u| 20 + u.json_len()).sum()
  }
}

#[derive(Debug,Serialize)]
pub enum PieceUpdateOp<NS> {
  Delete(),
  Insert(NS),
  Modify(NS),
  Move(Pos),
  SetZLevel(ZLevel),
}
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
  pub fn map_new_state<NS2,F: FnOnce(NS) -> NS2>(self, f:F)
                            -> PieceUpdateOp<NS2> {
    use PieceUpdateOp::*;
    match self {
      Delete() => Delete(),
      Insert(ns) => Insert(f(ns)),
      Modify(ns) => Modify(f(ns)),
      Move(pos) => Move(pos),
      SetZLevel(zl) => SetZLevel(zl),
    }
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

#[derive(Debug)]
pub struct PlayerUpdates {
  pub log : StableIndexVecDeque<Arc<PreparedUpdate>,sse::UpdateId>,
  pub cv : Arc<Condvar>,
}

const RECENT_BUFFER : usize = 50;

impl Default for PlayerUpdates {
  fn default() -> PlayerUpdates { PlayerUpdates {
    log : StableIndexVecDeque::with_capacity(RECENT_BUFFER),
    cv : Default::default(),
  } }
}

#[derive(Debug,Serialize)]
pub struct PreparedPieceState {
  pub pos : Pos,
  pub svg : String,
  pub held : Option<PlayerId>,
  pub z : ZCoord,
  pub zg : Generation,
}
