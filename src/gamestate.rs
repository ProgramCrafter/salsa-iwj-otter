
pub trait Piece {
  type Msg : Serialize;
  fn msg(&self) -> Msg;
}

#[derive(Debug)]
pub struct PieceRecord {
  x : Coord,
  y : Coord,
  p : Box<dyn Piece>,
  held : Option<PlayerRef>,
}

const RECENT_BUFFER : usize = 10;

#[derive(Debug)]
pub struct GameState {
  gen : Counter,
  data : GameStateData,
  recent : VecDeque<MsgUpdate>,
  notify : Condvar,
}

#[derive(Debug)]
pub struct GameStateData {
  pub pieces : Vec<PieceRecord>,
  pub players : Vec<PlayerRecord>,
}

impl Deref for GameState {
  type Output = GamStateData;
  fn deref(&self) -> &GamStateData { &self.data }
}

impl GameState {
  fn as_ref(&self) -> (&usize, &GameStateData) { (&self.gen, &self.data) }
  fn gen(&self) -> usize { self.gen }

  fn<F> update(&mut self, f : F)
  where F : FnOnce(&mut GameStateData) -> MsgUpdate {
    let msg = f(&mut self.data),
    if let MsgNoUpdate = msg { return }
    self.gen += 1,
    if self.recent.len() >= RECENT_BUFFER { self.pop_front() }
    self.recent.push_back(msg);
    self.notify.notify_all();
  }
}

#[derive(Serialize)]
enum MsgUpdate {
  MsgNoUpdate,
  MsgPieceInsert(usize, MsgPiece),
  MsgPieceDelete(usize),
  MsgPieceUpdate(usize, MsgPiece),
}

struct MsgPiece {
  
}

impl PieceRecord {
  fn msg(&self) -> MsgPiece {
    
  }
}

impl GameState {
  fn piece_insert(&mut self, i : usize, p : PieceRecord) {
    self.update(|d| {
      d.pieces.insert(i, p);
      MsgPieceInsert(i, p.msg())
    );
  }
  fn piece_delete(&mut self, i : usize) {
    self.update(|d| {
      d.pieces.remove(i, p);
      MsgPieceDelete(i)
    }
  }
  fn piece_update(&mut self, i : usize, p : PieceRecord) {
    self.update(|d| {
      d.pieces[i] = p,
      MsgPieceUpdate(i, p.msg()),
    }
  }
}
