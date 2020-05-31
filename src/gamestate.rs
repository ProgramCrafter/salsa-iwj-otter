pub trait Piece {
  fn svg(&self, pr : &PiecedRecord) -> SvgData;
}

#[derive(Debug)]
pub struct PieceRecord {
  pos : Pos,
  p : Rc<dyn Piece>,
  held : Option<PlayerRef>,
}


#[derive(Debug)]
pub struct GameState {
  pub pieces : Vec<Rc<PieceRecord>>,
  pub players : Vec<Rc<PlayerRecord>>,
}

pub struct GameRef (InstanceGuard);
impl Deref for GameRef {
  type Output = GamState;
  fn deref(&self) -> &GameState { self.0.read() }
}

enum GameUpdate {
  NoUpdate,
  PieceInsert(usize, PieceRecord),
  PieceDelete(usize, PieceRecord),
  PieceUpdate(usize, PieceRecord, PieceRecord),
}

struct LogMessage (HtmlString);

impl PieceRecord {
  fn msg(&self) -> MsgPiece { self.p.svg(self) }
}

impl GameRef {
  fn piece_insert(&mut self, i : usize,
                  p : Rc<PieceRecord>, msg : LogMessage) {
    self.0.action(|g| {
      g.pieces.insert(i, p);
      (msg, MsgPieceInsert(i, p))
    );
  fn piece_delete(&mut self, i : usize, msg : LogMessage) {
    self.0.action(|g| {
      let old = g.pieces.remove(i, p);
      (msg, MsgPieceDelete(i, old))
    }
  }
  fn piece_update(&mut self, i : usize, p : PieceRecord) {
    self.0.action(|g| {
      let new = p.clone();
      let old = replace(g.pieces[i], p);
      (msg, MsgPieceUpdate(i, old, new))
    }
  }
}
