pub trait Piece {
  fn svg(&self, pr : &PiecedRecord) -> SvgData;
}

#[derive(Debug)]
pub struct PieceRecord {
  pos : Pos,
  p : Box<dyn Piece>,
  held : Option<PlayerRef>,
}


#[derive(Debug)]
pub struct GameState {
  pub pieces : Vec<PieceRecord>,
  pub players : Vec<PlayerRecord>,
}

type MsgPiece = SvgData;

pub struct GameRef (InstanceGuard);
impl Deref for GameRef {
  type Output = GamState;
  fn deref(&self) -> &GameState { self.0.read() }
}


#[derive(Serialize)]
enum MsgUpdate {
  MsgNoUpdate,
  MsgPieceInsert(usize, MsgPiece),
  MsgPieceDelete(usize),
  MsgPieceUpdate(usize, MsgPiece),
}

impl PieceRecord {
  fn msg(&self) -> MsgPiece { self.p.svg(self) }
}

impl GameRef {
  fn piece_insert(&mut self, i : usize, p : PieceRecord) {
    self.0.update(|d| {
      d.pieces.insert(i, p);
      MsgPieceInsert(i, p.msg())
    );
  }
  fn piece_delete(&mut self, i : usize) {
    self.0.update(|d| {
      d.pieces.remove(i, p);
      MsgPieceDelete(i)
    }
  }
  fn piece_update(&mut self, i : usize, p : PieceRecord) {
    self.0.update(|d| {
      d.pieces[i] = p,
      MsgPieceUpdate(i, p.msg()),
    }
  }
}
