
use crate::imports::*;

slotmap::new_key_type!{
  pub struct PieceId;
}

pub trait Piece : Send + Debug {
}

#[derive(Debug)]
pub struct PieceRecord {
  pos : Pos,
  p : Box<dyn Piece>,
  face : FaceId,
  held : Option<UserId>,
}

#[derive(Debug)]
pub struct GameState {
  pub pieces : DenseSlotMap<PieceId,PieceRecord>,
}

pub fn xxx_gamestate_init() -> GameState {
  let mut pieces = DenseSlotMap::with_key();
  for (pos, p) in xxx_make_pieces() {
    let pr = PieceRecord {
      pos, p,
      face : 0.into(),
      held : None,
    };
    pieces.insert(pr);
  }
  GameState { pieces }
}
