
use crate::imports::*;

slotmap::new_key_type!{
  pub struct PieceId;
}

type Counter = u64;

visible_slotmap_key!{ VisiblePieceId('.') }

pub fn make_pieceid_visible(p : PieceId) -> VisiblePieceId {
  // xxx need to do censorship mapping here
  let kd : slotmap::KeyData = p.into();
  VisiblePieceId(kd)
}

pub fn decode_visible_pieceid(p : VisiblePieceId) -> PieceId {
  // xxx need to do censorship mapping here
  let kd : slotmap::KeyData = p.into();
  PieceId(kd)
}

#[derive(Debug)]
pub struct PieceRenderInstructions {
  pub id : VisiblePieceId,
  pub face : FaceId,
}

pub type VisiblePieceIdSvgIds = &'static [&'static str];

impl PieceRenderInstructions {
  pub fn id_piece(&self) -> String { format!("piece{}", self.id) }
  pub fn id_select(&self) -> String { format!("select{}", self.id) }
  pub fn id_x(&self, w : &str) -> String { format!("def.{}.{}", self.id, w) }
}

pub trait Piece : Send + Debug {
  fn svg_piece(&self, pri : &PieceRenderInstructions) -> String;
  fn svg_select(&self, pri : &PieceRenderInstructions) -> String;
  fn svg_x_ids(&self) -> VisiblePieceIdSvgIds;
  fn svg_x_defs(&self, pri : &PieceRenderInstructions) -> String;
}

#[derive(Debug)]
pub struct PieceRecord {
  pub pos : Pos,
  pub p : Box<dyn Piece>,
  pub face : FaceId,
  pub held : Option<PlayerId>,
  pub lastclient : ClientId,
  pub gen_lastclient : Counter,
  pub gen_before_lastclient : Counter,
}

#[derive(Debug)]
pub struct GameState {
  pub pieces : DenseSlotMap<PieceId,PieceRecord>,
  pub players : DenseSlotMap<PlayerId,Player>,
  pub gen : Counter,
}

#[derive(Debug)]
pub struct Player {
  pub nick : String,
}

pub fn xxx_gamestate_init() -> GameState {
  let mut pieces = DenseSlotMap::with_key();
  for (pos, p) in xxx_make_pieces() {
    let pr = PieceRecord {
      pos, p,
      face : 0.into(),
      held : None,
      gen : 0,
    };
    pieces.insert(pr);
  }
  GameState { pieces, gen : 1, players : Default::default(),  }
}
