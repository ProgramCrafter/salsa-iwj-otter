
use crate::imports::*;

slotmap::new_key_type!{
  pub struct PieceId;
}

#[derive(Copy,Clone,Debug,Ord,PartialOrd,Eq,PartialEq)]
#[derive(Serialize,Deserialize)]
#[serde(transparent)]
pub struct Generation (pub u64);

impl Generation {
  pub fn increment(&mut self) { self.0 += 1 }
}

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
  pub gen_lastclient : Generation,
  pub gen_before_lastclient : Generation,
}

impl PieceRecord {
  pub fn make_defs(&self, pri : &PieceRenderInstructions) -> Vec<String> {
    let pr = self;
    let mut defs = vec![];
    defs.push(format!(r##"<g id="{}">{}</g>"##,
                      pri.id_piece(),
                      pr.p.svg_piece(&pri)));
    defs.push(format!(r##"<g id="{}" stroke="black" fill="none">{}</g>"##,
                      pri.id_select(),
                      pr.p.svg_select(&pri)));
    defs.push(pr.p.svg_x_defs(&pri));
    defs
  }

  pub fn mk_update(&self) -> PieceUpdate {
    PieceUpdate {
      pos        : self.pos,
      held       : self.held,
      // xxx want all piece's stuff in the same def
    }
  }
}

#[derive(Debug)]
pub struct GameState {
  pub pieces : DenseSlotMap<PieceId,PieceRecord>,
  pub players : DenseSlotMap<PlayerId,Player>,
  pub gen : Generation,
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
      lastclient : Default::default(),
      gen_lastclient : Generation(0),
      gen_before_lastclient : Generation(0),
    };
    pieces.insert(pr);
  }
  GameState { pieces, gen : Generation(1), players : Default::default(),  }
}
