
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
  pub fn id_use(&self) -> String { format!("use{}", self.id) }
  pub fn id_piece(&self) -> String { format!("piece{}", self.id) }
  pub fn id_select(&self) -> String { format!("select{}", self.id) }
  pub fn id_x(&self, w : &str) -> String { format!("def.{}.{}", self.id, w) }
}

pub trait Piece : Send + Debug {
  fn svg_piece(&self, pri : &PieceRenderInstructions) -> String;
  fn svg_select(&self, pri : &PieceRenderInstructions) -> String;
  fn svg_x_ids(&self) -> VisiblePieceIdSvgIds;
  fn svg_x_defs(&self, pri : &PieceRenderInstructions) -> String;
  fn describe_html(&self, face : Option<FaceId>) -> String;
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

#[derive(Debug,Serialize)]
pub struct PreparedPieceState {
  pub pos : Pos,
  pub held : Option<PlayerId>,
  pub svg : String,
}

impl PieceRecord {
  pub fn make_defs(&self, pri : &PieceRenderInstructions) -> String {
    let pr = self;
    let mut defs = String::new();
    write!(defs, r##"<g id="{}">{}</g>"##,
           pri.id_piece(),
           pr.p.svg_piece(&pri)).unwrap();
    write!(defs, r##"<g id="{}" stroke="black" fill="none">{}</g>"##,
           pri.id_select(),
           pr.p.svg_select(&pri)).unwrap();
    write!(defs, "{}", pr.p.svg_x_defs(&pri)).unwrap();
    defs
  }

  pub fn prep_piecestate(&self, pri : &PieceRenderInstructions)
                         -> PreparedPieceState {
    PreparedPieceState {
      pos        : self.pos,
      held       : self.held,
      svg        : self.make_defs(pri),
      // xxx want all piece's stuff in the same def
    }
  }

  pub fn describe_html(&self, pri : &PieceRenderInstructions) -> String {
    // xxx want to be able to hide things
    self.p.describe_html(Some(pri.face))
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
