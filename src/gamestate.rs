
use crate::imports::*;

slotmap::new_key_type!{
  pub struct PieceId;
}

#[derive(Copy,Clone,Serialize)]
#[serde(into="String")]
pub struct VisiblePieceId (pub u64);

#[derive(Debug)]
pub struct PieceRenderInstructions {
  pub id : VisiblePieceId,
  pub face : FaceId,
}

pub type VisiblePieceIdSvgIds = &'static [&'static str];

impl Display for VisiblePieceId {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "{}.{}", self.0 >> 32, self.0 & 0xffffffff)
  }
}
impl Debug for VisiblePieceId {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    <Self as Display>::fmt(self, f)
  }
}
impl From<VisiblePieceId> for String {
  fn from(p: VisiblePieceId) -> String { format!("{}",p) }
}

impl FromStr for VisiblePieceId {
  type Err = AE;
  fn from_str(s : &str) -> Result<VisiblePieceId,AE> {
    let e = || anyhow!("could not deserialise visibile piece id");
    let mut i = s.splitn(2,'.').map(|s| s.parse().map_err(|_| e()));
    let h : u32 = i.next().ok_or_else(e)??;
    let l : u32 = i.next().ok_or_else(e)??;
    Ok(VisiblePieceId(((h as u64) << 32) | (l as u64)))
  }
//fn from(_: T) -> Self { todo!() }`
}

struct VisiblePieceIdVisitor { }
impl<'de> serde::de::Visitor<'de> for VisiblePieceIdVisitor {
  type Value = VisiblePieceId;
  fn expecting(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f,"(visible) piece id")
  }
  fn visit_str<DE>(self, s : &str) -> Result<VisiblePieceId, DE>
    where DE: serde::de::Error,
  {
    s.parse().map_err(DE::custom)
  }
}

impl<'de> Deserialize<'de> for VisiblePieceId {
  fn deserialize<D>(d: D) -> Result<Self, <D as serde::de::Deserializer<'de>>::Error>
  where D: serde::de::Deserializer<'de> {
    d.deserialize_str(VisiblePieceIdVisitor{})
  }
}

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
  pub held : Option<UserId>,
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
