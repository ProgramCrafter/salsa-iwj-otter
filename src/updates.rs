
use crate::imports::*;

#[derive(Debug,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(String);

#[derive(Debug,Serialize)]
pub struct Update {
  pub gen : Counter,
  pub u : UpdatePayload,
}

#[derive(Debug,Serialize)]
pub struct PieceUpdate {
  pub pos : Pos,
  pub held : Option<PlayerId>,
  pub svg_piece : String,
  pub svg_select : String,
  pub svg_x_ids : VisiblePieceIdSvgIds,
  pub svg_defs : String,
}

#[derive(Debug,Serialize)]
pub enum UpdatePayload {
  NoUpdate,
  ClientSequence(ClientSequence),
  PieceDelete(PieceId),
  PieceInsert(PieceId, PieceUpdate),
  PieceUpdate(PieceId, PieceUpdate),
  PieceMove(PieceId, Pos),
}
