
use crate::imports::*;

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);

#[derive(Debug,Serialize)]
pub struct Update {
  pub gen : Generation,
  pub u : UpdatePayload,
}

#[derive(Debug,Serialize)]
pub struct PieceUpdate {
  pub pos : Pos,
  pub held : Option<PlayerId>,
  pub svgs : String,
}

#[derive(Debug,Serialize)]
pub enum UpdatePayload {
  NoUpdate,
  ClientSequence(PieceId, ClientSequence),
  PieceDelete(VisiblePieceId),
  PieceInsert(VisiblePieceId, PieceUpdate),
  PieceUpdate(VisiblePieceId, PieceUpdate),
  PieceMove(VisiblePieceId, Pos),
}
