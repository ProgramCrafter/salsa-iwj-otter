
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
//  pub svgs : Vec<String,String>;
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
