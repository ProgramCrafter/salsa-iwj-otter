
use crate::imports::*;

#[derive(Debug,Copy,Clone,Eq,PartialEq,Deserialize,Serialize)]
#[serde(transparent)]
pub struct ClientSequence(u64);
/*
#[derive(Debug,Serialize)]
pub struct Update {
  pub gen : Generation,
  pub u : UpdatePayload,
}

#[derive(Debug,Serialize)]
pub enum PieceUpdate {
}
*/
