
use crate::imports::*;

#[derive(Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { }
}

#[derive(Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine { }
}
