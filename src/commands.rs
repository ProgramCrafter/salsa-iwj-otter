
use crate::imports::*;

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtCommand {
  Noop { }
}

#[derive(Debug,Serialize,Deserialize)]
pub enum MgmtResponse {
  Fine { },
  Error(String),
}

#[derive(Debug,Error)]
pub enum MgmtError {
  ParseFailed(String),
}
display_as_debug!{MgmtError}

