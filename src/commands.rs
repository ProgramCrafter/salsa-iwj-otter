
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
enum MgmtError {
  ParseFailed(String),
}
display_as_debug!{MgmtError}
type ME = MgmtError;

  use MgmtCommand::*;
  use MgmtResponse::*;
  use MgmtError::*;

impl From<serde_json::Error> for MgmtError {
  fn from(je: serde_json::Error) -> ME {
    ParseFailed(format!("{}", &je))
  }
}

pub fn decode_and_process(s: &str) -> MgmtResponse {
  self::decode_process_inner(s)
    .unwrap_or_else(|e| MgmtResponse::Error(format!("{}", e)))
}

#[throws(ME)]
fn decode_process_inner(s: &str)-> MgmtResponse {
  let cmd : MgmtCommand = serde_json::from_str(s)?;
  execute(cmd)?
}

#[throws(ME)]
fn execute(cmd: MgmtCommand) -> MgmtResponse {
  match cmd {
    Noop { } => Fine { },
  }
}
