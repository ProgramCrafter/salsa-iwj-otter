// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

#[derive(Debug,Error)]
pub enum MgmtChannelReadError {
  EOF,
  Parse(String),
  IO(#[from] io::Error),
}
display_as_debug!{MgmtChannelReadError}

pub struct MgmtChannel {
  read: io::Lines<BufReader<Box<dyn Read>>>,
  write: BufWriter<Box<dyn Write>>,
}

impl Debug for MgmtChannel{ 
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    f.write_str("MgmtChannel{...}")?
  }
}

impl MgmtChannel {
  #[throws(AE)]
  pub fn connect(socket_path: &str) -> MgmtChannel {
    let unix = UnixStream::connect(socket_path)
      .with_context(||socket_path.to_owned())
      .context("connect to server")?; 
    let chan = MgmtChannel::new(unix)?;
    chan
  }

  #[throws(AE)]
  pub fn new<U: IoTryClone + Read + Write + 'static>(conn: U) -> MgmtChannel {
    let read = conn.try_clone().context("dup the command stream")?;
    let read = Box::new(read) as Box<dyn Read>;
    let read = BufReader::new(read);
    let read = read.lines();
    let write = Box::new(conn) as Box<dyn Write>;
    let write = BufWriter::new(write);
    MgmtChannel { read, write }
  }

  #[throws(MgmtChannelReadError)]
  pub fn read<T:DeserializeOwned>(&mut self) -> T {
    use MgmtChannelReadError::*;
    let l = self.read.next().ok_or(EOF)??;
    let r = serde_json::from_str(&l);
    let v = r.map_err(|e| Parse(format!("{}", &e)))?;
    v
  }

  #[throws(io::Error)]
  pub fn write<T:Serialize>(&mut self, val: &T) {
    serde_json::to_writer(&mut self.write, val)?;
    write!(self.write, "\n")?;
    self.write.flush()?;
  }

  #[throws(AE)]
  pub fn cmd(&mut self, cmd: &MgmtCommand) -> MgmtResponse {
    use MgmtResponse::*;
    self.write(&cmd).context("send command")?;
    let resp = self.read().context("read response")?;
    match &resp {
      Fine | GamesList{..} | LibraryItems(_) => { },
      AlterGame { error: None, .. } => { },
      Error { error } => {
        Err(error.clone()).context(
          format!("got error response to: {:?}",&cmd)
        )?;
      },
      AlterGame { error: Some(error), ref responses } => {
        if let MgmtCommand::AlterGame { insns, .. } = &cmd {
          if responses.len() < insns.len() {
            Err(error.clone())
              .context("AlterGame insn failed")
              .with_context(|| format!(" {:?}", &insns[responses.len()]))?;
          }
        }
        Err(error.clone()).context(format!(
          "game alterations failed (maybe partially); response to: {:?}",
          &cmd
        ))?;
      }
    };
    resp
  }
}

pub trait IoTryClone: Sized {
  fn try_clone(&self) -> io::Result<Self>;
}

impl IoTryClone for UnixStream {
  fn try_clone(&self) -> io::Result<UnixStream> { self.try_clone() }
}
