// Copyright 2020 Ian Jackson
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
  read : io::Lines<BufReader<Box<dyn Read>>>,
  write : BufWriter<Box<dyn Write>>,
}

impl MgmtChannel {
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
}

pub trait IoTryClone : Sized {
  fn try_clone(&self) -> io::Result<Self>;
}

impl IoTryClone for UnixStream {
  fn try_clone(&self) -> io::Result<UnixStream> { self.try_clone() }
}
