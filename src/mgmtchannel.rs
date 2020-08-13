
use crate::imports::*;

#[derive(Debug,Error)]
pub enum MgmtChannelReadError {
  EOF,
  Parse(String),
  IO(#[from] io::Error),
}
display_as_debug!{MgmtChannelReadError}

#[derive(Debug)]
pub struct MgmtChannel<U : Read + Write> {
  read : io::Lines<BufReader<U>>,
  write : BufWriter<U>,
}

impl<U: IoTryClone + Read + Write> MgmtChannel<U> {
  #[throws(AE)]
  pub fn new(conn: U) -> MgmtChannel<U> {
    let read = conn.try_clone().context("dup the command stream")?;
    let read = BufReader::new(read);
    let read = read.lines();
    let write = conn;
    let write = BufWriter::new(write);
    MgmtChannel { read, write }
  }

  #[throws(MgmtChannelReadError)]
  pub fn read<T:DeserializeOwned>(&mut self) -> T {
    use MgmtChannelReadError::*;
    let l = self.read.next().ok_or(EOF)??;
    let v = serde_lexpr::from_str(&l)
      .map_err(|e| Parse(format!("{}", &e)))?;
    v
  }

  #[throws(io::Error)]
  pub fn write<T:Serialize>(&mut self, val: &T) {
    serde_lexpr::to_writer(&mut self.write, val)?;
  }
}

trait IoTryClone : Sized {
  fn try_clone(&self) -> io::Result<Self>;
}

impl IoTryClone for UnixStream {
  fn try_clone(&self) -> io::Result<UnixStream> { self.try_clone() }
}
