
use crate::imports::*;

#[derive(Debug,Error)]
pub enum MgmtChannelReadError {
  Parse(String),
  IO(#[from] io::Error),
}
display_as_debug!{MgmtChannelError}

#[derive(Clone,Debug)]
pub struct MgmtChannel<U : Read + Write> {
  read : io::Lines<BufReader<U>>,
  write : BufWriter<U>,
}

impl<U: IoTryClone + Read + Write> MgmtChannel<U> {
  #[throws(AE)]
  fn new(conn: U) -> MgmtChannel<U> {
    let read = conn.try_clone().context("dup the command stream")?;
    let read = BufReader::new(read);
    let read = read.lines();
    let write = conn;
    let write = BufWriter::new(write);
    MgmtChannel { read, write }
  }

  #[throws(MgmthannelReadError)]
  fn read<T>(&mut self) -> Option<T> {
    let lq = self.read.next().map_err(MgmtChannelReadError::IO)?;
    let incoming : T = lq.map(
      |l| serde_lexpr::from_str(l)
    ).collect().map_err(|e| MgmtChannelReadError::Parse("{}", &e))?;
    incoming
  }
}

trait IoTryClone : Sized {
  fn try_clone(&self) -> io::Result<Self>;
}

impl IoTryClone for UnixStream {
  fn try_clone(&self) -> io::Result<UnixStream> { self.try_clone() }
}
