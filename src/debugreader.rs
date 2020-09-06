
use crate::imports::*;

#[derive(Debug)]
pub struct DebugReader<T : Read>(pub T, pub ClientId);

impl<T : Read> Read for DebugReader<T> {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize,io::Error> {
    let l = buf.len();
    trace!("{} read({})...", &self.1, l);
    let r = self.0.read(buf);
    debug!("{} read({}) = {:?} {:?}", &self.1, l, &r,
           r.as_ref().map(|&r| str::from_utf8(&buf[0..r])));
    r
  }
}

