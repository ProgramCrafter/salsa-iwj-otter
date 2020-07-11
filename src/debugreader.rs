#[derive(Debug)]
pub struct DebugReader<T : Read>(pub T);

impl<T : Read> Read for DebugReader<T> {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize,io::Error> {
    let l = buf.len();
    eprintln!("DebugReader({:?}).read()...", l);
    let r = self.0.read(buf);
    eprintln!("DebugReader({:?}).read() = {:?} {:?}", l, &r,
              r.as_ref().map(|&r| str::from_utf8(&buf[0..r])));
    r
  }
}

