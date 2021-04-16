// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Frame format:
//!    zero or more chunks
//!    end marker
//!
//! Chunk format:
//!    u16     chunk length, nonzero
//!    <n>     chunk data
//! Packet end marker:
//!    0u16         marker
//!    0xffffu16    marker, error!

use crate::prelude::*;

const CHUNK_MAX: u16 = 65534;
const CHUNK_ERR: u16 = 65535;
const CHUNK_DEF: u16 = 8192;

type BO = BigEndian;

#[derive(Debug)]
struct Fuse<RW>(Result<RW, Broken>);

#[derive(Debug)]
pub struct FrameReader<R: BufRead> {
  inner: Fuse<R>,
  in_frame: Option<usize>,
}

#[derive(Debug)]
pub struct ReadFrame<'r,R:BufRead> {
  fr: Result<&'r mut FrameReader<R>, Option<SenderError>>,
}

#[derive(Debug)]
pub struct FrameWriter<W:Write> {
  inner: Fuse<W>,
  in_frame: Option<()>,
}

#[derive(Debug)]
struct WriteFrameRaw<'w,W:Write> {
  fw: &'w mut FrameWriter<W>,
}
#[derive(Debug)]
pub struct WriteFrame<'w,W:Write> {
  buf: BufWriter<WriteFrameRaw<'w,W>>,
}

#[derive(Debug,Copy,Clone,Error)]
#[error("error occurred at peer, during construction of frame data")]
pub struct SenderError;

#[derive(Clone,Error,Debug)]
pub struct Broken {
  msg: String,
  kind: io::ErrorKind,
}

impl<RW> Fuse<RW> {
  #[throws(io::Error)]
  fn get(&mut self) -> &mut RW {
    self.0.as_mut().map_err(|broken| broken.clone())?
  }

  #[throws(io::Error)]
  fn with<F,T>(&mut self, f: F) -> T
    where F: FnOnce(&mut RW) -> Result<T, io::Error>
  {
    let inner = self.get()?;
    let r = f(inner);
    if let Err(e) = &r {
      self.0 = Err(Broken {
        msg: e.to_string(),
        kind: e.kind(),
      });
    }
    r?
  }
}

impl<R:Read> Read for Fuse<R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    self.with(|inner| dbgc!(inner.read(buf)))?
  }
}
impl<W:Write> Write for Fuse<W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    self.with(|inner| dbgc!(inner.write(buf)))?
  }
  #[throws(io::Error)]
  fn flush(&mut self) {
    self.with(|inner| dbgc!(inner.flush()))?
  }
}

impl From<Broken> for io::Error {
  fn from(broken: Broken) -> io::Error {
    io::Error::new(broken.kind, broken)
  }
}
impl From<SenderError> for io::Error {
  fn from(se: SenderError) -> io::Error {
    io::Error::new(io::ErrorKind::Other, se)
  }
}

impl Display for Broken {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.msg)
  }
}

impl<R:BufRead> FrameReader<R> {
  pub fn new(r: R) -> FrameReader<R> {
    FrameReader { inner: Fuse(Ok(r)), in_frame: None }
  }

  #[throws(io::Error)]
  pub fn new_frame<'r>(&'r mut self) -> ReadFrame<'r,R> {
    if self.in_frame.is_some() {
      let mut buf = vec![0u8; CHUNK_DEF.into()];
      while self.in_frame.is_some() {
        let _: Result<_, SenderError> = self.do_read(&mut buf)?;
      }
    }
    self.in_frame = Some(0);
    ReadFrame { fr: Ok(self) }
  }

  fn do_read(&mut self, buf: &mut [u8]) ->
    Result<Result<usize, SenderError>, io::Error>
  {
    assert_ne!(buf.len(), 0);
    let remaining = self.in_frame.as_mut().unwrap();
    if *remaining == 0 {
      *remaining = match match self.inner.read_u16::<BO>()? {
        0         => Left(Ok(0)),
        CHUNK_ERR => Left(Err(SenderError)),
        x         => Right(x as usize),
      } {
        Left(r) => { self.in_frame = None; return Ok(r); }
        Right(x) => x,
      }
    }
    dbgc!(buf.len(), &remaining);

    let n = min(buf.len(), *remaining);
    let r = self.inner.read(&mut buf[0..n])?;
    dbgc!(&r);
    assert!(r <= n);
    *remaining -= n;
    Ok(Ok(r))
  }

  #[throws(MgmtChannelReadError)]
  pub fn read_rmp<T:DeserializeOwned>(&mut self) -> T {
    let mut frame = self.new_frame()?;
    rmp_serde::decode::from_read(&mut frame)
      .map_err(|e| MgmtChannelReadError::Parse(format!("{}", &e)))?
  }
}

impl<'r, R:BufRead> Read for ReadFrame<'r, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    if buf.len() == 0 { return 0 }
    dbgc!(buf.len(), self.fr.as_ref().err());
    let fr = match self.fr {
      Ok(ref mut fr) => fr,
      Err(None) => return 0,
      Err(Some(e@ SenderError)) => throw!(e),
    };
    match dbgc!(fr.do_read(buf))? {
      Ok(0) => { self.fr = Err(None); 0 },
      Ok(x) => x,
      Err(e@ SenderError) => { self.fr = Err(Some(e)); throw!(e) },
    }
  }
}

impl<W:Write> FrameWriter<W> {
  pub fn new(w: W) -> FrameWriter<W> {
    FrameWriter { inner: Fuse(Ok(w)), in_frame: None }
  }

  #[throws(io::Error)]
  pub fn new_frame<'w>(&'w mut self) -> WriteFrame<'w,W> {
    self.tidy()?;
    self.in_frame = Some(());
    let raw = WriteFrameRaw { fw: self };
    let buf = BufWriter::with_capacity(CHUNK_DEF.into(), raw);
    WriteFrame { buf }
  }

  #[throws(io::Error)]
  pub fn flush(&mut self) {
    self.tidy()?;
    self.inner.flush()?;
  }

  #[throws(io::Error)]
  fn tidy(&mut self) {
    self.finish_any_frame(Err(SenderError))?;
  }

  #[throws(io::Error)]
  fn finish_any_frame(&mut self, how: Result<(), SenderError>) {
    if let Some(_) = self.in_frame {
      self.inner.write_u16::<BO>(match how {
        Ok(()) => 0,
        Err(SenderError) => CHUNK_ERR,
      })?;
      self.in_frame = None;
    }
  }
}

impl<'w,W:Write> WriteFrame<'w,W> {
  #[throws(io::Error)]
  pub fn finish_with(self, how: Result<(), SenderError>) {
    self.buf
      .into_inner()
      .map_err(|e| e.into_error())?
      .fw
      .finish_any_frame(how)?
  }

  #[throws(io::Error)]
  pub fn finish(self) { self.finish_with(Ok(()))? }
}
impl<'w,W:Write> WriteFrameRaw<'w,W> {
}
impl<'w,W:Write> Drop for WriteFrameRaw<'w,W> {
  fn drop(&mut self) {
    self.fw.tidy()
      .unwrap_or_else(|_: io::Error| () /* Fuse will replicate this */);
  }
}
impl<'w,W:Write> Write for WriteFrameRaw<'w,W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    let now = min(buf.len(), CHUNK_MAX.into());
    self.fw.inner.write_u16::<BO>(now.try_into().unwrap())?;
    self.fw.inner.write(&buf[0..now])?;
    now
  }

  #[throws(io::Error)]
  fn flush(&mut self) {
    self.fw.inner.flush()?
  }
}
impl<'w,W:Write> Write for WriteFrame<'w,W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize { self.buf.write(buf)? }
  #[throws(io::Error)]
  fn flush(&mut self) { self.buf.flush()? }
}

#[test]
fn write_test(){
  let mut msg = vec![];
  let mut wr = FrameWriter::new(&mut msg);
  {
    let mut frame = wr.new_frame().unwrap();
    frame.write(b"hello").unwrap();
    frame.finish().unwrap();
  }
  {
    let mut frame = wr.new_frame().unwrap();
    frame.write(b"boom").unwrap();
  }
  dbgc!(&msg);

  let mut rd = FrameReader::new(&*msg);
  let mut buf = [0u8;10];
  {
    let mut frame = rd.new_frame().unwrap();
    let y = frame.read(&mut buf).unwrap();
    dbgc!(&buf[0..y]);
  }
  let expect_boom = |rd: &mut FrameReader<_>| {
    let mut buf = [0u8;10];
    let mut frame = rd.new_frame().unwrap();
    let y = frame.read(&mut buf).unwrap();
    dbgc!(&buf[0..y]);
    let r = frame.read(&mut buf).unwrap_err();
    dbgc!(&r);
    assert_eq!(r.kind(), ErrorKind::Other);
    assert!(r.into_inner().unwrap().is::<SenderError>());
  };
  expect_boom(&mut rd);
}
