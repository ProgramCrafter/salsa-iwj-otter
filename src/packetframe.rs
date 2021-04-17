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

// ---------- common ----------

const CHUNK_MAX: u16 = 65534;
const CHUNK_ERR: u16 = 65535;
const CHUNK_DEF: u16 = 8192;

type BO = BigEndian;

#[derive(Debug,Copy,Clone,Error)]
#[error("error occurred at peer, during construction of frame data")]
pub struct SenderError;

#[derive(Debug)]
pub struct Fuse<RW>(Result<RW, Broken>);

/// An error saved by `Fuse` so it can be repeatedly returned.
#[derive(Clone,Error,Debug)]
pub struct Broken {
  msg: String,
  kind: io::ErrorKind,
}

// ---------- read ----------

#[derive(Debug)]
pub struct FrameReader<R: Read> {
  inner: Fuse<R>,
  state: ReaderState,
}

#[derive(Debug)]
pub struct ReadFrame<'r,R:Read> {
  fr: Result<&'r mut FrameReader<R>, Option<SenderError>>,
}

#[derive(Debug,Copy,Clone)]
enum ReaderState {
  Idle,
  FrameStart,
  InFrame(usize),
}
use ReaderState::*;

#[derive(Debug,Error)]
enum ReadError {
  GoodEof,
  IO(#[from] io::Error),
  SE(#[from] SenderError),
}
display_as_debug!{ReadError}
use ReadError as RE;

// ---------- write ----------

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

// ==================== implementation -====================

impl From<SenderError> for io::Error {
  fn from(se: SenderError) -> io::Error {
    io::Error::new(io::ErrorKind::Other, se)
  }
}

// ---------- fuse ----------

impl Display for Broken {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str(&self.msg)
  }
}
impl From<Broken> for io::Error {
  fn from(broken: Broken) -> io::Error {
    io::Error::new(broken.kind, broken)
  }
}

impl<RW> Fuse<RW> {
  #[throws(io::Error)]
  pub fn get(&mut self) -> &mut RW {
    self.0.as_mut().map_err(|broken| broken.clone())?
  }

  #[throws(io::Error)]
  pub fn with<F,T>(&mut self, f: F) -> T
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
    self.with(|inner| inner.read(buf))?
  }
}
impl<W:Write> Write for Fuse<W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    self.with(|inner| inner.write(buf))?
  }
  #[throws(io::Error)]
  fn flush(&mut self) {
    self.with(|inner| inner.flush())?
  }
}

// ---------- read ----------

impl ReaderState {
  fn idle(&self) -> bool {
    matches_doesnot!(self,
                     = Idle,
                     ! FrameStart | InFrame(_))
  }
}

fn badeof() -> ReadError { RE::IO(io::ErrorKind::UnexpectedEof.into()) }

impl<R:Read> FrameReader<R> {
  pub fn new(r: R) -> FrameReader<R> where R:BufRead {
    Self::new_unbuf(r)
  }
  fn new_unbuf(r: R) -> FrameReader<R> {
    FrameReader { inner: Fuse(Ok(r)), state: Idle }
  }

  #[throws(io::Error)]
  pub fn new_frame<'r>(&'r mut self) -> ReadFrame<'r,R> {
    if ! self.state.idle() {
      let mut buf = vec![0u8; CHUNK_DEF.into()];
      while ! self.state.idle() {
        match self.do_read(&mut buf) {
          Ok(_) | Err(RE::SE(_)) => {},
          Err(RE::GoodEof) => break,
          Err(RE::IO(ioe)) => throw!(ioe),
        }
      }
    }
    self.state = FrameStart;
    ReadFrame { fr: Ok(self) }
  }

  #[throws(ReadError)]
  fn chunk_remaining<'s>(inner: &mut Fuse<R>, state: &'s mut ReaderState)
                         -> &'s mut usize {
    match *state {
      Idle => panic!(),
      FrameStart | InFrame(0) => {
        *state = InFrame(match match {
          let mut lbuf = [0u8;2];
          let mut q = &mut lbuf[..];
          match io::copy(
            &mut inner.take(2),
            &mut q,
          )? {
            // length of chunk header
            0 => { match state { FrameStart => throw!(RE::GoodEof),
                                 InFrame(0) => throw!(badeof()),
                                 _ => panic!(), } },
            1 => throw!(badeof()),
            2 => (&lbuf[..]).read_u16::<BO>().unwrap(),
            _ => panic!(),
          }
        } {
          // value in chunk header
          0         => Left(RE::GoodEof),
          CHUNK_ERR => Left(RE::SE(SenderError)),
          x         => Right(x as usize),
        } {
          // Left( end of frame )  Right( nonempty chunk len )
          Left(e) => { *state = Idle; throw!(e); }
          Right(x) => x,
        });
        match *state { InFrame(ref mut x) => x, _ => panic!() }
      },
      InFrame(ref mut remaining) => remaining,
    }
  }

  #[throws(ReadError)]
  fn do_read(&mut self, buf: &mut [u8]) -> usize {
    assert_ne!(buf.len(), 0);
    let remaining = Self::chunk_remaining(&mut self.inner, &mut self.state)?;

    //dbgc!(buf.len(), &remaining);

    let n = min(buf.len(), *remaining);
    let r = self.inner.read(&mut buf[0..n])?;
    assert!(r <= n);
    if r == 0 { throw!(badeof()); }
    *remaining -= r;
    //dbgc!(r, self.in_frame);
    r
  }

  #[throws(MgmtChannelReadError)]
  pub fn read_rmp<T:DeserializeOwned>(&mut self) -> T {
    let mut frame = self.new_frame()?;
    rmp_serde::decode::from_read(&mut frame)
      .map_err(|e| MgmtChannelReadError::Parse(format!("{}", &e)))?
  }
}

impl<'r, R:Read> Read for ReadFrame<'r, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    if buf.len() == 0 { return 0 }
    //dbgc!(buf.len(), self.fr.as_ref().err());
    let fr = match self.fr {
      Ok(ref mut fr) => fr,
      Err(None) => return 0,
      Err(Some(e@ SenderError)) => throw!(e),
    };
    //dbgc!(fr.in_frame);
    match fr.do_read(buf) {
      Ok(0) | Err(RE::GoodEof) => { self.fr = Err(None); 0 },
      Ok(x) => x,
      Err(RE::IO(ioe)) => throw!(ioe),
      Err(RE::SE(e@ SenderError)) => { self.fr = Err(Some(e)); throw!(e) },
    }
  }
}

// ---------- write ----------

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

  #[throws(MgmtChannelWriteError)]
  pub fn write_rmp<T:Serialize>(&mut self, t: &T) {
    let mut frame = self.new_frame()?;
    rmp_serde::encode::write_named(&mut frame, t)?
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

// ==================== tests ====================

#[test]
fn write_test(){

  // pretty printing the test message buffer
  #[derive(Clone,Default)]
  struct Framed {
    buf: Vec<u8>,
  }
  deref_to_field_mut!{ Framed, Vec<u8>, buf }
  impl Debug for Framed {
    #[throws(fmt::Error)]
    fn fmt(&self, f: &mut fmt::Formatter) {
      let mut delim = iter::once("[").chain(iter::repeat(" "));
      let mut p = self.buf.as_slice();
      macro_rules! byte { () => {
        let b = p.read_u8().unwrap();
        write!(f, "{:02x}", b)?;
      } }
      while p.len() > 0 {
        write!(f, "{}", delim.next().unwrap())?;
        if_let!{ Ok(l) = p.read_u16::<BO>(); else byte!(); continue; }
        write!(f, "{:04x}", l)?;
        if l == 0 || l == CHUNK_ERR { continue }
        write!(f, " ")?;
        let l = l.into();
        if_chain! {
          if l <= p.len();
          let s = &p[0..l];
          if let Ok(s) = str::from_utf8(s);
          then {
            p = &p[l..];
            write!(f, "{:?}", s)?;
          }
          else {
            for _ in 0..min(l, p.len()) { byte!(); }
          }
        }
      }
      write!(f, "]")?;
    }
  }

  // make the test message buffer
  let mut msg = Framed::default();
  let mut wr = FrameWriter::new(&mut msg.buf);
  {
    let mut frame = wr.new_frame().unwrap();
    frame.write(b"hello").unwrap();
    frame.finish().unwrap();
  }
  {
    let mut frame = wr.new_frame().unwrap();
    frame.write(b"boom").unwrap();
  }
  (||{
    msg.buf.write_u16::<BO>(3)?;
    msg.buf.write(b"lon")?;
    msg.buf.write_u16::<BO>(4)?;
    msg.buf.write(b"ger!")?;
    msg.buf.write_u16::<BO>(0)?;
    Ok::<_,AE>(())
  })().unwrap();
  dbgc!(&msg);

  // utility functions for helping with test reads
  fn expect_boom<R:Read>(rd: &mut FrameReader<R>) {
    let mut buf = [0u8;10];
    let mut frame = rd.new_frame().unwrap();
    let mut before: Vec<u8> = vec![];
    let r = loop {
      match frame.read(&mut buf) {
        Ok(y) => before.extend(&buf[0..y]),
        Err(e) => break e,
      };
    };
    dbgc!(&r);
    assert_eq!(r.kind(), ErrorKind::Other);
    assert!(r.into_inner().unwrap().is::<SenderError>());
    assert_eq!(before, b"boom");
  }
  fn expect_bad_eof<R:Read>(frame: &mut ReadFrame<R>) {
    let mut buf = [0u8;10];
    let r = frame.read(&mut buf).unwrap_err();
    assert_eq!(r.kind(), ErrorKind::UnexpectedEof);
    r.into_inner().map(|i| panic!("unexpected {:?}", &i));
  }

  // a very simple test as far as the first boom
  let mut rd = FrameReader::new(&*msg.buf);
  let mut buf = [0u8;10];
  {
    let mut frame = rd.new_frame().unwrap();
    let y = frame.read(&mut buf).unwrap();
    dbgc!(str::from_utf8(&buf[0..y]).unwrap());
  }
  expect_boom(&mut rd);

  // check how dropping a reading frame works
  let mut rd = FrameReader::new(&*msg.buf);
  {
    let mut _frame = rd.new_frame().unwrap();
  }
  expect_boom(&mut rd);

  // utilitiesfor reading the whole input, collecting into vecs
  fn expect_good<R:Read>(rd: &mut FrameReader<R>, expected: &[u8]) {
    let mut buf = vec![];
    let mut frame = rd.new_frame().unwrap();
    frame.read_to_end(&mut buf).unwrap();
    assert_eq!(&*buf ,expected);
    dbgc!(str::from_utf8(&buf).unwrap());
  }
  fn expect_good_eof<R:Read>(rd: &mut FrameReader<R>) {
    let mut buf = [0u8;10];
    let mut frame = rd.new_frame().unwrap();
    let r = frame.read(&mut buf).unwrap(); dbgc!(&r); assert_eq!(r, 0);
    let r = frame.read(&mut buf).unwrap(); dbgc!(&r); assert_eq!(r, 0);
  }
  let read_all = |input: &mut dyn Read| {
    let mut rd = FrameReader::new_unbuf(input);
    expect_good(&mut rd, b"hello");
    expect_boom(&mut rd);
    expect_good(&mut rd, b"longer!");
    expect_good_eof(&mut rd);
  };
  read_all(&mut &*msg.buf);

  // try lumpy reads (ie, short reads) at every plausible boundary size
  // this approach is not very principled but ought to test every boundary
  #[cfg(not(miri))]
  for lumpsize in 1..=msg.buf.len() {
    #[derive(Debug)]
    struct LumpReader<R: Read> {
      inner: R,
      inlump: usize,
      lumpsize: usize,
    }
    impl<R:Read> LumpReader<R> {
      fn new(lumpsize: usize, inner: R) -> Self {
        LumpReader { inner, lumpsize, inlump: 0 }
      }
    }
    impl<R:Read> Read for LumpReader<R> {
      #[throws(io::Error)]
      fn read(&mut self, buf: &mut [u8]) -> usize {
        if self.inlump == 0 { self.inlump = self.lumpsize }
        let want = min(self.inlump, buf.len());
        let r = self.inner.read(&mut buf[0..want])?;
        self.inlump -= r;
        r
      }
    }

    dbgc!(lumpsize);
    let mut lr = LumpReader::new(lumpsize, &*msg.buf);
    read_all(&mut lr);
  }

  // Unexpected EOF mid-chunk-header
  {
    let mut rd = FrameReader::new(&[0x55][..]);
    let mut frame = rd.new_frame().unwrap();
    expect_bad_eof(&mut frame);
  }

  // Unexpected EOF mid-data
  {
    let mut rd = FrameReader::new(&msg.buf[0..3]);
    let mut frame = rd.new_frame().unwrap();
    let y = frame.read(&mut buf).unwrap();
    assert_eq!(y, 1);
    expect_bad_eof(&mut frame);
  }

  // Unexpected EOF after nonempty chunk
  {
    let mut rd = FrameReader::new(&msg.buf[0..7]);
    let mut frame = rd.new_frame().unwrap();
    let y = frame.read(&mut buf).unwrap();
    assert_eq!(&buf[0..y], b"hello");
    expect_bad_eof(&mut frame);
  }
}
