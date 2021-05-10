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
use crate::commands::ProgressInfo;

// ---------- common ----------

type ChunkLen = u16;

const CHUNK_MAX: ChunkLen = 65534;
const CHUNK_ERR: ChunkLen = 65535;
const CHUNK_DEF: ChunkLen = 8192;

pub const BUFREADER_CAPACITY: usize = CHUNK_DEF as usize + 4;

type BO = BigEndian;

#[derive(Debug,Copy,Clone,Error)]
#[error("error occurred at peer, during construction of frame data")]
pub struct SenderError;

#[derive(Debug)]
pub struct Fuse<RW>{ inner: Result<RW, Broken> }

/// An error saved by `Fuse` so it can be repeatedly returned.
#[derive(Clone,Error,Debug)]
pub struct Broken {
  msg: String,
  kind: io::ErrorKind,
}

// ---------- read ----------

#[derive(Debug)]
pub struct FrameReader<R: Read> {
  state: ReaderState,
  inner: BufReader<Fuse<R>>,
}

#[derive(Debug)]
pub struct ReadFrame<'r,R:Read> {
  fr: &'r mut FrameReader<R>,
}

#[derive(Debug,Copy,Clone)]
enum ReaderState {
  InBuffer { ibuf_used: ChunkLen, chunk_remaining: ChunkLen },
  InChunk { remaining: ChunkLen },
  HadFrameEnd(Result<(), SenderError>),
}
use ReaderState::*;

#[derive(Debug,Error)]
enum ReadHeaderError {
  TolerableEof,
  IO(#[from] io::Error),
}
display_as_debug!{ReadHeaderError}
use ReadHeaderError as RHE;

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
  pub fn new(rw: RW) -> Self { Fuse { inner: Ok(rw) } }

  #[throws(io::Error)]
  pub fn get(&mut self) -> &mut RW {
    self.inner.as_mut().map_err(|broken| broken.clone())?
  }

  #[throws(io::Error)]
  pub fn with<F,T>(&mut self, f: F) -> T
    where F: FnOnce(&mut RW) -> Result<T, io::Error>
  {
    let inner = self.get()?;
    let r = f(inner);
    if let Err(e) = &r {
      self.inner = Err(Broken {
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

fn badeof() -> io::Error { io::ErrorKind::UnexpectedEof.into() }

impl<R:Read> FrameReader<R> {
  pub fn new(r: R) -> FrameReader<R> {
    let r = Fuse::new(r);
    let r = BufReader::with_capacity(BUFREADER_CAPACITY, r);
    Self::from_bufreader(r)
  }
  pub fn from_bufreader(r: BufReader<Fuse<R>>) -> FrameReader<R> {
    FrameReader { inner: r, state: HadFrameEnd(Ok(())) }
  }

  #[throws(io::Error)]
  pub fn new_frame<'r>(&'r mut self) -> Option<ReadFrame<'r,R>> {
    self.finish_reading_frame()?;

    match self.read_chunk_header() {
      Ok(_) => {},
      Err(RHE::TolerableEof) => return None,
      Err(RHE::IO(e)) => throw!(e),
    }
    Some(ReadFrame { fr: self })
  }

  #[throws(io::Error)]
  fn finish_reading_frame(&mut self) {
    while matches_doesnot!(
      self.state,
      = InBuffer{..} | InChunk{..},
      ! HadFrameEnd(..),
    ) {
      struct Discard;
      impl ReadOutput for Discard {
        #[inline]
        fn copy_from_buf(&mut self, input: &[u8]) -> usize { input.len() }
      }
      self.read_from_frame(&mut Discard)?;
    }
  }

  #[throws(ReadHeaderError)]
  fn read_chunk_header(&mut self) {
    assert!(matches_doesnot!(
      self.state,
      = InChunk { remaining: 0 },
      = HadFrameEnd(..),
      ! InChunk { remaining: _ },
      ! InBuffer{..},
    ), "bad state {:?}", self.state);

    let header_value = {
      let mut lbuf = [0u8;2];
      let mut q = &mut lbuf[..];
      match io::copy(
        &mut (&mut self.inner).take(2),
        &mut q,
      )? {
        // length of chunk header read
        0 => throw!(RHE::TolerableEof), // EOF on underlying stream
        1 => throw!(badeof()),
        2 => (&lbuf[..]).read_u16::<BO>().unwrap(),
        _ => panic!(),
      }
    };

    self.state = match header_value {
      0         => HadFrameEnd(Ok(())),
      CHUNK_ERR => HadFrameEnd(Err(SenderError)),
      len       => InChunk { remaining: len },
    }
  }

  #[throws(io::Error)]
  fn read_from_frame<O:ReadOutput+?Sized>(&mut self, output: &mut O) -> usize {
    loop {
      if let InBuffer { ref mut ibuf_used, chunk_remaining } = self.state {
        let ibuf = self.inner.buffer();
        let cando = &ibuf[ (*ibuf_used).into() ..
                             min(ibuf.len(), chunk_remaining.into()) ];
        let got = output.copy_from_buf(cando);
        *ibuf_used += ChunkLen::try_from(got).unwrap();
        if got != 0 { break got }
        assert_eq!(cando.len(), 0);
        self.inner.consume((*ibuf_used).into());
        let remaining = chunk_remaining - *ibuf_used;
        self.state = InChunk { remaining };
      }

      if let InChunk { remaining } = self.state {
        if remaining > 0 {
          let got = self.inner.fill_buf()?.len();
          if got == 0 { throw!(badeof()) }
          self.state = InBuffer { ibuf_used: 0, chunk_remaining: remaining };
          continue;
        }
      }

      match self.state {
        InChunk { remaining: 0 } => { },
        HadFrameEnd(Ok(())) => break 0,
        HadFrameEnd(Err(e)) => throw!(e),
        _ => panic!("bad state {:?}", self.state),
      }

      match self.read_chunk_header() {
        Ok(()) => { },
        Err(RHE::TolerableEof) => throw!(badeof()),
        Err(RHE::IO(e)) => throw!(e),
      }
    }
  }

  #[throws(MgmtChannelReadError)]
  pub fn read_withbulk<'c,T>(&'c mut self) -> (T, ReadFrame<impl Read + 'c>)
  where T: DeserializeOwned + Debug
  {
    let mut f = self.new_frame()?.ok_or(MgmtChannelReadError::EOF)?;
    let v = f.read_rmp()?;
    trace!("read OK {:?}", &v);
    (v, f)
  }

  #[throws(MgmtChannelReadError)]
  pub fn read<T>(&mut self) -> T
  where T: DeserializeOwned + Debug
  {
    self.read_withbulk()?.0
  }
}

#[ext(pub, name=ReadExt)]
impl<R: Read> R {
  #[throws(MgmtChannelReadError)]
  fn read_rmp<T>(&mut self) -> T
  where T: DeserializeOwned,
        R: Read
  {
    use MgmtChannelReadError as MCRE;
    let r = rmp_serde::decode::from_read(self);
    let v = r.map_err(|e| MCRE::Parse(format!("{}", &e)))?;
    v
  }
}

trait ReadOutput {
  fn copy_from_buf(&mut self, input: &[u8]) -> usize;
}
    
impl ReadOutput for [u8] {
  #[inline]
  fn copy_from_buf(&mut self, input: &[u8]) -> usize {
    let mut p = self;
    p.write(input).unwrap()
  }
}

impl<'r, R:Read> Read for ReadFrame<'r, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    if buf.len() == 0 { return 0 }
    self.fr.read_from_frame(buf)?
  }
}

// ---------- write ----------

impl<W:Write> FrameWriter<W> {
  pub fn new(w: W) -> FrameWriter<W> {
    FrameWriter { inner: Fuse::new(w), in_frame: None }
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
      self.inner.flush()?;
    }
  }

  #[throws(MgmtChannelWriteError)]
  pub fn write_withbulk<'c>(&'c mut self) -> ResponseWriter<impl Write + 'c>
  {
    ResponseWriter { f: self.new_frame()? }
  }

  #[throws(MgmtChannelWriteError)]
  pub fn write<T>(&mut self, val: &T)
  where T: Serialize + Debug
  {
    let f = self.write_withbulk()?.respond(val)?;
    f.finish()?;
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

pub struct ResponseWriter<'c,W:Write> { f: WriteFrame<'c,W> }

impl<'c,W:Write> ResponseWriter<'c,W> {
  #[throws(MgmtChannelWriteError)]
  pub fn respond<'t,T>(mut self, val: &'t T) -> WriteFrame<'c, impl Write + 'c>
  where T: Serialize + Debug
  {
    rmp_serde::encode::write_named(&mut self.f, val)?;
    trace!("writing {:?}", val);
    self.f
  }

  
  #[throws(MgmtChannelWriteError)]
  pub fn progress(&mut self, pi: ProgressInfo) {
    let resp = crate::commands::MgmtResponse::Progress(pi);
    rmp_serde::encode::write_named(&mut self.f, &resp)?;
  }
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
  {
    let frame = wr.new_frame().unwrap();
    frame.finish().unwrap();
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
    let mut frame = rd.new_frame().unwrap().unwrap();
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
  fn expect_is_bad_eof(ioe: io::Error) {
    assert_eq!(ioe.kind(), ErrorKind::UnexpectedEof);
    ioe.into_inner().map(|i| panic!("unexpected {:?}", &i));
  }
  fn expect_bad_eof<R:Read>(frame: &mut ReadFrame<R>) {
    let mut buf = [0u8;10];
    let r = frame.read(&mut buf).unwrap_err();
    expect_is_bad_eof(r);
  }

  // a very simple test as far as the first boom
  let mut rd = FrameReader::new(&*msg.buf);
  let mut buf = [0u8;10];
  {
    let mut frame = rd.new_frame().unwrap().unwrap();
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
  #[cfg(not(miri))]
  fn expect_good<R:Read>(rd: &mut FrameReader<R>, expected: &[u8]) {
    let mut buf = vec![];
    let mut frame = rd.new_frame().unwrap().unwrap();
    frame.read_to_end(&mut buf).unwrap();
    assert_eq!(&*buf ,expected);
    dbgc!(str::from_utf8(&buf).unwrap());
  }
  #[cfg(not(miri))]
  fn expect_good_eof<R:Read>(rd: &mut FrameReader<R>) {
    let frame = rd.new_frame().unwrap(); assert!(frame.is_none());
    let frame = rd.new_frame().unwrap(); assert!(frame.is_none());
  }

  // try lumpy reads (ie, short reads) at every plausible boundary size
  // this approach is not very principled but ought to test every boundary
  #[cfg(not(miri))]
  for lumpsize in 1..=msg.buf.len()+1 {
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

    for bufsize in 1..=msg.buf.len()+1 {
      dbgc!(lumpsize, bufsize);
      let rd = LumpReader::new(lumpsize, &*msg.buf);
      let rd = Fuse::new(rd);
      let rd = BufReader::with_capacity(bufsize, rd);
      let mut rd = FrameReader::from_bufreader(rd);

      expect_good(&mut rd, b"hello");
      expect_boom(&mut rd);
      expect_good(&mut rd, b"");
      expect_good(&mut rd, b"longer!");
      expect_good_eof(&mut rd);
    }
  }

  // Unexpected EOF mid-chunk-header
  {
    let mut rd = FrameReader::new(&[0x55][..]);
    let r = rd.new_frame().unwrap_err();
    expect_is_bad_eof(r);
  }

  // Unexpected EOF mid-data
  {
    let mut rd = FrameReader::new(&msg.buf[0..3]);
    let mut frame = rd.new_frame().unwrap().unwrap();
    let y = frame.read(&mut buf).unwrap();
    assert_eq!(y, 1);
    expect_bad_eof(&mut frame);
  }

  // Unexpected EOF after nonempty chunk
  {
    let mut rd = FrameReader::new(&msg.buf[0..7]);
    let mut frame = rd.new_frame().unwrap().unwrap();
    let y = frame.read(&mut buf).unwrap();
    assert_eq!(&buf[0..y], b"hello");
    expect_bad_eof(&mut frame);
  }
}
