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

#[derive(Debug,Copy,Clone,Error)]
#[error("error occurred at peer, during construction of frame data")]
struct SenderError;

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
}

impl<R:Read> Read for Fuse<R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    let inner = self.get()?;
    let r = inner.read(buf);
    if let Err(e) = &r {
      self.0 = Err(Broken {
        msg: e.to_string(),
        kind: e.kind(),
      });
    }
    r?
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
  fn frame<'r>(&'r mut self) -> ReadFrame<'r,R> { ReadFrame {
    fr: Ok(self),
  } }

  #[throws(MgmtChannelReadError)]
  pub fn read_rmp<T:DeserializeOwned>(&mut self) -> T {
    let mut frame = self.frame();
    rmp_serde::decode::from_read(&mut frame)
      .map_err(|e| MgmtChannelReadError::Parse(format!("{}", &e)))?
  }
}

impl<'r, R:BufRead> Read for ReadFrame<'r, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    if buf.len() == 0 { return 0 }
    loop { match self.fr {
      Err(None) => return 0,
      Err(Some(e)) => throw!(e),
      Ok(ref mut fr) => {
        if fr.in_frame.is_none() || fr.in_frame == Some(0) {
          match match fr.inner.read_u16::<BO>()? {
            0         => Err(None),
            CHUNK_ERR => Err(Some(SenderError)),
            x         => Ok(x as usize),
          } {
            Err(done) => {
              fr.in_frame = None;
              self.fr = Err(done);
              continue;
            },
            Ok(in_chunk) => {
              fr.in_frame = Some(in_chunk);
            }
          };
        }
        let remaining = fr.in_frame.as_mut().unwrap();

        let n = min(buf.len(), *remaining);
        let r = fr.inner.read(&mut buf[0..n])?;
        assert!(r <= n);
        *remaining -= n;
        break r;
      }
    } }
  }
}
