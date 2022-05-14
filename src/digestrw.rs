// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::crates::*;
use crate::prelude::*;

#[derive(Debug,Copy,Clone)]
pub struct DigestRead<D: Digest, R: Read> {
  d: D,
  r: R,
}

impl<D: Digest, R: Read> DigestRead<D, R> {
  pub fn new(r: R) -> Self { DigestRead { r, d: D::new() } }
  pub fn into_inner(self) -> (D, R) { (self.d, self.r) }
  pub fn finish(self) -> digest::Output<D> {
    self.d.finalize()
  }
}

impl<D: Digest, R: Read> Read for DigestRead<D, R> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    let count = self.r.read(buf)?;
    self.d.update(&buf[0..count]);
    count
  }
}

#[test]
#[cfg(not(miri))]
fn test_digest_read() {
  let ibuffer = b"abc";
  let exp = Sha512_256::digest(&ibuffer[..]);
  let inner = &ibuffer[..];
  let mut dr = DigestRead::<Sha512_256,_>::new(inner);
  let mut obuffer = [0;4];
  assert_eq!( dr.read(&mut obuffer).unwrap(), 3 );
  assert_eq!( &obuffer, b"abc\0" );
  let got = dr.finish();
  assert_eq!( got, exp );
}

#[derive(Debug,Copy,Clone)]
pub struct DigestWrite<D: Digest, W: Write> {
  d: D,
  w: W,
}

impl<D: Digest, W: Write> DigestWrite<D, W> {
  pub fn new(w: W) -> Self { DigestWrite { w, d: D::new() } }
  pub fn into_inner(self) -> (D, W) { (self.d, self.w) }
  pub fn finish(self) -> (digest::Output<D>, W) {
    (self.d.finalize(), self.w)
  }
}
impl<D: Digest> DigestWrite<D, io::Sink> {
  pub fn sink() -> Self { DigestWrite::new(io::sink()) }

  #[throws(io::Error)]
  pub fn of<R>(r: &mut R) -> digest::Output<D> where R: Read {
    let mut dw = DigestWrite::<D,_>::sink();
    io::copy(r, &mut dw)?;
    dw.finish().0
  }
}

impl<D: Digest, W: Write> Write for DigestWrite<D, W> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    let count = self.w.write(buf)?;
    self.d.update(&buf[0..count]);
    count
  }
  #[throws(io::Error)]
  fn flush(&mut self) { self.w.flush()? }
}

#[test]
#[cfg(not(miri))]
fn test_digest_write() {
  let ibuffer = b"xyz";
  let exp = Sha512_256::digest(&ibuffer[..]);
  let mut obuffer = [0;4];
  let inner = &mut obuffer[..];
  let mut dw = bundles::DigestWrite::new(inner);
  assert_eq!( dw.write(&ibuffer[..]).unwrap(), 3);
  let (got, recov) = dw.finish();
  assert_eq!( recov, b"\0" );
  assert_eq!( got, exp );
  assert_eq!( &obuffer, b"xyz\0" );
}
