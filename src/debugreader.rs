// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: MIT-0 OR AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

#[derive(Debug)]
pub struct DebugReader<T:Read>(pub T, pub ClientId);

impl<T: Read> Read for DebugReader<T> {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error> {
    let l = buf.len();
    trace!("{} read({})...", &self.1, l);
    let r = self.0.read(buf);
    let level = match &r {
      Err(e) if e.kind() == io::ErrorKind::WouldBlock => log::Level::Trace,
      Err(_) => log::Level::Info,
      _ => log::Level::Debug,
    };
    log!(level,"{} read({}) = {:?} {:?}", &self.1, l, &r,
         r.as_ref().map(|&r| str::from_utf8(&buf[0..r])));
    r
  }
}

