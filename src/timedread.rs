// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use io::ErrorKind as EK;

use nix::fcntl::{fcntl, OFlag, FcntlArg};
use nix::Error as NE;
use nix::errno::Errno;

use mio::Token;

pub struct TimedFdReader {
  fd: Fd,
  poll: mio::Poll,
  events: mio::event::Events,
  deadline: Option<Instant>,
}

pub struct Fd(RawFd);
impl Fd {
  pub fn from_raw_fd(fd: RawFd) -> Self { Fd(fd) }
  fn extract_raw_fd(&mut self) -> RawFd { mem::replace(&mut self.0, -1) }
}
impl IntoRawFd for Fd {
  fn into_raw_fd(mut self) -> RawFd { self.extract_raw_fd() }
}
impl AsRawFd for Fd {
  fn as_raw_fd(&self) -> RawFd { self.0 }
}

#[ext(pub)]
impl nix::Error {
  fn as_ioe(self) -> io::Error {
    match self {
      NE::Sys(e) => return io::Error::from_raw_os_error(e as i32),
      NE::UnsupportedOperation => EK::Unsupported,
      NE::InvalidPath          => EK::InvalidData,
      NE::InvalidUtf8          => EK::InvalidData,
    }.into()
  }
}

impl TimedFdReader {
  /// Takes ownership of the fd
  #[throws(io::Error)]
  pub fn from_fd(fd: Fd) -> Self {
    fcntl(fd.as_raw_fd(), FcntlArg::F_SETFL(OFlag::O_NONBLOCK))
      .map_err(|e| e.as_ioe())?;

    let poll = mio::Poll::new()?;
    poll.registry().register(
      &mut mio::unix::SourceFd(&fd.as_raw_fd()),
      Token(0),
      mio::Interest::READABLE,
    )?;
    let events = mio::event::Events::with_capacity(1);
    TimedFdReader { fd, poll, events, deadline: None }
  }
}

impl Read for TimedFdReader {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    'again: loop {
      for event in &self.events {
        if event.token() == Token(0) {
          match unistd::read(self.fd.as_raw_fd(), buf) {
            Ok(got) => { break 'again got },
            Err(NE::Sys(Errno::EINTR)) => { continue 'again }
            Err(NE::Sys(Errno::EAGAIN)) => break,
            Err(ne) => throw!(ne.as_ioe()),
          }
        }
      }

      let timeout = if let Some(deadline) = self.deadline {
        let now = Instant::now();
        if now >= deadline { throw!(io::ErrorKind::TimedOut) }
        Some(deadline - now)
      } else {
        None
      };
      self.poll.poll(&mut self.events, timeout)?;
      if self.events.is_empty() { throw!(io::ErrorKind::TimedOut) }
    }
  }
}

impl Drop for Fd {
  fn drop(&mut self) {
    let fd = self.extract_raw_fd();
    if fd >= 2 { let _ = nix::unistd::close(fd); }
  }
}
