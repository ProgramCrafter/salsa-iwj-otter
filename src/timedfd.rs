// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use io::ErrorKind as EK;

use nix::fcntl::{fcntl, OFlag, FcntlArg};
use nix::Error as NE;
use nix::errno::Errno;

use mio::Token;

pub struct TimedFd<RW: TimedFdReadWrite> {
  fd: Fd,
  poll: mio::Poll,
  events: mio::event::Events,
  deadline: Option<Instant>,
  rw: PhantomData<RW>,
}

pub trait TimedFdReadWrite {
  const INTEREST: mio::Interest;
}

pub type TimedFdReader = TimedFd<TimedFdRead>;
pub type TimedFdWriter = TimedFd<TimedFdWrite>;

#[derive(Debug,Copy,Clone)] pub struct TimedFdRead;
impl TimedFdReadWrite for TimedFdRead {
  const INTEREST : mio::Interest = mio::Interest::READABLE;
}
#[derive(Debug,Copy,Clone)] pub struct TimedFdWrite;
impl TimedFdReadWrite for TimedFdWrite {
  const INTEREST : mio::Interest = mio::Interest::WRITABLE;
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

impl<RW> TimedFd<RW> where RW: TimedFdReadWrite {
  /// Takes ownership of the fd
  ///
  /// Will change the fd's open-file to nonblocking.
  #[throws(io::Error)]
  pub fn new<F>(fd: F) -> TimedFd<RW> where F: IntoRawFd {
    Self::from_fd( Fd::from_raw_fd( fd.into_raw_fd() ))?
  }

  /// Takes ownership of the fd
  ///
  /// Will change the fd's open-file to nonblocking.
  #[throws(io::Error)]
  fn from_fd(fd: Fd) -> Self {
    fcntl(fd.as_raw_fd(), FcntlArg::F_SETFL(OFlag::O_NONBLOCK))
      .map_err(|e| e.as_ioe())?;

    let poll = mio::Poll::new()?;
    poll.registry().register(
      &mut mio::unix::SourceFd(&fd.as_raw_fd()),
      Token(0),
      RW::INTEREST,
    )?;
    let events = mio::event::Events::with_capacity(1);
    TimedFd { fd, poll, events, deadline: None, rw: PhantomData }
  }

  pub fn set_deadline(&mut self, deadline: Option<Instant>) {
    self.deadline = deadline;
  }
  pub fn set_timeout(&mut self, timeout: Option<Duration>) {
    self.set_deadline(timeout.map(|timeout|{
      Instant::now() + timeout
    }));
  }

  #[throws(io::Error)]
  fn rw<F,O>(&mut self, mut f: F) -> O
  where F: FnMut(i32) -> Result<O, nix::Error>
  {
    'again: loop {
      for event in &self.events {
        if event.token() == Token(0) {
          match f(self.fd.as_raw_fd()) {
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
      loop {
        match self.poll.poll(&mut self.events, timeout) {
          Err(e) if e.kind() == ErrorKind::Interrupted => continue,
          Err(e) => throw!(e),
          Ok(()) => break,
        }
      }
      if self.events.is_empty() { throw!(io::ErrorKind::TimedOut) }
    }
  }
}

impl Read for TimedFd<TimedFdRead> {
  #[throws(io::Error)]
  fn read(&mut self, buf: &mut [u8]) -> usize {
    self.rw(|fd| unistd::read(fd, buf))?
  }
}

impl Write for TimedFd<TimedFdWrite> {
  #[throws(io::Error)]
  fn write(&mut self, buf: &[u8]) -> usize {
    self.rw(|fd| unistd::write(fd, buf))?
  }
  #[throws(io::Error)]
  fn flush(&mut self) {
  }
}

impl Drop for Fd {
  fn drop(&mut self) {
    let fd = self.extract_raw_fd();
    if fd >= 2 { let _ = nix::unistd::close(fd); }
  }
}
