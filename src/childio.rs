// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use std::process::{self, ChildStdin, ChildStdout};

#[derive(Debug)]
pub struct ChildIo<RW> {
  rw: RW,
  child: Arc<Mutex<ChildWrapper>>,
}

#[derive(Debug)]
struct ChildWrapper {
  reported: bool,
  desc: String,
  child: process::Child,
}

impl Display for ChildWrapper {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    write!(f, "subprocess {} [{}]", &self.desc, self.child.id())?
  }
}

impl<RW> ChildIo<RW> {
  fn rw_result(&self, r: io::Result<usize>) -> io::Result<usize> {
    if r.is_ok() && *r.as_ref().unwrap() > 0 { return r }
    let status = self.child.lock().child.try_wait()?;
    match status {
      None => r,
      Some(es) if es.success() => r,
      Some(es) => {
        let mut child = self.child.lock();
        child.reported = true;
        let ae = anyhow!("{} failed: {}", &child, es);
        Err(io::Error::new(ErrorKind::Other, ae))
      },
    }
  }
}

pub fn new_pair(mut input: process::Child, desc: String)
                -> (ChildIo<ChildStdin>, ChildIo<ChildStdout>) {
  let stdin  = input.stdin .take().expect("ChildIo::pair, no stdin¬");
  let stdout = input.stdout.take().expect("ChildIo::pair, no stdout¬");
  let wrapper = Arc::new(Mutex::new(ChildWrapper {
    reported: false,
    desc,
    child: input,
  }));
  (ChildIo { rw: stdin,  child: wrapper.clone() },
   ChildIo { rw: stdout, child: wrapper         })
}

#[throws(io::Error)]
pub fn run_pair(mut cmd: process::Command, desc: String)
                -> (ChildIo<ChildStdin>, ChildIo<ChildStdout>) {
  cmd.stdin (Stdio::piped());
  cmd.stdout(Stdio::piped());
  new_pair(cmd.spawn()?, desc)
}

impl Drop for ChildWrapper {
  fn drop(&mut self) {
    use nix::sys::signal::{self, Signal::*};
    use nix::unistd::Pid;

    if let Err(e) = (||{
      let es = match self.child.try_wait().context("wait")? {
        Some(es) => es,
        None => {
          let pid = self.child.id();
          let pid = pid.try_into()
            .map_err(|_| anyhow!("pid {:?} out of range!", pid))?;
          let pid = Pid::from_raw(pid);
          signal::kill(pid, SIGTERM).context("kill")?;
          self.child.wait().context("wait after kill")?
        },
      };
      if ! self.reported && ! es.success()
      && es.signal() != Some(SIGPIPE as _) {
        warn!("{} failed: {}", &self, es);
      }
      Ok::<_,AE>(())
    })() {
      warn!("{} cleanup failed: {}", &self, e);
    }
  }
}

impl<R> Read for ChildIo<R> where R: Read {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    let r = self.rw.read(buf);
    self.rw_result(r)
  }
}

impl<W> Write for ChildIo<W> where W: Write {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    let r = self.rw.write(buf);
    self.rw_result(r)
  }
  fn flush(&mut self) -> io::Result<()> {
    let r = self.rw.flush();
    self.rw_result(r.map(|()|0)).map(|_|())
  }
}
