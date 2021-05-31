// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

use std::process;

#[derive(Debug)]
pub struct ChildIo<RW> {
  rw: RW,
  child: Arc<Mutex<ChildWrapper>>,
}

pub type Stdin  = ChildIo<process::ChildStdin >;
pub type Stdout = ChildIo<process::ChildStdout>;
pub type Pair   = (Stdin /*w*/, Stdout /*r*/);

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

pub fn new_pair(mut input: process::Child, desc: String) -> Pair {
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
pub fn run_pair(mut cmd: process::Command, desc: String) -> Pair {
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

#[cfg(test)]
#[cfg(not(miri))]
use crate::capture_warns_warn as warn;

#[cfg(test)]
#[cfg(not(miri))]
pub mod test {
use crate::prelude::*;
use super::*;

pub mod capture_warns {
  use crate::prelude::*;
  use std::cell::RefCell;

  thread_local! {
    pub static WARNINGS: RefCell<Option<Vec<String>>> = RefCell::new(None)
  }

  #[macro_export]
  macro_rules! capture_warns_warn {
    { $fmt:literal $($rhs:tt)* } => {
      $crate::childio::test::capture_warns::WARNINGS.with(|w| {
        let mut w = w.borrow_mut();
        if let Some(ref mut msgs) = *w {
          let s = format!($fmt $($rhs)*);
          msgs.push(s);
        } else {
          crate::prelude::warn!($fmt $($rhs)*);
        }
      });
    }
  }

  pub fn run(f: &dyn Fn()) -> Vec<String> {
    WARNINGS.with(|w| {
      let mut w =w.borrow_mut();
      *w = Some(vec![]);
    });
    f();
    WARNINGS.with(|w| {
      let mut w =w.borrow_mut();
      mem::take(&mut *w).unwrap()
    })
  }
}

#[test]
fn t_cat() {
  let setup = ||{
    let c = Command::new("cat");
    run_pair(c, "cat".into()).unwrap()
  };

  {
    let (mut w, mut r) = setup();
    assert_eq!( write!(w, "hi").unwrap(), () );
    assert_eq!( w.flush()      .unwrap(), () );
    let mut buf = [0;10];
    assert_eq!( r.read(&mut buf).unwrap(), 2 );
    assert_eq!(&buf[0..2], b"hi");
  }
}

#[test]
fn t_false() {
  let setup = ||{
    let c = Command::new("false");
    run_pair(c, "cat".into()).unwrap()
  };

  let one = | f: &dyn Fn(&mut Stdin, &mut Stdout) -> io::Result<()> |{
    let (mut w, mut r) = setup();

    let r = f(&mut w, &mut r);
    let e = r.unwrap_err();
    assert_eq!( e.kind(), ErrorKind::Other );
    let es = e.to_string();
    assert!( es.ends_with("exit status: 1"), "actually {:?}", es );
  };

  one(&|_w, r|{
    let mut buf = [0;10];
    r.read(&mut buf).map(|_|())
  });

  let lose_race = |w: &mut Stdin| {
    w.child.lock().child.wait().unwrap();
  };

  one(&|w, _r|{
    // make sure we will get EPIPE
    lose_race(w);
    write!(w, "hi")
  });
}

}
