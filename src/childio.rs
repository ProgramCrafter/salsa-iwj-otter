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

impl DebugIdentify for ChildWrapper {
  #[throws(fmt::Error)]
  fn debug_identify(&self, f: &mut fmt::Formatter) {
    write!(f, "ChildWrapper([{}] {})", self.child.id(), &self.desc)?;
  }
  #[throws(fmt::Error)]
  fn debug_identify_type(f: &mut fmt::Formatter) {
    write!(f, "ChildWrapper")?;
  }
}

impl<RW> ChildIo<RW> {
  fn rw_result(&self, eofblock: bool, r: io::Result<usize>)
               -> io::Result<usize>
  {
    let block = match &r {
      &Ok(v) if v == 0 => eofblock,
      &Ok(_)           => return r,
      Err(_)           => false,
    };
    let status = {
      let mut child = self.child.lock();
      let child = &mut child.child;
      if block { Some(child.wait()?) } else { child.try_wait()? }
    };
    match status {
      None => r,
      Some(es) if es.success() => r,
      Some(es) => {
        let mut child = self.child.lock();
        child.reported = true;
        let ae = anyhow!("{}: failed: {}", &child, es);
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
          let mut es = self.child.wait().context("wait after kill")?;
          if es.signal() == Some(SIGTERM as _) {
            es = process::ExitStatus::from_raw(0);
          }
          es
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
    self.rw_result(true,r)
  }
}

impl<W> Write for ChildIo<W> where W: Write {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    let r = self.rw.write(buf);
    self.rw_result(false,r)
  }
  fn flush(&mut self) -> io::Result<()> {
    let r = self.rw.flush();
    self.rw_result(false, r.map(|()|0) ).map(|_|())
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

    let w = capture_warns::run(&||{
      let (_w, _r) = setup();
    });
    assert_eq!( &w, &[] as &[String] );
  }

  static STATUS_1: &str = "exit status: 1";

  fn assert_is_status_1(e: &io::Error) {
    assert_eq!( e.kind(), ErrorKind::Other );
    let es = e.to_string();
    assert!( es.ends_with(STATUS_1), "actually {:?}", es );
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
      assert_is_status_1(&e);
    };

    one(&|_w, r|{
      let mut buf = [0;10];
      dbgc!( r.read(&mut buf).map(|_|()) )
    });

    let lose_race = |w: &mut Stdin| {
      w.child.lock().child.wait().unwrap();
    };

    one(&|w, _r|{
      // make sure we will get EPIPE
      lose_race(w);
      dbgc!( write!(w, "hi") )
    });

    let w = capture_warns::run(&||{
      let (mut w, _r) = setup();
      lose_race(&mut w);
    });
    dbgc!(&w);
    assert_eq!( w.len(), 1 );
    assert!( w[0].ends_with(STATUS_1) );
  }


  #[test]
  fn t_like_linux_wtf() {
    let mut c = Command::new("sh");
    c.args(&["-ec", "exec >/dev/null; sleep 0.1; exit 1"]);
    let (_w, mut r) = run_pair(c, "sh".to_owned()).unwrap();
    let mut buf = [0;10];
    let e = dbgc!( r.read(&mut buf) ).unwrap_err();
    assert_is_status_1(&e);
  }
}
