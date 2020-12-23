// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use anyhow::{anyhow, Context};
pub use fehler::{throw, throws};
pub use structopt::StructOpt;
pub use void::Void;

pub use std::env;
pub use std::fs;
pub use std::io::{BufRead, BufReader, ErrorKind, Write};
pub use std::net::TcpStream;
pub use std::os::unix::process::CommandExt;
pub use std::os::unix::fs::DirBuilderExt;
pub use std::os::linux::fs::MetadataExt; // todo why linux for st_mode??
pub use std::process::{Command, Stdio};

pub type AE = anyhow::Error;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
struct Opts {
  #[structopt(long="--no-bwrap")]
  no_bwrap: bool,

  #[structopt(long="--tmp-dir", default_value="tmp")]
  tmp_dir: String,
}

#[derive(Debug,Clone)]
pub struct Setup {
  tmp: String,
}

mod cleanup_notify {
  use anyhow::Context;
  use fehler::{throw, throws};
  use libc::_exit;
  use nix::{unistd::*, fcntl::OFlag};
  use nix::sys::signal::*;
  use void::Void;
  use std::io;
  use std::os::unix::io::RawFd;
  use std::panic::catch_unwind;
  use std::process::Command;
  type AE = anyhow::Error;

  pub struct Handle(RawFd);

  #[throws(AE)]
  fn mkpipe() -> (RawFd,RawFd) {
    pipe2(OFlag::O_CLOEXEC)?
  }

  #[throws(io::Error)]
  fn read_await(fd: RawFd) {
    loop {
      let mut buf = [0u8; 1];
      match nix::unistd::read(fd, &mut buf) {
        Ok(0) => break,
        Ok(_) => throw!(io::Error::from_raw_os_error(libc::EINVAL)),
        Err(nix::Error::Sys(nix::errno::Errno::EINTR)) => continue,
        _ => throw!(io::Error::last_os_error()),
      }
    }
  }

  fn nix2io(_n: nix::Error) -> io::Error {
    io::Error::last_os_error()
  }

  impl Handle {
    #[throws(AE)]
    pub fn new() -> Self {
      let (reading_end, _writing_end) = mkpipe()
        .context("create cleanup notify pipe")?;
      // we leak the writing end, keeping it open only in this process
      Handle(reading_end)
    }

    #[throws(AE)]
    pub fn arm_hook(&self, cmd: &mut Command) { unsafe {
      use std::os::unix::process::CommandExt;

      let (reading_end, writing_end) = mkpipe()
        .context("create permission to carry on pipe")?;

      let notify_writing_end = self.0;
      let all_signals = nix::sys::signal::SigSet::all();

      cmd.pre_exec(move || -> Result<(), io::Error> {
        let semidaemon = nix::unistd::getpid();

        match fork().map_err(nix2io)? {
          ForkResult::Child => {
            let _ = catch_unwind(move || -> Void {
              let _ = sigprocmask(
                SigmaskHow::SIG_BLOCK,
                Some(&all_signals),
                None
              );

              let _ = close(writing_end);
              let _ = read_await(notify_writing_end);
              let _ = kill(semidaemon, SIGTERM);
              _exit(0);
            });
            let _ = raise(SIGABRT);
            _exit(127);
          },
          ForkResult::Parent{..} => {
            // parent
            close(writing_end).map_err(nix2io)?;
            read_await(reading_end)?;
          },
        };

        Ok(())
      });
    } }
  }
}

#[throws(AE)]
fn reinvoke_via_bwrap(_opts: &Opts, current_exe: &str) -> Void {
  println!("running bwrap");
  
  let mut bcmd = Command::new("bwrap");
  bcmd
    .args("--unshare-net \
           --dev-bind / / \
           --tmpfs /tmp \
           --die-with-parent".split(" "))
    .arg(current_exe)
    .arg("--no-bwrap")
    .args(env::args_os().skip(1));

  std::io::stdout().flush().context("flush stdout")?;
  let e : AE = bcmd.exec().into();
  throw!(e.context("exec bwrap"));
}

#[throws(AE)]
fn prepare_tmpdir(opts: &Opts, current_exe: &str) -> String {
  (||{
    match fs::metadata(&opts.tmp_dir) {
      Ok(m) => {
        if !m.is_dir() {
          throw!(anyhow!("existing object is not a directory"));
        }
        if (m.st_mode() & 0o01002) != 0 {
          throw!(anyhow!(
            "existing directory mode {:#o} is sticky or world-writeable. \
             We use predictable pathnames so that would be a tmp race",
            m.st_mode()
          ));
        }
      }
      Err(e) if e.kind() == ErrorKind::NotFound => {
        fs::create_dir(&opts.tmp_dir)
          .context("create")?;
      }
      Err(e) => {
        let e : AE = e.into();
        throw!(e.context("stat existing directory"))
      }
    }

    env::set_current_dir(&opts.tmp_dir)
      .context("chdir into it")?;

    Ok::<_,AE>(())
  })()
    .with_context(|| opts.tmp_dir.to_owned())
    .context("prepare/create tmp-dir")?;

  let leaf = current_exe.rsplitn(2, '/').next().unwrap();
  let our_tmpdir = format!("{}/{}", &opts.tmp_dir, &leaf);
  (||{
    match fs::remove_dir_all(&leaf) {
      Ok(()) => {},
      Err(e) if e.kind() == ErrorKind::NotFound => {},
      Err(e) => throw!(AE::from(e).context("remove previous directory")),
    };
    fs::DirBuilder::new().mode(0o700).create(&leaf)
      .context("create fresh subdirectory")?;
    Ok::<_,AE>(())
  })()
    .with_context(|| our_tmpdir.to_owned())
    .context("prepare/create our tmp subdir")?;

  our_tmpdir
}

#[throws(AE)]
fn fork_something_which_prints(mut cmd: Command,
                               cln: &cleanup_notify::Handle)
                               -> String
{
  cmd.stdout(Stdio::piped());
  cln.arm_hook(&mut cmd)?;
  let mut child = cmd.spawn().context("spawn")?;
  let mut report = BufReader::new(child.stdout.take().unwrap()).lines();

  let l = report.next();

  let s = child.try_wait().context("check on spawned child")?;
  if let Some(e) = s {
    throw!(anyhow!("failed to start: wait status = {}", &e));
  }

  let l = match l {
    Some(Ok(l)) => l,
    None => throw!(anyhow!("EOF (but it's still running?")),
    Some(Err(e)) => throw!(AE::from(e).context("failed to read")),
  };

  l
}

#[throws(AE)]
fn prepare_xserver(cln: &cleanup_notify::Handle) {
  const DISPLAY : u16 = 12;

  let mut xcmd = Command::new("Xvfb");
  xcmd
    .args("-nolisten unix \
           -nolisten local \
           -listen inet \
           -listen inet6 \
           -terminate \
           -wr \
           -displayfd 1".split(' '))
    .arg(format!(":{}", DISPLAY));

  let l = fork_something_which_prints(xcmd, cln).context("Xvfb")?;

  if l != DISPLAY.to_string() {
    throw!(anyhow!(
      "Xfvb said {:?}, expected {:?}",
      l, DISPLAY
    ));
  }

  let display = format!("[::1]:{}", DISPLAY);
  env::set_var("DISPLAY", &display);

  // Doesn't do IPv6 ??
  let v4display = format!("127.0.0.1:{}", DISPLAY);
  let (xconn, _) = x11rb::connect(Some(&v4display))
    .context("make keepalive connection to X server")?;

  // Sadly, if we die between spawning Xfvb, and here, we will
  // permanently leak the whole Xfvb process (and the network
  // namespace).  There doesn't seem to a way to avoid this without
  // editing Xvfb,

  Box::leak(Box::new(xconn));
}

#[throws(AE)]
fn prepare_geckodriver(cln: &cleanup_notify::Handle) {
  const EXPECTED : &str = "Listening on 127.0.0.1:4444";
  let cmd = Command::new("geckodriver");
  let l = fork_something_which_prints(cmd, cln).context("geckodriver")?;
  let fields : Vec<_> = l.split('\t').skip(2).take(2).collect();
  let expected = ["INFO", EXPECTED];
  if fields != expected {
    throw!(anyhow!("geckodriver did not report as expected \
                    - got {:?}, expected {:?}",
                   fields, &expected));
  }
}

#[throws(AE)]
pub fn setup() -> Setup {
  let current_exe : String = env::current_exe()
    .context("find current executable")?
    .to_str()
    .ok_or_else(|| anyhow!("current executable path is not UTF-8 !"))?
    .to_owned();

  let opts = Opts::from_args();
  if !opts.no_bwrap {
    reinvoke_via_bwrap(&opts, &current_exe)
      .context("reinvoke via bwrap")?;
  }

  let cln = cleanup_notify::Handle::new()?;

  let tmp = prepare_tmpdir(&opts, &current_exe)?;

  prepare_xserver(&cln).context("setup X server")?;
  prepare_geckodriver(&cln).context("setup webdriver serverr")?;

  Setup {
    tmp,
  }
}
