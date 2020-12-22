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
fn prepare_xserver() {
  const DISPLAY : &str = "12";

  let mut xcmd = Command::new("Xvfb");
  xcmd
    .args("-nolisten unix \
           -nolisten local \
           -listen inet6 \
           -noreset \
           -displayfd 1".split(' '))
    .arg(format!(":{}", DISPLAY))
    .stdout(Stdio::piped());
  let mut child = xcmd.spawn()
    .context("spawn Xvfb")?;
  let mut report = BufReader::new(child.stdout.take().unwrap()).lines();

  let l = report.next();

  let s = child.try_wait().context("check on Xvfb")?;
  if let Some(e) = s {
    throw!(anyhow!("Xvfb failed to start: wait status = {}", &e));
  }

  match l {
    Some(Ok(l)) if l == DISPLAY => { l },
    Some(Ok(l)) => throw!(anyhow!(
      "Xfvb said {:?}, expected {:?}",
      l, DISPLAY
    )),
    None => throw!(anyhow!("EOF from Xvfb (but it's still running?")),
    Some(Err(e)) => throw!(AE::from(e).context("failed to read from Xfvb")),
  };

  env::set_var("DISPLAY", format!("[::1]:{}", DISPLAY));
  
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

  let tmp = prepare_tmpdir(&opts, &current_exe)?;

  prepare_xserver().context("setup X server")?;

  Setup {
    tmp,
  }
}
