// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub mod imports {
  pub use otter;
  pub use otter::imports::*;

  pub use humantime;
}

pub use imports::*;

pub use anyhow::{anyhow, ensure, Context};

pub use boolinator::Boolinator;
pub use fehler::{throw, throws};
pub use if_chain::if_chain;
pub use log::{debug, error, info, trace, warn};
pub use log::{log, log_enabled};
pub use nix::unistd::LinkatFlags;
pub use num_traits::NumCast;
pub use num_derive::FromPrimitive;
pub use parking_lot::{Mutex, MutexGuard};
pub use regex::{Captures, Regex};
pub use serde::{Serialize, Deserialize};
pub use structopt::StructOpt;
pub use strum::{EnumIter, EnumProperty, IntoEnumIterator, IntoStaticStr};
pub use void::Void;

pub use std::env;
pub use std::fmt::{self, Debug};
pub use std::collections::hash_map::HashMap;
pub use std::collections::btree_set::BTreeSet;
pub use std::convert::TryInto;
pub use std::fs;
pub use std::io::{self, BufRead, BufReader, ErrorKind, Write};
pub use std::iter;
pub use std::mem;
pub use std::net::TcpStream;
pub use std::ops::Deref;
pub use std::os::unix::process::CommandExt;
pub use std::os::unix::fs::DirBuilderExt;
pub use std::os::linux::fs::MetadataExt; // todo why linux for st_mode??
pub use std::path;
pub use std::process::{self, Command, Stdio};
pub use std::thread::{self, sleep};
pub use std::time::{self, Duration};

pub use otter_base::misc::default;

pub use otter::ensure_eq;
pub use otter::commands::{MgmtCommand, MgmtResponse};
pub use otter::commands::{MgmtGameInstruction, MgmtGameResponse};
pub use otter::commands::{MgmtGameUpdateMode};
pub use otter::gamestate::{self, Generation, PlayerId};
pub use otter::global::InstanceName;
pub use otter::mgmtchannel::MgmtChannel;
pub use otter::slotmap_slot_idx::KeyDataExt;
pub use otter::spec::{Coord, GameSpec, Pos, PosC};
pub use otter::toml_de;
pub use otter::ui::{AbbrevPresentationLayout, PresentationLayout};
pub use otter::ui::player_num_dasharray;

pub const MS: time::Duration = time::Duration::from_millis(1);
pub type AE = anyhow::Error;

pub const URL: &str = "http://localhost:8000";

use otter::config::DAEMON_STARTUP_REPORT;

pub const TABLE: &str = "server::dummy";
pub const CONFIG: &str = "server-config.toml";

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(FromPrimitive,EnumIter,IntoStaticStr,EnumProperty)]
#[strum(serialize_all = "snake_case")]
pub enum StaticUser {
  #[strum(props(Token="kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe"))] Alice,
  #[strum(props(Token="ccg9kzoTh758QrVE1xMY7BQWB36dNJTx"))] Bob,
}

pub trait AlwaysContext<T,E> {
  fn always_context(self, msg: &'static str) -> anyhow::Result<T>;
}

impl<T,E> AlwaysContext<T,E> for Result<T,E>
where Self: anyhow::Context<T,E>,
{
  fn always_context(self, msg: &'static str) -> anyhow::Result<T> {
    let x = self.context(msg);
    if x.is_ok() { info!("completed {}.", msg) };
    x
  }
}

pub trait JustWarn<T> {
  fn just_warn(self) -> Option<T>;
}

impl<T> JustWarn<T> for Result<T,AE> {
  fn just_warn(self) -> Option<T> {
    match self {
      Ok(x) => Some(x),
      Err(e) => {
        warn!("{:#}", e);
        None
      },
    }
  }
}

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(long="--as-if")]
  pub as_if: Option<String>,

  #[structopt(long="--no-bwrap")]
  pub no_bwrap: bool,

  #[structopt(long="--tmp-dir", default_value="tmp")]
  pub tmp_dir: String,

  #[structopt(long="--pause", default_value="0ms")]
  pub pause: humantime::Duration,

  #[structopt(flatten)]
  pub tests: WantedTestsOpt,
}

pub struct SetupCore {
  pub ds: DirSubst,
  pub mgmt_conn: MgmtChannel,
  pub server_child: process::Child,
  pub wanted_tests: TrackWantedTests,
}

#[derive(Clone,Debug)]
#[derive(StructOpt)]
pub struct WantedTestsOpt {
  tests: Vec<String>,
}

#[derive(Debug)]
pub struct TrackWantedTests {
  wanted: WantedTestsOpt,
  found: BTreeSet<String>,
}

impl WantedTestsOpt {
  pub fn track(&self) -> TrackWantedTests {
    TrackWantedTests { wanted: self.clone(), found: default() }
  }
}

impl TrackWantedTests {
  pub fn wantp(&mut self, tname: &str) -> bool {
    self.found.insert(tname.to_owned());
    let y =
      self.wanted.tests.is_empty() ||
      self.wanted.tests.iter().any(|s| s==tname);
    y
  }
}

impl Drop for TrackWantedTests {
  fn drop(&mut self) {
    let missing_tests = self.wanted.tests.iter().cloned()
      .filter(|s| !self.found.contains(s))
      .collect::<Vec<_>>();

    if !missing_tests.is_empty() && !self.found.is_empty() {
      for f in &self.found {
        eprintln!("fyi: test that exists: {}", f);
      }
      for m in &missing_tests {
        eprintln!("warning: unknown test requested: {}", m);
      }
    }
  }
}

#[derive(Clone,Debug)]
pub struct DirSubst {
  pub tmp: String,
  pub abstmp: String,
  pub start_dir: String,
  pub src: String,
}

pub struct Instance(pub InstanceName);

#[derive(Clone,Debug)]
pub struct Subst(HashMap<String,String>);

#[derive(Clone,Debug)]
pub struct ExtendedSubst<B: Substitutor, X: Substitutor>(B, X);

impl<'i,
     T: AsRef<str> + 'i,
     U: AsRef<str> + 'i,
     L: IntoIterator<Item=&'i (T, U)>>
  From<L> for Subst
{
  fn from(l: L) -> Subst {
    let map = l.into_iter()
      .map(|(k,v)| (k.as_ref().to_owned(), v.as_ref().to_owned())).collect();
    Subst(map)
  }
}

pub trait Substitutor {
  fn get(&self, kw: &str) -> Option<String>;

  fn also<L: Into<Subst>>(&self, xl: L) -> ExtendedSubst<Self, Subst>
  where Self: Clone + Sized {
    ExtendedSubst(self.clone(), xl.into())
  }

  #[throws(AE)]
  fn subst<S: AsRef<str>>(&self, s: S) -> String 
  where Self: Sized {
    #[throws(AE)]
    fn inner(self_: &dyn Substitutor, s: &dyn AsRef<str>) -> String {
      let s = s.as_ref();
      let re = Regex::new(r"@(\w+)@").expect("bad re!");
      let mut errs = vec![];
      let out = re.replace_all(s, |caps: &regex::Captures| {
        let kw = caps.get(1).expect("$1 missing!").as_str();
        if kw == "" { return "".to_owned() }
        let v = self_.get(kw);
        v.unwrap_or_else(||{
          errs.push(kw.to_owned());
          "".to_owned()
        })
      });
      if ! errs.is_empty() {
        throw!(anyhow!("bad substitution(s) {:?} in {:?}",
                       &errs, s));
      }
      out.into()
    }
    inner(self, &s)?
  }

  #[throws(AE)]
  fn ss(&self, s: &str) -> Vec<String> 
  where Self: Sized {
    self.subst(s)?
      .trim()
      .split(' ')
      .filter(|s| !s.is_empty())
      .map(str::to_string)
      .collect()
  }
}

impl Substitutor for Subst {
  fn get(&self, kw: &str) -> Option<String> {
    self.0.get(kw).map(String::clone)
  }
}

impl<B:Substitutor, X:Substitutor> Substitutor for ExtendedSubst<B, X> {
  fn get(&self, kw: &str) -> Option<String> {
    self.1.get(kw).or_else(|| self.0.get(kw))
  }
}

impl Substitutor for DirSubst {
  fn get(&self, kw: &str) -> Option<String> {
    Some(match kw {
      "url"    => URL.to_owned(),
      "src"    => self.src.clone(),
      "build"  => self.start_dir.clone(),
      "abstmp" => self.abstmp.clone(),
      "target" => format!("{}/target", &self.start_dir),
      "specs"  => self.specs_dir(),
      _ => return None,
    })
  }
}

pub mod cleanup_notify {
  use super::imports::*;
  use super::AE;

  use anyhow::Context;
  use fehler::{throw, throws};
  use libc::_exit;
  use nix::errno::Errno::*;
  use nix::{unistd::*, fcntl::OFlag};
  use nix::sys::signal::*;
  use nix::Error::Sys;
  use void::Void;
  use std::io;
  use std::os::unix::io::RawFd;
  use std::panic::catch_unwind;
  use std::process::Command;

  pub struct Handle(RawFd);

  #[throws(io::Error)]
  fn mkpipe() -> (RawFd,RawFd) {
    pipe2(OFlag::O_CLOEXEC).map_err(nix2io)?
  }

  #[throws(io::Error)]
  fn read_await(fd: RawFd) {
    loop {
      let mut buf = [0u8; 1];
      match nix::unistd::read(fd, &mut buf) {
        Ok(0) => break,
        Ok(_) => throw!(io::Error::from_raw_os_error(libc::EINVAL)),
        Err(Sys(EINTR)) => continue,
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

      let notify_writing_end = self.0;
      let all_signals = nix::sys::signal::SigSet::all();

      cmd.pre_exec(move || -> Result<(), io::Error> {
        let semidaemon = nix::unistd::getpid();
        let (reading_end, writing_end) = mkpipe()?;

        match fork().map_err(nix2io)? {
          ForkResult::Child => {
            let _ = catch_unwind(move || -> Void {
              let _ = sigprocmask(
                SigmaskHow::SIG_BLOCK,
                Some(&all_signals),
                None
              );

              let _ = close(writing_end);
              let _ = nix::unistd::dup2(2, 1);

              for fd in 2.. {
                if fd == notify_writing_end { continue }
                let r = close(fd);
                if fd >= writing_end && matches!(r, Err(Sys(EBADF))) {
                  break;
                }                  
              }
              let _ = read_await(notify_writing_end);
              let _ = kill(semidaemon, SIGTERM);
              let _ = kill(semidaemon, SIGCONT);
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
pub fn reinvoke_via_bwrap(_opts: &Opts, current_exe: &str) -> Void {
  debug!("running bwrap");
  
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
  let e: AE = bcmd.exec().into();
  throw!(e.context("exec bwrap"));
}

#[throws(AE)]
pub fn prepare_tmpdir<'x>(opts: &'x Opts, mut current_exe: &'x str) -> DirSubst {
  #[throws(AE)]
  fn getcwd() -> String {
    env::current_dir()
      .context("getcwd")?
      .to_str()
      .ok_or_else(|| anyhow!("path is not UTF-8"))?
      .to_owned()
  }

  if let Some(as_if) = &opts.as_if {
    current_exe = as_if;
  }

  let start_dir = getcwd()
    .context("canonicalise our invocation directory (getcwd)")?;

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
        let e: AE = e.into();
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

    fs::DirBuilder::new().create(&leaf)
      .context("create fresh subdirectory")?;

    env::set_current_dir(&leaf)
      .context("chdir into it")?;

    Ok::<_,AE>(())
  })()
    .with_context(|| our_tmpdir.to_owned())
    .context("prepare/create our tmp subdir")?;

  let abstmp =
    getcwd().context("canonicalise our tmp subdir (getcwd)")?;

  env::set_var("HOME", &abstmp);
  env::set_var("TMPDIR", &abstmp);
  for v in "http_proxy https_proxy XAUTHORITY CDPATH \
            SSH_AGENT_PID SSH_AUTH_SOCK WINDOWID WWW_HOME".split(' ')
  {
    env::remove_var(v);
  }

  let manifest_var = "CARGO_MANIFEST_DIR";
  let src: String = (|| Ok::<_,AE>(match env::var(manifest_var) {
    Ok(dir) => dir.into(),
    Err(env::VarError::NotPresent) => start_dir.clone(),
    e@ Err(_) => throw!(e.context(manifest_var).err().unwrap()),
  }))()
    .context("find source code")?;

  DirSubst {
    tmp: our_tmpdir,
    abstmp,
    src,
    start_dir,
  }
}

#[throws(AE)]
pub fn fork_something_which_prints(mut cmd: Command,
                               cln: &cleanup_notify::Handle,
                               what: &str)
                               -> (String, process::Child)
{
  (||{
    cmd.stdout(Stdio::piped());
    cln.arm_hook(&mut cmd)?;
    let mut child = cmd.spawn().context("spawn")?;
    let mut report = BufReader::new(child.stdout.take().unwrap())
      .lines().fuse();

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

    let what = what.to_owned();
    thread::spawn(move|| (||{
      for l in report {
        let l: Result<String, io::Error> = l;
        let l = l.context("reading further output")?;
        const MAXLEN: usize = 300;
        if l.len() <= MAXLEN {
          println!("{} {}", what, l);
        } else {
          println!("{} {}...", what, &l[..MAXLEN-3]);
        }
      }
      Ok::<_,AE>(())
    })().context(what).just_warn()
    );

    Ok::<_,AE>((l, child))
  })().with_context(|| what.to_owned())?
}

#[throws(AE)]
pub fn prepare_gameserver(cln: &cleanup_notify::Handle, ds: &DirSubst)
                      -> (MgmtChannel, process::Child) {
  let subst = ds.also(&[
    ("command_socket", "command.socket"),
  ]);
  let config = subst.subst(r##"
change_directory = "@abstmp@"
base_dir = "@build@"
public_url = "@url@"

save_dir = "."
command_socket = "@command_socket@"
template_dir = "@src@/templates"
nwtemplate_dir = "@src@/nwtemplates"
bundled_sources = "@target@/bundled-sources"
wasm_dir = "@target@/packed-wasm"
shapelibs = [ "@src@/library/*.toml" ]

debug_js_inject_file = "@src@/templates/log-save.js"
check_bundled_sources = false # For testing only! see LICENCE!

[log]
global_level = 'debug'

[log.modules]
rocket = 'error'
_ = "error" # rocket
# ^ comment these two out to see Tera errors, *sigh*

'hyper::server' = 'info'
"game::debugreader" = 'info'
"game::updates" = 'trace'
"##)?;

  fs::write(CONFIG, &config)
    .context(CONFIG).context("create server config")?;

  let server_exe = ds.subst("@target@/debug/daemon-otter")?;
  let mut cmd = Command::new(&server_exe);
  cmd
    .arg("--report-startup")
    .arg(CONFIG);

  let child = (||{
    let (l,child) = fork_something_which_prints(cmd, cln, &server_exe)?;
    if l != DAEMON_STARTUP_REPORT {
      throw!(anyhow!("otter-daemon startup report {:?}, expected {:?}",
                     &l, DAEMON_STARTUP_REPORT));
    }
    Ok::<_,AE>(child)
  })()
    .context("game server")?;

  let mut mgmt_conn = MgmtChannel::connect(
    &subst.subst("@command_socket@")?
  )?;

  mgmt_conn.cmd(&MgmtCommand::SetSuperuser(true))?;
  mgmt_conn.cmd(&MgmtCommand::SelectAccount("server:".parse()?))?;  

  (mgmt_conn, child)
}

impl DirSubst {
  pub fn specs_dir(&self) -> String {
    format!("{}/specs" , &self.src)
  }

  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&self, xargs: &[S]) {
    let ds = self;
    let exe = ds.subst("@target@/debug/otter")?;
    let mut args: Vec<&str> = vec![];
    args.extend(&["--config", CONFIG]);
    args.extend(xargs.iter().map(AsRef::as_ref));
    let dbg = format!("running {} {:?}", &exe, &args);
    debug!("{}", &dbg);
    (||{
      let mut cmd = Command::new(&exe);
      cmd.args(&args);
      let st = cmd
        .spawn().context("spawn")?
        .wait().context("wait")?;
      if !st.success() {
        throw!(anyhow!("wait status {}", &st));
      }
      Ok::<_,AE>(())
    })()
      .context(dbg)
      .context("run otter client")?;
  }

  #[throws(AE)]
  pub fn game_spec_path(&self) -> String {
    self.subst("@specs@/demo.game.toml")?
  }

  #[throws(AE)]
  pub fn game_spec_data(&self) -> GameSpec {
    let path = self.game_spec_path()?;
    (||{
      let data = fs::read(&path).context("read")?;
      let data = std::str::from_utf8(&data).context("convert from UTF-8")?;
      let data: toml::Value = data.parse().context("parse TOM")?;
      dbg!(&data);
      let data = toml_de::from_value(&data).context("interperet TOML")?;
      Ok::<_,AE>(data)
    })()
      .context(path)
      .context("game spec")?
  }
}

#[throws(AE)]
pub fn prepare_game(ds: &DirSubst, table: &str) -> InstanceName {
  let game_spec = ds.game_spec_path()?;
  let subst = ds.also(&[
    ("table",     table),
    ("game_spec", &game_spec),
  ]);
  ds.otter(&subst.ss(
    "--account server:                                  \
     reset                                              \
     --reset-table @specs@/test.table.toml              \
                   @table@ @game_spec@ \
    ")?).context("reset table")?;

  let instance: InstanceName = table.parse()
    .with_context(|| table.to_owned())
    .context("parse table name")?;

  instance
}

#[throws(AE)]
pub fn setup_core<O>(module_paths: &[&str]) ->
  (O, cleanup_notify::Handle, Instance, SetupCore)
  where O: StructOpt + AsRef<Opts>
{
  let mut builder = env_logger::Builder::new();
  builder
    .format_timestamp_micros()
    .format_level(true)
    .filter_module("otter_apitest_tests", log::LevelFilter::Debug);

  for module in module_paths {
    builder
      .filter_module(module, log::LevelFilter::Debug);
  }

  builder
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTER_WDT_LOG")
    .init();
  debug!("starting");

  let caller_opts = O::from_args();
  let opts = caller_opts.as_ref();

  let current_exe: String = env::current_exe()
    .context("find current executable")?
    .to_str()
    .ok_or_else(|| anyhow!("current executable path is not UTF-8 !"))?
    .to_owned();

  if !opts.no_bwrap {
    reinvoke_via_bwrap(&opts, &current_exe)
      .context("reinvoke via bwrap")?;
  }

  info!("pid = {}", nix::unistd::getpid());
  sleep(opts.pause.into());

  let cln = cleanup_notify::Handle::new()?;
  let ds = prepare_tmpdir(&opts, &current_exe)?;

  let (mgmt_conn, server_child) =
    prepare_gameserver(&cln, &ds).always_context("setup game server")?;

  let instance_name =
    prepare_game(&ds, TABLE).context("setup game")?;

  let wanted_tests = opts.tests.track();

  (caller_opts,
   cln,
   Instance(
     instance_name
   ),
   SetupCore {
     ds,
     mgmt_conn,
     server_child,
     wanted_tests,
   })
}

#[derive(Debug)]
pub struct Window {
  pub name: String,
  pub instance: InstanceName,
}

impl Window {
  pub fn table(&self) -> String { self.instance.to_string() }
}

#[macro_export]
macro_rules! test {
  ($c:expr, $tname:expr, $s:stmt) => {
    if $c.su.want_test($tname) {
      debug!("-------------------- {} starting --------------------", $tname);
      $s
      info!("-------------------- {} completed --------------------", $tname);
    } else {
      trace!("- - - {} skipped - - -", $tname);
    }
  }
}
