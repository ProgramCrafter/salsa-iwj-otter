// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![feature(unboxed_closures)]
#![feature(fn_traits)]

pub use anyhow::{anyhow, Context};
pub use boolinator::Boolinator;
pub use fehler::{throw, throws};
pub use log::{debug, error, info, trace, warn};
pub use log::{log, log_enabled};
pub use nix::unistd::LinkatFlags;
pub use num_derive::FromPrimitive;
pub use parking_lot::{Mutex, MutexGuard};
pub use regex::{Captures, Regex};
pub use structopt::StructOpt;
pub use strum::{EnumIter, EnumProperty, IntoEnumIterator, IntoStaticStr};
pub use thirtyfour_sync as t4;
pub use void::Void;

pub use t4::WebDriverCommands;

pub use std::env;
pub use std::fs;
pub use std::collections::hash_map::HashMap;
pub use std::convert::TryInto;
pub use std::io::{BufRead, BufReader, ErrorKind, Write};
pub use std::mem;
pub use std::net::TcpStream;
pub use std::ops::Deref;
pub use std::os::unix::process::CommandExt;
pub use std::os::unix::fs::DirBuilderExt;
pub use std::os::linux::fs::MetadataExt; // todo why linux for st_mode??
pub use std::path;
pub use std::process::{Command, Stdio};
pub use std::thread::sleep;
pub use std::time;

pub use otter::mgmtchannel::MgmtChannel;

use otter::config::DAEMON_STARTUP_REPORT;

pub type T4d = t4::WebDriver;
pub type WDE = t4::error::WebDriverError;

pub const MS : time::Duration = time::Duration::from_millis(1);
pub type AE = anyhow::Error;

pub const URL : &str = "http://localhost:8000";

const CONFIG : &str = "server-config.toml";

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
struct Opts {
  #[structopt(long="--no-bwrap")]
  no_bwrap: bool,

  #[structopt(long="--tmp-dir", default_value="tmp")]
  tmp_dir: String,

  #[structopt(long="--pause", default_value="0ms")]
  pause: humantime::Duration,
}

#[derive(Debug)]
pub struct FinalInfoCollection;

type ScreenShotCount = u32;
type WindowState = Option<String>;

#[derive(Debug)]
pub struct Setup {
  pub ds: DirSubst,
  driver: T4d,
  current_window: WindowState,
  screenshot_count: ScreenShotCount,
  final_hook: FinalInfoCollection,
  windows_squirreled: Vec<String>, // see Drop impl
}

#[derive(Clone,Debug)]
pub struct DirSubst {
  pub tmp: String,
  pub abstmp: String,
  pub start_dir: String,
  pub src: String,
}

#[derive(Clone,Debug)]
pub struct RawSubst(HashMap<String,String>);

#[derive(Clone,Debug)]
pub struct ExtendedSubst<B: Subst, X: Subst>(B, X);

impl<'i,
     T: AsRef<str> + 'i,
     U: AsRef<str> + 'i,
     L: IntoIterator<Item=&'i (T, U)>>
  From<L> for RawSubst
{
  fn from(l: L) -> RawSubst {
    let map = l.into_iter()
      .map(|(k,v)| (k.as_ref().to_owned(), v.as_ref().to_owned())).collect();
    RawSubst(map)
  }
}

pub trait Subst {
  fn get(&self, kw: &str) -> Option<String>;

  fn also<L: Into<RawSubst>>(&self, xl: L) -> ExtendedSubst<Self, RawSubst>
  where Self: Clone + Sized {
    ExtendedSubst(self.clone(), xl.into())
  }

  #[throws(AE)]
  fn subst<S: AsRef<str>>(&self, s: S) -> String 
  where Self: Sized {
    #[throws(AE)]
    fn inner(self_: &dyn Subst, s: &dyn AsRef<str>) -> String {
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
    self.subst(&s)?
      .trim()
      .split(' ')
      .filter(|s| !s.is_empty())
      .map(str::to_string)
      .collect()
  }
}

impl Subst for RawSubst {
  fn get(&self, kw: &str) -> Option<String> {
    self.0.get(kw).map(String::clone)
  }
}

impl<B:Subst, X:Subst> Subst for ExtendedSubst<B, X> {
  fn get(&self, kw: &str) -> Option<String> {
    self.1.get(kw).or_else(|| self.0.get(kw))
  }
}

impl Subst for DirSubst {
  fn get(&self, kw: &str) -> Option<String> {
    Some(match kw {
      "url"    => URL.to_owned(),
      "src"    => self.src.clone(),
      "build"  => self.start_dir.clone(),
      "target" => format!("{}/target", &self.start_dir),
      "specs"  => format!("{}/specs" , &self.src      ),
      _ => return None,
    })
  }
}

mod cleanup_notify {
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
  type AE = anyhow::Error;

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
              for fd in 2.. {
                if fd == notify_writing_end { continue }
                let r = close(fd);
                if fd >= writing_end && matches!(r, Err(Sys(EBADF))) {
                  break;
                }                  
              }
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
  let e : AE = bcmd.exec().into();
  throw!(e.context("exec bwrap"));
}

#[throws(AE)]
fn prepare_tmpdir(opts: &Opts, current_exe: &str) -> DirSubst {
  #[throws(AE)]
  fn getcwd() -> String {
    env::current_dir()
      .context("getcwd")?
      .to_str()
      .ok_or_else(|| anyhow!("path is not UTF-8"))?
      .to_owned()
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
  for v in "http_proxy https_proxy XAUTHORITY CDPATH \
            SSH_AGENT_PID SSH_AUTH_SOCK WINDOWID WWW_HOME".split(' ')
  {
    env::remove_var(v);
  }

  let manifest_var = "CARGO_MANIFEST_DIR";
  let src : String = (|| Ok::<_,AE>(match env::var(manifest_var) {
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
fn prepare_xserver(cln: &cleanup_notify::Handle, ds: &DirSubst) {
  const DISPLAY : u16 = 12;

  let mut xcmd = Command::new("Xvfb");
  xcmd
    .args("-nolisten unix \
           -nolisten local \
           -listen inet \
           -listen inet6 \
           -terminate \
           -retro \
           -displayfd 1".split(' '))
    .args(&["-fbdir", &ds.abstmp])
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
fn prepare_gameserver(cln: &cleanup_notify::Handle, ds: &DirSubst)
                      -> MgmtChannel {
  let subst = ds.also(&[
    ("command_socket", "command.socket"),
  ]);
  let config = subst.subst(r##"
base_dir = "@build@"
public_url = "@url@"

save_dir = "."
command_socket = "@command_socket@"
template_dir = "@src@/templates"
nwtemplate_dir = "@src@/nwtemplates"
bundled_sources = "@target@/bundled-sources"
wasm_dir = "@target@/packed-wasm"
shapelibs = [ "@src@/library/*.toml" ]

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

  let server_exe = ds.subst(&"@target@/debug/daemon-otter")?;
  let mut cmd = Command::new(&server_exe);
  cmd
    .arg("--report-startup")
    .arg(CONFIG);

  (||{
    let l = fork_something_which_prints(cmd, cln)?;
    if l != DAEMON_STARTUP_REPORT {
      throw!(anyhow!("otter-daemon startup report {:?}, expected {:?}",
                     &l, DAEMON_STARTUP_REPORT));
    }
    Ok::<_,AE>(())
  })()
    .context(server_exe).context("game server")?;

  let server_conn = MgmtChannel::connect(
    &subst.subst(&"@command_socket@")?
  )?;

  server_conn
}

impl DirSubst {
  #[throws(AE)]
  pub fn otter<S:AsRef<std::ffi::OsStr>>(&self, args: &[S]) {
    let ds = self;
    let exe = ds.subst(&"@target@/debug/otter")?;
    (||{
      let mut cmd = Command::new(&exe);
      cmd
        .args(&["--config", CONFIG])
        .args(&*args);
      let st = cmd
        .spawn().context("spawn")?
        .wait().context("wait")?;
      if !st.success() {
        throw!(anyhow!("wait status {}", &st));
      }
      Ok::<_,AE>(())
    })()
      .context(exe)
      .context("run otter client")?;
  }
}

#[throws(AE)]
pub fn prepare_game(ds: &DirSubst) {
  ds.otter(&ds.ss(
    "--account server:                                  \
     reset                                              \
     --reset-table @specs@/test.table.toml              \
                   server::dummy @specs@/demo.game.toml \
    ")?).context("reset table")?;
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
fn prepare_thirtyfour() -> (T4d, ScreenShotCount, Vec<String>) {
  let mut count = 0;
  let caps = t4::DesiredCapabilities::firefox();
  let mut driver = t4::WebDriver::new("http://localhost:4444", &caps)
    .context("create 34 WebDriver")?;

  const FRONT : &str = "front";
  let window_names = vec![FRONT.into()];
  driver.set_window_name(FRONT).context("set initial window name")?;
  screenshot(&mut driver, &mut count, "startup")?;
  driver.get(URL).context("navigate to front page")?;
  screenshot(&mut driver, &mut count, "front")?;

  let t = Some(5_000 * MS);
  driver.set_timeouts(t4::TimeoutConfiguration::new(t,t,t))
    .context("set webdriver timeouts")?;

  (driver, count, window_names)
}

#[derive(Debug)]
pub struct Window {
  name: String,
}

pub struct WindowGuard<'g> {
  su: &'g mut Setup,
  w: &'g Window,
}

#[throws(AE)]
fn check_window_name_sanity(name: &str) -> &str {
  let e = || anyhow!("bad window name {:?}", &name);

  name.chars().nth(0).ok_or(e())?
    .is_ascii_alphanumeric().ok_or(e())?;

  name.chars().all(
    |c| c.is_ascii_alphanumeric() || c == '-' || c == '_'
  ).ok_or(e())?;

  name
}

impl Setup {
  #[throws(AE)]
  pub fn new_window<'s>(&'s mut self, name: &str) -> Window {
    let name = check_window_name_sanity(name)?;
    let window = (||{

      self.current_window = None; // we might change the current window

      match self.driver.switch_to().window_name(name) {
        Ok(()) => throw!(anyhow!("window already exists")),
        Err(WDE::NoSuchWindow(_)) |
        Err(WDE::NotFound(..)) => (),
        e@ Err(_) => {
          eprintln!("wot {:?}", &e);
          throw!(e
                            .context("check for pre-existing window")
                            .err().unwrap())
        },
      };

      self.driver.execute_script(&format!(
        r#"window.open('', target='{}');"#,
        name,
      ))
        .context("execute script to create window")?;

      Ok::<_,AE>(Window { name: name.to_owned() })
    })()
      .with_context(|| name.to_owned())
      .context("create window")?;

    self.windows_squirreled.push(name.to_owned());
    window
  }
}

impl Setup {
  #[throws(AE)]
  pub fn w<'s>(&'s mut self, w: &'s Window) -> WindowGuard<'s> {
    if self.current_window.as_ref() != Some(&w.name) {
      self.driver.switch_to().window_name(&w.name)
        .with_context(|| w.name.to_owned())
        .context("switch to window")?;
      self.current_window = Some(w.name.clone());
    }
    WindowGuard { su: self, w }
  }
}

impl<'g> Deref for WindowGuard<'g> {
  type Target = T4d;
  fn deref(&self) -> &T4d { &self.su.driver }
}

pub trait Screenshottable {
  fn screenshot(&mut self, slug: &str) -> Result<(),AE>;
}

impl<'g> Screenshottable for WindowGuard<'g> {
  #[throws(AE)]
  fn screenshot(&mut self, slug: &str) {
    screenshot(&self.su.driver, &mut self.su.screenshot_count,
               &format!("{}-{}", &self.w.name, slug))?
  }
}

#[throws(AE)]
fn screenshot(driver: &T4d, count: &mut ScreenShotCount, slug: &str) {
  let path = format!("{:03}{}.png", count, slug);
  *count += 1;
  driver.screenshot(&path::PathBuf::from(&path))
    .context(path)
    .context("take screenshot")?;
}

impl Drop for FinalInfoCollection {
  fn drop(&mut self) {
    nix::unistd::linkat(None, "Xvfb_screen0",
                        None, "Xvfb_keep.xwd",
                        LinkatFlags::NoSymlinkFollow)
      .context("preserve Xvfb screen")
      .just_warn();
  }
}

impl Drop for Setup {
  fn drop(&mut self) {
    (||{
      for name in mem::take(&mut self.windows_squirreled) {
        // This constructor is concurrency-hazardous.  It's only OK
        // here because we have &mut self.  If there is only one
        // Setup, there can be noone else with a Window with an
        // identical name to be interfered with by us.
        let w = Window { name: name.clone() };
        self.w(&w)?.screenshot("final")
          .context(name)
          .context("final screenshot")
          .just_warn();
      }
      Ok::<_,AE>(())
    })()
      .context("screenshots, in Setup::drop")
      .just_warn();
  }
}

#[throws(AE)]
pub fn setup(exe_module_path: &str) -> Setup {
  env_logger::Builder::new()
    .format_timestamp_micros()
    .format_level(true)
    .filter_module("otter_webdriver_tests", log::LevelFilter::Debug)
    .filter_module(exe_module_path, log::LevelFilter::Debug)
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTER_WDT_LOG")
    .init();
  debug!("starting");

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

  info!("pid = {}", nix::unistd::getpid());
  sleep(opts.pause.into());

  let cln = cleanup_notify::Handle::new()?;
  let ds = prepare_tmpdir(&opts, &current_exe)?;

  prepare_xserver(&cln, &ds).always_context("setup X server")?;
  prepare_gameserver(&cln, &ds).always_context("setup game server")?;
  prepare_game(&ds).context("setup game")?;

  let final_hook = FinalInfoCollection;

  prepare_geckodriver(&cln).always_context("setup webdriver server")?;
  let (driver, screenshot_count, windows_squirreled) =
    prepare_thirtyfour().always_context("prepare web session")?;

  Setup {
    ds,
    driver,
    screenshot_count,
    current_window: None,
    windows_squirreled,
    final_hook
  }
}

impl Setup {
  #[throws(AE)]
  pub fn setup_static_users(&mut self) -> Vec<Window> {
    #[throws(AE)]
    fn mk(su: &mut Setup, u: StaticUser) -> Window {
      let nick: &str = u.into();
      let token = u.get_str("Token").expect("StaticUser missing Token");
      let subst = su.ds.also([("nick",  nick),
                              ("token", token)].iter());
      su.ds.otter(&subst
                  .ss("--super                          \
                       --account server:@nick@       \
                       --fixed-token @token@         \
                       join-game server::dummy")?)?;
      let w = su.new_window(nick)?;
      let url = subst.subst(&"@url@/?@token@")?;
      su.w(&w)?.get(url)?;
      su.w(&w)?.screenshot("initial")?;
      w
    }
    StaticUser::iter().map(
      |u| mk(self, u)
        .with_context(|| format!("{:?}", u))
        .context("make static user")
    )
      .collect::<Result<Vec<Window>,AE>>()?
  }
}
