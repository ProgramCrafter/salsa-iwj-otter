// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

// ==================== namespace preparation ====================

pub mod imports {
  pub use otter;
  pub use otter::imports::*;

  pub use humantime;
}

pub use imports::*;
pub use otter::prelude::*;

pub use std::cell::{RefCell, RefMut};

pub use num_traits::NumCast;
pub use serde_json::json;
pub use structopt::StructOpt;

pub type JsV = serde_json::Value;
pub type MC = MgmtCommand;

// -------------------- private imports ----------

use otter::config::DAEMON_STARTUP_REPORT;

// ==================== public constants ====================

pub const TABLE: &str = "server::dummy";
pub const CONFIG: &str = "server-config.toml";

pub const URL: &str = "http://localhost:8000";

#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd)]
#[derive(FromPrimitive,EnumIter,IntoStaticStr,EnumProperty)]
#[strum(serialize_all = "snake_case")]
pub enum StaticUser {
  #[strum(props(Token="kmqAKPwK4TfReFjMor8MJhdRPBcwIBpe"))] Alice,
  #[strum(props(Token="ccg9kzoTh758QrVE1xMY7BQWB36dNJTx"))] Bob,
}

// ==================== principal public structs ====================

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

  #[structopt(long="--test")]
  test_name: Option<String>,
}

#[derive(Debug)]
pub struct SetupCore {
  pub ds: DirSubst,
  pub mgmt_conn: RefCell<MgmtChannelForGame>,
  server_child: Child,
  pub wanted_tests: TrackWantedTests,
}

#[derive(Clone,Debug)]
pub struct DirSubst {
  pub tmp: String,
  pub abstmp: String,
  pub start_dir: String,
  pub src: String,
}

pub struct Instance(pub InstanceName);

// ==================== Facilities for tests ====================

impl AsRef<Opts> for Opts { fn as_ref(&self) -> &Opts { self } }

// -------------------- Substition --------------------

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

#[derive(Clone,Debug)]
pub struct Subst(HashMap<String,String>);

impl Substitutor for Subst {
  fn get(&self, kw: &str) -> Option<String> {
    self.0.get(kw).map(String::clone)
  }
}

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

#[derive(Clone,Debug)]
pub struct ExtendedSubst<B: Substitutor, X: Substitutor>(B, X);

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
      "table"  => TABLE.to_owned(),
      _ => return None,
    })
  }
}

// ---------- requested/available test tracking ----------

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

#[macro_export]
macro_rules! usual_wanted_tests {
  ($ctx:ty, $su:ident) => {
    impl $ctx {
      fn wanted_tests(&mut self) -> &mut TrackWantedTests {
        &mut self.su.wanted_tests
      }
    }
  }
}

#[macro_export]
macro_rules! test {
  ($c:expr, $tname:expr, $s:stmt) => {
    if $c.wanted_tests().wantp($tname) {
      debug!("==================== {} starting ====================", $tname);
      $s
      info!("==================== {} completed ====================", $tname);
    } else {
      trace!("= = = {} skipped = = =", $tname);
    }
  }
}

// -------------------- Extra anyhow result handling --------------------

#[ext(pub)]
impl<T,E> Result<T,E> {
  fn did(self, msg: &'static str) -> anyhow::Result<T>
  where Self: anyhow::Context<T,E>
  {
    let x = self.context(msg);
    if x.is_ok() { info!("did {}.", msg) };
    x
  }

  fn just_warn(self) -> Option<T> where E: Display {
    match self {
      Ok(x) => Some(x),
      Err(e) => {
        warn!("{:#}", e);
        None
      },
    }
  }
}

// -------------------- cleanup_notify (signaling) --------------------

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

// -------------------- generalised daemon startup --------------------

#[throws(AE)]
pub fn fork_something_which_prints(mut cmd: Command,
                               cln: &cleanup_notify::Handle,
                               what: &str)
                               -> (String, Child)
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

// ==================== principal actual setup code ====================

pub type EarlyArgPredicate<'f> = &'f mut dyn FnMut(&OsStr) -> bool;

#[throws(AE)]
pub fn reinvoke_via_bwrap(_opts: &Opts, current_exe: &str,
                          early: EarlyArgPredicate<'_>) -> Void {
  debug!("running bwrap");
  
  let mut bcmd = Command::new("bwrap");
  bcmd
    .args("--unshare-net \
           --dev-bind / / \
           --tmpfs /tmp \
           --die-with-parent".split(" "))
    .arg(current_exe);

  let (early, late) = {
    let mut still_early = true;
    env::args_os().skip(1)
      .partition::<Vec<_>,_>(|s| {
        still_early &= early(&s);
        still_early
      })
  };
  bcmd.args(early);
  bcmd.arg("--no-bwrap");
  bcmd.args(late);

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
  } else if let Some(test_name) = &opts.test_name {
    current_exe = test_name;
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
pub fn prepare_gameserver(cln: &cleanup_notify::Handle, ds: &DirSubst)
                      -> (MgmtChannel, Child) {
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

fake_rng = []

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

// ---------- game spec ----------

#[derive(Copy,Clone,Error,Debug)]
#[error("wait status: {0}")]
pub struct ExitStatusError(pub std::process::ExitStatus);

impl DirSubst {
  pub fn specs_dir(&self) -> String {
    format!("{}/specs" , &self.src)
  }

  #[throws(AE)]
  pub fn otter<S:AsRef<str>>(&self, xargs: &[S]) {
    let ds = self;
    let exe = ds.subst("@target@/debug/otter")?;
    let specs = self.subst("@src@/specs")?;
    let mut args: Vec<&str> = vec![];
    args.extend(&["--config", CONFIG]);
    args.extend(&["--spec-dir", &specs]);
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
        throw!(ExitStatusError(st));
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
      dbgc!(&data);
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

// ==================== post-setup facilities ====================

// -------------------- static users --------------------

pub struct StaticUserSetup {
  pub nick: &'static str,
  pub url: String,
  pub player: PlayerId,
}

impl DirSubst {
  #[throws(AE)]
  pub fn setup_static_users(&self, mgmt_conn: &mut MgmtChannelForGame,
                            layout: PresentationLayout)
     -> Vec<StaticUserSetup>
  {
    #[throws(AE)]
    fn mk(su: &DirSubst, mgmt_conn: &mut MgmtChannelForGame,
          layout: PresentationLayout, u: StaticUser)
          -> StaticUserSetup
    {
      let nick: &str = u.into();
      let token = u.get_str("Token").expect("StaticUser missing Token");
      let pl = AbbrevPresentationLayout(layout).to_string();
      let subst = su.also([
        ("nick",  nick),
        ("token", token),
        ("pl",    &pl),
      ].iter());

      su.otter(&subst
                  .ss("--super                          \
                       --account server:@nick@       \
                       --fixed-token @token@         \
                       join-game @table@")?)?;

      let player = mgmt_conn.has_player(
        &subst.subst("server:@nick@")?.parse()?
      )?.unwrap().0;

      let url = subst.subst("@url@/@pl@?@token@")?;
      StaticUserSetup { nick, url, player }
    }

    StaticUser::iter().map(
      |u| (||{
        let ssu = mk(self, mgmt_conn, layout, u).context("create")?;
        Ok::<_,AE>(ssu)
      })()
        .with_context(|| format!("{:?}", u))
        .context("make static user")
    )
      .collect::<Result<Vec<StaticUserSetup>,AE>>()?
  }
}

// -------------------- concurrency management --------------------

pub struct OtterPauseable(nix::unistd::Pid);
pub struct OtterPaused(nix::unistd::Pid);

impl SetupCore {
  pub fn otter_pauseable(&self) -> OtterPauseable {
    OtterPauseable(nix::unistd::Pid::from_raw(
      self.server_child.id() as nix::libc::pid_t
    ))
  }

  #[throws(AE)]
  pub fn pause_otter(&self) -> OtterPaused {
    self.otter_pauseable().pause()?
  }

  pub fn mgmt_conn<'m>(&'m self) -> RefMut<'m, MgmtChannelForGame> {
    self.mgmt_conn.borrow_mut()
  }
}

impl OtterPauseable {
  #[throws(AE)]
  pub fn pause(self) -> OtterPaused {
    nix::sys::signal::kill(self.0, nix::sys::signal::SIGSTOP)?;
    OtterPaused(self.0)
  }
}

impl OtterPaused {
  #[throws(AE)]
  pub fn resume(self) -> OtterPauseable {
    nix::sys::signal::kill(self.0, nix::sys::signal::SIGCONT)?;
    OtterPauseable(self.0)
  }
}

impl Drop for OtterPaused {
  fn drop(&mut self) {
    debug!("note, otter server pid={} was still paused", self.0);
  }
}

// -------------------- utilities --------------------

#[ext(pub)]
impl MgmtChannel {
  #[throws(AE)]
  fn game_synch(&mut self, game: InstanceName) -> Generation {
    let cmd = MgmtCommand::AlterGame {
      how: MgmtGameUpdateMode::Online,
      insns: vec![ MgmtGameInstruction::Synch ],
      game
    };
    let gen = if_chain!{
      let resp = self.cmd(&cmd)?;
      if let MgmtResponse::AlterGame {
        error: None,
        ref responses
      } = resp;
      if let [MgmtGameResponse::Synch(gen)] = responses[..];
      then { gen }
      else { throw!(anyhow!("unexpected resp to synch {:?}", resp)) }
    };
    trace!("gen={} ...", gen);
    gen
  }

  fn fakerng_load(&mut self, values: &[&dyn ToString]) -> Result<(),AE> {
    let values = values.iter().map(|v| v.to_string()).collect();
    self.cmd(&MC::LoadFakeRng(values))?;
    Ok(())
  }
  fn fakerng_unfake(&mut self) -> Result<(),AE> {
    self.cmd(&MC::LoadFakeRng(vec![]))?;
    Ok(())
  }
}

// ==================== core entrypoint, for wdriver too ====================

#[throws(AE)]
pub fn setup_core<O>(module_paths: &[&str], early_args: EarlyArgPredicate) ->
  (O, cleanup_notify::Handle, Instance, SetupCore)
  where O: StructOpt + AsRef<Opts>
{
  let mut builder = env_logger::Builder::new();
  builder
    .format_timestamp_micros()
    .format_level(true)
    .filter_module("otter_apitest_tests", log::LevelFilter::Debug)
    .filter_module("html5ever::tokenizer", log::LevelFilter::Info)
    .filter_module("html5ever::tree_builder", log::LevelFilter::Info)
    .filter_module("selectors::matching", log::LevelFilter::Info)
    ;

  for module in module_paths {
    builder
      .filter_module(module, log::LevelFilter::Debug);
  }

  builder
    .filter_level(log::LevelFilter::Debug)
    .parse_env("OTTER_TEST_LOG")
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
    reinvoke_via_bwrap(&opts, &current_exe, early_args)
      .context("reinvoke via bwrap")?;
  }

  info!("pid = {}", nix::unistd::getpid());
  sleep(opts.pause.into());

  let cln = cleanup_notify::Handle::new()?;
  let ds = prepare_tmpdir(&opts, &current_exe)?;

  let (mgmt_conn, server_child) =
    prepare_gameserver(&cln, &ds).did("setup game server")?;

  let mgmt_conn = mgmt_conn.for_game(
    TABLE.parse()?,
    MgmtGameUpdateMode::Online
  );

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
     mgmt_conn: mgmt_conn.into(),
     server_child,
     wanted_tests,
   })
}
