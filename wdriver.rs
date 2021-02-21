// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

pub use otter_api_tests::*;
pub use otter_api_tests as apitest;

pub use thirtyfour_sync as t4;

pub use t4::WebDriverCommands;
pub use t4::By;

pub type T4d = t4::WebDriver;
pub type WDE = t4::error::WebDriverError;

use t4::Capabilities;

use once_cell::sync::OnceCell;

#[derive(Debug,Clone)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(flatten)]
  at: apitest::Opts,

  #[structopt(long="--layout", default_value="Portrait")]
  layout: PresentationLayout,

  #[structopt(long="--geckodriver-args", default_value="")]
  geckodriver_args: String,
}

impl Deref for Opts {
  type Target = apitest::Opts;
  fn deref(&self) -> &Self::Target { &self.at }
}

#[derive(Debug)]
pub struct FinalInfoCollection;

type ScreenShotCount = u32;
type WindowState = Option<String>;

#[derive(Debug)]
pub struct Setup {
  pub ds: DirSubst,
  pub mgmt_conn: MgmtChannel,
  pub opts: Opts,
  pub server_child: process::Child,
  found_tests: BTreeSet<String>,
  driver: T4d,
  current_window: WindowState,
  screenshot_count: ScreenShotCount,
  final_hook: FinalInfoCollection,
  windows_squirreled: Vec<String>, // see Drop impl
}

#[throws(AE)]
fn prepare_xserver(cln: &cleanup_notify::Handle, ds: &DirSubst) {
  const DISPLAY: u16 = 12;

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

  let (l,_) = fork_something_which_prints(xcmd, cln, "Xvfb")?;

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
fn prepare_geckodriver(opts: &Opts, cln: &cleanup_notify::Handle) {
  const EXPECTED: &str = "Listening on 127.0.0.1:4444";
  let mut cmd = Command::new("geckodriver");
  if opts.geckodriver_args != "" {
    cmd.args(opts.geckodriver_args.split(' '));
  }
  let (l,_) = fork_something_which_prints(cmd, cln, "geckodriver")?;
  let fields: Vec<_> = l.split('\t').skip(2).take(2).collect();
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
  let mut caps = t4::DesiredCapabilities::firefox();
  let prefs: HashMap<_,_> = [
    ("devtools.console.stdout.content", true),
  ].iter().cloned().collect();
  caps.add("prefs", prefs)?;
  caps.add("stdio", "inherit")?;
  let mut driver = t4::WebDriver::new("http://localhost:4444", &caps)
    .context("create 34 WebDriver")?;

  const FRONT: &str = "front";
  let window_names = vec![FRONT.into()];
  driver.set_window_name(FRONT).context("set initial window name")?;
  screenshot(&mut driver, &mut count, "startup", log::Level::Trace)?;
  driver.get(URL).context("navigate to front page")?;
  screenshot(&mut driver, &mut count, "front", log::Level::Trace)?;

  fetch_log(&driver, "front")?;
  
  let t = Some(5_000 * MS);
  driver.set_timeouts(t4::TimeoutConfiguration::new(t,t,t))
    .context("set webdriver timeouts")?;

  (driver, count, window_names)
}

/// current window must be `name`
#[throws(AE)]
fn fetch_log(driver: &T4d, name: &str) {
  (||{
    let got = driver.execute_script(r#"
      var returning = window.console.saved;
      window.console.saved = [];
      return returning;
    "#).context("get log")?;

    for ent in got.value().as_array()
      .ok_or(anyhow!("saved isn't an array?"))?
    {
      #[derive(Deserialize)]
      struct LogEnt(String, Vec<serde_json::Value>);
      impl fmt::Display for LogEnt {
        #[throws(fmt::Error)]
        fn fmt(&self, f: &mut fmt::Formatter) {
          write!(f, "{}:", self.0)?;
          for a in &self.1 { write!(f, " {}", a)?; }
        }
      }

      let ent: LogEnt = serde_json::from_value(ent.clone())
        .context("parse log entry")?;

      debug!("JS {} {}", name, &ent);
    }
    Ok::<_,AE>(())
  })()
    .with_context(|| name.to_owned())
    .context("fetch JS log messages")?;
}

type ScreenCTM = ndarray::Array2::<f64>;

pub struct WindowGuard<'g> {
  su: &'g mut Setup,
  w: &'g Window,
  matrix: OnceCell<ScreenCTM>,
  client: OnceCell<String>,
}

impl Debug for WindowGuard<'_> {
  #[throws(fmt::Error)]
  fn fmt(&self, f: &mut fmt::Formatter) {
    f.debug_struct("WindowGuard")
      .field("w.name", &self.w.name)
      .field("w.instance", &self.w.instance.to_string())
      .finish()?
  }
}

impl<'g> WindowGuard<'g> {
  #[throws(AE)]
  pub fn find_piece(&'g self, pieceid: &'g str) -> PieceElement<'g> {
    let id = format!("use{}", pieceid);
    let elem = self.su.driver.find_element(By::Id(&id))?;
    PieceElement {
      pieceid, elem,
      w: self,
    }
  }

  #[throws(AE)]
  pub fn client(&mut self) -> String {
    let us = self.client.get_or_try_init(||{
      let us = self.execute_script(r##"return us;"##)?;
      let us = us.value();
      let us = us.as_str().ok_or_else(
        || anyhow!("return us script gave {:?}", &us))?;
      Ok::<_,AE>(us.to_owned())
    }).context("obtain client ID")?;
    us.clone()
  }

  #[throws(AE)]
  pub fn piece_held(&self, pc: &str) -> Option<String> {
    let held = self.execute_script(&format!(r##"
        let pc = pieces['{}'];
        return pc.held;
                       "##, &pc))?;
    let held = held.value();
    dbg!(held);
    match held {
      serde_json::Value::Null => None,
      serde_json::Value::String(s) => Some(s.to_owned()),
      _ => Err(anyhow!("held check script gave {:?}", held))?,
    }
  }

  #[throws(AE)]
  pub fn posg2posw(&self, posg: Pos) -> WebPos {
    let mat = self.matrix.get_or_try_init(||{
      let ary = self.su.driver.execute_script(r#"
        let m = space.getScreenCTM();
        return [m.a, m.b, m.c, m.d, m.e, m.f];
      "#)?;
      let ary = ary.value();
      dbg!(ary);

      let mat = (||{
        let ary = ary.as_array().ok_or_else(|| anyhow!("not array"))?;
        let mut mat: ScreenCTM = ndarray::Array2::zeros((3,3));
        for got in itertools::Itertools::zip_longest(
          [11, 12, 21, 22, 41, 42].iter(),
          // ^ from https://developer.mozilla.org/en-US/docs/Web/API/DOMMatrix
          ary.iter(),
        ) {
          let (mij, v) = got.both().ok_or_else(|| anyhow!("wrong length"))?;
          let adj = |v| (if v == 4 { 3 } else { v }) - 1;
          let i = adj(mij / 10);
          let j = adj(mij % 10);
          mat[(j,i)] = v.as_f64().ok_or_else(|| anyhow!("entry not f64"))?;
        }
        Ok::<_,AE>(mat)
      })()
        .with_context(|| format!("getScreenCGM script gave {:?}", &ary))?;

      dbg!(&mat);
      Ok::<_,AE>(mat)
    })?;
    (||{
      let vec: ndarray::Array1<f64> =
        posg.0.iter()
        .cloned()
        .map(|v| v as f64)
        .chain(iter::once(1.))
        .collect();
      dbg!(&vec);

      let vec = mat.dot(&vec);
      let mut coords = vec.iter().map(
        |v| NumCast::from(v.round()).ok_or_else(
          || anyhow!("coordinate {} out of range in {:?}", v, &vec))
      );
      let mut coord = || coords.next().unwrap();
      Ok::<WebPos,AE>((
        coord()?,
        coord()?,
      ))
    })()
      .context("convert game position to web page coordinates")?
  }
}

pub type WebCoord = i32;
pub type WebPos = (WebCoord, WebCoord);

pub struct PieceElement<'g> {
  pieceid: &'g str,
  w: &'g WindowGuard<'g>,
  elem: t4::WebElement<'g>,
}

impl<'g> Deref for PieceElement<'g> {
  type Target = t4::WebElement<'g>;
  fn deref<'i>(&'i self) -> &'i t4::WebElement<'g> { &self.elem }
}

impl<'g> PieceElement<'g> {
  #[throws(AE)]
  pub fn posg(&self) -> Pos {
    (||{
      let a = |a| Ok::<_,AE>(
        self.get_attribute(a)?.ok_or(anyhow!("{}", a))?.parse()?
      );
      let x = a("x")?;
      let y = a("y")?;
      Ok::<_,AE>(PosC([x,y]))
    })()
      .with_context(|| self.pieceid.to_owned())
      .context("read position of piece out of x,y attributes")?
  }

  #[throws(AE)]
  pub fn posw(&self) -> WebPos {
    let posg = self.posg()?;

    self.w.posg2posw(posg)
      .with_context(|| self.pieceid.to_owned())?
  }
}

impl<'g> TryInto<WebPos> for &'g PieceElement<'g> {
  type Error = AE;
  #[throws(AE)]
  fn try_into(self) -> WebPos {
    self.posw()?
  }
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

#[macro_export]
macro_rules! ctx_with_setup {
  {$ctx:ident} => {
    impl Deref for $ctx {
      type Target = self::Setup;
      fn deref(&self) -> &self::Setup { &self.su }
    }
  }
}

impl Setup {
  pub fn want_test(&mut self, tname: &str) -> bool {
    self.found_tests.insert(tname.to_owned());
    let y =
      self.opts.tests.is_empty() ||
      self.opts.tests.iter().any(|s| s==tname);
    y
  }

  #[throws(AE)]
  pub fn new_window<'s>(&'s mut self, instance: &Instance, name: &str)
                        -> Window {
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

      Ok::<_,AE>(Window {
        name: name.to_owned(),
        instance: instance.0.clone(),
      })
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
    WindowGuard {
      w,
      su: self,
      matrix: default(),
      client: default(),
    }
  }
}

impl<'g> Deref for WindowGuard<'g> {
  type Target = T4d;
  fn deref(&self) -> &T4d { &self.su.driver }
}

impl<'g> Drop for WindowGuard<'g> {
  fn drop(&mut self) {
    fetch_log(&self.su.driver, &self.w.name)
      .just_warn();
  }
}

pub trait Screenshottable {
  fn screenshot(&mut self, slug: &str, level: log::Level) -> Result<(),AE>;
}

impl<'g> Screenshottable for WindowGuard<'g> {
  #[throws(AE)]
  fn screenshot(&mut self, slug: &str, level: log::Level) {
    screenshot(&self.su.driver, &mut self.su.screenshot_count,
               &format!("{}-{}", &self.w.name, slug), level)?
  }
}

#[throws(AE)]
fn screenshot(driver: &T4d, count: &mut ScreenShotCount, slug: &str,
              level: log::Level) {
  if !log_enabled!(level) {
    debug!("skipping screenshot {}", slug);
    return
  }
  let path = format!("{:03}{}.png", count, slug);
  *count += 1;
  driver.screenshot(&path::PathBuf::from(&path))
    .with_context(|| path.clone())
    .context("take screenshot")?;
  debug!("screenshot {}", &path);
}

impl<'g> WindowGuard<'g> {

  #[throws(AE)]
  pub fn otter(&mut self, verb: &[&str], args: &[&str]) {
    let args: Vec<String> =
      ["--account", "server:"].iter().cloned().map(Into::into)
      .chain(verb.iter().cloned().map(Into::into))
      .chain(iter::once(self.w.table()))
      .chain(args.iter().cloned().map(Into::into))
      .collect();
    self.su.ds.otter(&args)?;
  }

  #[throws(AE)]
  fn synch_raw(&mut self) {
    let cmd = MgmtCommand::AlterGame {
      game: self.w.instance.clone(),
      how: MgmtGameUpdateMode::Online,
      insns: vec![ MgmtGameInstruction::Synch ],
    };
    let gen = if_chain!{
      let resp = self.su.mgmt_conn.cmd(&cmd)?;
      if let MgmtResponse::AlterGame {
        error: None,
        ref responses
      } = resp;
      if let [MgmtGameResponse::Synch(gen)] = responses[..];
      then { gen }
      else { throw!(anyhow!("unexpected resp to synch {:?}", resp)) }
    };
    trace!("{:?} gen={} ...", self, gen);
    (|| {
      loop {
        let tgen = self.su.driver.execute_async_script(
          &Subst::from(&[
            ("wanted", &gen.to_string())
          ]).subst(r#"
            var done = arguments[0];
            if (gen >= @wanted@) { done(gen); return; }
            window.gen_update_hook = function() {
              window.gen_update_hook = function() { };
              done(gen);
            };
          "#)?
        )
          .context("run async script")?
          .value().as_u64().ok_or(anyhow!("script return is not u64"))?;
        let tgen = Generation(tgen);
        trace!("{:?} gen={} tgen={}", self, gen, tgen);
        if tgen >= gen { break; }
      }
      Ok::<(),AE>(())
    })()
      .context("await gen update via async js script")?;
  }

  #[throws(AE)]
  pub fn synch_ignore_js_errors(&mut self) {
    self.synch_raw()?;

    self.su.driver.execute_script(r#"
      let e = document.getElementById('error');
      e.innerHTML = "";
    "#)
      .context("clear in-client trapped errors")?;
  }

  #[throws(AE)]
  pub fn synch(&mut self) {
    self.synch_raw()?;

    (|| {
      let errors = self.su.driver.execute_script(r#"
        let e = document.getElementById('error');
        if (e) {
          return e.innerHTML;
        } else {
          console.log('wdt-*: no errors element, no trapped errors check');
          return "";
        }
      "#)
        .context("get errors")?;
      let errors = errors
        .value()
        .as_str()
        .ok_or_else(|| anyhow!("errors script gave non-string"))?;
      if ! errors.is_empty() {
        throw!(anyhow!("JS errors - HTML: {}", errors));
      }
      Ok::<(),AE>(())
    })()
      .context("check for in-client trapped errors")?;
  }
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

pub trait IntoInWindow<T> {
  fn w_into<'g>(self, w: &'g WindowGuard) -> Result<T, AE>;
}

impl<T> IntoInWindow<T> for T {
  #[throws(AE)]
  fn w_into<'g>(self, _w: &'g WindowGuard) -> T { self }
}

impl IntoInWindow<WebPos> for Pos {
  #[throws(AE)]
  fn w_into<'g>(self, w: &'g WindowGuard) -> WebPos {
    w.posg2posw(self)?
  }
}

pub trait ActionChainExt: Sized {
  fn move_w<'g, P: Debug + Copy + IntoInWindow<WebPos>>
    (self, w: &'g WindowGuard, pos: P) -> Result<Self,AE>;

  fn move_pos<'g,
              E,
              P: TryInto<WebPos, Error=E>>
    (self, pos: P) -> Result<Self,AE>
    where Result<WebPos,E>: anyhow::Context<WebPos,E>;

  fn move_pc<'g>(self, w: &'g WindowGuard, pc: &str) -> Result<Self, AE>;
}

impl<'a> ActionChainExt for t4::action_chain::ActionChain<'a> {
  #[throws(AE)]
  fn move_pos<'g,
              E,
              P: TryInto<WebPos, Error=E>>
    (self, pos: P) -> Self
    where Result<WebPos,E>: anyhow::Context<WebPos,E>
  {
    let (px,py) = pos.try_into().context("convert")?;
    trace!("move_pos: ({}, {})", px, py);
    self.move_to(px,py)
  }

  #[throws(AE)]
  fn move_w<'g, P: Debug + Copy + IntoInWindow<WebPos>>
    (self, w: &'g WindowGuard, pos: P) -> Self
  {
    let pos: WebPos = pos.w_into(w)
      .with_context(|| format!("{:?}", pos))
      .context("find coordinate")?;
    self.move_pos(pos)?
  }

  #[throws(AE)]
  fn move_pc<'g>(self, w: &'g WindowGuard, pc: &str) -> Self {
    (||{
      let p = w.find_piece(pc).context("find")?;
      let pos = p.posw().context("get pos")?;
      let r = self.move_pos(pos).context("move")?;
      Ok::<_,AE>(r)
    })().with_context(|| format!("piece {}", pc))?
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
        let w = Window {
          name: name.clone(),
          instance: TABLE.parse().context(TABLE)?,
        };
        self.w(&w)?.screenshot("final", log::Level::Info)
          .context(name)
          .context("final screenshot")
          .just_warn();
      }
      Ok::<_,AE>(())
    })()
      .context("screenshots, in Setup::drop")
      .just_warn();

    let missing_tests = self.opts.tests.iter().cloned()
      .filter(|s| !self.found_tests.contains(s))
      .collect::<Vec<_>>();

    if !missing_tests.is_empty() {
      for f in &self.found_tests {
        eprintln!("fyi: test that exists: {}", f);
      }
      for m in &missing_tests {
        eprintln!("warning: unknown test requested: {}", m);
      }
    }
  }
}

#[throws(AE)]
pub fn setup(exe_module_path: &str) -> (Setup, Instance) {
  env_logger::Builder::new()
    .format_timestamp_micros()
    .format_level(true)
    .filter_module("otter_webdriver_tests", log::LevelFilter::Debug)
    .filter_module(exe_module_path, log::LevelFilter::Debug)
    .filter_level(log::LevelFilter::Info)
    .parse_env("OTTER_WDT_LOG")
    .init();
  debug!("starting");

  let current_exe: String = env::current_exe()
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

  let (mgmt_conn, server_child) =
    prepare_gameserver(&cln, &ds).always_context("setup game server")?;

  let instance_name =
    prepare_game(&ds, TABLE).context("setup game")?;

  let final_hook = FinalInfoCollection;

  prepare_geckodriver(&opts, &cln).always_context("setup webdriver server")?;
  let (driver, screenshot_count, windows_squirreled) =
    prepare_thirtyfour().always_context("prepare web session")?;

  (Setup {
    ds,
    mgmt_conn,
    server_child,
    driver,
    opts,
    found_tests: default(),
    screenshot_count,
    current_window: None,
    windows_squirreled,
    final_hook,
  },
   Instance(
     instance_name
   ))
}

impl Setup {
  #[throws(AE)]
  pub fn setup_static_users(&mut self, instance: &Instance) -> Vec<Window> {
    #[throws(AE)]
    fn mk(su: &mut Setup, instance: &Instance, u: StaticUser) -> Window {
      let nick: &str = u.into();
      let token = u.get_str("Token").expect("StaticUser missing Token");
      let pl = AbbrevPresentationLayout(su.opts.layout).to_string();
      let subst = su.ds.also([
        ("nick",  nick),
        ("token", token),
        ("pl",    &pl),
      ].iter());
      su.ds.otter(&subst
                  .ss("--super                          \
                       --account server:@nick@       \
                       --fixed-token @token@         \
                       join-game server::dummy")?)?;
      let w = su.new_window(instance, nick)?;
      let url = subst.subst("@url@/@pl@?@token@")?;
      su.w(&w)?.get(url)?;
      su.w(&w)?.screenshot("initial", log::Level::Trace)?;
      w
    }
    StaticUser::iter().map(
      |u| mk(self, instance, u)
        .with_context(|| format!("{:?}", u))
        .context("make static user")
    )
      .collect::<Result<Vec<Window>,AE>>()?
  }
}

// Ideally individual test files' Ctx would simply contain this,
// but it would need to be borrowed in pieces which is awkward.
pub struct UsualSetup {
  pub su: Setup,
  pub inst: Instance,
  pub alice: Window,
  pub bob: Window,
  pub spec: GameSpec,
}

impl UsualSetup {
  #[throws(AE)]
  pub fn new() -> UsualSetup {
    let (mut su, inst) = setup(module_path!()).always_context("setup")?;
    let [alice, bob] : [Window; 2] =
      su.setup_static_users(&inst)?.try_into().unwrap();
    let spec = su.ds.game_spec_data()?;
    UsualSetup { su, inst, alice, bob, spec }
  }
}

#[throws(AE)]
pub fn as_usual<F: FnOnce(UsualSetup) -> Result<(), AE>>(f: F) {
  let usual = UsualSetup::new()?;
  f(usual)?;
  info!("ok");
}
