// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

//! Otter game system (part thereeof)
//!
//! <https://www.chiark.greenend.org.uk/~ianmdlvl/otter/docs/README.html>
//!
//! This crate is intended for use only by other parts of Otter.

#![allow(clippy::or_fun_call)]
#![allow(clippy::unnecessary_operation)] // trips on #[throws(Explode)]
#![allow(clippy::no_effect)] // trips on #[throws(Explode)]

pub use otter_api_tests::*;
pub use otter_api_tests as apitest;

pub use thirtyfour_sync as t4;

pub use t4::WebDriverCommands;
pub use t4::By;
pub use t4::Keys;

pub type T4d = t4::WebDriver;
pub type WDE = t4::error::WebDriverError;

use t4::Capabilities;

use once_cell::sync::OnceCell;

pub use std::rc::Rc;

#[derive(Debug,Clone,Deref)]
#[derive(StructOpt)]
pub struct Opts {
  #[structopt(flatten)] #[deref]
  at: apitest::Opts,

  #[structopt(long="--layout", default_value="Portrait")]
  layout: PresentationLayout,

  #[structopt(long="--geckodriver-args", default_value="")]
  geckodriver_args: String,
}
impl AsRef<apitest::Opts> for Opts {
  fn as_ref(&self) -> &apitest::Opts { &self.at }
}

#[derive(Debug)]
pub struct FinalInfoCollection;

type ScreenShotCount = u32;
type WindowState = Option<String>;

#[derive(Debug,Deref,DerefMut)]
pub struct Setup {
  pub opts: Opts,
  #[deref] #[deref_mut] pub core: SetupCore,
  driver: T4d,
  current_window: WindowState,
  screenshot_count: ScreenShotCount,
  #[allow(dead_code)] /* here for Drop impl */ final_hook: FinalInfoCollection,
  windows_squirreled: Vec<JsLogfile>, // see Drop impl
}

#[derive(Debug)]
pub struct Window {
  pub name: String,
  pub player: PlayerId,
  pub instance: InstanceName,
  pub url: String,
  js_logfile: JsLogfile,
  vpid_cache: RefCell<HashMap<String, Vpid>>,
}

#[derive(Debug,Clone,Eq,PartialEq,Ord,PartialOrd,Hash)]
#[derive(Deserialize)]
#[serde(transparent)]
pub struct Vpid(pub String);
impl Display for Vpid {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { f.write_str(&self.0) }
}

impl Window {
  pub fn table(&self) -> String { self.instance.to_string() }
}

#[throws(AE)]
fn prepare_xserver(cln: &cleanup_notify::Handle, ds: &DirSubst) {
  const DISPLAY: u16 = 12;

  let mut xcmd = Command::new("Xvfb");
  xcmd
    .args("-screen 0 2000x2000x24 \
           -nolisten unix \
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
fn prepare_geckodriver(opts: &Opts, ds: &DirSubst,
                       cln: &cleanup_notify::Handle) {
  const EXPECTED: &str = "Listening on 127.0.0.1:4444";
  let mut cmd = Command::new("geckodriver");
  cmd.args(&["--binary", &ds.subst("@src@/wdriver/firefox-wrapper")?]);
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
fn prepare_thirtyfour(ds: &DirSubst)
                      -> (T4d, ScreenShotCount, Vec<JsLogfile>)
{
  let mut count = 0;
  let mut caps = t4::DesiredCapabilities::firefox();
  let prefs: HashMap<_,_> = [
    ("devtools.console.stdout.content", true),
  ].iter().cloned().collect();
  caps.add("prefs", prefs)?;
  caps.add("stdio", "inherit")?;
  let driver = t4::WebDriver::new("http://localhost:4444", &caps)
    .context("create 34 WebDriver")?;

  const FRONT: &str = "front";
  let mut js_logfile = JsLogfileImp::open(ds, FRONT)?;

  driver.set_window_name(FRONT).context("set initial window name")?;
  screenshot(&driver, &mut count, "startup", log::Level::Trace)?;
  driver.get(URL).context("navigate to front page")?;
  screenshot(&driver, &mut count, "front", log::Level::Trace)?;

  js_logfile.fetch(&driver)?;
  let js_logs = vec![Rc::new(RefCell::new(js_logfile))];
  
  let t = Some(5_000 * MS);
  driver.set_timeouts(t4::TimeoutConfiguration::new(t,t,t))
    .context("set webdriver timeouts")?;

  (driver, count, js_logs)
}

pub type JsLogfile = Rc<RefCell<JsLogfileImp>>;

#[derive(Debug)]
pub struct JsLogfileImp {
  name: String,
  fh: File,
  counter: usize,
}

impl JsLogfileImp {
  #[throws(AE)]
  pub fn open(ds: &DirSubst, name: &str) -> Self {
    let path = ds.also(&[("name", name)]).subst(
      "@abstmp@/js-@name@.log"
    )?;
    let name = name.to_owned();
    let fh = File::create(&path).with_context(|| path.clone())?;
    let counter = 0;
    JsLogfileImp { name, fh, counter }
  }

  pub fn name(&self) -> String {
    self.name.clone()
  }

  /// current window must be this one, named `name` as we passed to open
  #[throws(AE)]
  pub fn fetch(&mut self, driver: &T4d) {
    self.fetchx(driver, false)?;
  }

  /// current window must be this one, named `name` as we passed to open
  #[throws(AE)]
  pub fn fetchx(&mut self, driver: &T4d, tolerate_errors: bool) {
    self.counter += 1;
    let head = format!(
 "-------------------- JS {} {} --------------------",
      &self.name, self.counter
    );
    writeln!(&mut self.fh, "{}", &head)?;

    let mut intolerable = vec![];

    (||{
      let got = driver.execute_script(r#"
        var returning = window.console.saved;
        window.console.saved = [];
        return returning;
    "#).context("get log")?;

      for ent in got.value().as_array()
        .ok_or(anyhow!("saved isn't an array?"))?
      {
        #[derive(Deserialize,Debug)]
        struct LogEnt(String, Vec<JsV>);
        impl fmt::Display for LogEnt {
          #[throws(fmt::Error)]
          fn fmt(&self, f: &mut fmt::Formatter) {
            write!(f, "{}:", self.0)?;
            for a in &self.1 { write!(f, " {}", a)?; }
          }
        }

        let ent: LogEnt = serde_json::from_value(ent.clone())
          .context("parse log entry")?;

        writeln!(&mut self.fh, "{:?}", &ent)?;

        if ! tolerate_errors {
          match ent.0.as_str() {
            "log" => { },
            _ => intolerable.push(ent)
          }
        }
      }
      Ok::<_,AE>(())
    })()
      .with_context(|| self.name.clone())
      .context("fetch JS log messages")?;

    assert!{ intolerable.is_empty(),
             "Intolerable JS error(s) {:#?}", intolerable };

    info!("{}", head);
  }
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

#[derive(Deserialize,Clone,Debug)]
pub struct WPiece {
  pub piece: Vpid,
  pub p: JsV,
}

pub trait LogIgnoreBefore {
  fn matches(&mut self, s: &str) -> bool;
}
type LogIgnoreBeforeFn<'r> = &'r mut dyn FnMut(&str) -> bool;

impl<'r> LogIgnoreBefore for LogIgnoreBeforeFn<'r> {
  fn matches(&mut self, s: &str) -> bool { self(s) }
}
impl LogIgnoreBefore for HtmlLit {
  fn matches(&mut self, s: &str) -> bool { s.contains(self.as_html_str()) }
}
impl LogIgnoreBefore for Generation {
  fn matches(&mut self, s: &str) -> bool {
    s == synch_logentry(*self).as_html_str()
  }
}

impl<'g> WindowGuard<'g> {
  #[throws(AE)]
  pub fn piece_vpid(&'g self, some_pieceid: &'_ str) -> Vpid {
    if some_pieceid.contains('.') { return Vpid(some_pieceid.to_owned()) }

    let mut cache = self.w.vpid_cache.borrow_mut();
    if let Some(got) = cache.get(some_pieceid) { return got.clone() }

    let (l, r) = some_pieceid.split_once('v').unwrap();
    let s = format!(r#"{{ "idx":{}, "version":{} }}"#, l,r); // cheesy!
    let kd: slotmap::KeyData = serde_json::from_str(&s).unwrap();
    let piece: PieceId = kd.into();
    let resp = self.su.mgmt_conn().cmd(&MC::AlterGame {
      game: TABLE.parse().unwrap(),
      how: MgmtGameUpdateMode::Online,
      insns: vec![ MgmtGameInstruction::PieceIdLookupFwd {
        piece,
        player: self.w.player,
      } ],
    })?;
    let vpid = if_chain!{
      if let MgmtResponse::AlterGame { error: None, responses } = &resp;
      if let [MgmtGameResponse::VisiblePieceId(vpid)] = responses.as_slice();
      then { vpid }
      else { unreachable(Err::<Void,_>(&resp).unwrap()) }
    };

    let got = Vpid(vpid.unwrap().to_string());
    cache.insert(some_pieceid.to_owned(), got.clone());
    got
  }

  pub fn vpid_clear_cache(&'g self) {
    self.w.vpid_cache.borrow_mut().clear();
  }

  #[throws(AE)]
  pub fn vpidelem(&'g self, prefix: &'_ str, some_pieceid: &'_ str) -> String {
    prefix.to_string() + &self.piece_vpid(some_pieceid)?.0
  }

  #[throws(AE)]
  pub fn find_piece(&'g self, pieceid: &'_ str) -> PieceElement<'g> {
    let pieceid = self.piece_vpid(pieceid)?;
    let elemid = format!("use{}", &pieceid);
    let elem = self.su.driver.find_element(By::Id(&elemid))?;
    PieceElement {
      elem, pieceid,
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
                       "##, &self.piece_vpid(pc)?))?;
    let held = held.value();
    dbg!(held);
    match held {
      JsV::Null => None,
      JsV::String(s) => Some(s.to_owned()),
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
        posg.coords.iter()
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

  #[throws(AE)]
  pub fn retrieve_log<I: LogIgnoreBefore>(&self, mut ignore_before: I)
                                          -> Vec<String>
  {
    #[throws(AE)]
    fn inner<'g, 'i>
      (w: &'g WindowGuard, ignore_before: LogIgnoreBeforeFn<'i>)
       -> Vec<String>
    {
      let log = w.find_elements(By::ClassName("logmsg"))?;
      let log = log.iter()
        .rev()
        .map(|e| e.inner_html())
        .take_while(|h| {
          h.as_ref().ok()
            .map(|s: &String| ignore_before(s))
            != Some(true)
        })
        .collect::<Result<Vec<String>,_>>()?;

      assert!( ! log.is_empty() );

      dbg!(log)
    }

    inner(self, &mut move |s| ignore_before.matches(s))?
  }

  #[throws(AE)]
  pub fn fetch_js_log(&self) {
    self.w.js_logfile.borrow_mut().fetch(&self.su.driver)?
  }
}

#[ext(pub)]
impl Vec<String> {
  fn find_conflicts(&self) -> Vec<String> {
    self.iter().filter(|m| m.starts_with("Conflict!"))
      .cloned().collect()
  }

  fn assert_no_conflicts(&self) {
    assert_eq!(self.find_conflicts(), vec![] as Vec<String>);
  }
}

pub type WebCoord = i32;
pub type WebPos = (WebCoord, WebCoord);

#[derive(Deref)]
pub struct PieceElement<'g> {
  pieceid: Vpid,
  w: &'g WindowGuard<'g>,
  #[deref] elem: t4::WebElement<'g>,
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
      Ok::<_,AE>(PosC::new(x,y))
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

impl Setup {
  #[throws(AE)]
  pub fn new_window<'s>(&'s mut self, instance: &Instance, name: &str,
                        player: PlayerId, url: String)
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

      let js_logfile = JsLogfileImp::open(&self.core.ds, name)?;
      let js_logfile = Rc::new(RefCell::new(js_logfile));
      self.windows_squirreled.push(js_logfile.clone());

      Ok::<_,AE>(Window {
        name: name.to_owned(),
        instance: instance.0.clone(),
        vpid_cache: default(),
        url, player, js_logfile,
      })
    })()
      .with_context(|| name.to_owned())
      .context("create window")?;

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
    (|| Ok::<_,AE>(
      self.w.js_logfile.try_borrow_mut().context("borrow js log")?
        .fetch(&self.su.driver)?
    ))().just_warn();
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
  driver.screenshot(&PathBuf::from(&path))
    .with_context(|| path.clone())
    .context("take screenshot")?;
  debug!("screenshot {}", &path);
}

impl<'g> WindowGuard<'g> {

  #[throws(AE)]
  pub fn otter(&mut self, verb: &[&str], args: &[&str]) {
    let args: Vec<String> =
      ["--account", "server:", "--game", &self.w.table()]
      .iter().cloned().map(Into::into)
      .chain(verb.iter().cloned().map(Into::into))
      .chain(args.iter().cloned().map(Into::into))
      .collect();
    self.su.ds.otter(&args)?;
  }

  #[throws(AE)]
  fn synch_raw(&mut self) -> Generation {
    let gen = self.su.mgmt_conn().game_synch(self.w.instance.clone())?;
    (|| {
      loop {
        info!("{:?} gen={}", self, gen);
        let tgen = self.su.driver.execute_async_script(
          &Subst::from(&[
            ("wanted", &gen.to_string())
          ]).subst(r#"
            var done = arguments[0];
            function no_queue() { return !api_queue.length && !api_posting; }
            if (gen >= @wanted@ && no_queue()) { done(gen); return; }
            window.test_update_hook = function() {
              window.test_update_hook = function() { };
              done(no_queue() ? gen : 0);
            };
          "#)?
        )
          .context("run async script")?
          .value().as_u64().ok_or(anyhow!("script return is not u64"))?;
        let tgen = Generation(tgen);
        info!("{:?} gen={} tgen={}", self, gen, tgen);
        if tgen >= gen { break; }
      }
      Ok::<(),AE>(())
    })()
      .context("await gen update via async js script")?;
    gen
  }

  #[throws(AE)]
  pub fn synch_ignore_js_errors(&mut self) -> Generation {
    let gen = self.synch_raw()?;

    self.su.driver.execute_script(r#"
      let e = document.getElementById('error');
      e.innerHTML = "";
    "#)
      .context("clear in-client trapped errors")?;

    gen
  }

  #[throws(AE)]
  pub fn synch(&mut self) -> Generation {
    let gen = self.synch_raw()?;

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

    gen
  }

  /// These come in stacking order, bottom to top.
  #[throws(AE)]
  pub fn pieces(&mut self) -> Vec<WPiece> {
    self.su.driver.execute_script(r#"
      let uelem = pieces_marker;
      let out = [];
      for (;;) {
        uelem = uelem.nextElementSibling;
        let piece = uelem.dataset.piece;
        if (!piece) break;
        let p = pieces[piece];
        out.push({ piece: piece, p: p });
      }
      return out;
    "#)
      .did("fetch ids")?
      .convert()?
  }
}

impl Drop for FinalInfoCollection {
  fn drop(&mut self) {
    nix::unistd::linkat(None, "Xvfb_screen0",
                        None, "Xvfb_keep.xwd",
                        unistd::LinkatFlags::NoSymlinkFollow)
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

#[ext(pub, name=ActionChainExt, supertraits=Sized)]
impl<'a> t4::action_chain::ActionChain<'a> {
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
  fn move_pc<'g>(self, w: &'g WindowGuard, pc: &'_ str) -> Self {
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
      for jslog in mem::take(&mut self.windows_squirreled) {
        // This constructor is concurrency-hazardous.  It's only OK
        // here because we have &mut self.  If there is only one
        // Setup, there can be noone else with a Window with an
        // identical name to be interfered with by us.
        let name = jslog.try_borrow()?.name();
        let w = Window {
          name: name.clone(),
          instance: TABLE.parse().context(TABLE)?,
          player: default(),
          vpid_cache: default(),
          js_logfile: jslog.clone(),
          url: default(),
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
  }
}

#[throws(AE)]
pub fn setup(exe_module_path: &str) -> (Setup, Instance) {
  let (opts, instance, core) =
    apitest::setup_core(
      &[exe_module_path, "otter_webdriver_tests"],
    )?;

  prepare_xserver(&core.cln, &core.ds).did("setup X server")?;

  let final_hook = FinalInfoCollection;

  prepare_geckodriver(&opts, &core.ds, &core.cln).did("setup webdriverr")?;
  let (driver, screenshot_count, windows_squirreled) =
    prepare_thirtyfour(&core.ds).did("prepare web session")?;

  (Setup {
    core,
    driver,
    opts,
    screenshot_count,
    current_window: None,
    windows_squirreled,
    final_hook,
  }, instance)
}

impl Setup {
  #[throws(AE)]
  pub fn setup_static_users(&mut self, instance: &Instance) -> Vec<Window> {
    let susus = self.core.ds.clone()
      .setup_static_users(&mut self.mgmt_conn(), self.opts.layout)?;
    susus
      .into_iter().map(|sus|
    {
      let w = self.new_window(instance, sus.nick,
                              sus.player, sus.url.clone())?;
      self.w(&w)?.get(sus.url)?;
      self.w(&w)?.screenshot("initial", log::Level::Trace)?;
      Ok::<_,AE>(w)
    })
      .collect::<Result<_,_>>()?
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
  pub fn new(exe_module_path: &str) -> UsualSetup {
    let (mut su, inst) = setup(exe_module_path).did("usual setup")?;
    let [alice, bob] : [Window; 2] =
      su.setup_static_users(&inst)?.try_into().unwrap();
    let spec = su.ds.game_spec_data()?;
    UsualSetup { su, inst, alice, bob, spec }
  }
}

#[throws(Explode)]
pub fn as_usual<F: FnOnce(UsualSetup) -> Result<(), Explode>>(
  f: F, exe_module_path: &str,
) {
  let usual = UsualSetup::new(exe_module_path)?;
  f(usual)?;
  info!("ok");
}

// ==================== portmanteau binary ====================

portmanteau_has!("wdt-altergame.rs", wdt_altergame);
portmanteau_has!("wdt-hand.rs",      wdt_hand);
portmanteau_has!("wdt-simple.rs",    wdt_simple);
portmanteau_has!("wdt-bundles.rs",   wdt_bundles);

#[throws(AE)]
fn main() { portmanteau_main("wdt")? }
