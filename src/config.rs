// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;
use pwd::Passwd;

pub const EXIT_SPACE     : i32 =  2;
pub const EXIT_NOTFOUND  : i32 =  4;
pub const EXIT_SITUATION : i32 =  8;
pub const EXIT_USAGE     : i32 = 12;
pub const EXIT_DISASTER  : i32 = 16;

pub const DEFAULT_CONFIG_DIR       : &str = "/etc/otter";
pub const DEFAULT_CONFIG_LEAFNAME  : &str = "server.toml";
pub const DEFAULT_SENDMAIL_PROGRAM : &str = "/usr/sbin/sendmail";
pub const DEFAULT_SSH_PROXY_CMD    : &str = "otter-ssh-proxy";
pub const SSH_PROXY_SUBCMD         : &str = "mgmtchannel-proxy";

pub const DAEMON_STARTUP_REPORT: &str = "otter-daemon started";
pub const LOG_ENV_VAR: &str = "OTTER_LOG";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub change_directory: Option<String>,
  pub base_dir: Option<String>,
  pub save_dir: Option<String>,
  pub libexec_dir: Option<String>,
  pub usvg_bin: Option<String>,
  pub command_socket: Option<String>,
  pub debug: Option<bool>,
  pub http_port: Option<u16>,
  pub public_url: String,
  pub sse_wildcard_url: Option<String>,
  pub rocket_workers: Option<u16>,
  pub template_dir: Option<String>,
  pub nwtemplate_dir: Option<String>,
  pub wasm_dir: Option<String>,
  pub log: Option<toml::Value>,
  pub bundled_sources: Option<String>,
  pub shapelibs: Option<Vec<shapelib::Config1>>,
  pub specs_dir: Option<String>,
  pub sendmail: Option<String>,
  /// for auth keys, split on spaces
  pub ssh_proxy_command: Option<String>,
  pub ssh_proxy_user: Option<String>,
  pub ssh_restrictions: Option<String>,
  pub authorized_keys: Option<String>,
  pub authorized_keys_include: Option<String>,
  pub debug_js_inject_file: Option<String>,
  #[serde(default)] pub fake_rng: FakeRngSpec,
  /// Disable this for local testing only.  See LICENCE.
  pub check_bundled_sources: Option<bool>,
}

#[derive(Debug,Clone)]
pub struct WholeServerConfig {
  server: Arc<ServerConfig>,
  log: LogSpecification,
}

#[derive(Debug,Clone)]
pub struct ServerConfig {
  save_dir: String,
  pub command_socket: String,
  pub debug: bool,
  pub http_port: Option<u16>,
  pub public_url: String,
  pub sse_wildcard_url: Option<(String, String)>,
  pub rocket_workers: u16,
  pub template_dir: String,
  pub nwtemplate_dir: String,
  pub wasm_dir: String,
  pub libexec_dir: String,
  pub usvg_bin: String,
  pub bundled_sources: String,
  pub shapelibs: Vec<shapelib::Config1>,
  pub specs_dir: String,
  pub sendmail: String,
  pub ssh_proxy_bin: String,
  pub ssh_proxy_uid: Uid,
  pub ssh_restrictions: String,
  pub authorized_keys: String,
  pub authorized_keys_include: String,
  pub debug_js_inject: Arc<String>,
  pub check_bundled_sources: bool,
  pub game_rng: RngWrap,
  pub prctx: PathResolveContext,
}

#[derive(Debug,Copy,Clone)]
pub enum PathResolveMethod {
  Chdir,
  Prefix,
}
impl Default for PathResolveMethod { fn default() -> Self { Self::Prefix } }
#[derive(Debug,Clone)]
pub enum PathResolveContext {
  RelativeTo(String),
  Noop,
}
impl Default for PathResolveContext { fn default() -> Self { Self::Noop } }

static PROGRAM_NAME: RwLock<String> = parking_lot::const_rwlock(String::new());

impl PathResolveMethod {
  #[throws(io::Error)]
  fn chdir(self, cd: &str) -> PathResolveContext {
    use PathResolveMethod::*;
    use PathResolveContext::*;
    match self {
      Chdir => { env::set_current_dir(cd)?; Noop },
      Prefix if cd == "." => Noop,
      Prefix => RelativeTo(cd.to_string()),
    }
  }
}
impl PathResolveContext {
  pub fn resolve(&self, input: &str) -> String {
    use PathResolveContext::*;
    match (self, input.as_bytes()) {
      (Noop           , _         ) |
      (RelativeTo(_  ), &[b'/',..]) => input.to_owned(),
      (RelativeTo(cd), _          ) => format!("{}/{}", &cd, input),
    }
  }
}

impl ServerConfigSpec {
  //#[throws(AE)]
  pub fn resolve(self, prmeth: PathResolveMethod)
                 -> Result<WholeServerConfig,AE> {
    let ServerConfigSpec {
      change_directory, base_dir, save_dir, command_socket, debug,
      http_port, public_url, sse_wildcard_url, rocket_workers,
      template_dir, specs_dir, nwtemplate_dir, wasm_dir, libexec_dir, usvg_bin,
      log, bundled_sources, shapelibs, sendmail,
      debug_js_inject_file, check_bundled_sources, fake_rng,
      ssh_proxy_command, ssh_proxy_user, ssh_restrictions, authorized_keys,
      authorized_keys_include,
    } = self;

    let game_rng = fake_rng.make_game_rng();
    let home = || env::var("HOME").context("HOME");

    let prctx;
    if let Some(ref cd) = change_directory {
      prctx = prmeth.chdir(cd)
        .with_context(|| cd.clone())
        .context("config change_directory")?;
    } else {
      prctx = PathResolveContext::Noop;
    };

    let defpath = |specd: Option<String>, leaf: &str| -> String {
      prctx.resolve(&specd.unwrap_or_else(|| match &base_dir {
        Some(base) => format!("{}/{}", &base, &leaf),
        None       => leaf.to_owned(),
      }))
    };
    let save_dir        = defpath(save_dir,        "save"              );
    let specs_dir       = defpath(specs_dir,       "specs"             );
    let command_socket  = defpath(command_socket,  "var/command.socket");
    let template_dir    = defpath(template_dir,    "assets"            );
    let wasm_dir        = defpath(wasm_dir,        "assets"            );
    let nwtemplate_dir  = defpath(nwtemplate_dir,  "nwtemplates"       );
    let libexec_dir     = defpath(libexec_dir,     "libexec"           );
    let bundled_sources = defpath(bundled_sources, "bundled-sources"   );
    const DEFAULT_LIBRARY_GLOB: &str = "library/*.toml";

    let in_libexec = |specd: Option<String>, leaf: &str| -> String {
      specd.unwrap_or_else(|| format!("{}/{}", &libexec_dir, leaf))
    };
    let usvg_bin        = in_libexec(usvg_bin,     "usvg"              );
    let ssh_proxy_bin = in_libexec(ssh_proxy_command, DEFAULT_SSH_PROXY_CMD );

    let ssh_restrictions = ssh_restrictions.unwrap_or_else(
      || concat!("restrict,no-agent-forwarding,no-port-forwarding,",
                 "no-pty,no-user-rc,no-X11-forwarding").into());

    let authorized_keys = if let Some(ak) = authorized_keys { ak } else {
      let home = home().context("for authorized_keys")?;
      // we deliberately don't create the ~/.ssh dir
      format!("{}/.ssh/authorized_keys", home)
    };
    let authorized_keys_include = authorized_keys_include.unwrap_or_else(
      || format!("{}.static", authorized_keys)
    );
    if authorized_keys == authorized_keys_include {
      throw!(anyhow!(
        "ssh authorized_keys and authorized_keys_include are equal {:?} \
         which would imply including a file in itself",
        &authorized_keys
      ));
    }

    let ssh_proxy_uid = match ssh_proxy_user {
      None => Uid::current(),
      Some(spec) => Uid::from_raw(if let Ok(num) = spec.parse() {
        num
      } else {
        let pwent = (|| Ok::<_,AE>({
          Passwd::from_name(&spec)
            .map_err(|e| anyhow!("lookup failed: {}", e))?
            .ok_or_else(|| anyhow!("does not exist"))?
        }))()
          .with_context(|| spec.clone())
          .context("ssh_proxy_uidr")?;
        pwent.uid
      })
    };

    let shapelibs = shapelibs.unwrap_or_else(||{
      let glob = defpath(None, DEFAULT_LIBRARY_GLOB);
      vec![ shapelib::Config1::PathGlob(glob) ]
    });

    let sendmail = prctx.resolve(&sendmail.unwrap_or_else(
      || DEFAULT_SENDMAIL_PROGRAM.into()
    ));

    let public_url = public_url
      .trim_end_matches('/')
      .into();

    let sse_wildcard_url = sse_wildcard_url.map(|pat| {
      let mut it = pat.splitn(2, '*');
      let lhs = it.next().unwrap();
      let rhs = it.next().ok_or_else(||anyhow!(
        "sse_wildcard_url must containa '*'"
      ))?;
      let rhs = rhs.trim_end_matches('/');
      Ok::<_,AE>((lhs.into(), rhs.into()))
    }).transpose()?;

    let debug = debug.unwrap_or(cfg!(debug_assertions));
    let rocket_workers = rocket_workers.unwrap_or(
      if debug { 20 } else { 1000 });

    let log = {
      use toml::Value::Table;

      let mut log = match log {
        Some(Table(log)) => log,
        None => default(),
        Some(x) => throw!(anyhow!(
          r#"wanted table for "log" config key, not {}"#,
          x.type_str())
        ),
      };
      
      // flexi_logger doesn't allow env var to override config, sigh
      // But we can simulate this by having it convert the env results
      // to toml and merging it with the stuff from the file.
      (||{
        if let Some(v) = env::var_os(LOG_ENV_VAR) {
          let v = v.to_str().ok_or(anyhow!("UTF-8 conversion"))?;
          let v = LogSpecification::parse(v).context("parse")?;
          let mut buf: Vec<u8> = default();
          v.to_toml(&mut buf).context("convert to toml")?;
          let v = toml_de::from_slice(&buf).context("reparse")?;
          match v {
            Some(Table(v)) => toml_merge(&mut log, &v),
            None => default(),
            Some(x) => throw!(anyhow!("reparse gave {:?}, no table", x)),
          };
        }
        Ok::<_,AE>(())
      })()
        .context(LOG_ENV_VAR)
        .context("processing env override")?;

      Table(log)
    };

    let log = toml::to_string(&log)?;
    let log = LogSpecification::from_toml(&log)
      .context("log specification")?;

    let debug_js_inject = Arc::new(match &debug_js_inject_file {
      Some(f) => fs::read_to_string(f)
        .with_context(|| f.clone()).context("debug_js_inject_file")?,
      None => "".into(),
    });

    let check_bundled_sources = check_bundled_sources.unwrap_or(true); 

    let server = ServerConfig {
      save_dir, command_socket, debug,
      http_port, public_url, sse_wildcard_url, rocket_workers,
      template_dir, specs_dir, nwtemplate_dir, wasm_dir, libexec_dir,
      bundled_sources, shapelibs, sendmail, usvg_bin,
      debug_js_inject, check_bundled_sources, game_rng, prctx,
      ssh_proxy_bin, ssh_proxy_uid, ssh_restrictions,
      authorized_keys, authorized_keys_include,
    };
    trace_dbg!("config resolved", &server);
    Ok(WholeServerConfig {
      server: Arc::new(server),
      log,
    })
  }
}

pub fn config() -> Arc<ServerConfig> {
  GLOBAL.config.read().server.clone()
}
pub fn log_config() -> LogSpecification {
  GLOBAL.config.read().log.clone()
}

fn set_config(whole: WholeServerConfig) {
  *GLOBAL.config.write() = whole;
}

impl ServerConfig {
  #[throws(StartupError)]
  pub fn read(config_filename: Option<&str>, prmeth: PathResolveMethod) {
    let config_filename = config_filename.map(|s| s.to_string())
      .unwrap_or_else(
        || format!("{}/{}", DEFAULT_CONFIG_DIR, DEFAULT_CONFIG_LEAFNAME)
      );
    let mut buf = String::new();
    File::open(&config_filename).with_context(||config_filename.to_string())?
      .read_to_string(&mut buf)?;
    let spec: ServerConfigSpec = toml_de::from_str(&buf)?;
    let whole = spec.resolve(prmeth)?;
    set_config(whole);
  }

  #[throws(AE)]
  pub fn lock_save_area(&self) {
    let mut st = GLOBAL.save_area_lock.lock();
    let st = &mut *st;
    if st.is_none() {
      let lockfile = format!("{}/lock", config().save_dir);
      *st = Some((||{
        let file = File::create(&lockfile).context("open")?;
        file.try_lock_exclusive().context("lock")?;
        Ok::<_,AE>(file)
      })().context(lockfile).context("lock global save area")?);
    }
  }

  pub fn save_dir(&self) -> &String {
    let st = GLOBAL.save_area_lock.lock();
    let mut _f: &File = st.as_ref().unwrap();
    &self.save_dir
  }
}

impl Default for WholeServerConfig {
  fn default() -> WholeServerConfig {
    let spec: ServerConfigSpec = toml_de::from_str(r#"
      public_url = "INTERNAL ERROR"
      "#)
      .expect("parse dummy config as ServerConfigSpec");
    spec.resolve(default()).expect("empty spec into config")
  }
}

pub fn set_program_name(s: String) {
  *PROGRAM_NAME.write() = s;
}

pub fn program_name() -> String {
  {
    let set = PROGRAM_NAME.read();
    if set.len() > 0 { return set.clone() }
  }

  let mut w = PROGRAM_NAME.write();
  if w.len() > 0 { return w.clone() }

  let new = env::args().next().expect("expected at least 0 arguments");
  let new = match new.rsplit_once('/') {
    Some((_path,leaf)) => leaf.to_owned(),
    None => new,
  };
  let new = if new.len() > 0 { new } else { "otter".to_owned() };
  *w = new.clone();
  new
}
