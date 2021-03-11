// Copyright 2020-2021 Ian Jackson and contributors to Otter
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::prelude::*;

pub const EXIT_SPACE     : i32 =  2;
pub const EXIT_NOTFOUND  : i32 =  4;
pub const EXIT_SITUATION : i32 =  8;
pub const EXIT_USAGE     : i32 = 12;
pub const EXIT_DISASTER  : i32 = 16;

pub const DEFAULT_CONFIG_DIR       : &str = "/etc/otter";
pub const DEFAULT_CONFIG_LEAFNAME  : &str = "server.toml";
pub const DEFAULT_SENDMAIL_PROGRAM : &str = "/usr/sbin/sendmail";

pub const DAEMON_STARTUP_REPORT: &str = "otter-daemon started";
pub const LOG_ENV_VAR: &str = "OTTER_LOG";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub change_directory: Option<String>,
  pub base_dir: Option<String>,
  pub save_dir: Option<String>,
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
  pub sendmail: Option<String>,
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
  pub bundled_sources: String,
  pub shapelibs: Vec<shapelib::Config1>,
  pub sendmail: String,
  pub debug_js_inject: Arc<String>,
  pub check_bundled_sources: bool,
  pub fake_rng: RngWrap,
}

impl TryFrom<ServerConfigSpec> for WholeServerConfig {
  type Error = AE;
  #[throws(Self::Error)]
  fn try_from(spec: ServerConfigSpec) -> WholeServerConfig {
    let ServerConfigSpec {
      change_directory, base_dir, save_dir, command_socket, debug,
      http_port, public_url, sse_wildcard_url, rocket_workers,
      template_dir, nwtemplate_dir, wasm_dir,
      log, bundled_sources, shapelibs, sendmail,
      debug_js_inject_file, check_bundled_sources, fake_rng,
    } = spec;

    let fake_rng = fake_rng.start();

    if let Some(cd) = change_directory {
      env::set_current_dir(&cd)
        .context(cd)
        .context("config change_directory")?;
    }

    let defpath = |specd: Option<String>, leaf: &str| -> String {
      specd.unwrap_or_else(|| match &base_dir {
        Some(base) => format!("{}/{}", &base, &leaf),
        None       => leaf.to_owned(),
      })
    };

    let save_dir        = defpath(save_dir,        "save"              );
    let command_socket  = defpath(command_socket,  "var/command.socket");
    let template_dir    = defpath(template_dir,    "assets"            );
    let wasm_dir        = defpath(wasm_dir,        "assets"            );
    let nwtemplate_dir  = defpath(nwtemplate_dir,  "nwtemplates"       );
    let bundled_sources = defpath(bundled_sources, "bundled-sources"   );
    const DEFAULT_LIBRARY_GLOB: &str = "library/*.toml";

    let shapelibs = shapelibs.unwrap_or_else(||{
      let glob = defpath(None, DEFAULT_LIBRARY_GLOB);
      vec![ shapelib::Config1::PathGlob(glob) ]
    });

    let sendmail = sendmail.unwrap_or_else(
      || DEFAULT_SENDMAIL_PROGRAM.into()
    );

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
      template_dir, nwtemplate_dir, wasm_dir,
      bundled_sources, shapelibs, sendmail,
      debug_js_inject, check_bundled_sources, fake_rng,
    };
    WholeServerConfig {
      server: Arc::new(server),
      log,
    }
  }
}

pub fn config() -> Arc<ServerConfig> {
  GLOBAL.config.read().unwrap().server.clone()
}
pub fn log_config() -> LogSpecification {
  GLOBAL.config.read().unwrap().log.clone()
}

fn set_config(whole: WholeServerConfig) {
  *GLOBAL.config.write().unwrap() = whole;
}

impl ServerConfig {
  #[throws(StartupError)]
  pub fn read(config_filename: Option<&str>) {
    let config_filename = config_filename.map(|s| s.to_string())
      .unwrap_or_else(
        || format!("{}/{}", DEFAULT_CONFIG_DIR, DEFAULT_CONFIG_LEAFNAME)
      );
    let mut buf = String::new();
    File::open(&config_filename).with_context(||config_filename.to_string())?
      .read_to_string(&mut buf)?;
    let spec: ServerConfigSpec = toml_de::from_str(&buf)?;
    let whole = spec.try_into()?;
    set_config(whole);
  }

  #[throws(AE)]
  pub fn lock_save_area(&self) {
    let mut st = GLOBAL.save_area_lock.lock().unwrap();
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
    let st = GLOBAL.save_area_lock.lock().unwrap();
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
    spec.try_into().expect("empty spec into config")
  }
}
