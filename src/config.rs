// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

pub const EXIT_SPACE     : i32 =  2;
pub const EXIT_NOTFOUND  : i32 =  4;
pub const EXIT_SITUATION : i32 =  8;
pub const EXIT_USAGE     : i32 = 12;
pub const EXIT_DISASTER  : i32 = 16;

pub const DEFAULT_CONFIG_DIR      : &str = "/etc/otter";
pub const DEFAULT_CONFIG_LEAFNAME : &str = "server.toml";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub base_dir: Option<String>,
  pub save_dir: Option<String>,
  pub command_socket: Option<String>,
  pub debug: Option<bool>,
  pub http_port: Option<u16>,
  pub public_url: String,
  pub sse_wildcard_url: Option<String>,
  pub rocket_workers: Option<u16>,
  pub template_dir: Option<String>,
  pub wasm_dir: Option<String>,
  pub log: Option<toml::Value>,
  pub bundled_sources: Option<String>,
  pub shapelibs: Option<Vec<shapelib::Config1>>,
}

#[derive(Debug,Clone)]
pub struct ServerConfig {
  pub save_dir: String,
  pub command_socket: String,
  pub debug: bool,
  pub http_port: Option<u16>,
  pub public_url: String,
  pub sse_wildcard_url: Option<(String, String)>,
  pub rocket_workers: u16,
  pub template_dir: String,
  pub wasm_dir: String,
  pub log: LogSpecification,
  pub bundled_sources: String,
  pub shapelibs: Vec<shapelib::Config1>,
}

impl TryFrom<ServerConfigSpec> for ServerConfig {
  type Error = AE;
  #[throws(Self::Error)]
  fn try_from(spec: ServerConfigSpec) -> ServerConfig {
    let ServerConfigSpec {
      base_dir, save_dir, command_socket, debug,
      http_port, public_url, sse_wildcard_url, rocket_workers,
      template_dir, wasm_dir,
      log, bundled_sources, shapelibs,
    } = spec;

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
    let bundled_sources = defpath(bundled_sources, "bundled-sources"   );
    const DEFAULT_LIBRARY_GLOB : &str = "library/*.toml";

    let shapelibs = shapelibs.unwrap_or_else(||{
      let glob = defpath(None, DEFAULT_LIBRARY_GLOB);
      vec![ shapelib::Config1::PathGlob(glob) ]
    });

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
      match log {
        None => Table(Default::default()),
        Some(log @Table(_)) => log,
        Some(x) => throw!(anyhow!(
          r#"wanted table for "log" config key, not {}"#,
          x.type_str())
        ),
      }
    };
    let log = toml::to_string(&log)?;
    let log = LogSpecification::from_toml(&log)
      .context("log specification")?;

    ServerConfig {
      save_dir, command_socket, debug,
      http_port, public_url, sse_wildcard_url, rocket_workers,
      template_dir, wasm_dir,
      log, bundled_sources, shapelibs,
    }
  }
}

pub fn config() -> Arc<ServerConfig> {
  GLOBAL.config.read().unwrap().clone()
}

fn set_config(config: ServerConfig) {
  *GLOBAL.config.write().unwrap() = Arc::new(config)
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
    let config : ServerConfigSpec = toml_de::from_str(&buf)?;
    let config = config.try_into()?;
    set_config(config);
  }
}

impl Default for ServerConfig {
  fn default() -> ServerConfig {
    let spec : ServerConfigSpec = toml_de::from_str(r#"
      public_url = "INTERNAL ERROR"
      "#)
      .expect("parse dummy config as ServerConfigSpec");
    spec.try_into().expect("empty spec into config")
  }
}
