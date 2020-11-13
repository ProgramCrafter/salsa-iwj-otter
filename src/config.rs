// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use crate::imports::*;

const DEFAULT_CONFIG_FILENAME : &str = "server.toml";

const DEFAULT_SAVE_DIRECTORY : &str = "save";
const DEFAULT_COMMAND_SOCKET : &str = "command.socket"; // in save dir
const DEFAULT_TEMPLATE_DIR : &str = "templates";
const DEFAULT_LIBRARY_DIR : &str = "library";
const DEFAULT_WASM_DIR : &str = "target/packed-wasm";

#[derive(Deserialize,Debug,Clone)]
pub struct ServerConfigSpec {
  pub save_directory: Option<String>,
  pub command_socket: Option<String>,
  pub debug: Option<bool>,
  pub http_port: Option<u16>,
  pub public_url: String,
  pub rocket_workers: Option<u16>,
  pub template_dir: Option<String>,
  pub wasm_dir: Option<String>,
  pub log: Option<toml::Value>,
  pub bundled_sources: Option<String>,
  pub shapelibs: Option<Vec<shapelib::Config1>>,
}

#[derive(Debug,Clone)]
pub struct ServerConfig {
  pub save_directory: String,
  pub command_socket: String,
  pub debug: bool,
  pub http_port: Option<u16>,
  pub public_url: String,
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
      save_directory, command_socket, debug,
      http_port, public_url, rocket_workers, template_dir, wasm_dir,
      log, bundled_sources, shapelibs,
    } = spec;

    let save_directory = save_directory
      .unwrap_or_else(|| DEFAULT_SAVE_DIRECTORY.to_owned());

    let mut command_socket = command_socket
      .unwrap_or_else(|| DEFAULT_COMMAND_SOCKET.to_owned());
    if !command_socket.starts_with('/') {
      command_socket = format!("{}/{}", save_directory, command_socket);
    }

    let debug = debug.unwrap_or(cfg!(debug_assertions));
    let rocket_workers = rocket_workers.unwrap_or(
      if debug { 20 } else { 1000 });

    let template_dir = template_dir
      .unwrap_or_else(|| DEFAULT_TEMPLATE_DIR.to_owned());

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

    let bundled_sources = bundled_sources
      .unwrap_or_else(|| save_directory.clone());

    let shapelibs = shapelibs.unwrap_or_else(
      ||vec![ shapelib::Config1::PathGlob(
        format!("{}/*.toml", DEFAULT_LIBRARY_DIR)
      )]);

    let wasm_dir = wasm_dir.unwrap_or_else(|| DEFAULT_WASM_DIR.to_owned());

    ServerConfig {
      save_directory, command_socket, debug,
      http_port, public_url, rocket_workers, template_dir, wasm_dir,
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
    let config_filename = config_filename
      .unwrap_or_else(|| DEFAULT_CONFIG_FILENAME);
    let mut buf = String::new();
    File::open(&config_filename).with_context(||config_filename.to_string())?
      .read_to_string(&mut buf)?;
    let config : ServerConfigSpec = toml::de::from_str(&buf)?;
    let config = config.try_into()?;
    set_config(config);
  }
}

impl Default for ServerConfig {
  fn default() -> ServerConfig {
    let spec : ServerConfigSpec = toml::de::from_str("")
      .expect("parse empty string as ServerConfigSpec");
    spec.try_into().expect("empty spec into config")
  }
}
